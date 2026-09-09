library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(readxl)

source("R/helpers.R")
init_text_log("rate_sensitivity_rba.txt", overwrite = TRUE)

# Historical interest rate sensitivity from RBA policy surprises ------------
# Historical rate sensitivity as an alternative to the sector-contraction
# score this project used to build from 2022-2024 vacancy rates (a score that
# partly absorbed the AI effect it was meant to control for; that script is
# now model_eu_runup.R, which builds a pre-ChatGPT hiring run-up instead).
# Here we estimate, occupation by occupation, how online vacancies responded
# to unanticipated RBA rate cuts BEFORE the pandemic, using the Internet
# Vacancy Index (IVI, monthly from 2006).
#
# Events: the hand-curated surprise decisions in data/rba/surprise_events.csv,
# restricted to those before 2020. That leaves three unanticipated 25bp cuts
# (May 2013, Feb 2015, May 2016). The one post-2020 event in the file, the
# October 2021 yield curve control collapse, is a tightening whose window runs
# through the 2022 reopening boom; pooling it with the cuts turns the score
# into a "2022 versus the 2010s" contrast, so it is left out.
#
# Estimator: with event x state fixed effects the relative response of an
# occupation is identified from cross-occupation variation inside each window,
# so no sign variation in the shock is needed. For occupation o and event e,
#
#   y[o,s,e] = log OJA[m+h] - log OJA[m-1]  minus  log OJA[m-1] - log OJA[m-1-h]
#
# i.e. relative vacancy growth over the h months after the decision minus
# relative growth over the h months before it. Differencing against the
# pre-window removes each occupation's secular trend within the 2013-2017
# period (mining bust, NDIS roll-out) that would otherwise load onto the score.
# beta[o] is the mean of y across the three events, relative to the average
# occupation; the score is +beta (higher = expands more after an easing, i.e.
# more rate-sensitive), shrunk toward the ANZSCO 3-digit group mean, scaled 0-1.
#
# Placebo: the same estimator applied to three months with no rate decision
# (May 2014, May 2017, Mar 2018). A high correlation between the real and the
# placebo ranking would mean the score is picking up trends rather than the
# response to the cut. At h = 6 the correlation is around 0.5, driven by
# seasonal (education) hiring; at h = 12 it is close to zero, which is why the
# 12-month horizon is the headline.
#
# Broad series (foil): every cash rate change 2006-2021 in percentage points,
# occupation-specific slopes on the signed shock. Policy is endogenous (hikes
# in booms, cuts in busts), so its aggregate response has the "wrong" sign and
# the relative slopes rank counter-cyclical occupations as rate-sensitive.
# Kept for comparison only.
#
# The score is exported as a horse-race control for the Australian samples
# (native ANZSCO keys) and the EU sample (mapped ANZSCO -> ISCO-08 3-digit via
# the ABS correspondence); the race itself runs in model_horse_race.R.

t0 <- as.Date("2022-11-30") # chatgpt release date
delta_window_start <- as.Date("2021-10-01") # as in model_aus.R
max_end_month <- as.Date("2022-11-01") # last outcome month before ChatGPT
horizons <- c(6, 12, 18)
headline_h <- 12
min_base_oja <- 5 # drop occupation x state cells too thin to log-difference

exposure_vars <- c(
  "Anthropic Usage Score" = "anthropic_usage_score",
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score"
)
results <- list()

# RBA shocks ---------------------------------------------------------------
surprise_events <- read_csv("data/rba/surprise_events.csv", show_col_types = FALSE) %>%
  filter(date < as.Date("2020-01-01")) %>%
  transmute(month = floor_date(date, "month"), shock = direction, event)
stopifnot(all(surprise_events$shock < 0)) # the estimator below assumes easings only

placebo_events <- tibble(
  month = as.Date(c("2014-05-01", "2017-05-01", "2018-03-01")),
  shock = -1,
  event = "placebo (no decision)"
)

broad_events <- read_csv(
  "data/rba/cash_rate_decisions.csv", col_types = cols(.default = col_character())
) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2006-01-01"), date < as.Date("2022-01-01")) %>%
  mutate(change_pp = as.numeric(change_pp), month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(shock = sum(change_pp), .groups = "drop") %>%
  filter(shock != 0, month != as.Date("2020-03-01")) %>%
  mutate(event = paste0(format(month, "%b %Y"), " (", sprintf("%+.2f", shock), "pp)"))

shock_series <- list(surprise = surprise_events, placebo = placebo_events, broad = broad_events)

log_text(surprise_events, "Surprise easings before 2020 (headline series):")
log_text(placebo_events, "Placebo months (no rate decision):")
log_text(broad_events, "All cash rate changes 2006-2021 (percentage points; Mar 2020 dropped):", n = Inf)

# IVI monthly panel ----------------------------------------------------------
# Same read as model_aus.R (read_ivi in helpers.R), keeping the full 2006+
# monthly series.
ivi_long <- read_ivi()

anzsco_titles <- ivi_long %>% distinct(anzsco_4digit, anzsco_title)

# Local projection frame: one row per (event, occupation, state). The base
# month is m-1 so the 3-month moving average is entirely pre-decision; the
# pre-window runs the same h months back from the base month.
build_lp_frame <- function(shocks, h, require_pre = TRUE) {
  ivi_at <- function(name) {
    ivi_long %>% select(anzsco_4digit, state, !!name := month, oja = OJA)
  }
  shocks %>%
    crossing(ivi_long %>% distinct(anzsco_4digit, state)) %>%
    mutate(
      base_month = month %m-% months(1),
      end_month = month %m+% months(h),
      pre_month = base_month %m-% months(h)
    ) %>%
    filter(end_month <= max_end_month) %>%
    left_join(ivi_at("base_month") %>% rename(oja_base = oja), by = c("anzsco_4digit", "state", "base_month")) %>%
    left_join(ivi_at("end_month") %>% rename(oja_end = oja), by = c("anzsco_4digit", "state", "end_month")) %>%
    left_join(ivi_at("pre_month") %>% rename(oja_pre = oja), by = c("anzsco_4digit", "state", "pre_month")) %>%
    filter(!is.na(oja_base), !is.na(oja_end), oja_base >= min_base_oja) %>%
    filter(!require_pre | !is.na(oja_pre)) %>%
    mutate(
      dlog = log(oja_end + 1) - log(oja_base + 1),
      dlog_pre = log(oja_base + 1) - log(oja_pre + 1),
      ddlog = dlog - dlog_pre,
      event_state = paste(month, state)
    )
}

# Relative response, easing-only series: demean the differenced outcome within
# each event x state cell (weighted by base-period vacancies, so thin cells do
# not dominate), then one coefficient per occupation. Dropping the intercept
# keeps every occupation rather than treating one as the reference.
estimate_relative_response <- function(lp, outcome = "ddlog") {
  lp <- lp %>%
    group_by(event_state) %>%
    mutate(y_dm = .data[[outcome]] - weighted.mean(.data[[outcome]], oja_base)) %>%
    ungroup()
  m <- feols(
    y_dm ~ 0 + i(anzsco_4digit),
    data = lp, weights = ~oja_base, cluster = ~ anzsco_4digit^state
  )
  tidy(m) %>%
    transmute(
      anzsco_4digit = str_remove(term, "^anzsco_4digit::"),
      beta = estimate, se = std.error
    )
}

# Occupation-specific slopes on the signed shock, broad series (as before).
estimate_shock_slope <- function(lp, outcome = "dlog") {
  m <- feols(
    as.formula(paste(outcome, "~ i(anzsco_4digit, shock) | event_state")),
    data = lp, weights = ~oja_base, cluster = ~ anzsco_4digit^state
  )
  tidy(m) %>%
    transmute(
      anzsco_4digit = str_remove(term, "^anzsco_4digit::") %>% str_remove(":shock$"),
      beta = estimate, se = std.error
    )
}

# Aggregate checks. For the easing series there is no sign variation, so the
# check is simply whether vacancies grew, and whether growth accelerated, after
# the cuts, against zero and against the placebo months. Expect weak results:
# the cuts were surprises in timing, not in motive, so the year after each is
# a weak year. The event x state fixed effects remove this aggregate entirely,
# so it does not enter the score. For the broad series the check is the slope
# on the signed shock with state fixed effects, which comes out positive
# (vacancies rise after hikes) because policy is endogenous.
aggregate_check_easing <- function(lp_easing, lp_placebo) {
  both <- bind_rows(easing = lp_easing, placebo = lp_placebo, .id = "type")
  map_dfr(c("dlog", "ddlog"), function(y) {
    m0 <- feols(
      as.formula(paste(y, "~ 1")), # just weighted mean across occupation-state-event pairs
      data = lp_easing, weights = ~oja_base, cluster = ~event_state
    )
    m1 <- feols(
      as.formula(paste(y, "~ i(type, ref = 'placebo') | state")), # nets out state effects
      data = both, weights = ~oja_base, cluster = ~event_state
    )
    tibble(
      outcome = y,
      easing_mean = coef(m0)[[1]], easing_p = pvalue(m0)[[1]],
      easing_minus_placebo = coef(m1)[[1]], diff_p = pvalue(m1)[[1]]
    )
  })
}

aggregate_check_broad <- function(lp) {
  m <- feols(dlog ~ shock | state, data = lp, weights = ~oja_base, cluster = ~event_state)
  tidy(m) %>% filter(term == "shock") %>% select(estimate, std.error, p.value)
}

# Empirical-Bayes style shrinkage of the 4-digit slope toward its ANZSCO
# 3-digit group mean, weight = signal variance / (signal + noise variance).
shrink_to_group <- function(est) {
  tau2 <- max(var(est$beta) - mean(est$se^2), 0)
  est %>%
    mutate(anzsco_3digit = substr(anzsco_4digit, 1, 3)) %>%
    group_by(anzsco_3digit) %>%
    mutate(beta_group = mean(beta)) %>%
    ungroup() %>%
    mutate(
      w = tau2 / (tau2 + se^2),
      beta_shrunk = w * beta + (1 - w) * beta_group
    ) %>%
    select(-anzsco_3digit)
}

# estimate for each series and horizon ----------------------------------------
grid <- expand_grid(series = names(shock_series), h = horizons)

# the broad series is estimated on dlog, so it does not need the pre-window
lp_frames <- pmap(grid, function(series, h) {
  build_lp_frame(shock_series[[series]], h, require_pre = series != "broad")
})
names(lp_frames) <- paste0(grid$series, "_h", grid$h)

lp_summary <- imap_dfr(lp_frames, ~ tibble(
  spec = .y,
  n_events = n_distinct(.x$month),
  n_obs = nrow(.x),
  n_occupations = n_distinct(.x$anzsco_4digit)
))
log_text(lp_summary, "Local projection samples:")

log_text(
  map_dfr(horizons, ~ aggregate_check_easing(
    lp_frames[[paste0("surprise_h", .x)]], lp_frames[[paste0("placebo_h", .x)]]
  ) %>% mutate(h = .x, .before = 1)),
  paste(
    "Aggregate check, surprise easings: vacancy growth (dlog) and acceleration (ddlog)",
    "in the h months after the cuts, vs zero and vs the placebo months:"
  ),
  n = Inf
)
log_text(
  map_dfr(horizons, ~ aggregate_check_broad(lp_frames[[paste0("broad_h", .x)]]) %>% mutate(h = .x, .before = 1)),
  "Aggregate check, broad series: dlog on signed shock (pp), state FE, cluster event x state:"
)

# series -> (estimator, sign of the score). Score is always oriented so that
# higher = vacancies respond more to a tightening.
series_spec <- list(
  surprise = list(estimate = estimate_relative_response, sign = +1),
  placebo = list(estimate = estimate_relative_response, sign = +1),
  broad = list(estimate = estimate_shock_slope, sign = -1)
)

sensitivity_long <- imap_dfr(lp_frames, function(lp, spec) {
  series <- str_remove(spec, "_h\\d+$")
  series_spec[[series]]$estimate(lp) %>%
    shrink_to_group() %>%
    mutate(spec = spec, series = series, .before = 1)
})

sensitivity_wide <- sensitivity_long %>%
  mutate(score = map_dbl(series, ~ series_spec[[.x]]$sign) * beta_shrunk) %>%
  select(anzsco_4digit, spec, score) %>%
  group_by(spec) %>%
  mutate(score = scale_zero_to_one(score)) %>%
  ungroup() %>%
  pivot_wider(names_from = spec, values_from = score, names_prefix = "rs_")

headline_cols <- c(
  rate_sens_rba = paste0("rs_surprise_h", headline_h),
  rate_sens_rba_broad = paste0("rs_broad_h", headline_h)
)

occupation_rate_sensitivity <- sensitivity_wide %>%
  mutate(
    rate_sens_rba = .data[[headline_cols["rate_sens_rba"]]],
    rate_sens_rba_broad = .data[[headline_cols["rate_sens_rba_broad"]]]
  ) %>%
  left_join(anzsco_titles, by = "anzsco_4digit") %>%
  relocate(anzsco_4digit, anzsco_title)

log_text(
  sensitivity_long %>%
    group_by(spec) %>%
    summarise(
      mean_se = mean(se), sd_beta = sd(beta), sd_beta_shrunk = sd(beta_shrunk),
      mean_shrink_w = mean(w), .groups = "drop"
    ),
  "Slope dispersion and shrinkage by specification:"
)

log_text(
  map_dfr(horizons, function(h) {
    tibble(
      h = h,
      spearman_surprise_vs_placebo = cor(
        occupation_rate_sensitivity[[paste0("rs_surprise_h", h)]],
        occupation_rate_sensitivity[[paste0("rs_placebo_h", h)]],
        method = "spearman", use = "pairwise.complete.obs"
      )
    )
  }),
  "Placebo check: rank correlation of the surprise score with the same estimator on no-decision months:"
)

log_text(
  occupation_rate_sensitivity %>%
    select(starts_with("rs_surprise"), starts_with("rs_broad")) %>%
    cor(method = "spearman", use = "pairwise.complete.obs") %>%
    round(2),
  "Rank correlation across series and horizons:"
)

log_text(
  occupation_rate_sensitivity %>%
    filter(!is.na(rate_sens_rba)) %>%
    arrange(desc(rate_sens_rba)) %>%
    select(anzsco_4digit, anzsco_title, rate_sens_rba, rate_sens_rba_broad) %>%
    slice(c(1:15, (n() - 14):n())),
  "Most and least rate-sensitive occupations (surprise easings, 12-month horizon, differenced):",
  n = Inf
)

# Correlation with AI exposure on the horse-race samples ---------------------
# The race runs in model_horse_race.R; here only the score's own diagnostics
# on those samples, then the export of the score as a control.
aus_delta_l4 <- build_aus_delta(4, t0, delta_window_start, ivi_long = ivi_long) %>%
  left_join(
    occupation_rate_sensitivity %>% select(anzsco_4digit, rate_sens_rba, rate_sens_rba_broad),
    by = "anzsco_4digit"
  ) %>%
  filter(!is.na(rate_sens_rba))

log_text(
  aus_delta_l4 %>%
    distinct(anzsco_4digit, across(all_of(unname(exposure_vars))), rate_sens_rba, rate_sens_rba_broad) %>%
    summarise(across(
      all_of(unname(exposure_vars)),
      list(surprise = ~ cor(.x, rate_sens_rba, use = "complete.obs", method = "spearman"),
           broad = ~ cor(.x, rate_sens_rba_broad, use = "complete.obs", method = "spearman"))
    )) %>%
    pivot_longer(everything(), names_to = c("exposure", "series"), names_pattern = "(.*)_(surprise|broad)$") %>%
    pivot_wider(names_from = series, values_from = value),
  "Correlation between RBA rate sensitivity and AI exposure (ANZSCO 4-digit, ISCO-4 exposure):"
)

# EU: map the ANZSCO score onto ISCO-08 3-digit groups. Each ISCO-3 group gets
# the mean score of the ANZSCO 4-digit occupations the ABS correspondence maps
# into it (mirror of build_anzsco_exposure).
isco3_rate_sensitivity <- read_anzsco_correspondence() %>%
  transmute(anzsco_4digit, isco_level_3 = substr(isco08_4digit, 1, 3)) %>%
  distinct() %>%
  inner_join(
    occupation_rate_sensitivity %>% select(anzsco_4digit, rate_sens_rba, rate_sens_rba_broad),
    by = "anzsco_4digit"
  ) %>%
  group_by(isco_level_3) %>%
  summarise(
    rate_sens_rba = mean(rate_sens_rba),
    rate_sens_rba_broad = mean(rate_sens_rba_broad),
    n_anzsco = n(),
    .groups = "drop"
  ) %>%
  mutate(across(c(rate_sens_rba, rate_sens_rba_broad), scale_zero_to_one))

eu_delta <- build_eu_delta(t0) %>%
  left_join(isco3_rate_sensitivity, by = "isco_level_3") %>%
  filter(!is.na(rate_sens_rba))

log_text(
  eu_delta %>%
    distinct(isco_level_3, across(all_of(unname(exposure_vars))), rate_sens_rba, rate_sens_rba_broad) %>%
    summarise(across(
      all_of(unname(exposure_vars)),
      list(surprise = ~ cor(.x, rate_sens_rba, use = "complete.obs"),
           broad = ~ cor(.x, rate_sens_rba_broad, use = "complete.obs"))
    )) %>%
    pivot_longer(everything(), names_to = c("exposure", "series"), names_pattern = "(.*)_(surprise|broad)$") %>%
    pivot_wider(names_from = series, values_from = value),
  "Correlation between RBA rate sensitivity and AI exposure (ISCO-3 occupations, EU sample):"
)

# export as horse-race controls ------------------------------------------------
rba_controls <- c("rate_sens_rba", "rate_sens_rba_broad")
for (s in c("aus_l3", "aus_l4")) {
  write_controls(
    occupation_rate_sensitivity %>% filter(!is.na(rate_sens_rba)),
    sample = s, key = "anzsco_4digit", controls = rba_controls,
    file = paste0("rate_sensitivity_rba_", s)
  )
}
write_controls(
  isco3_rate_sensitivity, sample = "eu_l3", key = "isco_level_3",
  controls = rba_controls, file = "rate_sensitivity_rba_eu_l3"
)

# save --------------------------------------------------------------------------
results$shock_series <- shock_series
results$lp_summary <- lp_summary
results$sensitivity_long <- sensitivity_long
results$occupation_rate_sensitivity <- occupation_rate_sensitivity
results$isco3_rate_sensitivity <- isco3_rate_sensitivity

saveRDS(results, "results/RDS/rate_sensitivity_rba.RDS")
write_csv(
  occupation_rate_sensitivity,
  "results/intermediate_datasets/rate_sensitivity_rba_anzsco.csv"
)
