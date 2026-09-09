library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(readxl)

source("R/helpers.R")
init_text_log("horse_race.txt", overwrite = TRUE)

# Unified horse race ------------------------------------------------------------
# Every "is the AI exposure gradient really X?" check in the project runs the
# same delta specification with one or more controls added:
#
#   log(OJA_post / OJA_pre)_{c,o} = a_c + b * exposure_o + g' controls_{c,o} + e
#
# with country (state) fixed effects and standard errors clustered by country.
# The controls are built by their own scripts and exported to
# results/controls/*.csv in a common long format (see write_controls() in
# helpers.R):
#
#   rate_sens_rba, rate_sens_rba_broad   model_rate_sensitivity_rba.R
#   wfh_teleworkable                     wfh_correlation.R
#   runup_from_2019, runup_from_2020,    model_eu_runup.R (EU, country x ISCO-2)
#   runup_from_2019_{vac,mix,pooled}     model_aus_runup.R (AUS, state x ANZSCO-4)
#
# This script builds the three samples once (EU ISCO-3; Australia keyed to
# ISCO-3 and ISCO-4 exposure), attaches all controls, and runs the grid in
# `control_sets` below. For each set, on the sample where every control in
# the set is observed: controls alone, each exposure measure alone
# (baseline), and exposure plus the set. Everything lands in one tidy table
# (results/intermediate_datasets/horse_race_summary.csv) and in the compact
# matrices logged at the end. To add a check, export a control and add it to
# `control_sets`; to rerun everything, Rscript R/run_horse_races.R.

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "Anthropic Usage Score" = "anthropic_usage_score",
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score"
)
exposure_short <- c(
  anthropic_usage_score = "Anthropic", ai_product_exposure_score = "Demirev",
  beta_eloundou = "Eloundou", felten_exposure_score = "Felten",
  webb_exposure_score = "Webb"
)

aus_sets <- list(
  "rate_sens_rba", "rate_sens_rba_broad", "wfh_teleworkable",
  "runup_from_2019", "runup_from_2020",
  c("wfh_teleworkable", "rate_sens_rba"),
  c("wfh_teleworkable", "runup_from_2019"),
  c("rate_sens_rba", "runup_from_2019"),
  c("wfh_teleworkable", "rate_sens_rba", "runup_from_2019")
)
control_sets <- list(
  eu_l3 = list(
    "rate_sens_rba", "rate_sens_rba_broad", "wfh_teleworkable",
    "runup_from_2019", "runup_from_2020", "runup_from_2019_vac",
    "runup_from_2019_mix", "runup_from_2019_pooled",
    c("wfh_teleworkable", "rate_sens_rba"),
    c("wfh_teleworkable", "runup_from_2019_mix"),
    c("rate_sens_rba", "runup_from_2019_mix"),
    c("wfh_teleworkable", "rate_sens_rba", "runup_from_2019_mix")
  ),
  aus_l3 = aus_sets,
  aus_l4 = aus_sets
)

# samples ------------------------------------------------------------------------
ivi_long <- read_ivi()
samples <- list(
  eu_l3 = build_eu_delta(t0),
  aus_l3 = build_aus_delta(3, t0, ivi_long = ivi_long),
  aus_l4 = build_aus_delta(4, t0, ivi_long = ivi_long)
)

controls <- read_controls()

log_text(
  controls %>%
    group_by(sample, control, key, by_country = !is.na(idcountry)) %>%
    summarise(n_values = n(), .groups = "drop") %>%
    arrange(sample, control),
  "Controls found in results/controls:",
  n = Inf
)

# join each (key, country-specific or not) block of controls onto a sample
attach_controls <- function(dat, ctrl) {
  blocks <- ctrl %>%
    mutate(by_country = !is.na(idcountry)) %>%
    group_split(key, by_country)
  for (block in blocks) {
    key <- block$key[1]
    wide <- block %>%
      select(idcountry, occupation_id, control, value) %>%
      pivot_wider(names_from = control, values_from = value)
    dat <- if (block$by_country[1]) {
      left_join(dat, wide, by = c("idcountry", setNames("occupation_id", key)))
    } else {
      left_join(dat, select(wide, -idcountry), by = setNames("occupation_id", key))
    }
  }
  dat
}

samples <- imap(samples, ~ attach_controls(.x, filter(controls, sample == .y)))

log_text(
  imap_dfr(samples, function(dat, nm) {
    ctrls <- unique(controls$control[controls$sample == nm])
    tibble(
      sample = nm, n_obs = nrow(dat),
      control = ctrls,
      n_with_control = map_int(ctrls, ~ sum(!is.na(dat[[.x]])))
    )
  }),
  "Sample sizes and coverage of each control:",
  n = Inf
)

# race ---------------------------------------------------------------------------
run_delta <- function(rhs, dat) {
  feols(
    as.formula(paste("delta_OJA_log ~", rhs, "| idcountry")),
    data = dat, cluster = "idcountry"
  )
}

run_control_set <- function(dat, ctrls, sample_name) {
  missing <- setdiff(ctrls, names(dat))
  if (length(missing) > 0) {
    message("[", sample_name, "] skipping ", paste(ctrls, collapse = " + "), ": no control ", paste(missing, collapse = ", "))
    return(NULL)
  }
  set_name <- paste(ctrls, collapse = " + ")
  d <- dat %>% filter(if_all(all_of(ctrls), ~ !is.na(.x)))

  controls_only <- run_delta(set_name, d)
  baseline <- map(exposure_vars, ~ run_delta(.x, d))
  race <- map(exposure_vars, ~ run_delta(paste(.x, "+", set_name), d))

  summary <- map2_dfr(baseline, race, function(b, r) {
    v <- names(coef(b))[1]
    tibble(
      sample = sample_name, control_set = set_name,
      n_obs = nrow(d), n_occ = n_distinct(d$occupation_id),
      exposure = v,
      baseline = coef(b)[v], baseline_p = pvalue(b)[v],
      with_controls = coef(r)[v], with_controls_p = pvalue(r)[v]
    ) %>%
      mutate(pct_attenuation = (1 - with_controls / baseline) * 100)
  })

  control_coefs <- bind_rows(
    tibble(exposure = "(controls only)", control = ctrls, coef = coef(controls_only)[ctrls], p = pvalue(controls_only)[ctrls]),
    map_dfr(race, function(r) {
      tibble(exposure = names(coef(r))[1], control = ctrls, coef = coef(r)[ctrls], p = pvalue(r)[ctrls])
    })
  ) %>%
    mutate(sample = sample_name, control_set = set_name, n_obs = nrow(d), .before = 1)

  list(
    summary = summary, control_coefs = control_coefs,
    models = list(controls_only = controls_only, baseline = baseline, race = race)
  )
}

races <- imap(control_sets, function(sets, sample_name) {
  out <- map(sets, ~ run_control_set(samples[[sample_name]], .x, sample_name))
  names(out) <- map_chr(sets, paste, collapse = " + ")
  compact(out)
})

summary_table <- map_dfr(races, ~ map_dfr(.x, "summary"))
control_table <- map_dfr(races, ~ map_dfr(.x, "control_coefs"))

# compact views ------------------------------------------------------------------
stars <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")

for (s in names(races)) {
  st <- summary_table %>% filter(sample == s)
  if (nrow(st) == 0) next

  log_text(
    st %>%
      transmute(
        control_set, n_obs,
        exposure = exposure_short[exposure],
        # attenuation is only meaningful when there is a gradient to attenuate
        cell = ifelse(
          baseline_p < 0.05,
          sprintf("%.3f%s [%.0f%%]", with_controls, stars(with_controls_p), pct_attenuation),
          sprintf("%.3f%s [.]", with_controls, stars(with_controls_p))
        )
      ) %>%
      pivot_wider(names_from = exposure, values_from = cell) %>%
      as.data.frame(),
    paste0(
      "[", s, "] Exposure coefficient with controls, stars (* p<0.05, ** p<0.01, *** p<0.001), ",
      "[% attenuation vs exposure alone on the same sample; . if exposure alone is not significant]:"
    )
  )

  log_text(
    st %>%
      transmute(
        control_set, n_obs,
        exposure = exposure_short[exposure],
        cell = sprintf("%.3f%s", baseline, stars(baseline_p))
      ) %>%
      pivot_wider(names_from = exposure, values_from = cell) %>%
      as.data.frame(),
    paste0("[", s, "] Exposure alone on each control set's sample (baseline):")
  )

  log_text(
    control_table %>%
      filter(sample == s, exposure == "(controls only)") %>%
      transmute(control_set, control, cell = sprintf("%.3f%s", coef, stars(p))) %>%
      pivot_wider(names_from = control, values_from = cell, values_fill = "") %>%
      as.data.frame(),
    paste0("[", s, "] Controls alone (no exposure):")
  )
}

# full model printouts, for the record
for (s in names(races)) {
  for (set_name in names(races[[s]])) {
    log_text(races[[s]][[set_name]]$models$race, paste0("[", s, "] exposure + ", set_name, ":"))
  }
}

# save ----------------------------------------------------------------------------
write_csv(summary_table, "results/intermediate_datasets/horse_race_summary.csv")
write_csv(control_table, "results/intermediate_datasets/horse_race_controls.csv")
saveRDS(
  list(control_sets = control_sets, summary = summary_table, control_coefs = control_table, races = races),
  "results/RDS/horse_race.RDS"
)
