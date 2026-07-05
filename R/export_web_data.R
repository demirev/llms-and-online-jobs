# Export the data behind the interactive web dashboard (web/index.html).
# Re-estimates the headline delta and event-study models with the same helpers
# and data preparation as R/model_oja.R and R/model_aus.R (so the numbers match
# the paper), then writes a compact JSON payload that the dashboard build step
# inlines into a single self-contained HTML file.
#
# Output: results/web/web_data.json

library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(readxl)
library(jsonlite)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_cols <- c(
  "ai_product_exposure_score", "ai_product_automation_score",
  "ai_product_augmentation_score", "felten_exposure_score",
  "webb_exposure_score", "beta_eloundou", "anthropic_usage_score",
  "anthropic_automation_score", "anthropic_augmentation_score"
)

# one row per exposure measure; auto/augm columns only where the measure has
# an automation vs augmentation breakdown
index_meta <- tribble(
  ~key,        ~col,                        ~label,                                    ~short,
  "demirev",   "ai_product_exposure_score", "Demirev (2024) AI product exposure",      "Demirev",
  "felten",    "felten_exposure_score",     "Felten et al. (2018) AI exposure",        "Felten",
  "webb",      "webb_exposure_score",       "Webb (2022) AI exposure",                 "Webb",
  "eloundou",  "beta_eloundou",             "Eloundou et al. (2023) GPT exposure",     "Eloundou",
  "anthropic", "anthropic_usage_score",     "Handa et al. (2025) Anthropic usage",     "Anthropic"
) %>%
  mutate(
    auto_col = c("ai_product_automation_score", NA, NA, NA, "anthropic_automation_score"),
    augm_col = c("ai_product_augmentation_score", NA, NA, NA, "anthropic_augmentation_score")
  )

# a couple of CEDEFOP occupation labels arrive with a mojibaked apostrophe
fix_name <- function(x) str_replace_all(x, fixed("‚Äô"), "’")

r3 <- function(x) round(x, 3)
r4 <- function(x) round(x, 4)
qlab <- function(d) paste0(year(d), "Q", quarter(d))

# read and format EU data (mirrors model_oja.R) -------------------------------
oja_eu <- list.files(
  "data/cedefop_skills_ovate_skill_demand/csv/05_occupation_skill_across_occupations_hyper",
  full.names = TRUE
) %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  mutate(
    idcountry = ifelse(is.na(idcountry), countryset, idcountry),
    esco_level_3_short = esco_level_3
  ) %>%
  select(-c(countryset, esco_level_3)) %>%
  filter(!str_detect(idcountry, "EU27"))

ai_exposure <- list(
  l3 = read_ai_exposure_file("data/ai_exposure_scores/scored_esco_occupations_matched.csv", level = 3),
  l4 = read_ai_exposure_file("data/ai_exposure_scores/scored_esco_occupations_matched.csv", level = 4)
)

eu_fmt <- format_twfe_oja_data(oja_eu, ai_exposure$l3, level = 3, t0 = t0)

eu_delta <- format_delta_data(
  eu_fmt, n_periods = Inf, base_date = t0, level = 3, across_countries = FALSE
) %>%
  filter(pre_OJA > 20 & post_OJA > 20)

# read and format Australian data (mirrors model_aus.R) -----------------------
event_window_start <- as.Date("2016-01-01")
delta_window_start <- as.Date("2021-10-01")

build_anzsco_exposure <- function(correspondence, ai_exposure, isco_level) {
  isco_col <- paste0("isco_level_", isco_level)

  correspondence %>%
    transmute(
      anzsco_4digit,
      isco_key = substr(isco08_4digit, 1, isco_level)
    ) %>%
    distinct() %>%
    left_join(ai_exposure, by = setNames(isco_col, "isco_key")) %>%
    group_by(anzsco_4digit) %>%
    summarise(
      across(all_of(exposure_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

format_aus_oja <- function(oja_long, anzsco_exposure, level, t0) {
  id_col <- paste0("idesco_level_", level)
  short_col <- paste0("esco_level_", level, "_short")

  oja_long %>%
    left_join(anzsco_exposure, by = "anzsco_4digit") %>%
    filter(!is.na(ai_product_exposure_score)) %>%
    mutate(
      idcountry = state,
      !!id_col := paste0("OC", anzsco_4digit),
      !!short_col := anzsco_title,
      post_chatgpt = ifelse(dmax >= t0, 1, 0),
      log_OJA = log(OJA + 1),
      country_occupation_pair = paste0(state, "_", anzsco_4digit),
      across(all_of(exposure_cols), scale_zero_to_one),
      event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
    )
}

ivi_path <- file.path(
  "data/aus",
  "internet_vacancies_anzsco4_occupations_states_and_territories_-_may_2026.xlsx"
)

ivi_long <- read_excel(ivi_path, sheet = "4 digit 3 month average", col_types = "text") %>%
  rename(anzsco_4digit = ANZSCO_CODE, anzsco_title = ANZSCO_TITLE) %>%
  filter(anzsco_4digit != "0", state != "AUST") %>%
  pivot_longer(
    cols = -c(anzsco_4digit, anzsco_title, state),
    names_to = "date_serial", values_to = "OJA"
  ) %>%
  mutate(
    OJA = suppressWarnings(as.numeric(OJA)),
    date = as.Date(as.numeric(date_serial), origin = "1899-12-30")
  ) %>%
  filter(!is.na(OJA), date >= event_window_start)

oja_q <- ivi_long %>%
  mutate(dmax = ceiling_date(date, "quarter") - days(1)) %>%
  group_by(anzsco_4digit, anzsco_title, state, dmax) %>%
  summarise(OJA = mean(OJA, na.rm = TRUE), .groups = "drop")

correspondence <- read_csv(
  "data/aus/anzsco_isco08_correspondence.csv",
  col_types = cols(.default = col_character())
)

anzsco_exposure <- list(
  l3 = build_anzsco_exposure(correspondence, ai_exposure$l3, isco_level = 3),
  l4 = build_anzsco_exposure(correspondence, ai_exposure$l4, isco_level = 4)
)

aus_fmt <- list(
  l3 = format_aus_oja(oja_q, anzsco_exposure$l3, level = 3, t0 = t0),
  l4 = format_aus_oja(oja_q, anzsco_exposure$l4, level = 4, t0 = t0)
)

aus_delta <- list(
  l3 = format_delta_data(
    aus_fmt$l3 %>% filter(dmax >= delta_window_start),
    n_periods = Inf, base_date = t0, level = 3, across_countries = FALSE
  ) %>% filter(pre_OJA > 20 & post_OJA > 20),
  l4 = format_delta_data(
    aus_fmt$l4 %>% filter(dmax >= delta_window_start),
    n_periods = Inf, base_date = t0, level = 4, across_countries = FALSE
  ) %>% filter(pre_OJA > 20 & post_OJA > 20)
)

# estimation ------------------------------------------------------------------
fit_delta <- function(delta_df, col) {
  m <- feols(
    as.formula(paste("delta_OJA_log ~", col, "| idcountry")),
    data = delta_df, cluster = "idcountry"
  )
  td <- tidy(m) %>% filter(term == col)
  list(
    est = r4(td$estimate), se = r4(td$std.error), p = signif(td$p.value, 3),
    n = unname(m$nobs), r2w = r4(unname(fixest::r2(m, "wr2")))
  )
}

fit_delta_pair <- function(delta_df, auto_col, augm_col) {
  m <- feols(
    as.formula(paste("delta_OJA_log ~", auto_col, "+", augm_col, "| idcountry")),
    data = delta_df, cluster = "idcountry"
  )
  td <- tidy(m)
  pick <- function(col) {
    row <- td %>% filter(term == col)
    list(est = r4(row$estimate), se = r4(row$std.error), p = signif(row$p.value, 3))
  }
  list(auto = pick(auto_col), augm = pick(augm_col), n = unname(m$nobs))
}

fit_event <- function(fmt_df, col, level) {
  m <- run_event_study_model(col, fmt_df, level = level)
  cf <- extract_event_study_coefs(m, col)
  list(t = cf$event_time, est = r4(cf$estimate), se = r4(cf$std.error))
}

# per-country means of the outcome and each (non-missing) exposure score over
# the delta estimation sample: the dashboard uses these to reproduce the
# partial-regression (within-country demeaned) scatter client-side
country_means <- function(delta_df, countries) {
  set_names(index_meta$key) %>%
    map(function(key) {
      col <- index_meta$col[index_meta$key == key]
      cm <- delta_df %>%
        filter(!is.na(.data[[col]])) %>%
        group_by(idcountry) %>%
        summarise(
          mx = mean(.data[[col]]),
          my = mean(delta_OJA_log),
          .groups = "drop"
        )
      cm <- cm[match(countries, cm$idcountry), ]
      list(mx = r4(cm$mx), my = r4(cm$my))
    })
}

build_estimates <- function(delta_df, fmt_df, level, occ_codes, countries) {
  id_col <- paste0("idesco_level_", level)

  delta_models <- set_names(index_meta$key) %>%
    map(~ fit_delta(delta_df, index_meta$col[index_meta$key == .x]))

  bmeta <- index_meta %>% filter(!is.na(auto_col))
  breakdown <- set_names(bmeta$key) %>%
    map(function(k) {
      row <- bmeta[bmeta$key == k, ]
      fit_delta_pair(delta_df, row$auto_col, row$augm_col)
    })

  event <- set_names(index_meta$key) %>%
    map(~ fit_event(fmt_df, index_meta$col[index_meta$key == .x], level))

  # scatter: one point per country-occupation pair, referencing the panel
  # occupation and country arrays by 0-based index
  sc <- delta_df %>%
    mutate(
      occ = match(.data[[id_col]], occ_codes) - 1L,
      ctry = match(idcountry, countries) - 1L
    ) %>%
    filter(!is.na(occ))

  list(
    delta = delta_models,
    deltaBreakdown = breakdown,
    event = event,
    scatter = list(occ = sc$occ, ctry = sc$ctry, y = r3(sc$delta_OJA_log)),
    countryMeans = country_means(delta_df, countries)
  )
}

# panels (occupation-level explorer data) --------------------------------------
# quarterly postings per occupation summed across countries/states, indexed to
# 100 = average of the four quarters preceding the ChatGPT release
index_series <- function(fmt_df, id_col, quarters) {
  ser <- fmt_df %>%
    group_by(code = .data[[id_col]], dmax, event_time) %>%
    summarise(OJA = sum(OJA), .groups = "drop")

  base <- ser %>%
    filter(event_time %in% -4:-1) %>%
    group_by(code) %>%
    summarise(base = mean(OJA), .groups = "drop")

  ser %>%
    inner_join(base, by = "code") %>%
    filter(base > 0) %>%
    mutate(value = round(100 * OJA / base, 1)) %>%
    select(code, dmax, value) %>%
    group_by(code) %>%
    group_map(~ {
      v <- .x$value[match(quarters, .x$dmax)]
      list(code = .y$code, series = v)
    }) %>%
    set_names(map_chr(., "code"))
}

occ_scores <- function(fmt_df, id_col) {
  fmt_df %>%
    distinct(code = .data[[id_col]], across(all_of(exposure_cols))) %>%
    group_by(code) %>%
    slice(1) %>%
    ungroup()
}

occ_deltas <- function(delta_df, id_col) {
  delta_df %>%
    group_by(code = .data[[id_col]]) %>%
    summarise(delta = r3(mean(delta_OJA_log)), .groups = "drop")
}

scores_list <- function(scores_row, col_field) {
  set_names(index_meta$key) %>%
    map(function(key) {
      col <- index_meta[[col_field]][index_meta$key == key]
      if (is.na(col)) return(NULL)
      v <- scores_row[[col]]
      if (is.null(v) || length(v) == 0 || is.na(v)) NULL else r3(v)
    }) %>%
    compact()
}

avg_series <- function(fmt_df, quarters) {
  tot <- fmt_df %>%
    group_by(dmax, event_time) %>%
    summarise(OJA = sum(OJA), .groups = "drop")
  base <- mean(tot$OJA[tot$event_time %in% -4:-1])
  round(100 * tot$OJA[match(quarters, tot$dmax)] / base, 1)
}

# EU panel
eu_countries <- sort(unique(eu_delta$idcountry))
eu_quarters <- sort(unique(eu_fmt$dmax))
eu_occ_codes <- sort(unique(eu_delta$idesco_level_3))

eu_names <- eu_fmt %>% distinct(idesco_level_3, esco_level_3_short)
eu_series <- index_series(
  eu_fmt %>% filter(idesco_level_3 %in% eu_occ_codes), "idesco_level_3", eu_quarters
)
eu_sc <- occ_scores(eu_fmt, "idesco_level_3")
eu_dl <- occ_deltas(eu_delta, "idesco_level_3")

eu_occupations <- map(eu_occ_codes, function(code) {
  srow <- eu_sc %>% filter(code == !!code)
  list(
    code = str_remove(code, "^OC"),
    name = fix_name(eu_names$esco_level_3_short[match(code, eu_names$idesco_level_3)]),
    scores = scores_list(srow, "col"),
    auto = scores_list(srow, "auto_col"),
    augm = scores_list(srow, "augm_col"),
    delta = eu_dl$delta[match(code, eu_dl$code)],
    series = eu_series[[code]]$series
  )
})

eu_panel <- list(
  quarters = qlab(eu_quarters),
  eventTimes = as.integer((year(eu_quarters) - year(t0)) * 4 + (quarter(eu_quarters) - quarter(t0))),
  countries = eu_countries,
  occupations = eu_occupations,
  avgSeries = avg_series(eu_fmt %>% filter(idesco_level_3 %in% eu_occ_codes), eu_quarters)
)

# Australian panel: one shared occupation list (ANZSCO 4-digit); the l3 / l4
# distinction only changes which ISCO level the exposure scores are attached at
aus_states <- sort(unique(aus_delta$l4$idcountry))
aus_quarters <- sort(unique(aus_fmt$l4$dmax))
aus_occ_codes <- sort(union(
  unique(aus_delta$l3$idesco_level_3), unique(aus_delta$l4$idesco_level_4)
))

aus_names <- oja_q %>%
  distinct(code = paste0("OC", anzsco_4digit), anzsco_title) %>%
  group_by(code) %>% slice(1) %>% ungroup()
aus_series <- index_series(
  aus_fmt$l4 %>% filter(idesco_level_4 %in% aus_occ_codes), "idesco_level_4", aus_quarters
)
aus_sc <- list(
  l3 = occ_scores(aus_fmt$l3, "idesco_level_3"),
  l4 = occ_scores(aus_fmt$l4, "idesco_level_4")
)
aus_dl <- list(
  l3 = occ_deltas(aus_delta$l3, "idesco_level_3"),
  l4 = occ_deltas(aus_delta$l4, "idesco_level_4")
)

aus_occupations <- map(aus_occ_codes, function(code) {
  srow3 <- aus_sc$l3 %>% filter(code == !!code)
  srow4 <- aus_sc$l4 %>% filter(code == !!code)
  list(
    code = str_remove(code, "^OC"),
    name = fix_name(aus_names$anzsco_title[match(code, aus_names$code)]),
    scores_l3 = scores_list(srow3, "col"),
    auto_l3 = scores_list(srow3, "auto_col"),
    augm_l3 = scores_list(srow3, "augm_col"),
    scores_l4 = scores_list(srow4, "col"),
    auto_l4 = scores_list(srow4, "auto_col"),
    augm_l4 = scores_list(srow4, "augm_col"),
    delta_l3 = aus_dl$l3$delta[match(code, aus_dl$l3$code)],
    delta_l4 = aus_dl$l4$delta[match(code, aus_dl$l4$code)],
    series = if (code %in% names(aus_series)) aus_series[[code]]$series else NULL
  )
})

aus_panel <- list(
  quarters = qlab(aus_quarters),
  eventTimes = as.integer((year(aus_quarters) - year(t0)) * 4 + (quarter(aus_quarters) - quarter(t0))),
  countries = aus_states,
  occupations = aus_occupations,
  avgSeries = avg_series(aus_fmt$l4 %>% filter(idesco_level_4 %in% aus_occ_codes), aus_quarters)
)

# estimates for the three model datasets ---------------------------------------
cat("Estimating EU models...\n")
est_eu <- build_estimates(eu_delta, eu_fmt, 3, eu_occ_codes, eu_countries)
cat("Estimating AUS l3 models...\n")
est_aus_l3 <- build_estimates(aus_delta$l3, aus_fmt$l3, 3, aus_occ_codes, aus_states)
cat("Estimating AUS l4 models...\n")
est_aus_l4 <- build_estimates(aus_delta$l4, aus_fmt$l4, 4, aus_occ_codes, aus_states)

# EURES by-experience models (mirrors the eures block in model_oja.R) ----------
cat("Estimating EURES experience models...\n")
eures <- list.files(
  "data/cedefop_eures_job_vacancy_insights/csv/14_exp_occupation_skill_country_hyper",
  full.names = TRUE
) %>%
  map_df(read_csv, show_col_types = FALSE) %>%
  format_eures_data(ai_exposure = ai_exposure$l4)

delta_eures <- eures %>%
  group_by(idcountry, idesco_level_4, experience) %>%
  arrange(dmax) %>%
  summarise(
    delta_log_OJA = last(log_OJA) - first(log(OJA)),
    across(all_of(exposure_cols), last),
    .groups = "drop"
  )

exp_levels <- c(
  "No experience", "Up to 1 year", "From 1 to 2 years", "From 2 to 4 years",
  "From 4 to 6 years", "From 6 to 8 years", "From 8 to 10 years", "Over 10 years"
)

experience_coefs <- set_names(index_meta$key) %>%
  map(function(key) {
    col <- index_meta$col[index_meta$key == key]
    m <- feols(
      as.formula(paste0("delta_log_OJA ~ i(experience, ", col, ") | idcountry")),
      data = delta_eures, cluster = ~idcountry
    )
    td <- tidy(m) %>%
      mutate(experience = str_extract(term, "(?<=::)[^:]+(?=:)")) %>%
      filter(!is.na(experience))
    td <- td[match(exp_levels, td$experience), ]
    list(est = r4(td$estimate), se = r4(td$std.error), p = signif(td$p.value, 3))
  })

# assemble and write -----------------------------------------------------------
payload <- list(
  meta = list(
    generated = format(Sys.Date()),
    t0 = "2022-11-30",
    t0Quarter = "2022Q4"
  ),
  indices = map(seq_len(nrow(index_meta)), function(i) {
    list(
      key = index_meta$key[i],
      label = index_meta$label[i],
      short = index_meta$short[i],
      hasBreakdown = !is.na(index_meta$auto_col[i])
    )
  }),
  panels = list(eu = eu_panel, aus = aus_panel),
  estimates = list(eu = est_eu, aus_l3 = est_aus_l3, aus_l4 = est_aus_l4),
  experience = list(levels = exp_levels, coefs = experience_coefs)
)

dir.create("results/web", showWarnings = FALSE, recursive = TRUE)
write_json(
  payload, "results/web/web_data.json",
  auto_unbox = TRUE, na = "null", digits = NA, null = "null"
)
cat(
  "Wrote results/web/web_data.json (",
  round(file.size("results/web/web_data.json") / 1024), "KB )\n"
)
