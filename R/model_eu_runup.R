library(tidyverse)
library(fixest)
library(broom)
library(lubridate)

source("R/helpers.R")
init_text_log("rate_sensitivity.txt", overwrite = TRUE)

# Rate-sensitivity horse race. Occupations concentrated in sectors whose
# hiring contracted during the 2022-2023 monetary tightening may drive the
# AI exposure estimates. We build an occupation-level "contraction exposure"
# score by weighting each sector's job vacancy rate decline (Eurostat JVS,
# a non-CEDEFOP source) by the occupation's employment mix across sectors,
# and race it against the AI exposure measures in the delta specification.

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Anthropic Usage Score" = "anthropic_usage_score"
)

results <- list()

# read OJA data (same prep as model_oja.R) ---------------------------------
oja_l3 <- list.files(
  "data/cedefop_skills_ovate_skill_demand/csv/05_occupation_skill_across_occupations_hyper",
  full.names = TRUE
) %>%
  map_dfr(read_csv) %>%
  mutate(
    idcountry = ifelse(is.na(idcountry), countryset, idcountry),
    esco_level_3_short = esco_level_3
  ) %>%
  select(-c(countryset, esco_level_3)) %>%
  filter(!str_detect(idcountry, "EU27"))

ai_exposure_l3 <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
  level = 3
)

oja_delta_l3 <- format_twfe_oja_data(oja_l3, ai_exposure_l3, level = 3, t0 = t0) %>%
  format_delta_data(
    n_periods = Inf, base_date = t0, level = 3, across_countries = FALSE
  ) %>%
  filter(pre_OJA > 20 & post_OJA > 20) # same infrequent-occupation filter as model_oja.R

# sectoral vacancy contraction (Eurostat jvs_q_nace2) -----------------------
# Job vacancy rates by NACE section, cached from the Eurostat API:
# https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/jvs_q_nace2/
#   Q.NSA.A+B+C+D+E+F+G+H+I+J+K+M+N+O+P+Q+R.TOTAL.JVR.?format=SDMX-CSV
# Individual-country series are used because the EU27 aggregate is not
# published for sections A and O-R.
jvr <- read_csv("data/eurostat_jvs/jvs_q_nace2_jvr_countries.csv") %>%
  filter(str_length(geo) == 2) %>% # drop EU/EA aggregates
  select(nace_rev2_code = nace_r2, geo, period = TIME_PERIOD, jvr = OBS_VALUE) %>%
  filter(!is.na(jvr) & jvr > 0)

# Sector contraction: log change in the vacancy rate over the tightening
# cycle, median across countries. Same quarter at both endpoints so the NSA
# series is seasonally comparable. 2024-Q4 endpoint kept as an alternative.
sector_contraction <- jvr %>%
  filter(period %in% c("2022-Q2", "2024-Q2", "2024-Q4")) %>%
  pivot_wider(names_from = period, values_from = jvr) %>%
  mutate(
    d_jvr_log     = log(`2024-Q2` / `2022-Q2`),
    d_jvr_log_alt = log(`2024-Q4` / `2022-Q2`)
  ) %>%
  group_by(nace_rev2_code) %>%
  summarise(
    contraction     = -median(d_jvr_log, na.rm = TRUE),
    contraction_alt = -median(d_jvr_log_alt, na.rm = TRUE),
    n_countries = sum(!is.na(d_jvr_log)),
    .groups = "drop"
  )

log_text(
  sector_contraction %>% arrange(desc(contraction)),
  "Sectoral vacancy contraction (JVR log change 2022Q2 to 2024Q2, sign flipped, median across countries):",
  n = Inf
)

# occupation-level rate sensitivity ----------------------------------------
# Transpose of the industry exposure construction in model_nama.R: each
# occupation's score is the employment-weighted average contraction of the
# sectors it is employed in.
cedefop_sectoral <- read_csv("data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv") %>%
  filter(str_detect(occupation_code, "\\.")) %>%
  mutate(
    isco_level_2 = substr(occupation_code, 3, 4),
    n = parse_number(as.character(n))
  ) %>%
  filter(!is.na(n) & n > 0)

occupation_rate_sensitivity <- cedefop_sectoral %>%
  inner_join(sector_contraction, by = "nace_rev2_code") %>%
  group_by(isco_level_2) %>%
  summarise(
    rate_sensitivity     = weighted.mean(contraction, n, na.rm = TRUE),
    rate_sensitivity_alt = weighted.mean(contraction_alt, n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate_sensitivity     = scale_zero_to_one(rate_sensitivity),
    rate_sensitivity_alt = scale_zero_to_one(rate_sensitivity_alt)
  )

# country-specific version (weights vary by country, contraction EU-wide)
occupation_rate_sensitivity_cntry <- cedefop_sectoral %>%
  inner_join(sector_contraction, by = "nace_rev2_code") %>%
  group_by(country_code, isco_level_2) %>%
  summarise(
    rate_sensitivity_cntry = weighted.mean(contraction, n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rate_sensitivity_cntry = scale_zero_to_one(rate_sensitivity_cntry))

oja_delta_rs <- oja_delta_l3 %>%
  mutate(isco_level_2 = substr(idesco_level_3, 3, 4)) %>%
  left_join(occupation_rate_sensitivity, by = "isco_level_2") %>%
  left_join(
    occupation_rate_sensitivity_cntry,
    by = c("idcountry" = "country_code", "isco_level_2")
  ) %>%
  filter(!is.na(rate_sensitivity))

log_text(
  oja_delta_rs %>%
    distinct(isco_level_2, across(all_of(unname(exposure_vars))), rate_sensitivity) %>%
    summarise(across(
      all_of(unname(exposure_vars)),
      ~cor(.x, rate_sensitivity, use = "complete.obs")
    )),
  "Correlation between rate sensitivity and AI exposure (ISCO level-2 occupations):"
)

# models --------------------------------------------------------------------
run_delta <- function(rhs, dat = oja_delta_rs) {
  feols(
    as.formula(paste("delta_OJA_log ~", rhs, " | idcountry")),
    data = dat,
    cluster = "idcountry"
  )
}

rate_only_model <- run_delta("rate_sensitivity")

log_text(
  rate_only_model,
  "Rate sensitivity alone:"
)

baseline_models <- map(exposure_vars, ~run_delta(.x))

log_text(
  baseline_models,
  "Baseline (merged sample, for comparison):"
)

horse_race_models <- map(
  exposure_vars, ~run_delta(paste(.x, "+ rate_sensitivity"))
)

log_text(
  horse_race_models,
  "Horse race: exposure + rate sensitivity:"
)

horse_race_models_cntry <- map(
  exposure_vars, ~run_delta(paste(.x, "+ rate_sensitivity_cntry"))
)

log_text(
  horse_race_models_cntry,
  "Horse race with country-specific rate sensitivity:"
)

horse_race_models_alt <- map(
  exposure_vars, ~run_delta(paste(.x, "+ rate_sensitivity_alt"))
)

log_text(
  horse_race_models_alt,
  "Horse race with alternative endpoint (2022Q2 to 2024Q4):"
)

# attenuation summary
attenuation <- map2_dfr(
  baseline_models, horse_race_models,
  function(base_m, hr_m) {
    v <- setdiff(names(coef(base_m)), "rate_sensitivity")
    tibble(
      exposure_var = v,
      baseline = coef(base_m)[v],
      horse_race = coef(hr_m)[v],
      horse_race_p = tidy(hr_m) %>% filter(term == v) %>% pull(p.value),
      rate_coef = coef(hr_m)["rate_sensitivity"],
      rate_p = tidy(hr_m) %>% filter(term == "rate_sensitivity") %>% pull(p.value)
    ) %>%
      mutate(pct_attenuation = (1 - horse_race / baseline) * 100)
  }
)

log_text(
  attenuation,
  "Attenuation summary (exposure coefficient with vs without rate sensitivity):",
  n = Inf
)

results$sector_contraction <- sector_contraction
results$occupation_rate_sensitivity <- occupation_rate_sensitivity
results$rate_only <- rate_only_model
results$baseline <- baseline_models
results$horse_race <- horse_race_models
results$horse_race_cntry <- horse_race_models_cntry
results$horse_race_alt <- horse_race_models_alt
results$attenuation <- attenuation

saveRDS(results, "results/RDS/rate_sensitivity.RDS")
