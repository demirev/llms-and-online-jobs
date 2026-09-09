library(tidyverse)
library(fixest)
library(broom)
library(lubridate)

source("R/helpers.R")
init_text_log("eu_runup.txt", overwrite = TRUE)

# EU run-up control -----------------------------------------------------------
# Counterpart of the run-up check in model_aus.R. There, each state x
# occupation's own hiring run-up into the pre-ChatGPT baseline,
#
#   runup = log(mean OJA in the pre-period) - log(mean OJA in 2019 [or 2020]),
#
# enters the delta specification alongside exposure (the race itself runs in
# model_horse_race.R): if the exposure gradient were mean reversion from an
# AI-correlated 2021-2022 hiring boom, the run-up should absorb it. The CEDEFOP OJA series only starts in 2022 Q1, so the EU
# run-up cannot be measured at the occupation level. Instead we build it from
# two non-CEDEFOP sources:
#
#   1. Eurostat JVS (jvs_q_nace2): quarterly job vacancy rates by NACE
#      section and country, back to 2018. The sector run-up is the log change
#      in the vacancy rate between the 2019 (or 2020) quarters and the SAME
#      calendar quarters of the OJA pre-period (2022 Q1-Q3), so the NSA
#      series is seasonally comparable at both ends.
#   2. CEDEFOP Skills Intelligence sectoral employment: each ISCO 2-digit
#      occupation's employment across NACE sections, by country. An
#      occupation's run-up is the employment-weighted mean of the run-ups of
#      the sectors it works in.
#
# The headline score is country-specific in both ingredients (sector run-up
# and occupation mix), so it varies across country x occupation cells exactly
# as the Australian state x occupation run-up does, and it is identified from
# within-country variation under the country fixed effect. Unlike the
# sector-contraction score this file used to build (log JVR change 2022-2024,
# which straddled the AI "treatment"), every input here is pre-ChatGPT.
#
# Two limitations are inherent to the construction. The score only picks up
# the part of an occupation's boom that runs through its sector mix, so any
# within-sector over-hiring of, say, software developers relative to other
# ICT-sector staff is missed. And the occupation mix is ISCO-2, so the score
# is constant across the ISCO-3 groups within a 2-digit minor group.
#
# Variants: base year 2019 (last pre-pandemic year, headline) or 2020
# (pandemic trough); vacancy rate (headline) or vacancy count (JOBVAC, closer
# in spirit to an OJA count but with patchier country coverage); a hybrid
# that keeps the own-country occupation mix but replaces each country's
# sector run-up with the EU median across countries (less noise from small
# countries' sectoral series, variation across countries only through the
# mix); and an EU-pooled version (median sector run-up, pooled weights),
# which varies only across occupations.

t0 <- as.Date("2022-11-30") # chatgpt release date
min_covered_share <- 0.5 # drop occupation x country cells whose sectors mostly lack JVS data

exposure_vars <- c(
  "Anthropic Usage Score" = "anthropic_usage_score",
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score"
)

results <- list()

# EU delta sample (build_eu_delta in helpers.R, same as model_oja.R) --------
oja_l3 <- read_oja_l3()
oja_delta_l3 <- build_eu_delta(t0, oja_l3 = oja_l3)

# the pre-period quarters of the delta specification, in Eurostat notation
as_eurostat_q <- function(d) paste0(year(d), "-Q", quarter(d))
pre_quarters <- oja_l3 %>%
  mutate(dmax = as.Date(dmax)) %>%
  filter(dmax < t0) %>%
  distinct(dmax) %>%
  pull(dmax) %>%
  sort() %>%
  as_eurostat_q()
base_quarters <- list(
  runup_from_2019 = str_replace(pre_quarters, "^\\d{4}", "2019"),
  runup_from_2020 = str_replace(pre_quarters, "^\\d{4}", "2020")
)

log_text(
  tibble(window = c("pre", names(base_quarters)), quarters = c(
    paste(pre_quarters, collapse = ", "),
    map_chr(base_quarters, paste, collapse = ", ")
  )),
  "Run-up windows (OJA pre-period vs base-year quarters):"
)

# sector run-up (Eurostat jvs_q_nace2) --------------------------------------
# Job vacancy rates (JVR) and counts (JOBVAC) by NACE section, all reporting
# countries, NSA, 2018Q1-2025Q4; cached from the Eurostat API, see
# data/eurostat_jvs/README.md. Geo aggregates (EU*/EA*) are dropped.
jvs <- read_csv("data/eurostat_jvs/jvs_q_nace2_countries.csv", show_col_types = FALSE) %>%
  filter(str_length(geo) == 2) %>%
  transmute(
    indicator = indic_em, geo, nace_rev2_code = nace_r2,
    period = TIME_PERIOD, value = OBS_VALUE
  ) %>%
  filter(!is.na(value) & value > 0)

# Mean over each window, requiring every quarter of the window to be present,
# then the log change from the base window to the pre-period.
sector_runup <- jvs %>%
  mutate(window = case_when(
    period %in% pre_quarters ~ "pre",
    period %in% base_quarters$runup_from_2019 ~ "y2019",
    period %in% base_quarters$runup_from_2020 ~ "y2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(indicator, geo, nace_rev2_code, window) %>%
  summarise(value = mean(value), n_q = n(), .groups = "drop") %>%
  filter(n_q == length(pre_quarters)) %>%
  select(-n_q) %>%
  pivot_wider(names_from = window, values_from = value) %>%
  mutate(
    runup_from_2019 = log(pre) - log(y2019),
    runup_from_2020 = log(pre) - log(y2020)
  ) %>%
  select(-c(pre, y2019, y2020))

log_text(
  sector_runup %>%
    filter(indicator == "JVR") %>%
    group_by(nace_rev2_code) %>%
    summarise(
      median_runup_from_2019 = median(runup_from_2019, na.rm = TRUE),
      median_runup_from_2020 = median(runup_from_2020, na.rm = TRUE),
      n_countries = sum(!is.na(runup_from_2019)),
      .groups = "drop"
    ) %>%
    arrange(desc(median_runup_from_2019)),
  "Sector run-up (log change in JVR, base-year quarters to 2022 Q1-Q3), median across countries:",
  n = Inf
)

log_text(
  sector_runup %>%
    group_by(indicator, geo) %>%
    summarise(
      n_sectors_2019 = sum(!is.na(runup_from_2019)),
      n_sectors_2020 = sum(!is.na(runup_from_2020)),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = indicator, values_from = starts_with("n_sectors")),
  "Sector coverage by country (of 17 NACE sections):",
  n = Inf
)

# occupation run-up -----------------------------------------------------------
# Transpose of the industry exposure construction in model_nama.R: each
# ISCO-2 occupation's run-up is the employment-weighted mean run-up of the
# sectors it is employed in. Sectors with no JVS series in a country drop out
# of the weighted mean; cells where those sectors hold more than half of the
# occupation's employment are dropped.
cedefop_sectoral <- read_csv(
  "data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv",
  show_col_types = FALSE
) %>%
  filter(str_detect(occupation_code, "\\.")) %>%
  mutate(
    isco_level_2 = substr(occupation_code, 3, 4),
    n = parse_number(as.character(n))
  ) %>%
  filter(!is.na(n) & n > 0)

weight_sector_runup <- function(weights, sector_scores, group_cols) {
  weights %>%
    left_join(sector_scores, by = intersect(names(weights), names(sector_scores))) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      covered_share = sum(n[!is.na(runup_from_2019)]) / sum(n),
      runup_from_2019 = weighted.mean(runup_from_2019, n, na.rm = TRUE),
      runup_from_2020 = weighted.mean(runup_from_2020, n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(covered_share >= min_covered_share)
}

# country-specific: own-country sector run-up, own-country occupation mix
runup_cntry <- map(
  c(jvr = "JVR", vac = "JOBVAC"),
  ~ weight_sector_runup(
    cedefop_sectoral %>% select(geo = country_code, nace_rev2_code, isco_level_2, n),
    sector_runup %>% filter(indicator == .x) %>% select(-indicator),
    group_cols = c("geo", "isco_level_2")
  )
)

# EU-median sector run-up (JVR), used by the hybrid and pooled variants
sector_runup_eu <- sector_runup %>%
  filter(indicator == "JVR") %>%
  group_by(nace_rev2_code) %>%
  summarise(across(starts_with("runup"), ~ median(.x, na.rm = TRUE)), .groups = "drop")

# hybrid: EU-median sector run-up, own-country occupation mix
runup_mix <- weight_sector_runup(
  cedefop_sectoral %>% select(geo = country_code, nace_rev2_code, isco_level_2, n),
  sector_runup_eu,
  group_cols = c("geo", "isco_level_2")
)

# EU-pooled: EU-median sector run-up, occupation mix pooled across countries;
# one score per ISCO-2 group
runup_pooled <- weight_sector_runup(
  cedefop_sectoral %>% count(nace_rev2_code, isco_level_2, wt = n, name = "n"),
  sector_runup_eu,
  group_cols = "isco_level_2"
)

occupation_runup <- runup_cntry$jvr %>%
  rename(idcountry = geo) %>%
  full_join(
    runup_cntry$vac %>%
      transmute(idcountry = geo, isco_level_2, runup_from_2019_vac = runup_from_2019),
    by = c("idcountry", "isco_level_2")
  ) %>%
  full_join(
    runup_mix %>%
      transmute(idcountry = geo, isco_level_2, runup_from_2019_mix = runup_from_2019),
    by = c("idcountry", "isco_level_2")
  ) %>%
  left_join(
    runup_pooled %>% transmute(isco_level_2, runup_from_2019_pooled = runup_from_2019),
    by = "isco_level_2"
  )

log_text(
  occupation_runup %>%
    group_by(idcountry) %>%
    summarise(
      n_isco2 = sum(!is.na(runup_from_2019)),
      mean_covered_share = mean(covered_share, na.rm = TRUE),
      n_isco2_vac = sum(!is.na(runup_from_2019_vac)),
      .groups = "drop"
    ),
  "Occupation run-up coverage by country (ISCO-2 groups scored, employment share of sectors with JVS data):",
  n = Inf
)

log_text(
  occupation_runup %>%
    group_by(isco_level_2) %>%
    summarise(
      mean_runup_from_2019 = mean(runup_from_2019, na.rm = TRUE),
      sd_across_countries = sd(runup_from_2019, na.rm = TRUE),
      pooled = first(runup_from_2019_pooled),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_runup_from_2019)),
  "Occupation run-up (JVR, from 2019) by ISCO-2 group: mean and s.d. across countries, and the pooled score:",
  n = Inf
)

# merge with the delta sample -------------------------------------------------
oja_delta_ru <- oja_delta_l3 %>%
  left_join(occupation_runup, by = c("idcountry", "isco_level_2"))

log_text(
  oja_delta_ru %>%
    group_by(idcountry) %>%
    summarise(
      n_obs = n(),
      n_scored = sum(!is.na(runup_from_2019)),
      n_scored_vac = sum(!is.na(runup_from_2019_vac)),
      .groups = "drop"
    ),
  "Delta sample: country x occupation observations with a run-up score, by country:",
  n = Inf
)

# How much do the variants agree within countries? If the own-country score
# were mostly measurement error from thin sectoral series, its two readings
# (vacancy rate, vacancy count) would disagree with each other as much as
# with the EU-median version.
runup_variants <- c(
  "runup_from_2019", "runup_from_2020", "runup_from_2019_vac",
  "runup_from_2019_mix", "runup_from_2019_pooled"
)
log_text(
  oja_delta_ru %>%
    group_by(idcountry) %>%
    mutate(across(all_of(runup_variants), ~ .x - mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    select(all_of(runup_variants)) %>%
    cor(use = "pairwise.complete.obs") %>%
    round(2),
  "Correlation between the run-up variants within countries (delta sample, demeaned by country):"
)

# Why the own-country and EU-median scores behave differently: decompose the
# own-country run-up into the EU-median score plus the country-specific
# sector deviation, own = mix + dev, and compare their within-country spread
# and their separate slopes. The slope on `own` is roughly the variance-
# weighted average of the two.
runup_decomp <- oja_delta_ru %>%
  filter(!is.na(runup_from_2019), !is.na(runup_from_2019_mix)) %>%
  mutate(runup_dev = runup_from_2019 - runup_from_2019_mix)

log_text(
  runup_decomp %>%
    group_by(idcountry) %>%
    mutate(across(
      c(delta_OJA_log, runup_from_2019, runup_from_2019_mix, runup_dev),
      ~ .x - mean(.x)
    )) %>%
    ungroup() %>%
    summarise(across(c(delta_OJA_log, runup_from_2019, runup_from_2019_mix, runup_dev), sd)) %>%
    as.data.frame(),
  "Within-country s.d. of the outcome, the own-country run-up, the EU-median run-up and their difference:"
)

log_text(
  etable(
    feols(delta_OJA_log ~ runup_from_2019 | idcountry, runup_decomp, cluster = "idcountry"),
    feols(delta_OJA_log ~ runup_from_2019_mix | idcountry, runup_decomp, cluster = "idcountry"),
    feols(delta_OJA_log ~ runup_from_2019_mix + runup_dev | idcountry, runup_decomp, cluster = "idcountry"),
    digits = 3
  ),
  "Run-up alone: own-country, EU-median, and decomposed into EU-median + country-specific deviation:"
)

# correlations with the outcome and with exposure --------------------------------
# within countries (variables demeaned by country), which is the variation
# the country fixed effect leaves for the race
log_text(
  oja_delta_ru %>%
    group_by(idcountry) %>%
    mutate(across(
      c(delta_OJA_log, all_of(unname(exposure_vars)), all_of(runup_variants)),
      ~ .x - mean(.x, na.rm = TRUE)
    )) %>%
    ungroup() %>%
    select(delta_OJA_log, all_of(unname(exposure_vars)), all_of(runup_variants)) %>%
    cor(use = "pairwise.complete.obs") %>%
    .[runup_variants, c("delta_OJA_log", unname(exposure_vars)), drop = FALSE] %>%
    round(2),
  "Within-country correlation of each run-up variant with the post-ChatGPT change and with exposure:"
)

# export as horse-race controls ---------------------------------------------------
write_controls(
  occupation_runup, sample = "eu_l3", key = "isco_level_2",
  controls = runup_variants, file = "eu_runup_eu_l3", idcountry_col = "idcountry"
)

# save ---------------------------------------------------------------------------
results$pre_quarters <- pre_quarters
results$base_quarters <- base_quarters
results$sector_runup <- sector_runup
results$occupation_runup <- occupation_runup

saveRDS(results, "results/RDS/eu_runup.RDS")
write_csv(occupation_runup, "results/intermediate_datasets/eu_runup_isco2.csv")
