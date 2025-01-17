library(tidyverse)
library(fixest)

source("R/helpers.R")

run_twfe_models <- function(data, exposure_vars) {
  map(exposure_vars, function(var) {
    feols(
      as.formula(paste("log_value ~", var, ": post_chatgpt | nace_rev2_code + country_code + year")),
      data = data,
      cluster = c("country_code", "nace_rev2_code")
    )
  })
}

print_model_summaries <- function(models, model_name) {
  walk(seq_along(models), function(i) {
    cat("\n", model_name, "Model for", names(models)[i], ":\n")
    print(summary(models[[i]]))
  })
}

exposure_vars <- c(
  "AI Product Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Beta Score" = "beta_eloundou"
)


# read data ---------------------------------------------------------------
nama_10_cp <- read_csv("results/intermediate_datasets/nama_10_cp_gvancs.csv")
nama_10_lp <- read_csv("results/intermediate_datasets/nama_10_lp_rlrphw.csv")

cedefop_sectoral <- read_csv("data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv")

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

sectoral_exposure <- derive_sectoral_exposure(ai_exposure, cedefop_sectoral)

sectoral_exposure %>%
  group_by(nace_rev2_code, sector_name) %>%
  summarise(
    ai_product_exposure_score = mean(ai_product_exposure_score), 
    felten_exposure_score = mean(felten_exposure_score), 
    webb_exposure_score = mean(webb_exposure_score), 
    beta_eloundou = mean(beta_eloundou)
  ) 

# prep data ---------------------------------------------------------------
nama_10_lp_prep <- prep_nama_data(nama_10_lp, sectoral_exposure)
nama_10_cp_prep <- prep_nama_data(nama_10_cp, sectoral_exposure)

# run models --------------------------------------------------------------
exposure_vars <- c(
  "ai_product_exposure_score",
  "felten_exposure_score",
  "webb_exposure_score",
  "beta_eloundou"
)

lp_models <- run_twfe_models(nama_10_lp_prep, exposure_vars)
cp_models <- run_twfe_models(nama_10_cp_prep, exposure_vars)

# Print summaries
print_model_summaries(lp_models, "Labor Productivity")
print_model_summaries(cp_models, "Capital Productivity")

# delta models ------------------------------------------------------------
nama_10_lp_prep_delta <- nama_10_lp_prep %>%
  mutate(post_chatgpt = ifelse(year >= 2023, 1, 0)) %>%
  group_by(
    post_chatgpt, nace_rev2_code, country_code
  ) %>%
  summarise(
    log_value = mean(log_value),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou)
  ) %>%
  arrange(nace_rev2_code, country_code, post_chatgpt) %>%
  group_by(nace_rev2_code, country_code) %>%
  mutate(delta_log_value = log_value - lag(log_value)) %>%
  filter(!is.na(delta_log_value))

nama_10_cp_prep_delta <- nama_10_cp_prep %>%
  mutate(post_chatgpt = ifelse(year >= 2023, 1, 0)) %>%
  group_by(
    post_chatgpt, nace_rev2_code, country_code
  ) %>%
  summarise(
    log_value = mean(log_value),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou)
  ) %>%
  arrange(nace_rev2_code, country_code, post_chatgpt) %>%
  group_by(nace_rev2_code, country_code) %>%
  mutate(delta_log_value = log_value - lag(log_value)) %>%
  filter(!is.na(delta_log_value))


delta_lp_models <- map(exposure_vars, function(var) {
  feols(
    as.formula(paste("delta_log_value ~", var, "| country_code")),
    data = nama_10_lp_prep_delta,
    cluster = c("country_code", "nace_rev2_code")
  )
})

delta_cp_models <- map(exposure_vars, function(var) {
  feols(
    as.formula(paste("delta_log_value ~", var, "| country_code")),
    data = nama_10_cp_prep_delta,
    cluster = c("country_code", "nace_rev2_code")
  )
})
