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

# read data ---------------------------------------------------------------
nama_10_cp <- read_csv("results/intermediate_datasets/nama_10_cp_gvancs.csv")
nama_10_lp <- read_csv("results/intermediate_datasets/nama_10_lp_rlrphw.csv")

cedefop_sectoral <- read_csv("data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv")

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

sectoral_exposure <- derive_sectoral_exposure(ai_exposure, cedefop_sectoral)

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
