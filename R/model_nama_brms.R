library(tidyverse)
library(brms)
library(tidybayes)

source("R/helpers.R")

# read and prep data ------------------------------------------------------
nama_10_cp <- read_csv("results/intermediate_datasets/nama_10_cp_gvancs.csv")
nama_10_lp <- read_csv("results/intermediate_datasets/nama_10_lp_rlrphw.csv")

cedefop_sectoral <- read_csv("data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv")

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

sectoral_exposure <- derive_sectoral_exposure(ai_exposure, cedefop_sectoral)

nama_10_lp_prep <- prep_nama_data(nama_10_lp, sectoral_exposure)
nama_10_cp_prep <- prep_nama_data(nama_10_cp, sectoral_exposure)

# Bayesian model function -------------------------------------------------
run_bayesian_model <- function(data, exposure_var) {
  data$exposure_var <- data[[exposure_var]]
  
  formula <- bf(
    log_value ~ 0 + exposure_var * post_chatgpt + (1 | nace_rev2_code) + (1 | country_code) + (1 | year)
  )
  
  priors <- c(
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd", group = "nace_rev2_code"),
    prior(exponential(1), class = "sd", group = "country_code"),
    prior(exponential(1), class = "sd", group = "year"),
    prior(exponential(1), class = "sigma")
  )
  
  model <- brm(
    formula = formula,
    data = data,
    family = gaussian(),
    prior = priors,
    cores = 4,
    chains = 4,
    iter = 4000,
    warmup = 2000,
    control = list(adapt_delta = 0.95)
  )
  
  return(model)
}

# Run Bayesian models -----------------------------------------------------
exposure_vars <- c(
  "ai_product_exposure_score",
  "felten_exposure_score",
  "webb_exposure_score",
  "beta_eloundou"
)

lp_bayesian_models <- map(exposure_vars, ~run_bayesian_model(nama_10_lp_prep, .x))
names(lp_bayesian_models) <- exposure_vars
saveRDS(lp_bayesian_models, file = "chkp/lp_bayesian_models.RDS")

cp_bayesian_models <- map(exposure_vars, ~run_bayesian_model(nama_10_cp_prep, .x))
names(cp_bayesian_models) <- exposure_vars
saveRDS(cp_bayesian_models, file = "chkp/cp_bayesian_models.RDS")

# Summarize and plot results ----------------------------------------------
summarize_and_plot_bayesian_models <- function(models, model_name) {
  walk(seq_along(models), function(i) {
    cat("\nSummary for", model_name, "-", names(models)[i], "model:\n")
    print(summary(models[[i]]))
    
    cat("\nPlotting posterior distribution for", names(models)[i], ":\n")
    print(mcmc_plot(models[[i]], type = "areas", regex_pars = "b_"))
    
    cat("\nPlotting conditional effects for", names(models)[i], "model:\n")
    conditional_effects <- conditional_effects(models[[i]])
    print(plot(conditional_effects))
  })
}

summarize_and_plot_bayesian_models(lp_bayesian_models, "Labor Productivity")
summarize_and_plot_bayesian_models(cp_bayesian_models, "Capital Productivity")
