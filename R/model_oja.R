library(tidyverse)
library(fixest)

source("R/helpers.R")

# read data ---------------------------------------------------------------
oja <- list.files("data/cedefop_skills_ovate/csv", full.names = TRUE) %>%
  map_dfr(read_csv)

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

# prep for model ----------------------------------------------------------
oja_twfe <- format_twfe_oja_data(oja, ai_exposure)

nrow(oja_twfe)
length(unique(oja_twfe$country_occupation_pair))
length(unique(oja_twfe$idesco_level_2))
length(unique(oja_twfe$idcountry))
length(unique(oja_twfe$dmax))

# Function to run models for different exposure variables
run_exposure_models <- function(exposure_var, data) {
  # Log-linear model with separate fixed effects
  model_separate <- feols(
    as.formula(paste("log_OJA ~", exposure_var, "* post_chatgpt | idesco_level_2 + idcountry + dmax")),
    data = data,
    cluster = c("idcountry", "idesco_level_2")
  )
  
  # Log-linear model with country-occupation pair
  model_pair <- feols(
    as.formula(paste("log_OJA ~", exposure_var, "* post_chatgpt | country_occupation_pair + dmax")),
    data = data,
    cluster = c("country_occupation_pair")
  )
  
  return(list(separate = model_separate, pair = model_pair))
}

# List of exposure variables
exposure_vars <- c(
  "ai_product_exposure_score",
  "felten_exposure_score",
  "webb_exposure_score",
  "beta_eloundou"
)

# Run models for each exposure variable
models <- map(exposure_vars, ~run_exposure_models(.x, oja_twfe))

# Print summaries
walk(seq_along(exposure_vars), function(i) {
  cat("\nModels for", exposure_vars[i], ":\n")
  cat("\nSeparate fixed effects model:\n")
  print(summary(models[[i]]$separate))
  #cat("\nCountry-occupation pair model:\n")
  #print(summary(models[[i]]$pair))
})
