library(brms)
library(tidybayes)
library(tidyverse)

source("R/helpers.R")

# read data ---------------------------------------------------------------
oja <- list.files("data/cedefop_skills_ovate/csv", full.names = TRUE) %>%
  map_dfr(read_csv)

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

# prep for model ----------------------------------------------------------
oja_twfe <- format_twfe_oja_data(oja, ai_exposure)

# run ---------------------------------------------------------------------
oja_twfe <- oja_twfe %>%
  mutate(
    time_factor = factor(format(dmax, "%Y-%m")),  # Create factor for each month-year combination
    ai_exposure_post_chatgpt = ai_product_exposure_score * post_chatgpt,
    felten_exposure_post_chatgpt = felten_exposure_score * post_chatgpt,
    webb_exposure_post_chatgpt = webb_exposure_score * post_chatgpt,
    beta_eloundou_post_chatgpt = beta_eloundou * post_chatgpt
  )

# Function to set up and run the model for different exposure scores
run_exposure_model <- function(exposure_var, data) {
  data$exposure_var <- data[[exposure_var]]
  
	formula <- bf(
		log_OJA ~ 0 + exposure_var + time_factor +
			(1 | idcountry) + (1 | idesco_level_2)
	)
	
	priors <- c(
		prior(normal(0, 0.5), class = "b", coef = "exposure_var"),
		prior(exponential(1), class = "sd", group = "idcountry"),
		prior(exponential(1), class = "sd", group = "idesco_level_2"),
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

# Run models for different exposure scores
exposure_vars <- c(
	"ai_exposure_post_chatgpt",
	"felten_exposure_post_chatgpt",
	"webb_exposure_post_chatgpt",
	"beta_eloundou_post_chatgpt"
)

models <- map(exposure_vars, ~run_exposure_model(.x, oja_twfe))
names(models) <- exposure_vars
saveRDS(models, file = "chkp/oja_exposure_models.RDS")

# Summarize and plot results for each model
map(names(models), function(name) {
  cat("\nSummary for", name, "model:\n")
  print(summary(models[[name]]))
  
  cat("\nPlotting posterior distribution for", name, ":\n")
  print(mcmc_plot(models[[name]], type = "areas", regex_pars = paste0("b_exposure_var")))
  
  cat("\nPlotting time effects for", name, "model:\n")
  time_effects <- conditional_effects(models[[name]], effects = "time_factor")
  print(plot(time_effects))
})


