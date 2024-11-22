library(tidyverse)
library(fixest)
library(broom)
library(lubridate)

source("R/helpers.R")


# > oja_twfe %>% filter(idesco_level_2 == "OC31") %>% group_by(dmax) %>% summarize(n = sum(OJA))
# # A tibble: 11 × 2
# dmax             n
# <date>       <dbl>
# 1 2021-12-31 1917107
# 2 2022-03-31 1917179
# 3 2022-06-30 1950048
# 4 2022-09-30 1875103
# 5 2022-12-31 1907044
# 6 2023-03-31 1990550
# 7 2023-06-30 1249340
# 8 2023-09-30 1283299
# 9 2023-12-31 1131832
# 10 2024-03-31  934778
# 11 2024-06-30  313085
# > oja_twfe_l3 %>% filter(substr(idesco_level_3,1,4) == "OC31") %>% group_by(dmax) %>% summarize(n = sum(OJA))
# # A tibble: 10 × 2
# dmax             n
# <date>       <dbl>
# 1 2022-03-31 5483762
# 2 2022-06-30 5553403
# 3 2022-09-30 5332835
# 4 2022-12-31 5447859
# 5 2023-03-31 1990550
# 6 2023-06-30 1249340
# 7 2023-09-30 1283299
# 8 2023-12-31 1131832
# 9 2024-03-31  934778
# 10 2024-06-30  908375
# TODO: only matching from 2022-03-31 - 2024-03-31

# read data ---------------------------------------------------------------
oja <- list.files("data/cedefop_skills_ovate_oja/csv/01__countries_and_occupations_hyper", full.names = TRUE) %>%
  map_dfr(read_csv)

oja_l3 <- list.files(
  "data/cedefop_skills_ovate_skill_demand/csv/05_occupation_skill_across_occupations_hyper", 
  full.names = TRUE
) %>%
  map_dfr(read_csv) %>%
  mutate(
    idcountry = ifelse(is.na(idcountry), countryset, idcountry),
    esco_level_3_short = esco_level_3 # to match fromat of l2
  ) %>%
  select(-c(countryset, esco_level_3))

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
  level = 2
) 

ai_exposure_l3 <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
  level = 3
)

# prep for model ----------------------------------------------------------
oja_twfe <- format_twfe_oja_data(oja, ai_exposure, level = 2)
oja_twfe_l3 <- format_twfe_oja_data(oja_l3, ai_exposure_l3, level = 3)

nrow(oja_twfe)
length(unique(oja_twfe$country_occupation_pair))
length(unique(oja_twfe$idesco_level_2))
length(unique(oja_twfe$idcountry))
length(unique(oja_twfe$dmax))

# Define the time of the event (ChatGPT release)
# ChatGPT release date: December 2022 (use the quarter end date)
t0 <- as.Date("2022-12-31")

# Ensure 'dmax' is in Date format, derive event_time for event studies
oja_twfe <- oja_twfe %>%
  mutate(
    dmax = as.Date(dmax),
    event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
  )
oja_twfe_l3 <- oja_twfe_l3 %>%
  mutate(
    dmax = as.Date(dmax),
    event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
  )

# List of exposure variables
exposure_vars <- c(
  "ai_product_exposure_score",
  "felten_exposure_score",
  "webb_exposure_score",
  "beta_eloundou"
)


# Plot time series --------------------------------------------------------
plot_exposure_quintiles <- function(data, level, exposure_var, n_tiles = 5) {
  esco_level_col <- paste0("idesco_level_", level)
  esco_level_short_col <- paste0("esco_level_", level, "_short")
  
  data %>%
    group_by(!!sym(esco_level_col)) %>%
    filter(!is.na(!!sym(exposure_var))) %>%
    summarise(
      mean_score = mean(!!sym(exposure_var)),
      esco_level_short = first(!!sym(esco_level_short_col)),
      .groups = "drop"
    ) %>%
    mutate(
      score_percentile = ntile(mean_score, n_tiles)
    ) %>%
    inner_join(
      select(data, !!sym(esco_level_col), dmax, OJA), 
      by = esco_level_col
    ) %>%
    mutate(score_percentile = factor(score_percentile, levels = 1:n_tiles)) %>%
    group_by(score_percentile, dmax) %>%
    arrange(dmax) %>%
    summarise(
      sum_OJA = sum(OJA), # total across n-tile
      .groups = "drop"
    ) %>%
    group_by(score_percentile) %>%
    mutate(
      OJA_index = sum_OJA / first(sum_OJA) * 100
    ) %>%
    ggplot(
      aes(
        x = dmax, 
        #y = OJA_index, 
        y = sum_OJA,
        color = score_percentile, 
        lty = score_percentile
      )
    ) +
    geom_line() +
    geom_vline(xintercept = as.Date("2022-11-30"), linetype = "dashed") +
    scale_color_grey(start = 0.8, end = 0.2) +
    labs(
      title = paste("OJA Index by", exposure_var, "Exposure Quintiles"),
      x = "Date",
      y = "OJA Index",
      color = paste(n_tiles, "-tiles"),
      linetype = paste(n_tiles, "-tiles")
    ) +
    theme_minimal()
}

plot_exposure_quintiles(
  oja_twfe_l3, level = 3, exposure_var = exposure_vars[4], n_tiles = 10
)

plot_exposure_quintiles(
  oja_twfe, level = 2, exposure_var = exposure_vars[4], n_tiles = 7
)

# plot delta
oja_twfe_l3 %>%
  filter(dmax %in% c(min(dmax), max(dmax))) %>%
  group_by(
    post_chatgpt,
    esco_level_3_short,
    idesco_level_3,
    dmax
  ) %>%
  summarize(
    OJA = sum(OJA), # sum across countries
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou)
  ) %>%
  group_by(post_chatgpt, esco_level_3_short, idesco_level_3) %>%
  summarise(
    OJA = mean(OJA), # mean per quarter in pre/post period
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou)
  ) %>%
  arrange(idesco_level_3, post_chatgpt) %>%
  group_by(esco_level_3_short, idesco_level_3) %>%
  filter(n() == 2) %>% # observations in both pre and post period
  summarise(
    delta_OJA = diff(OJA),
    delta_OJA_relative = diff(OJA) / OJA[1],
    delta_OJA_log = log(OJA[2] / OJA[1]),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou)
  ) %>%
  ggplot(
    aes(
      x = webb_exposure_score,
      y = delta_OJA_relative
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Delta OJA vs. AI Product Exposure Score",
    x = "AI Product Exposure Score",
    y = "Delta OJA (relative)"
  ) +
  theme_minimal()

oja_delta_l3 %>% 
  arrange(desc(ai_product_exposure_score)) %>% ungroup() %>% 
  summarise(
    fraction_pre = oja_pre / sum(oja_pre), 
    fraction_post = oja_post / sum(oja_post), 
    exposure_score = ai_product_exposure_score, 
    esco_level_3_short = esco_level_3_short)   %>% 
  mutate(fraction_change = fraction_post - fraction_pre) %>%
  ggplot(aes(x = exposure_score, y = fraction_change)) + geom_point() + geom_smooth(method = "lm")
  

# run models --------------------------------------------------------------
# Function to run models for different exposure variables
run_exposure_models <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  # Log-linear model with separate fixed effects
  model <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": post_chatgpt |", esco_level, "+ idcountry + dmax")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model)
}


# Run models for each exposure variable
models <- map(exposure_vars, ~run_exposure_models(.x, oja_twfe_l3, level = 3))


# event studies -----------------------------------------------------------
# Function to run event study models for different exposure variables
run_event_study_model <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  # Event study model with separate fixed effects
  model_event_study <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": i(event_time, ref = 0) |", esco_level, "+ idcountry")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model_event_study)
}

# Run event study models for each exposure variable
event_study_models <- map(
  exposure_vars, ~run_event_study_model(.x, oja_twfe_l3, level = 3)
)

# Print summaries
walk(seq_along(exposure_vars), function(i) {
	cat("\nModels for", exposure_vars[i], ":\n")
	print(summary(models[[i]]))
})

# Function to extract event study coefficients
extract_event_study_coefs <- function(model, exposure_var) {
	# Get the tidy coefficients
	tidy_model <- tidy(model)
	
	# Filter for coefficients of interest (the interaction terms)
	# The interaction terms will be of the form exposure_var:event_time::X
	interaction_pattern <- paste0(exposure_var, ":event_time::")
	
	coefs <- tidy_model %>%
		filter(str_detect(term, interaction_pattern)) %>%
		mutate(
			event_time = as.numeric(str_extract(term, "(?<=event_time::)-?\\d+"))
		) %>%
		arrange(event_time)
	
	return(coefs)
}

# Extract coefficients for each event study model
event_study_coefs <- map2(
  event_study_models, exposure_vars, ~extract_event_study_coefs(.x, .y)
)

# Function to plot event study estimates
plot_event_study <- function(coefs, exposure_var) {
	ggplot(coefs, aes(x = event_time, y = estimate)) +
		geom_point() +
		geom_line() +
		geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
			ymax = estimate + 1.96 * std.error), width = 0.2) +
		geom_vline(xintercept = 0, linetype = "dashed") +
		labs(title = paste("Event Study for", exposure_var),
			x = "Event Time (quarters since ChatGPT release)",
			y = "Coefficient Estimate") +
		theme_minimal()
}

# Plot event study estimates for each exposure variable
plots <- map2(event_study_coefs, exposure_vars, ~plot_event_study(.x, .y))

# Display plots
for (i in seq_along(plots)) {
	print(plots[[i]])
}
