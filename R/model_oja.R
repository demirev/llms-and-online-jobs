library(tidyverse)
library(fixest)
library(broom)
library(lubridate)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "ai_product_exposure_score",
  "felten_exposure_score",
  "webb_exposure_score",
  "beta_eloundou"
)

results <- list()

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


# functions ---------------------------------------------------------------
run_twfe_exposure_models <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  # Log-linear model with separate fixed effects
  model <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": post_chatgpt |", esco_level, "+ idcountry + dmax")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model)
}

run_event_study_model <- function(exposure_var, data, level) {
  esco_level <- paste0("idesco_level_", level)
  
  model_event_study <- feols(
    as.formula(paste("log_OJA ~", exposure_var, ": i(event_time, ref = 0) |", esco_level, "+ idcountry")),
    data = data,
    cluster = c("idcountry", esco_level)
  )
  
  return(model_event_study)
}

extract_event_study_coefs <- function(model, exposure_var) {
  tidy_model <- tidy(model)
  
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


plot_decile_coefficients <- function(model, exposure_var, conf_level = 0.95) {
  # Calculate z-score based on confidence level
  z_score <- qnorm(1 - (1 - conf_level)/2)
  
  # Extract coefficients and create tidy dataframe
  coefs <- tidy(model) %>%
    mutate(
      decile = as.numeric(str_extract(term, "\\d+")),
      conf_low = estimate - z_score * std.error,
      conf_high = estimate + z_score * std.error
    )
  
  # Create the plot
  ggplot(coefs, aes(x = decile, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Decile Effects for", exposure_var),
      subtitle = paste0(conf_level * 100, "% Confidence Intervals"),
      x = "Exposure Score Decile",
      y = "Coefficient Estimate"
    ) +
    theme_minimal()
}

# sensitivity analysis function
run_occupation_sensitivity <- function(data, exposure_vars, level = 3) {
  if (!(level %in% c(2, 3))) {
    stop("level must be either 2 or 3")
  }
  
  # Get unique occupations at specified level
  if (level == 2) {
    # Get level 2 occupations by removing last character
    occupations <- unique(substr(data$idesco_level_3, 1, nchar(data$idesco_level_3)-1))
  } else {
    occupations <- unique(data$idesco_level_3)
  }
  
  # For each occupation and exposure measure, run model excluding that occupation
  results <- expand_grid(
    occupation = occupations,
    exposure_var = exposure_vars
  ) %>%
    mutate(
      model = map2(occupation, exposure_var, function(occ, exp_var) {
        # Filter data differently based on level
        if (level == 2) {
          data_filtered <- data %>% 
            filter(substr(idesco_level_3, 1, nchar(occupation)) != occ)
        } else {
          data_filtered <- data %>% 
            filter(idesco_level_3 != occ)
        }
        
        model <- feols(
          as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
          data = data_filtered
        )
        
        # Extract coefficient, SE, and p-value
        coef_data <- tidy(model) %>%
          filter(term == exp_var) %>%
          select(estimate, std.error, p.value)
        
        return(coef_data)
      })
    ) %>%
    unnest(model)
  
  # Add baseline models for comparison
  baseline_results <- map_dfr(exposure_vars, function(exp_var) {
    model <- feols(
      as.formula(paste("delta_OJA_log ~", exp_var, " | idcountry")),
      data = data
    )
    
    tidy(model) %>%
      filter(term == exp_var) %>%
      select(estimate, std.error, p.value) %>%
      mutate(
        exposure_var = exp_var,
        occupation = "Baseline (All)"
      )
  })
  
  # Combine and format results
  results_table <- bind_rows(
    baseline_results,
    results
  ) %>%
    mutate(
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error
    ) %>%
    arrange(exposure_var, occupation != "Baseline (All)", abs(estimate))
  
  return(results_table)
}

# read data ---------------------------------------------------------------
oja <- list(
  l2 = list.files(
    "data/cedefop_skills_ovate_oja/csv/01__countries_and_occupations_hyper", 
    full.names = TRUE
    ) %>%
    map_dfr(read_csv),
  l3 = list.files(
    "data/cedefop_skills_ovate_skill_demand/csv/05_occupation_skill_across_occupations_hyper", 
    full.names = TRUE
  ) %>%
    map_dfr(read_csv) %>%
    mutate(
      idcountry = ifelse(is.na(idcountry), countryset, idcountry),
      esco_level_3_short = esco_level_3 # to match fromat of l2
    ) %>%
    select(-c(countryset, esco_level_3))
) %>%
  map(function(data) filter(data, !str_detect(idcountry, "EU27"))) # remove aggregate EU27

ai_exposure <- list(
  l2 = read_ai_exposure_file(
    "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
    level = 2
  ),
  l3 = read_ai_exposure_file(
    "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
    level = 3
  )
)

# format data -------------------------------------------------------------
oja_twfe <- list(
  l2 = format_twfe_oja_data(oja$l2, ai_exposure$l2, level = 2, t0 = t0),
  l3 = format_twfe_oja_data(oja$l3, ai_exposure$l3, level = 3, t0 = t0)
)

oja_delta <- list(
  l3_ap = format_delta_data(
    oja_twfe$l3, n_periods = Inf, 
    base_date = t0, level = 3, across_countries = FALSE
  ) # all periods, separate entry for each country
)

# run models --------------------------------------------------------------
# TWFE ----
twfe_models <- map(
  exposure_vars, ~run_twfe_exposure_models(.x, oja_twfe$l3, level = 3)
)

results$twfe <- twfe_models

# Print summaries
walk(seq_along(exposure_vars), function(i) {
  cat("\nModels for", exposure_vars[i], ":\n")
  print(summary(twfe_models[[i]]))
})

# Event studies ----
event_study_models <- map(
  exposure_vars, ~run_event_study_model(.x, oja_twfe$l3, level = 3)
)

result$event_study <- event_study_models

event_study_coefs <- map2(
  event_study_models, exposure_vars, ~extract_event_study_coefs(.x, .y)
)

plots <- map2(event_study_coefs, exposure_vars, ~plot_event_study(.x, .y))

for (i in seq_along(plots)) {
	print(plots[[i]])
}

# delta models ----
delta_models <- map(
  exposure_vars, function(exposure_var) {
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = oja_delta$l3_ap
    )
  }
)

names(delta_models) <- exposure_vars

result$delta <- delta_models

# decile models ----
decile_models <- map(
  exposure_vars, function(exposure_var) {
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = oja_delta$l3_ap %>%
        mutate(!!exposure_var := as.factor(ntile(!!sym(exposure_var), 10)))
    )
  }
)

names(decile_models) <- exposure_vars

results$decile <- decile_models

decile_plots <- map2(
  decile_models, 
  exposure_vars, 
  ~plot_decile_coefficients(.x, .y, .95)
)

for (i in seq_along(decile_plots)) {
  print(decile_plots[[i]])
}

results$decile_plots <- decile_plots


# sensitivity to occupations ----------------------------------------------
# Run sensitivity analysis
sensitivity_results_l3 <- run_occupation_sensitivity(oja_delta$l3_ap, exposure_vars, level = 3)
sensitivity_results_l2 <- run_occupation_sensitivity(oja_delta$l3_ap, exposure_vars, level = 2)

results$senstivity_l3 <- sensitivity_results_l3
results$senstivity_l2 <- sensitivity_results_l2

# Print results table
sensitivity_results_l3 %>%
 group_by(exposure_var) %>%
 mutate(
   pct_diff_from_baseline = (estimate / first(estimate) - 1) * 100
 ) %>%
 select(
   exposure_var, occupation,
   estimate, std.error,
   pct_diff_from_baseline,
   new_p_val = p.value
 ) %>%
 arrange(exposure_var, abs(pct_diff_from_baseline)) %>%
 print(n = Inf)

sensitivity_results_l2 %>%
  group_by(exposure_var) %>%
  mutate(
    pct_diff_from_baseline = (estimate / first(estimate) - 1) * 100
  ) %>%
  select(
    exposure_var, occupation,
    estimate, std.error,
    pct_diff_from_baseline,
    new_p_val = p.value
  ) %>%
  arrange(exposure_var, abs(pct_diff_from_baseline)) %>%
  print(n = Inf)


# save results ------------------------------------------------------------

saveRDS(results, "results/RDS/oja_models.RDS")

