library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "AI Product Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Beta Score" = "beta_eloundou"
)

results <- list()


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
  # Get the name for the exposure variable
  var_name <- names(exposure_vars)[exposure_vars == exposure_var]
  
  ggplot(coefs, aes(x = event_time, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = paste("Event Study for", var_name),
         x = "Event Time (quarters since ChatGPT release)",
         y = "Coefficient Estimate") +
    theme_minimal()
}


plot_decile_coefficients <- function(model, exposure_var, conf_level = 0.95) {
  # Get the name for the exposure variable
  var_name <- names(exposure_vars)[exposure_vars == exposure_var]
  
  z_score <- qnorm(1 - (1 - conf_level)/2)
  
  coefs <- tidy(model) %>%
    mutate(
      decile = as.numeric(str_extract(term, "\\d+")),
      conf_low = estimate - z_score * std.error,
      conf_high = estimate + z_score * std.error
    )
  
  ggplot(coefs, aes(x = decile, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Decile Effects for", var_name),
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
  cat("\nModels for", names(exposure_vars)[i], ":\n")
  print(summary(twfe_models[[i]]))
})

# Event studies ----
event_study_models <- map(
  exposure_vars, ~run_event_study_model(.x, oja_twfe$l3, level = 3)
)

results$event_study <- event_study_models

event_study_coefs <- map2(
  event_study_models, exposure_vars, ~extract_event_study_coefs(.x, .y)
)

plots <- map2(event_study_coefs, exposure_vars, ~plot_event_study(.x, .y))

# Combine plots into a 2x2 grid using patchwork
combined_plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) +
  plot_layout(guides = "collect") +  # Combine legends
  plot_annotation(
    title = "Event Study Results Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

# Print combined plot
print(combined_plot)

# delta models ----
delta_models <- map(
  exposure_vars, function(exposure_var) {
    dat <- oja_delta$l3_ap %>%
      filter(pre_OJA > 20 & post_OJA > 20)
    
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = oja_delta$l3_ap,
      cluster = "idcountry"
    )
  }
)

names(delta_models) <- exposure_vars

results$delta <- delta_models

partial_plots <- map(
  exposure_vars,
  ~ {
    var_name <- names(exposure_vars)[exposure_vars == .x]
    
    data_filtered <- oja_delta$l3_ap %>%
      filter(
        !is.na(delta_OJA_log),
        !is.na(!!sym(.x)),
        !is.na(idesco_level_3)
      ) %>%
      filter(pre_OJA > 20 & post_OJA > 20)
    
    py <- resid(feols(delta_OJA_log ~ 1 | idcountry, data = data_filtered))
    px <- resid(feols(as.formula(paste(.x, "~ 1 | idcountry")), data = data_filtered))
    
    df_partial <- tibble(
      px = px,
      py = py,
      idesco_level_3 = data_filtered$idesco_level_3
    )
    
    df_means <- df_partial %>%
      group_by(idesco_level_3) %>%
      summarize(
        px_mean = mean(px, na.rm = TRUE),
        py_mean = mean(py, na.rm = TRUE)
      )
    
    ggplot(df_partial, aes(px, py)) +
      # make the points faint and gray
      geom_point(color = "gray", alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      geom_point(
        data = df_means,
        aes(x = px_mean, y = py_mean),
        color = "gray10",
        shape = 4,
        size = 2,
        stroke = 1
      ) +
      geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
      # limit the y-axis from -1 to 1
      coord_cartesian(ylim = c(-1, 1)) +
      labs(x = var_name, y = "log (OJA Post / OJA Pre)") +
      theme_minimal()
  }
)


# decile models ----
decile_models <- map(
  exposure_vars, function(exposure_var) {
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = oja_delta$l3_ap %>%
        mutate(!!exposure_var := as.factor(ntile(!!sym(exposure_var), 10))),
      cluster = "idcountry"
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

# Combine partial plots into 2x2 grid
combined_partial_plots <- (partial_plots[[1]] + partial_plots[[2]]) / 
  (partial_plots[[3]] + partial_plots[[4]]) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Partial Regression Plots Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

# Print combined partial plots
print(combined_partial_plots)

# Replace individual decile plot printing with combined 2x2 grid
combined_decile_plots <- (decile_plots[[1]] + decile_plots[[2]]) / 
  (decile_plots[[3]] + decile_plots[[4]]) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Decile Effects Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

# Print combined decile plots
print(combined_decile_plots)

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

