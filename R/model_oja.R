library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork)
library(showtext)
library(sysfonts)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou"
)

results <- list()

font_add_google("Merriweather", "merriweather")
showtext_auto()

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
  ) %>% # all periods, separate entry for each country
    filter(pre_OJA > 20 & post_OJA > 20) # filter out infrequent occupations - around 14% of obs 2817/3270
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

results$event_study_plots <- c(plots, list(combined = combined_plot))

# delta models ----
delta_models <- map(
  exposure_vars, function(exposure_var) {
    dat <- oja_delta$l3_ap 
    
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = dat,
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
      ) 
    
    py <- resid(feols(delta_OJA_log ~ 1 | idcountry, data = data_filtered))
    px <- resid(feols(as.formula(paste(.x, "~ 1 | idcountry")), data = data_filtered))
    
    # Get coefficient and standard error from the model
    model <- feols(
      as.formula(paste("delta_OJA_log ~", .x, "| idcountry")),
      data = data_filtered,
      cluster = "idcountry"
    )
    
    coef <- coef(model)[.x]
    se <- sqrt(vcov(model)[.x, .x])
    
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
    
    # Calculate line endpoints for manual plotting
    x_range <- range(px, na.rm = TRUE)
    line_x <- x_range
    line_y <- coef * line_x
    
    # Calculate confidence interval lines
    ci_upper <- (coef + 1.96 * se) * line_x
    ci_lower <- (coef - 1.96 * se) * line_x
    
    # Create data frame for confidence interval ribbon
    ci_data <- tibble(
      x = c(line_x, rev(line_x)),
      y = c(ci_lower, rev(ci_upper))
    )
    
    ggplot(df_partial, aes(px, py)) +
      # make the points faint and gray
      geom_point(color = "lightgray") +
      #geom_smooth(method = "lm", se = TRUE, color = "blue") +
      # Add confidence interval ribbon
      geom_polygon(
        data = ci_data, aes(x = x, y = y), fill = "blue", alpha = 0.1
      ) +
      # Add regression line
      geom_line(
        data = tibble(x = line_x, y = line_y), 
        aes(x = x, y = y), 
        color = "blue", linewidth = 0.5
      ) +
      geom_point(
        data = df_means,
        aes(x = px_mean, y = py_mean),
        color = "gray10",
        shape = 4,
        size = 2,
        stroke = 1
      ) +
      geom_hline(yintercept = 0, color = "black") +
      # limit the y-axis from -1 to 1
      coord_cartesian(ylim = c(-1, 1)) +
      labs(x = var_name, y = "log (OJA Post / OJA Pre)") +
      theme_minimal() +
      theme(text = element_text(family = "merriweather"))
  }
)

partial_plots$combined <- (partial_plots[[1]] + partial_plots[[2]]) / 
  (partial_plots[[3]] + partial_plots[[4]]) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Partial Regression Plots Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

results$partial_plots <- partial_plots

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

decile_plots$combined <- (decile_plots[[1]] + decile_plots[[2]]) / 
  (decile_plots[[3]] + decile_plots[[4]]) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Decile Effects Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

results$decile_plots <- decile_plots

# sensitivity to occupations ----------------------------------------------
# Run sensitivity analysis
sensitivity_results_l3 <- run_occupation_sensitivity(oja_delta$l3_ap, exposure_vars, level = 3)
sensitivity_results_l2 <- run_occupation_sensitivity(oja_delta$l3_ap, exposure_vars, level = 2)
sensitivity_result_countries <- run_country_sensitivity(oja_delta$l3_ap, exposure_vars)

results$senstivity_l3 <- sensitivity_results_l3
results$senstivity_l2 <- sensitivity_results_l2
results$senstivity_countries <- sensitivity_result_countries

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
 #filter(occupation == "Baseline (All)" | new_p_val > 0.05) %>%
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
  #filter(occupation == "Baseline (All)" | new_p_val > 0.05) %>%
  print(n = Inf)

sensitivity_result_countries %>%
  group_by(exposure_var) %>%
  mutate(
    pct_diff_from_baseline = (estimate / first(estimate) - 1) * 100
  ) %>%
  select(
    exposure_var, country,
    estimate, std.error,
    pct_diff_from_baseline,
    new_p_val = p.value
  ) %>%
  arrange(exposure_var, abs(pct_diff_from_baseline)) %>%
  #filter(country == "Baseline (All)" | new_p_val > 0.05) %>%
  print(n = Inf)

# save results ------------------------------------------------------------
saveRDS(results, "results/RDS/oja_models.RDS")

ggsave(
  file.path("results/plots", "partial_plots_all.eps"),
  results$partial_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "partial_plots_all.svg"),
  results$partial_plots$combined,
  width = 10,
  height = 6,
  device = svg
)

# partial plots
ggsave(
  file.path("results/plots", "partial_plots_eloundou.eps"),
  results$partial_plots$`Eloundou Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "partial_plots_demirev.eps"),
  results$partial_plots$`Demirev Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "partial_plots_webb.eps"),
  results$partial_plots$`Webb AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "partial_plots_felten.eps"),
  results$partial_plots$`Felten AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

# decile plots
ggsave(
  file.path("results/plots", "decile_plots_combined.eps"),
  results$decile_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "decile_plots_eloundou.eps"),
  results$decile_plots$beta_eloundou,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "decile_plots_demirev.eps"),
  results$decile_plots$ai_product_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "decile_plots_webb.eps"),
  results$decile_plots$webb_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "decile_plots_felten.eps"),
  results$decile_plots$felten_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

# event study plots
ggsave(
  file.path("results/plots", "event_study_all.eps"),
  results$event_study_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "event_study_eloundou.eps"),
  results$event_study_plots$`Eloundou Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "event_study_demirev.eps"),
  results$event_study_plots$`Demirev Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "event_study_webb.eps"),
  results$event_study_plots$`Webb AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

ggsave(
  file.path("results/plots", "event_study_felten.eps"),
  results$event_study_plots$`Felten AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)
