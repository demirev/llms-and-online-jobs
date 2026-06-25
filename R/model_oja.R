library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork)
library(showtext)
library(sysfonts)

source("R/helpers.R")
init_text_log("oja_models.txt", overwrite = TRUE)

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Anthropic Usage Score" = "anthropic_usage_score"
)
breakdown_vars <- c(
  "Automation Exposure Score" = "ai_product_automation_score",
  "Augmentation Exposure Score" = "ai_product_augmentation_score",
  "Anthropic Automation Score" = "anthropic_automation_score",
  "Anthropic Augmentation Score" = "anthropic_augmentation_score"
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
  ),
  l4 = read_ai_exposure_file(
    "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
    level = 4
  )
)

eures <- list.files(
  "data/cedefop_eures_job_vacancy_insights/csv/14_exp_occupation_skill_country_hyper",
  full.names = TRUE
) %>%
  map_df(read_csv) %>%
  format_eures_data(
    ai_exposure = ai_exposure$l4
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

# examples of highly exposed occupations
#oja_delta$l3_ap %>% distinct(esco_level_3_short, beta_eloundou, ai_product_exposure_score, felten_exposure_score, webb_exposure_score) %>% arrange(desc(beta_eloundou)) %>% print(n = 20)

# run models --------------------------------------------------------------
# TWFE ----
twfe_models <- map(
  exposure_vars, ~run_twfe_exposure_models(.x, oja_twfe$l3, level = 3)
)
twfe_model_breakdown <- run_twfe_exposure_models(breakdown_vars, oja_twfe$l3, level = 3) # if we run separately, both are significant on their own. Together, only augmentation (negative)

results$twfe <- twfe_models

# Print summaries
walk(seq_along(exposure_vars), function(i) {
  log_text(summary(twfe_models[[i]]), paste("TWFE:", names(exposure_vars)[i]))
})

# Event studies ----
event_study_models <- map(
  exposure_vars, ~run_event_study_model(.x, oja_twfe$l3, level = 3)
)
event_study_model_breakdown <- run_event_study_model(breakdown_vars, oja_twfe$l3, level = 3) 

results$event_study <- event_study_models

log_text(
  event_study_models,
  label = "Event study models:"
)

event_study_coefs <- map2(
  event_study_models, exposure_vars, ~extract_event_study_coefs(.x, .y)
)

plots <- map2(
  event_study_coefs, exposure_vars, 
  ~plot_event_study(.x, .y, exposure_vars = exposure_vars, ylims = c(-4.8,0.4))
)

plots_breakdown <- map2(
  extract_event_study_coefs(event_study_model_breakdown, breakdown_vars),
  breakdown_vars,
  ~plot_event_study(.x, .y, exposure_vars = breakdown_vars, ylims = c(-1.5,0.4))
)

# Combine plots into a 2x2 grid using patchwork
combined_plot <- (plots[[1]] + plots[[2]]) / 
  (plots[[3]] + plots[[4]]) / 
  (plots[[5]] + (ggplot() + theme_minimal())) +
  plot_layout(guides = "collect") +  # Combine legends
  plot_annotation(
    title = "Event Study Results Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

combined_breakdown_plot <- (plots_breakdown[[1]] + plots_breakdown[[2]]) /
  (plots_breakdown[[3]] + plots_breakdown[[4]])

# Print combined plot
print(combined_plot)

results$event_study_plots <- c(plots, plots_breakdown, list(combined = combined_plot, combined_breakdown = combined_breakdown_plot))

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

log_text(
  delta_models, 
  "Delta models:"
)

delta_models_breakdown <- list(
  combined = feols(
    as.formula(paste("delta_OJA_log ~ ai_product_automation_score + ai_product_augmentation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  ),
  automation = feols(
    as.formula(paste("delta_OJA_log ~ ai_product_automation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  ),
  augmentation = feols(
    as.formula(paste("delta_OJA_log ~ ai_product_augmentation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  ),
  combined = feols(
    as.formula(paste("delta_OJA_log ~ anthropic_automation_score + anthropic_augmentation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  ),
  automation_anthropic = feols(
    as.formula(paste("delta_OJA_log ~ anthropic_automation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  ),
  augmentation = feols(
    as.formula(paste("delta_OJA_log ~ anthropic_augmentation_score | idcountry")),
    data = oja_delta$l3_ap,
    cluster = "idcountry"
  )
)

results$delta_breakdown <- delta_models_breakdown

log_text(
  delta_models_breakdown, 
  "Delta models, breakdown by intent:"
)

delta_plots <- map(
  exposure_vars,
  ~ {
    var_name <- names(exposure_vars)[exposure_vars == .x]
    
    data_filtered <- oja_delta$l3_ap %>%
      filter(
        !is.na(delta_OJA_log),
        !is.na(!!sym(.x)),
        !is.na(idesco_level_3)
      ) 
    
    data_filtered[["x"]] <- data_filtered[[.x]]
    
    df_means <- data_filtered %>%
      group_by(idesco_level_3) %>%
      summarize(
        px_mean = mean(x, na.rm = TRUE),
        py_mean = mean(delta_OJA_log, na.rm = TRUE)
      )
    
    ggplot(data_filtered, aes(x, delta_OJA_log)) +
      # make the points faint and gray
      geom_point(color = "lightgray") +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
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

delta_plots_breakdown <- map(
  breakdown_vars,
  ~ {
    var_name <- names(breakdown_vars)[breakdown_vars == .x]
    
    data_filtered <- oja_delta$l3_ap %>%
      filter(
        !is.na(delta_OJA_log),
        !is.na(!!sym(.x)),
        !is.na(idesco_level_3)
      ) 
    
    data_filtered[["x"]] <- data_filtered[[.x]]
    
    df_means <- data_filtered %>%
      group_by(idesco_level_3) %>%
      summarize(
        px_mean = mean(x, na.rm = TRUE),
        py_mean = mean(delta_OJA_log, na.rm = TRUE)
      )
    
    ggplot(data_filtered, aes(x, delta_OJA_log)) +
      # make the points faint and gray
      geom_point(color = "lightgray") +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
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

delta_plots$combined <- (delta_plots[[1]] + delta_plots[[2]]) / 
  (delta_plots[[3]] + delta_plots[[4]]) / 
  (delta_plots[[5]] + (ggplot() + theme_minimal())) +
  # (delta_plots_breakdown[[1]] + delta_plots_breakdown[[2]]) +
  plot_layout(guides = "collect") +  # Combine legends
  plot_annotation(
    title = "Delta OJA Log vs AI Exposure Measures",
    theme = theme_minimal()
  )

delta_plots$combined_breakdown <- (delta_plots_breakdown[[1]] + delta_plots_breakdown[[2]]) / 
  (delta_plots_breakdown[[3]] + delta_plots_breakdown[[4]]) +
  # (delta_plots_breakdown[[1]] + delta_plots_breakdown[[2]]) +
  plot_layout(guides = "collect") +  # Combine legends
  plot_annotation(
    title = "Delta OJA Log vs AI Automation and Augmentation Measures",
    theme = theme_minimal()
  )

results$delta_plots <- delta_plots

cor.test(oja_delta$l3_ap$ai_product_automation_score, oja_delta$l3_ap$ai_product_augmentation_score)
cor.test(oja_delta$l3_ap$anthropic_automation_score, oja_delta$l3_ap$anthropic_augmentation_score)

partial_plots <- map(
  c(exposure_vars, breakdown_vars),
  ~ {
    var_name <- names(c(exposure_vars, breakdown_vars))[c(exposure_vars, breakdown_vars) == .x]
    
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
  (partial_plots[[5]] + (ggplot() + theme_minimal())) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Partial Regression Plots Across Different AI Exposure Measures",
    theme = theme_minimal()
  )

results$partial_plots <- partial_plots

# delta models untreated adjustment ----
delta_models_adj <- map(
  exposure_vars, function(exposure_var) {
    dat <- oja_delta$l3_ap 
    
    mean_change_untreated <- dat %>%
      filter(dat[[exposure_var]] <= 0.1) %>%
      group_by(idcountry) %>%
      filter(!is.na(delta_OJA_log)) %>%
      summarize(
        mean_change = mean(delta_OJA_log, na.rm = TRUE)
      )
    
    dat <- dat %>%
      left_join(mean_change_untreated, by = "idcountry") %>%
      mutate(
        delta_OJA_log = delta_OJA_log - mean_change
      )
    
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, " | idcountry")),
      data = dat,
      cluster = "idcountry"
    )
  }
)

results$delta_models_adj <- delta_models_adj

log_text(
  delta_models_adj, 
  "Delta models, adjusted for 'untreated':"
)

# delta models by country ----
delta_models_cntry <- map(
  exposure_vars, function(exposure_var) {
    dat <- oja_delta$l3_ap 
    
    dat %>%
      split(~.$idcountry) %>%
      map(function(country_dat) {
        feols(
          as.formula(paste("delta_OJA_log ~", exposure_var)),
          data = country_dat
        ) %>%
          broom::tidy() %>%
          mutate(country = country_dat$idcountry[1])
      }) %>%
      bind_rows() %>%
      filter(term!="(Intercept)")
  }
)

results$delta_models_cntry <- delta_models_cntry

# Country maps ----
map_plots_cntry <- map2(
  delta_models_cntry, names(exposure_vars),
  ~plot_country_map(.x, title = .y)
)

map_plots_cntry$combined <- wrap_plots(map_plots_cntry[1:5], nrow = 3) +
  plot_annotation(
    title = "AI Exposure Effect by Country (significant estimates highlighted)",
    theme = theme_minimal()
  )

avg_cntry_coefs <- bind_rows(delta_models_cntry) %>%
  group_by(country) %>%
  summarise(estimate = mean(estimate), p.value = mean(p.value), .groups = "drop")

map_plot_avg <- plot_country_map(avg_cntry_coefs, title = "Average across exposure measures")

results$map_plots_cntry <- map_plots_cntry
results$map_plot_avg    <- map_plot_avg

hist(delta_models_cntry[[1]]$estimate)
hist(delta_models_cntry[[2]]$estimate)
hist(delta_models_cntry[[3]]$estimate)
hist(delta_models_cntry[[4]]$estimate)

delta_models_cntry[[1]] %>% filter(p.value < 0.05) %>% arrange(desc(estimate))
delta_models_cntry[[2]] %>% filter(p.value < 0.05) %>% arrange(desc(estimate))
delta_models_cntry[[3]] %>% filter(p.value < 0.05) %>% arrange(desc(estimate))
delta_models_cntry[[4]] %>% filter(p.value < 0.05) %>% arrange(desc(estimate))

delta_plots_cntry <- map(
  exposure_vars, function(exposure_var) {
    dat <- oja_delta$l3_ap 
    
    dat %>%
      split(~.$idcountry) %>%
      map(function(country_dat) {
        country_dat %>%
          ggplot(aes_string(x = exposure_var, y = "delta_OJA_log")) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = TRUE, color = "blue") +
          labs(
            title = paste("Country:", unique(country_dat$idcountry)),
            x = exposure_var,
            y = "log (OJA Post / OJA Pre)"
          ) +
          theme_minimal()
      })
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

log_text(
  decile_models, 
  "By-decile models:"
)

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

# EURES by experience -----------------------------------------------------
# just checking - we don't have data before 2024
delta_eures <- eures %>%
  group_by(
    idcountry, idesco_level_4, experience
  ) %>%
  arrange(dmax) %>%
  summarise(
    delta_log_OJA = last(log_OJA) - first(log(OJA)),
    beta_eloundou = last(beta_eloundou),
    ai_product_exposure_score = last(ai_product_exposure_score),
    ai_product_automation_score = last(ai_product_automation_score),
    ai_product_augmentation_score = last(ai_product_augmentation_score),
    webb_exposure_score = last(webb_exposure_score),
    felten_exposure_score = last(felten_exposure_score),
    anthropic_usage_score = last(anthropic_usage_score),
    anthropic_automation_score = last(anthropic_automation_score),
    anthropic_augmentation_score = last(anthropic_augmentation_score)
  )

eures_models <- setNames(c(exposure_vars, breakdown_vars), c(exposure_vars, breakdown_vars)) %>%
  map(~ feols(
    as.formula(paste0("delta_log_OJA ~ i(experience, ", .x, ") | idcountry")),
    data = delta_eures,
    cluster = ~idcountry
  )) %>%
  map(broom::tidy) %>%
  map2_dfr(names(.), function(md,nm) mutate(md, var = nm))

exp_levels <- c("No experience", "Up to 1 year", "From 1 to 2 years",
                "From 2 to 4 years", "From 4 to 6 years", "From 6 to 8 years",
                "From 8 to 10 years", "Over 10 years")

log_text(
  eures_models, 
  "Eures by experience level:", 
  n = Inf
)

# Nicer facet labels for the exposure measures
var_labels <- c(
  ai_product_exposure_score = "AI Product Exposure",
  beta_eloundou            = "Eloundou et al.",
  felten_exposure_score    = "Felten Exposure",
  webb_exposure_score      = "Webb Exposure",
  ai_product_augmentation_score = "Augmentation Exposure",
  ai_product_automation_score = "Automation Exposure",
  anthropic_usage_score        = "Anthropic Usage",
  anthropic_augmentation_score = "Anthropic Augmentation",
  anthropic_automation_score   = "Anthropic Automation"
)

eures_plot_df <- eures_models %>%
  mutate(
    experience = str_extract(term, "(?<=::)[^:]+(?=:)"),
    experience = factor(experience, levels = exp_levels),
    sig = case_when(
      p.value < 0.01 ~ "p < 0.01",
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10",
      TRUE           ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s.")),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error,
    var_label = var_labels[var]
  )

results$eures_plot <- eures_plot_df %>% 
  filter(var_label %in% var_labels[c(1:4, 7)]) %>% # no automation / augmentation breakdown here
  ggplot(aes(x = experience, y = estimate, colour = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi), size = 0.5) +
  scale_colour_manual(
    values = c("p < 0.01" = "#d62728", "p < 0.05" = "#ff7f0e",
               "p < 0.10" = "#2ca02c", "n.s." = "grey60"),
    name = "Significance"
  ) +
  facet_wrap(
    ~var_label#, scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = "coefficient estimate",
    title = "Association between AI Exposure and Job Ad Growth by Experience Level"
    #, subtitle = "Country fixed effects, SEs clustered by country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

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
 log_text("Sensitivity: drop-one ESCO Level 3 occupation", n = Inf)

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
  log_text("Sensitivity: drop-one ESCO Level 2 occupation", n = Inf)

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
  log_text("Sensitivity: drop-one country", n = Inf)

# save results ------------------------------------------------------------
saveRDS(results, "results/RDS/oja_models.RDS")

save_plot(
  "partial_plots_all.eps",
  results$partial_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "partial_plots_all.svg",
  results$partial_plots$combined,
  width = 10,
  height = 6,
  device = svg
)

# partial plots
save_plot(
  "partial_plots_eloundou.eps",
  results$partial_plots$`Eloundou Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

# top five exposed: OC413 (up), OC251, OC252, OC264, OC212 (all down)
# "keyboard operators" (up), "software developers", "database professionals", "authors and journalists", "mathematicians and statisticians" (down)
# biggest drop in OJA is OC815 (Mining plant operators), OC622 (Textile machine operators), OC811 (fishery workers and hunters) despite low exposure

save_plot(
  "partial_plots_demirev.eps",
  results$partial_plots$`Demirev Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "partial_plots_webb.eps",
  results$partial_plots$`Webb AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "partial_plots_felten.eps",
  results$partial_plots$`Felten AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

# delta plots
save_plot(
  "delta_plots_all.eps",
  results$delta_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "delta_plots_eloundou.eps",
  results$delta_plots$`Eloundou Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "delta_plots_demirev.eps",
  results$delta_plots$`Demirev Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "delta_plots_felten.eps",
  results$delta_plots$`Felten AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "delta_plots_webb.eps",
  results$delta_plots$`Webb AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

# decile plots
save_plot(
  "decile_plots_combined.eps",
  results$decile_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "decile_plots_eloundou.eps",
  results$decile_plots$beta_eloundou,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "decile_plots_demirev.eps",
  results$decile_plots$ai_product_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "decile_plots_webb.eps",
  results$decile_plots$webb_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "decile_plots_felten.eps",
  results$decile_plots$felten_exposure_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "decile_plots_anthropic.eps",
  results$decile_plots$anthropic_usage_score,
  width = 10,
  height = 6,
  device = cairo_ps
)

# event study plots
save_plot(
  "event_study_all.eps",
  results$event_study_plots$combined,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "event_study_eloundou.eps",
  results$event_study_plots$`Eloundou Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "event_study_demirev.eps",
  results$event_study_plots$`Demirev Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "event_study_webb.eps",
  results$event_study_plots$`Webb AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "event_study_felten.eps",
  results$event_study_plots$`Felten AI Exposure Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "event_study_anthropic.eps",
  results$event_study_plots$`Anthropic Usage Score`,
  width = 10,
  height = 6,
  device = cairo_ps
)

save_plot(
  "by_experience.eps",
  results$eures_plot,
  width = 10,
  height = 6,
  device = cairo_ps
)

