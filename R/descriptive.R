library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork) # remotes::install_github("thomasp85/patchwork")
library(showtext)
library(sysfonts)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "AI Product Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Beta Score" = "beta_eloundou"
)

results <- list()

font_add_google("Merriweather", "merriweather")
showtext_auto()

# functions --------------------------------------------------------------
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
      sum_OJA = sum(OJA),
      .groups = "drop"
    ) %>%
    group_by(score_percentile) %>%
    mutate(
      OJA_index = sum_OJA / first(sum_OJA) * 100
    ) %>%
    ggplot(
      aes(
        x = dmax, 
        y = sum_OJA,
        color = score_percentile, 
        lty = score_percentile
      )
    ) +
    geom_line() +
    geom_vline(xintercept = as.Date("2022-11-30"), linetype = "dashed") +
    scale_color_grey(start = 0.8, end = 0.2) +
    labs(
      title = paste("OJA Index by", names(exposure_vars)[exposure_vars == exposure_var], "Exposure"),
      x = "Date",
      y = "OJA Index",
      color = paste(n_tiles, "-tiles"),
      linetype = paste(n_tiles, "-tiles")
    ) +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

# read data ---------------------------------------------------------------
oja <- list(
  l2 = list.files("data/cedefop_skills_ovate_oja/csv/01__countries_and_occupations_hyper", full.names = TRUE) %>%
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
  l2_fl = format_delta_data(
    oja_twfe$l2, n_periods = -1, 
    base_date = t0, level = 2, across_countries = FALSE
  ),
  l3_fl = format_delta_data(
    oja_twfe$l3, n_periods = -1, 
    base_date = t0, level = 3, across_countries = FALSE
  ),
  l2_3p = format_delta_data(
    oja_twfe$l2, n_periods = 3, 
    base_date = t0, level = 2, across_countries = FALSE
  ),
  l3_3p = format_delta_data(
    oja_twfe$l3, n_periods = 3, 
    base_date = t0, level = 3, across_countries = FALSE
  ),
  l2_ap = format_delta_data(
    oja_twfe$l2, n_periods = Inf, 
    base_date = t0, level = 2, across_countries = FALSE
  ),
  l3_ap = format_delta_data(
    oja_twfe$l3, n_periods = Inf, 
    base_date = t0, level = 3, across_countries = FALSE
  ), # seems most sensible to use this one
  l2_ap_ac = format_delta_data(
    oja_twfe$l2, n_periods = Inf, 
    base_date = t0, level = 2, across_countries = TRUE
  ),
  l3_ap_ac = format_delta_data(
    oja_twfe$l3, n_periods = Inf, 
    base_date = t0, level = 3, across_countries = TRUE
  )
)

skills <- list.files(
  "data/cedefop_skills_ovate_skill_demand/csv/05_esco_skill_skill_across_occupations_hyper", 
  full.names = TRUE
) %>%
  map(function(file_name) {
    date <- str_extract(file_name, "\\d{4}_q\\d{1}") %>%
      str_replace("_q1", "-03-31") %>%
      str_replace("_q2", "-06-30") %>%
      str_replace("_q3", "-09-30") %>%
      str_replace("_q4", "-12-31") %>%
      as.Date()
    data <- read_csv(file_name)
    data <- data %>%
      mutate(dmax = date, dmin = date - months(12))
    data
  }) %>%
  bind_rows() %>%
  mutate(
    idcountry = ifelse(
      is.na(idcountry),
      countryset,
      idcountry
    )
  ) %>%
  select(-countryset)

# Calculate correlation matrix across exposure scores ------------------------
results$l3_exposure_correlation <- ai_exposure$l3 %>%
  select(
    ai_product_exposure_score,
    felten_exposure_score,
    webb_exposure_score,
    beta_eloundou
  ) %>%
  cor(use = "complete.obs") # used in manuscript

# Create correlation plot using corrplot
l3_exposure_correlation_plot <- corrplot::corrplot(
  results$l3_exposure_correlation,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  number.cex = 0.8,
  tl.cex = 0.8
)

results$l3_exposure_correlation_plot_2 <- PerformanceAnalytics::chart.Correlation(
  ai_exposure$l3 %>%
    select(
      "Demirev 2024" = ai_product_exposure_score,
      "Felten et al 2018" = felten_exposure_score,
      "Webb 2022" = webb_exposure_score,
      "Eloundou et al 2023" = beta_eloundou
    ) %>%
    na.omit(), 
  histogram = TRUE,
  pch = 19
) # used in manuscript

# biggest changes ---------------------------------------------------------
results$oja_changes_table <- oja_delta$l3_ap %>% 
  group_by(
    idesco_level_3, esco_level_3_short
  ) %>% 
  summarise(
    mean_delta_OJA = exp(mean(delta_OJA_log))#,
    # ai_product_exposure_score = mean(ai_product_exposure_score),
    # felten_exposure_score = mean(felten_exposure_score),
    # webb_exposure_score = mean(webb_exposure_score),
    # beta_eloundou = mean(beta_eloundou)
  ) %>% 
  ungroup() %>%
  select(esco_level_3_short, mean_delta_OJA) %>%
  arrange(mean_delta_OJA) %>% print(n = Inf) # used in manuscript

# OJA time series ---------------------------------------------------------
results$oja_time_series <- oja$l3 %>%
  group_by(idesco_level_1, dmax) %>%
  summarise(
    total_oja = sum(OJA)
  ) %>%
  left_join(
    bind_rows(
      tibble(idesco_level_1 = "OC1", idesco_label = "Managers"),
      tibble(idesco_level_1 = "OC2", idesco_label = "Professionals"),
      tibble(idesco_level_1 = "OC3", idesco_label = "Technical professionals"), # shortened from "Technicians and associate professionals"
      tibble(idesco_level_1 = "OC4", idesco_label = "Clerical support workers"), # shortened from "Clerical and support workers"
      tibble(idesco_level_1 = "OC5", idesco_label = "Service and sales workers"),
      tibble(idesco_level_1 = "OC6", idesco_label = "Agricultural workers"), # shortened from "Skilled agricultural, forestry and fishery workers"
      tibble(idesco_level_1 = "OC7", idesco_label = "Craft and trades workers"), # shortened from "Craft and related trades workers"
      tibble(idesco_level_1 = "OC8", idesco_label = "Plant and machine operators"), # shortened from "Plant and machine operators, and assemblers"
      tibble(idesco_level_1 = "OC9", idesco_label = "Elementary occupations")
    ),
    by = "idesco_level_1"
  ) %>%
  ggplot(
    aes(
      x = dmax, 
      y = total_oja, 
      color = idesco_label, 
      lty = idesco_label
    )
  ) + 
  geom_line() +
  theme_minimal() +
  scale_color_grey(start = 0, end = .7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Date",
    y = "Total Online Job Adverts"
  ) +
  ggtitle("Total Online Job Adverts by ISCO Major Group") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  theme(text = element_text(family = "merriweather"))

# two-fold reduction
# idesco_level_1 change
# <chr>           <dbl>
#   1 OC1              2.41
# 2 OC2              2.40
# 3 OC3              2.13
# 4 OC4              2.17
# 5 OC5              2.09
# 6 OC6              2.22
# 7 OC7              1.99
# 8 OC8              2.08
# 9 OC9              1.88

# > oja$l3 %>% filter(dmax == max(dmax)) %>% pull(OJA) %>% sum()
# [1] 18505936
# > oja$l3 %>% filter(dmax == min(dmax)) %>% pull(OJA) %>% sum()
# [1] 40277646

# base correlations ------------------------------------------------------
correlation_results <- map(exposure_vars, function(var) {
  cor.test(
    oja_delta$l3_ap$delta_OJA_log,
    oja_delta$l3_ap[[var]],
    use = "complete.obs"
  )
})

names(correlation_results) <- exposure_vars

# Print summary of correlations
correlation_table <- map_dfr(names(correlation_results), function(var) {
  test <- correlation_results[[var]]
  tibble(
    exposure = var,
    correlation = test$estimate,
    p_value = test$p.value,
    conf_low = test$conf.int[1],
    conf_high = test$conf.int[2]
  )
})

results$correlation_table <- correlation_table # used

# scatter plots -----------------------------------------------------------
exposure_plots <- map(exposure_vars, function(var) {
  ggplot(oja_delta$l3_ap, aes(x = .data[[var]], y = delta_OJA_log)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = paste(names(exposure_vars)[exposure_vars == var]),
      y = "Log Change in Job Ads",
      title = paste("Correlation between", names(exposure_vars)[exposure_vars == var], "and Change in Job Ads")
    ) +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
})

names(exposure_plots) <- exposure_vars

exposure_plots$combined <- wrap_plots(exposure_plots, ncol = 2)

results$exposure_plots <- exposure_plots # not used in manuscript

# change by n-tile --------------------------------------------------------
plot_change_by_ntile <- function(data, exposure_var) {
  data %>%
    mutate(
      score = .data[[exposure_var]],
      score_n_tile = ntile(score, 100)
    ) %>%
    group_by(score_n_tile) %>%
    summarise(
      pre_OJA = sum(pre_OJA, na.rm = TRUE),
      post_OJA = sum(post_OJA, na.rm = TRUE),
      mean_change_log = mean(delta_OJA_log, na.rm = TRUE),
      mean_change_rel = mean(delta_OJA_relative, na.rm = TRUE),
      total_change_log = log(post_OJA / pre_OJA),
      total_change_rel = (post_OJA - pre_OJA) / pre_OJA
    ) %>%
    ggplot(aes(x = score_n_tile, y = total_change_log)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = paste(names(exposure_vars)[exposure_vars == exposure_var], "(Percentile)"),
      y = "Mean Change in Job Ads (Log)",
      title = paste("Change in Job Ads by", names(exposure_vars)[exposure_vars == exposure_var])
    ) +
    theme_minimal()
}

# Create all plots
ntile_plots <- map(exposure_vars, ~plot_change_by_ntile(oja_delta$l3_ap, .x))
names(ntile_plots) <- exposure_vars

# Optional: display all plots in a grid
ntile_plots$combined <- wrap_plots(ntile_plots, ncol = 2)

results$ntile_plots <- ntile_plots # not used in manuscript

# Plot time series --------------------------------------------------------
# Level 3 plots
l3_time_plots <- map(exposure_vars, function(var) {
  plot_exposure_quintiles(
    oja_twfe$l3, 
    level = 3, 
    exposure_var = var, 
    n_tiles = 10
  )
})
names(l3_time_plots) <- exposure_vars

# Level 2 plots
l2_time_plots <- map(exposure_vars, function(var) {
  plot_exposure_quintiles(
    oja_twfe$l2, 
    level = 2, 
    exposure_var = var, 
    n_tiles = 7
  )
})
names(l2_time_plots) <- exposure_vars

# Optional: display all plots in grids
l3_time_plots$combined <- wrap_plots(l3_time_plots, ncol = 2)
l2_time_plots_combined <- wrap_plots(l2_time_plots, ncol = 2)

results$l3_time_plots <- l3_time_plots # not used in manuscript

# save results ------------------------------------------------------------
saveRDS(results, "results/RDS/descriptive.RDS")

ggsave(
  file.path("results/plots", "oja_time_series.eps"),
  results$oja_time_series,
  width = 12,
  height = 5
)
