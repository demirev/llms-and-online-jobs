library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork) # remotes::install_github("thomasp85/patchwork")
library(showtext)
library(sysfonts)

source("R/helpers.R")
init_text_log("descriptive.txt")

t0 <- as.Date("2022-11-30") # chatgpt release date

exposure_vars <- c(
  "Anthropic Usage Score" = "anthropic_usage_score",
  "AI Product Exposure Score" = "ai_product_exposure_score",
  "Eloundou Beta Score" = "beta_eloundou",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score"
  #, "Anthropic Automation Score" = "anthropic_automation_score",
  #, "Anthropic Augmentation Score" = "anthropic_augmentation_score"
)

results <- list()

font_add_google("Merriweather", "merriweather")
showtext_auto()

# functions --------------------------------------------------------------
plot_exposure_quintiles <- function(data, level, exposure_var, n_tiles = 5, index_date = NULL) {
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
      OJA_index = if (is.null(index_date)) {
        sum_OJA / first(sum_OJA) * 100
      } else {
        sum_OJA / sum_OJA[dmax == index_date] * 100 # index to the release window
      }
    ) %>%
    ggplot(
      aes(
        x = dmax, 
        y = if (is.null(index_date)) sum_OJA else OJA_index,
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
      y = if (is.null(index_date)) "OJA" else paste0("OJA index (", format(index_date, "%Y Q4"), " = 100)"),
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

eures_skills <- list.files(
  "data/cedefop_eures_job_vacancy_insights/csv/14_exp_occupation_skill_country_copy_1_hyper",
  full.names = TRUE
) %>%
  map_df(function(fl) {
    read_csv(fl) %>%                                                                                       
      mutate(       
        year    = str_extract(fl, "\\d{4}(?=_q)") %>% as.integer(),
        quarter = str_extract(fl, "(?<=_q)\\d")   %>% as.integer(),
        dmax    = make_date(year, quarter * 3, 1) + months(1) - days(1),
        dmin    = dmax - years(1) + days(1)
      )
  })

# eures %>% filter(str_length(idesco_level_4) == 6 & !is.na(experience))

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
    anthropic_usage_score,
    ai_product_exposure_score,
    beta_eloundou,
    felten_exposure_score,
    webb_exposure_score
  ) %>%
  cor(use = "complete.obs") # used in manuscript

log_text(results$l3_exposure_correlation, "Correlation coefs")

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

l3_exposure_chart_data <- ai_exposure$l3 %>%
  select(
    "Anthropic 2025" = anthropic_usage_score,
    "Demirev 2024" = ai_product_exposure_score,
    "Eloundou et al 2023" = beta_eloundou,
    "Felten et al 2018" = felten_exposure_score,
    "Webb 2022" = webb_exposure_score
  ) %>%
  na.omit()

results$l3_exposure_correlation_plot_2 <- PerformanceAnalytics::chart.Correlation(
  l3_exposure_chart_data,
  histogram = TRUE,
  pch = 19
) # used in manuscript

# tex/img/index_correlations.eps (Figure "Correlation between Indices of AI
# Exposure"). chart.Correlation draws with base graphics, so it is written to
# each output directory directly instead of through save_plot()
for (dir in c("results/plots", "tex/img")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  cairo_ps(file.path(dir, "index_correlations.eps"), width = 10, height = 6)
  PerformanceAnalytics::chart.Correlation(l3_exposure_chart_data, histogram = TRUE, pch = 19)
  dev.off()
}

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
  arrange(mean_delta_OJA) %>%
  log_text("OJA changes by occupation (ESCO Level 3)", n = Inf) # used in manuscript

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
  geom_vline(xintercept = t0, linetype = "dashed") +
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
  theme(text = element_text(family = "merriweather")) +
  theme_bw()

# OJA totals at sample bounds and per-ISCO-1 reduction ratios -------------
results$oja_totals <- oja$l3 %>%
  filter(dmax %in% c(min(dmax), max(dmax))) %>%
  group_by(dmax) %>%
  summarise(total_OJA = sum(OJA, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = if_else(dmax == min(dmax), "start", "end")) %>%
  select(period, dmax, total_OJA)

log_text(results$oja_totals, "Total OJA at sample bounds")

results$oja_iscolevel1_change <- oja$l3 %>%
  filter(dmax %in% c(min(dmax), max(dmax))) %>%
  group_by(idesco_level_1, dmax) %>%
  summarise(total_OJA = sum(OJA, na.rm = TRUE), .groups = "drop_last") %>%
  arrange(dmax) %>%
  summarise(
    pre_OJA  = first(total_OJA),
    post_OJA = last(total_OJA),
    ratio_pre_post = first(total_OJA) / last(total_OJA),
    .groups = "drop"
  ) %>%
  left_join(
    bind_rows(
      tibble(idesco_level_1 = "OC1", idesco_label = "Managers"),
      tibble(idesco_level_1 = "OC2", idesco_label = "Professionals"),
      tibble(idesco_level_1 = "OC3", idesco_label = "Technical professionals"),
      tibble(idesco_level_1 = "OC4", idesco_label = "Clerical support workers"),
      tibble(idesco_level_1 = "OC5", idesco_label = "Service and sales workers"),
      tibble(idesco_level_1 = "OC6", idesco_label = "Agricultural workers"),
      tibble(idesco_level_1 = "OC7", idesco_label = "Craft and trades workers"),
      tibble(idesco_level_1 = "OC8", idesco_label = "Plant and machine operators"),
      tibble(idesco_level_1 = "OC9", idesco_label = "Elementary occupations")
    ),
    by = "idesco_level_1"
  ) %>%
  arrange(desc(ratio_pre_post))

log_text(results$oja_iscolevel1_change, "OJA pre/post ratio by ISCO level 1", n = Inf)

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

log_text(
  results$correlation_table,
  "Raw Correlation with delta_OJA_log:"
)

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

# Raw series by exposure quartile, indexed to the ChatGPT release window
# (dmax 2022-12-31, the 12 months ending Dec 2022). Used in the manuscript as
# tex/img/oja_by_exposure_quartile_main.eps (Figure "Online Job Adverts by AI
# Exposure Quartile", usage-based and Eloundou scores) and
# tex/img/oja_by_exposure_quartile_rest.eps (appendix, the other three scores):
# shows that the top quartile of occupations fell further than the bottom
# quartile in the raw counts, before any regression.
l3_quartile_plots <- map(exposure_vars, function(var) {
  plot_exposure_quintiles(
    oja_twfe$l3, 
    level = 3, 
    exposure_var = var, 
    n_tiles = 4,
    index_date = as.Date("2022-12-31")
  ) +
    # ntile() assigns 1 to the lowest scores: quartile 1 is the least exposed
    scale_color_grey(
      start = 0.8, end = 0.2,
      labels = c("1 (least exposed)", "2", "3", "4 (most exposed)")
    ) +
    scale_linetype_discrete(
      labels = c("1 (least exposed)", "2", "3", "4 (most exposed)")
    ) +
    labs(color = "Exposure quartile", linetype = "Exposure quartile")
})
names(l3_quartile_plots) <- exposure_vars
l3_quartile_plots$combined <- wrap_plots(l3_quartile_plots, ncol = 2) +
  plot_layout(guides = "collect")
# main text: the usage-based score and the Eloundou score; appendix: the rest
l3_quartile_plots$main <- wrap_plots(
  l3_quartile_plots[c("anthropic_usage_score", "beta_eloundou")], ncol = 2
) +
  plot_layout(guides = "collect")
l3_quartile_plots$rest <- wrap_plots(
  l3_quartile_plots[c("ai_product_exposure_score", "felten_exposure_score", "webb_exposure_score")],
  ncol = 2
) +
  plot_layout(guides = "collect")

results$l3_quartile_plots <- l3_quartile_plots

# log the index values at the end of the sample for the top and bottom quartile
quartile_endpoints <- map_dfr(exposure_vars, function(var) {
  oja_twfe$l3 %>%
    group_by(idesco_level_3) %>%
    filter(!is.na(!!sym(var))) %>%
    summarise(mean_score = mean(!!sym(var)), .groups = "drop") %>%
    mutate(quartile = ntile(mean_score, 4)) %>%
    inner_join(select(oja_twfe$l3, idesco_level_3, dmax, OJA), by = "idesco_level_3") %>%
    group_by(quartile, dmax) %>%
    summarise(sum_OJA = sum(OJA), .groups = "drop") %>%
    group_by(quartile) %>%
    arrange(dmax) %>%
    mutate(index = sum_OJA / sum_OJA[dmax == as.Date("2022-12-31")] * 100) %>%
    filter(dmax %in% as.Date(c("2022-03-31", "2022-09-30", "2025-06-30"))) %>%
    mutate(exposure_var = var) %>%
    select(exposure_var, quartile, dmax, index)
}) %>%
  pivot_wider(names_from = dmax, values_from = index)

log_text(
  as.data.frame(quartile_endpoints),
  "OJA index by exposure quartile (2022 Q4 window = 100), selected windows",
  digits = 3
)

# Same figure for the Australian IVI (appendix, tex/img/aus_oja_by_exposure_quartile.eps).
# Data pipeline as in model_aus.R; the window is trimmed to the EU sample
# (2022 Q1 to 2025 Q2) so the two figures are directly comparable. The IVI is a
# quarterly mean of monthly 3-month moving averages, not a rolling 12-month total.
aus_quartile_data <- format_aus_oja(
  ivi_quarterly(read_ivi() %>% filter(date >= as.Date("2021-10-01"))),
  build_anzsco_exposure(
    read_anzsco_correspondence(),
    read_ai_exposure_file("data/ai_exposure_scores/scored_esco_occupations_matched.csv", level = 3),
    isco_level = 3
  ),
  level = 3,
  t0 = t0
) %>%
  filter(dmax >= as.Date("2022-03-31"), dmax <= as.Date("2025-06-30"))

aus_quartile_plots <- map(exposure_vars, function(var) {
  plot_exposure_quintiles(
    aus_quartile_data,
    level = 3,
    exposure_var = var,
    n_tiles = 4,
    index_date = as.Date("2022-12-31")
  ) +
    scale_color_grey(
      start = 0.8, end = 0.2,
      labels = c("1 (least exposed)", "2", "3", "4 (most exposed)")
    ) +
    scale_linetype_discrete(
      labels = c("1 (least exposed)", "2", "3", "4 (most exposed)")
    ) +
    labs(
      title = paste("IVI Index by", names(exposure_vars)[exposure_vars == var], "Exposure"),
      y = "IVI index (2022 Q4 = 100)",
      color = "Exposure quartile", linetype = "Exposure quartile"
    )
})
names(aus_quartile_plots) <- exposure_vars
aus_quartile_plots$combined <- wrap_plots(aus_quartile_plots, ncol = 2) +
  plot_layout(guides = "collect")

results$aus_quartile_plots <- aus_quartile_plots

aus_quartile_endpoints <- map_dfr(exposure_vars, function(var) {
  aus_quartile_data %>%
    group_by(idesco_level_3) %>%
    filter(!is.na(!!sym(var))) %>%
    summarise(mean_score = mean(!!sym(var)), .groups = "drop") %>%
    mutate(quartile = ntile(mean_score, 4)) %>%
    inner_join(select(aus_quartile_data, idesco_level_3, dmax, OJA), by = "idesco_level_3") %>%
    group_by(quartile, dmax) %>%
    summarise(sum_OJA = sum(OJA), .groups = "drop") %>%
    group_by(quartile) %>%
    arrange(dmax) %>%
    mutate(index = sum_OJA / sum_OJA[dmax == as.Date("2022-12-31")] * 100) %>%
    filter(dmax %in% as.Date(c("2022-03-31", "2022-09-30", "2025-06-30"))) %>%
    mutate(exposure_var = var) %>%
    select(exposure_var, quartile, dmax, index)
}) %>%
  pivot_wider(names_from = dmax, values_from = index)

log_text(
  as.data.frame(aus_quartile_endpoints),
  "Australia: IVI index by exposure quartile (2022 Q4 = 100), selected quarters",
  digits = 3
)


# EURES by experience -----------------------------------------------------
# just checking - we don't have data before 2024
eures %>%
  group_by(
    idcountry, idesco_level_4, experience
  ) %>%
  arrange(dmax) %>%
  summarise(
    delta_log_OJA = last(log_OJA) - first(log(OJA)),
    exposure_score = last(anthropic_usage_score)
  ) %>%
  filter(!is.na(exposure_score)) %>%
  group_by(idcountry, experience) %>%
  mutate(
    country_mean_oja = mean(delta_log_OJA),
    country_mean_exposure = mean(exposure_score)
  ) %>%
  ungroup() %>%
  mutate(
    delta_log_OJA = delta_log_OJA - country_mean_oja,
    exposure_score = exposure_score - country_mean_exposure
  ) %>%
  ggplot(
    aes(y = delta_log_OJA, x = exposure_score)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 0, intercept = 0) +
  ylim(c(-1,1)) +
  facet_wrap(~experience)


# save results ------------------------------------------------------------
saveRDS(results, "results/RDS/descriptive.RDS")

save_plot(
  "oja_time_series.eps",
  results$oja_time_series,
  width = 12,
  height = 5
)

save_plot(
  "oja_by_exposure_quartile.eps",
  results$l3_quartile_plots$combined,
  width = 12,
  height = 10
)

save_plot(
  "oja_by_exposure_quartile_main.eps",
  results$l3_quartile_plots$main,
  width = 12,
  height = 4.5
)

save_plot(
  "oja_by_exposure_quartile_rest.eps",
  results$l3_quartile_plots$rest,
  width = 12,
  height = 7
)

save_plot(
  "aus_oja_by_exposure_quartile.eps",
  results$aus_quartile_plots$combined,
  width = 12,
  height = 10
)
