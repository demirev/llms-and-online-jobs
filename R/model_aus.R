# Australian robustness check ------------------------------------------------
# Re-runs the two headline labour-demand specifications from R/model_oja.R -
# the log-difference ("delta") model and the event study - on the Internet
# Vacancy Index (IVI) published by Jobs and Skills Australia. Vacancies are
# coded to ANZSCO; the AI-exposure scores are keyed to ISCO-08, so we map
# ANZSCO -> ISCO via the ABS correspondence and average exposure across all
# matched ISCO categories when an ANZSCO occupation maps to several.
#
# Geography (states/territories) plays the role of "country" - it is both the
# observation panel dimension and the fixed effect, exactly as idcountry is in
# the EU analysis. We run everything twice: once attaching exposure at the
# ISCO 4-digit level (the native granularity of the IVI / correspondence) and
# once at the ISCO 3-digit level that matches the EU analysis.

library(tidyverse)
library(fixest)
library(broom)
library(lubridate)
library(patchwork)
library(showtext)
library(sysfonts)
library(readxl)

source("R/helpers.R")
init_text_log("aus_models.txt", overwrite = TRUE)

t0 <- as.Date("2022-11-30") # chatgpt release date

# The two specifications use different windows on purpose. The IVI reaches back
# to 2006, which lets us exploit a long pre-period for the EVENT STUDY (a much
# stronger pre-trends test than the EU data allows); we go back to 2016 - far
# enough for a credible pre-trend without dragging in the very different early
# 2000s labour market. The DELTA model, by contrast, keeps the EU design's
# recent baseline (four pre-treatment quarters vs. all post-treatment quarters),
# so a 2016-2022 average doesn't pollute the pre-ChatGPT counterfactual.
event_window_start <- as.Date("2016-01-01")
delta_window_start <- as.Date("2021-10-01")

exposure_vars <- c(
  "Anthropic Usage Score" = "anthropic_usage_score",
  "Demirev Exposure Score" = "ai_product_exposure_score",
  "Eloundou Exposure Score" = "beta_eloundou",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score"
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

# local helpers --------------------------------------------------------------
# (build_anzsco_exposure / format_aus_oja / read_ivi live in R/helpers.R,
# shared with the horse-race pipeline)

# Delta scatter (mirror of the delta_plots block in model_oja.R), generalised
# over the occupation id column so it can be reused for both ISCO levels.
make_delta_scatter <- function(data, exposure_var, var_name, id_col) {
  data_filtered <- data %>%
    filter(!is.na(delta_OJA_log), !is.na(.data[[exposure_var]]), !is.na(.data[[id_col]]))
  data_filtered[["x"]] <- data_filtered[[exposure_var]]

  df_means <- data_filtered %>%
    group_by(.data[[id_col]]) %>%
    summarize(px_mean = mean(x, na.rm = TRUE), py_mean = mean(delta_OJA_log, na.rm = TRUE))

  ggplot(data_filtered, aes(x, delta_OJA_log)) +
    geom_point(color = "lightgray") +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    geom_point(
      data = df_means, aes(x = px_mean, y = py_mean),
      color = "gray10", shape = 4, size = 2, stroke = 1
    ) +
    geom_hline(yintercept = 0, color = "black") +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(x = var_name, y = "log (OJA Post / OJA Pre)") +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

# Partial regression plot (mirror of the partial_plots block in model_oja.R).
make_partial_plot <- function(data, exposure_var, var_name, id_col) {
  data_filtered <- data %>%
    filter(!is.na(delta_OJA_log), !is.na(.data[[exposure_var]]), !is.na(.data[[id_col]]))

  py <- resid(feols(delta_OJA_log ~ 1 | idcountry, data = data_filtered))
  px <- resid(feols(as.formula(paste(exposure_var, "~ 1 | idcountry")), data = data_filtered))

  model <- feols(
    as.formula(paste("delta_OJA_log ~", exposure_var, "| idcountry")),
    data = data_filtered, cluster = "idcountry"
  )
  coef <- coef(model)[exposure_var]
  se <- sqrt(vcov(model)[exposure_var, exposure_var])

  df_partial <- tibble(px = px, py = py, id = data_filtered[[id_col]])
  df_means <- df_partial %>%
    group_by(id) %>%
    summarize(px_mean = mean(px, na.rm = TRUE), py_mean = mean(py, na.rm = TRUE))

  line_x <- range(px, na.rm = TRUE)
  ci_data <- tibble(
    x = c(line_x, rev(line_x)),
    y = c((coef - 1.96 * se) * line_x, rev((coef + 1.96 * se) * line_x))
  )

  ggplot(df_partial, aes(px, py)) +
    geom_point(color = "lightgray") +
    geom_polygon(data = ci_data, aes(x = x, y = y), fill = "blue", alpha = 0.1) +
    geom_line(
      data = tibble(x = line_x, y = coef * line_x), aes(x = x, y = y),
      color = "blue", linewidth = 0.5
    ) +
    geom_point(
      data = df_means, aes(x = px_mean, y = py_mean),
      color = "gray10", shape = 4, size = 2, stroke = 1
    ) +
    geom_hline(yintercept = 0, color = "black") +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(x = var_name, y = "log (OJA Post / OJA Pre)") +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

# Acute COVID-19 lockdown / restriction era in Australia, expressed in the same
# event-time quarters the event study uses (relative to t0 = 2022 Q4): from the
# first national lockdown in 2020 Q1 to the end of the Delta-wave lockdowns in
# 2021 Q4. Used to shade the long pre-period of the event-study plots so the
# pandemic boom-and-reversion is visible against the AI "treatment" date.
covid_et <- c(
  start = (2020 - year(t0)) * 4 + (1 - quarter(t0)),
  end   = (2021 - year(t0)) * 4 + (4 - quarter(t0))
)

# Shade the COVID band behind an event-study plot (prepended so the points and
# error bars stay on top), with a small label at the top of the band.
add_covid_marker <- function(p) {
  rect <- annotate(
    "rect", xmin = covid_et["start"], xmax = covid_et["end"],
    ymin = -Inf, ymax = Inf, fill = "#d6604d", alpha = 0.15
  )
  p$layers <- c(rect, p$layers)
  p + annotate(
    "text", x = mean(covid_et), y = Inf, label = "COVID-19",
    vjust = 1.4, size = 3, family = "merriweather", color = "grey30"
  )
}

# arrange five single-measure plots in the same 3x2 (last cell blank) grid the
# EU script uses for its combined figures
combine_five <- function(plots, title) {
  (plots[[1]] + plots[[2]]) /
    (plots[[3]] + plots[[4]]) /
    (plots[[5]] + (ggplot() + theme_minimal())) +
    plot_layout(guides = "collect") +
    plot_annotation(title = title, theme = theme_minimal())
}

# read data ------------------------------------------------------------------
# Internet Vacancy Index: ANZSCO 4-digit x state x month, 3-month moving
# averages, restricted to the widest window needed (event study), then
# collapsed to quarterly mean levels so the (quarterly) event_time logic from
# the EU pipeline carries over unchanged
ivi_long <- read_ivi() %>% filter(date >= event_window_start)
oja_q <- ivi_quarterly(ivi_long)

# AI exposure, keyed to ISCO and aggregated to ANZSCO --------------------------
correspondence <- read_anzsco_correspondence()

ai_exposure_isco <- list(
  l3 = read_ai_exposure_file("data/ai_exposure_scores/scored_esco_occupations_matched.csv", level = 3),
  l4 = read_ai_exposure_file("data/ai_exposure_scores/scored_esco_occupations_matched.csv", level = 4)
)

anzsco_exposure <- list(
  l3 = build_anzsco_exposure(correspondence, ai_exposure_isco$l3, isco_level = 3),
  l4 = build_anzsco_exposure(correspondence, ai_exposure_isco$l4, isco_level = 4)
)

# format data ----------------------------------------------------------------
# Event-study frame spans the full 2016+ window; the delta frame is restricted
# to the recent EU-comparable window so its PRE average stays a recent baseline.
aus_fmt <- list(
  l3 = format_aus_oja(oja_q, anzsco_exposure$l3, level = 3, t0 = t0),
  l4 = format_aus_oja(oja_q, anzsco_exposure$l4, level = 4, t0 = t0)
)

aus_delta <- list(
  l3 = format_delta_data(
    aus_fmt$l3 %>% filter(dmax >= delta_window_start),
    n_periods = Inf, base_date = t0, level = 3, across_countries = FALSE
  ) %>% filter(pre_OJA > 20 & post_OJA > 20),
  l4 = format_delta_data(
    aus_fmt$l4 %>% filter(dmax >= delta_window_start),
    n_periods = Inf, base_date = t0, level = 4, across_countries = FALSE
  ) %>% filter(pre_OJA > 20 & post_OJA > 20)
)

log_text(
  map_dfr(c("l3", "l4"), ~ tibble(
    level = .x,
    n_state_occ = nrow(aus_delta[[.x]]),
    n_occ = n_distinct(aus_delta[[.x]][[paste0("idesco_level_", substr(.x, 2, 2))]])
  )),
  "Delta sample sizes (state x occupation):"
)

# run models per ISCO level --------------------------------------------------
# Everything below mirrors the delta / event-study blocks of model_oja.R but is
# wrapped in a function so we can apply it to both the 3- and 4-digit variants.
run_level <- function(tag, level, fmt, delta) {
  id_col <- paste0("idesco_level_", level)
  out <- list()

  # Event study ----
  event_models <- map(exposure_vars, ~ run_event_study_model(.x, fmt, level = level))
  event_model_breakdown <- run_event_study_model(breakdown_vars, fmt, level = level)
  out$event_study <- event_models

  # log each model separately with n = Inf: the default print truncates the 41
  # event-time coefficients, and print(<list>, n = Inf) does not propagate n
  iwalk(event_models, function(md, nm) {
    log_text(md, paste0("Event study [", tag, "]: ", nm), n = Inf)
  })

  event_coefs <- map2(event_models, exposure_vars, ~ extract_event_study_coefs(.x, .y))
  event_plots <- map2(
    event_coefs, exposure_vars,
    ~ plot_event_study(.x, .y, exposure_vars = exposure_vars, ylims = NULL)
  ) %>% map(add_covid_marker)
  event_plots_breakdown <- map2(
    extract_event_study_coefs(event_model_breakdown, breakdown_vars), breakdown_vars,
    ~ plot_event_study(.x, .y, exposure_vars = breakdown_vars, ylims = NULL)
  ) %>% map(add_covid_marker)
  event_plots$combined <- combine_five(
    event_plots, paste0("Event Study Across AI Exposure Measures - Australia (", tag, ")")
  )
  event_plots$combined_breakdown <-
    (event_plots_breakdown[[1]] + event_plots_breakdown[[2]]) /
    (event_plots_breakdown[[3]] + event_plots_breakdown[[4]])
  out$event_study_plots <- event_plots

  # Delta models ----
  delta_models <- map(exposure_vars, function(exposure_var) {
    feols(
      as.formula(paste("delta_OJA_log ~", exposure_var, "| idcountry")),
      data = delta, cluster = "idcountry"
    )
  })
  names(delta_models) <- exposure_vars
  out$delta <- delta_models

  log_text(delta_models, paste0("Delta models [", tag, "]:"))

  delta_models_breakdown <- list(
    demirev_combined = feols(
      delta_OJA_log ~ ai_product_automation_score + ai_product_augmentation_score | idcountry,
      data = delta, cluster = "idcountry"
    ),
    anthropic_combined = feols(
      delta_OJA_log ~ anthropic_automation_score + anthropic_augmentation_score | idcountry,
      data = delta, cluster = "idcountry"
    )
  )
  out$delta_breakdown <- delta_models_breakdown
  log_text(delta_models_breakdown, paste0("Delta models, breakdown by intent [", tag, "]:"))

  # Delta scatter + partial regression plots ----
  delta_plots <- imap(exposure_vars, ~ make_delta_scatter(delta, .x, names(exposure_vars)[exposure_vars == .x], id_col))
  delta_plots$combined <- combine_five(
    delta_plots, paste0("Delta OJA Log vs AI Exposure - Australia (", tag, ")")
  )
  out$delta_plots <- delta_plots

  partial_plots <- imap(exposure_vars, ~ make_partial_plot(delta, .x, names(exposure_vars)[exposure_vars == .x], id_col))
  partial_plots$combined <- combine_five(
    partial_plots, paste0("Partial Regression Plots - Australia (", tag, ")")
  )
  out$partial_plots <- partial_plots

  out
}

results$l3 <- run_level("l3", 3, aus_fmt$l3, aus_delta$l3)
results$l4 <- run_level("l4", 4, aus_fmt$l4, aus_delta$l4)

# The run-up control (state x occupation hiring run-up into the pre-ChatGPT
# baseline) now lives in R/model_aus_runup.R and is raced against exposure in
# R/model_horse_race.R together with the other controls.

# save results ---------------------------------------------------------------
saveRDS(results, "results/RDS/aus_models.RDS")

# per-level, per-figure exports (combined grids + each single-measure panel),
# named with an aus_ prefix and the ISCO-level tag so they don't clash with the
# EU figures already in results/plots and tex/img
save_level_plots <- function(tag, level_results) {
  measure_files <- c(
    `Anthropic Usage Score` = "anthropic", `Demirev Exposure Score` = "demirev",
    `Eloundou Exposure Score` = "eloundou", `Felten AI Exposure Score` = "felten",
    `Webb AI Exposure Score` = "webb"
  )

  specs <- list(
    event_study = level_results$event_study_plots,
    delta = level_results$delta_plots,
    partial = level_results$partial_plots
  )

  for (spec in names(specs)) {
    plots <- specs[[spec]]
    save_plot(
      sprintf("aus_%s_%s_all.eps", spec, tag), plots$combined,
      width = 10, height = 6, device = cairo_ps
    )
    for (measure in names(measure_files)) {
      save_plot(
        sprintf("aus_%s_%s_%s.eps", spec, tag, measure_files[[measure]]),
        plots[[measure]], width = 10, height = 6, device = cairo_ps
      )
    }
  }
}

save_level_plots("l3", results$l3)
save_level_plots("l4", results$l4)
