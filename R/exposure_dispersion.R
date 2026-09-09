# Exposure dispersion and effect sizes in standard units ----------------------
# Every exposure index is rescaled to [0, 1] before estimation, so the
# coefficients in the delta and event-study models measure the gap between the
# single least and the single most exposed ISCO 3-digit group. This script
# reports the dispersion of each rescaled index within the estimation samples
# (standard deviation and interquartile range across country-occupation cells)
# and restates the long-difference and end-of-sample event-study estimates per
# standard deviation and per interquartile range of exposure. Nothing is
# re-estimated; the fitted objects from model_oja.R and model_aus.R are read
# back and their estimation samples recovered with fixest_data().

library(tidyverse)
library(fixest)

source("R/helpers.R")
init_text_log("exposure_dispersion.txt", overwrite = TRUE)

eu <- readRDS("results/RDS/oja_models.RDS")
aus <- readRDS("results/RDS/aus_models.RDS")

last_coef <- function(model) {
  cf <- coef(model)
  cf[[length(cf)]]
}

dispersion_table <- function(delta_models, event_models, exposure_vars) {
  map_dfr(seq_along(exposure_vars), function(i) {
    v <- exposure_vars[i]
    dat <- fixest_data(delta_models[[v]])
    x <- dat[[v]]
    b <- coef(delta_models[[v]])[[v]]
    e <- if (is.null(event_models)) NA_real_ else last_coef(event_models[[i]])
    tibble(
      measure = v,
      n = sum(!is.na(x)),
      sd = sd(x, na.rm = TRUE),
      iqr = IQR(x, na.rm = TRUE),
      p10_p90 = diff(quantile(x, c(0.1, 0.9), na.rm = TRUE)),
      delta_coef = b,
      delta_minmax_pct = 100 * (exp(b) - 1),
      delta_per_sd_pct = 100 * (exp(b * sd) - 1),
      delta_per_iqr_pct = 100 * (exp(b * iqr) - 1),
      es_last_coef = e,
      es_last_minmax_pct = 100 * (exp(e) - 1),
      es_last_per_sd_pct = 100 * (exp(e * sd) - 1),
      es_last_per_iqr_pct = 100 * (exp(e * iqr) - 1)
    )
  })
}

eu_table <- dispersion_table(eu$delta, eu$event_study, names(eu$delta))
log_text(
  as.data.frame(eu_table),
  "EU, ISCO level 3: dispersion of rescaled exposure in the delta sample and effect sizes per SD and IQR",
  digits = 3
)

aus_table <- tryCatch(
  dispersion_table(aus$l3$delta, aus$l3$event_study, names(aus$l3$delta)),
  error = function(e) {
    message("Australian dispersion table skipped: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(aus_table)) {
  log_text(
    as.data.frame(aus_table),
    "Australia, ISCO level 3: dispersion of rescaled exposure in the delta sample and effect sizes per SD and IQR",
    digits = 3
  )
}

occupation_tail <- fixest_data(eu$delta[[1]]) %>%
  group_by(idesco_level_3, esco_level_3_short) %>%
  summarise(across(all_of(names(eu$delta)), first), .groups = "drop") %>%
  arrange(desc(anthropic_usage_score))

log_text(
  as.data.frame(head(occupation_tail, 6)),
  "Six most exposed ISCO 3-digit groups by the Anthropic usage score (rescaled indices)",
  digits = 3
)

write_csv(eu_table, "results/intermediate_datasets/exposure_dispersion_eu.csv")
if (!is.null(aus_table)) {
  write_csv(aus_table, "results/intermediate_datasets/exposure_dispersion_aus.csv")
}
