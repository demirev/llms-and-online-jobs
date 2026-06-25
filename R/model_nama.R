library(tidyverse)
library(fixest)
library(patchwork)
library(showtext)
library(sysfonts)

source("R/helpers.R")
init_text_log("nama_models.txt")

font_add_google("Merriweather", "merriweather")
showtext_auto()

run_twfe_models <- function(data, exposure_vars) {
  map(exposure_vars, function(var) {
    feols(
      as.formula(paste("log_value ~", var, ": post_chatgpt | nace_rev2_code + country_code + year")),
      data = data,
      cluster = c("country_code", "nace_rev2_code")
    )
  })
}

run_nama_event_study_model <- function(data, exposure_var) {
  rhs <- paste(paste0(exposure_var, ":i(event_time, ref = 0)"), collapse = " + ")
  fml <- paste("log_value ~", rhs, "| nace_rev2_code + country_code")
  cat("Formula:", fml, "\n")
  feols(
    as.formula(fml),
    data = data,
    cluster = c("country_code", "nace_rev2_code")
  )
} # not run -- too few data periods

print_model_summaries <- function(models, model_name) {
  walk(seq_along(models), function(i) {
    log_text(summary(models[[i]]), paste(model_name, "-", names(models)[i]))
  })
}

exposure_vars <- c(
  "AI Product Exposure Score" = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score" = "webb_exposure_score",
  "Eloundou Beta Score" = "beta_eloundou",
  "Anthropic Usage Score" = "anthropic_usage_score"
)
breakdown_vars <- c(
  "Automation Exposure Score" = "ai_product_automation_score",
  "Augmentation Exposure Score" = "ai_product_augmentation_score",
  "Anthropic Automation Score" = "anthropic_automation_score",
  "Anthropic Augmentation Score" = "anthropic_augmentation_score"
)

# read data ---------------------------------------------------------------
nama_10_cp <- read_csv("results/intermediate_datasets/nama_10_cp_gvancs.csv")
nama_10_lp <- read_csv("results/intermediate_datasets/nama_10_lp_rlrphw.csv")

cedefop_sectoral <- read_csv("data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv")

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv"
) 

sectoral_exposure <- derive_sectoral_exposure(ai_exposure, cedefop_sectoral)

sectoral_summary <- sectoral_exposure %>%
  group_by(nace_rev2_code, sector_name) %>%
  summarise(
    ai_product_augmentation_score = mean(ai_product_augmentation_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou),
    anthropic_usage_score = mean(anthropic_usage_score),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    ai_product_automation_score = mean(ai_product_automation_score),
    anthropic_automation_score = mean(anthropic_automation_score),
    anthropic_augmentation_score = mean(anthropic_augmentation_score)
  )

log_text(
  sectoral_summary,
  "Sectoral summary:",
  n = Inf
)

# prep data ---------------------------------------------------------------
nama_10_lp_prep <- prep_nama_data(nama_10_lp, sectoral_exposure)
nama_10_cp_prep <- prep_nama_data(nama_10_cp, sectoral_exposure)

nama_10_lp_delta <- nama_10_lp_prep %>%
  group_by(nace_rev2_code, country_code, sector_name, post_chatgpt) %>%
  summarise(
    log_value = log(mean(value)),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    ai_product_automation_score = mean(ai_product_automation_score),
    ai_product_augmentation_score = mean(ai_product_augmentation_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou),
    anthropic_usage_score = mean(anthropic_usage_score),
    anthropic_automation_score = mean(anthropic_automation_score),
    anthropic_augmentation_score = mean(anthropic_augmentation_score)
  ) %>%
  arrange() %>%
  group_by(nace_rev2_code, country_code, sector_name) %>%
  mutate(delta_log_value = log_value - lag(log_value)) %>%
  filter(!is.na(delta_log_value)) %>%
  select(-log_value)

nama_10_cp_delta <- nama_10_cp_prep %>%
  group_by(nace_rev2_code, country_code, sector_name, post_chatgpt) %>%
  summarise(
    log_value = log(mean(value)),
    ai_product_exposure_score = mean(ai_product_exposure_score),
    ai_product_automation_score = mean(ai_product_automation_score),
    ai_product_augmentation_score = mean(ai_product_augmentation_score),
    felten_exposure_score = mean(felten_exposure_score),
    webb_exposure_score = mean(webb_exposure_score),
    beta_eloundou = mean(beta_eloundou),
    anthropic_usage_score = mean(anthropic_usage_score),
    anthropic_automation_score = mean(anthropic_automation_score),
    anthropic_augmentation_score = mean(anthropic_augmentation_score)
  ) %>%
  arrange() %>%
  group_by(nace_rev2_code, country_code, sector_name) %>%
  mutate(delta_log_value = log_value - lag(log_value)) %>%
  filter(!is.na(delta_log_value)) %>%
  select(-log_value)

# plot delta -------------------------------------------------------------
nama_10_lp_delta %>%
  ggplot(
    aes(x = ai_product_automation_score, y = delta_log_value)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "AI Exposure Score",
    y = "Delta Log Value",
    title = "Delta Log Value vs AI Exposure Score"
  )

# run models --------------------------------------------------------------
lp_models <- run_twfe_models(nama_10_lp_prep, c(exposure_vars, breakdown_vars))
cp_models <- run_twfe_models(nama_10_cp_prep, c(exposure_vars, breakdown_vars))

# Print summaries
print_model_summaries(lp_models, "Labor Productivity")
print_model_summaries(cp_models, "Capital Productivity")

# delta models ------------------------------------------------------------
delta_lp_models <- map(c(exposure_vars, breakdown_vars), function(var) {
  feols(
    as.formula(paste("delta_log_value ~", var, "| country_code")),
    data = nama_10_lp_delta,
    cluster = c("country_code", "nace_rev2_code")
  )
})

delta_cp_models <- map(c(exposure_vars, breakdown_vars), function(var) {
  feols(
    as.formula(paste("delta_log_value ~", var, "| country_code")),
    data = nama_10_cp_delta,
    cluster = c("country_code", "nace_rev2_code")
  )
})

log_text(delta_lp_models, "Delta (Labor Productivity)")

log_text(delta_cp_models, "Delta (Capital Productivity)")
