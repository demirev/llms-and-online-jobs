library(tidyverse)
library(lubridate)
library(fixest)
library(tidymodels)
library(showtext)
library(sysfonts)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

results <- list()

font_add_google("Merriweather", "merriweather")
showtext_auto()

# read data ---------------------------------------------------------------
skills <- read_skill_mentions(
  "data/cedefop_skills_ovate_skill_demand/csv/05_esco_skill_skill_across_occupations_hyper"
)

skill_exposure <- read_csv(
  "data/ai_exposure_scores/scored_esco_skills.csv"
)

skill_hierarchy <- read_csv("data/esco/skillsHierarchy_en.csv") %>%
  select(
    esco_skill_level_2 = `Level 2 preferred term`,
    esco_skill_level_3 = `Level 3 preferred term`,
    esco_skill_level_2_uri = `Level 2 URI`,
    esco_skill_level_3_uri = `Level 3 URI`,
  ) %>%
  filter(!is.na(esco_skill_level_3))

skill_relations <- read_csv("data/esco/broaderRelationsSkillPillar_en.csv") %>%
  select(
    esco_skill_uri = conceptUri,
    esco_skill_level_3_uri = broaderUri
  )

skills_exposure <- skill_hierarchy %>%
  left_join(skill_relations, by = "esco_skill_level_3_uri") %>%
  select(
    esco_skill_level_2,
    esco_skill_level_3,
    esco_skill_uri
  ) %>%
  filter(!is.na(esco_skill_uri)) %>%
  right_join(skill_exposure, by = "esco_skill_uri") %>%
  group_by(esco_skill_level_2, esco_skill_level_3) %>%
  filter(!is.na(esco_skill_level_3)) %>%
  summarise(
    ai_product_exposure = mean(max_similarity) 
  ) %>%
  ungroup() %>%
  mutate(
    ai_product_exposure = (ai_product_exposure - mean(ai_product_exposure)) / sd(ai_product_exposure)
    #ai_product_exposure = scale_zero_to_one(ai_product_exposure) 
  ) %>%
  right_join(skills, by = c("esco_skill_level_2", "esco_skill_level_3"))

# format data -------------------------------------------------------------
skill_delta <- skills_exposure %>%
  filter(!str_detect(idcountry, "EU")) %>% # remove EU aggregates
  group_by(
    esco_skill_level_3,
    isco_level_3,
    idcountry,
    ai_product_exposure
  ) %>%
  summarise(
    mentions_pre = mean(mentions[dmax <= t0]),
    mentions_post = mean(mentions[dmax > t0]),
    .groups = "drop"
  ) %>%
  filter(
    mentions_pre > 10 & mentions_post > 10 # remove infrequent skills
  ) %>%
  mutate(
    delta_mentions_log = log(mentions_post) - log(mentions_pre)
  ) %>%
  filter(!is.na(delta_mentions_log))

# plots -------------------------------------------------------------------
plot_skill_delta <- skill_delta %>%
  ggplot(aes(x = ai_product_exposure, y = delta_mentions_log)) +
  geom_point(color = "gray", alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  # geom_point(
  #   data = df_means,
  #   aes(x = px_mean, y = py_mean),
  #   color = "gray10",
  #   shape = 4,
  #   size = 2,
  #   stroke = 1
  # ) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    x = "AI product exposure",
    y = "Change in log mentions"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "merriweather"))

summary(lm(delta_mentions_log ~ ai_product_exposure, data = skill_delta))
skill_detla_cor <- cor.test(skill_delta$ai_product_exposure, skill_delta$delta_mentions_log)

results$plot_skill_delta <- plot_skill_delta
results$skill_detla_cor <- skill_detla_cor

results$most_exposed_skills <- skill_delta %>%
  group_by(esco_skill_level_3) %>%
  summarise(
    mean_delta_mentions_log = mean(delta_mentions_log),
    ai_product_exposure = mean(ai_product_exposure, na.rm = TRUE)
  ) %>%
  filter(!is.na(ai_product_exposure)) %>%
  arrange(ai_product_exposure) %>%
  print(n = Inf) # used in manuscript

# models --------------------------------------------------------------------
skills_twfe <- skills_exposure %>%
  # group_by(
  #   esco_skill_level_3, 
  #   idcountry,
  #   dmax,
  #   ai_product_exposure
  # ) %>%
  # summarise(
  #   mentions = sum(mentions)
  # ) %>%
  mutate(
    log_mentions = log(mentions + 1),
    post_chatgpt = (dmax > t0)*1,
    event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
  )

# event study ----
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
  var_name <- "AI Product Exposure"
  
  ggplot(coefs, aes(x = event_time, y = estimate)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = paste("Event Study for", var_name, "against Log Mentions of Skills"),
         x = "Event Time (quarters since ChatGPT release)",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

model_event_study <- feols(
  log_mentions ~ ai_product_exposure:i(event_time, ref = 0) | esco_skill_level_3 + isco_level_3 + idcountry,
  data = skills_twfe,
  cluster = c(
    "idcountry", 
    "esco_skill_level_3",
    "isco_level_3"
  )
)

summary(model_event_study)

results$even_study_plot <- extract_event_study_coefs(
  model_event_study, "ai_product_exposure"
) %>%
  plot_event_study() +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5)

results$model_event_study <- model_event_study

# twfe ----
model_twfe <- feols(
  log_mentions ~ ai_product_exposure:post_chatgpt | esco_skill_level_3 + isco_level_3  + idcountry,
  data = skills_twfe,
  cluster = c(
    "idcountry", 
    "esco_skill_level_3",
    "isco_level_3"
  )
)

summary(model_twfe)

results$model_twfe <- model_twfe

# delta ----
model_delta <- feols(
  delta_mentions_log ~ ai_product_exposure | isco_level_3 + idcountry,
  data = skill_delta,
  cluster = c(
    "idcountry", 
    "isco_level_3"
  )
)

summary(model_delta)

results$model_delta <- model_delta

results$exposure_and_change_in_mentions <- skill_delta %>%
  group_by(esco_skill_level_3, ai_product_exposure) %>%
  summarise(
    mean_delta_mentions_log = mean(delta_mentions_log)
  ) %>%
  filter(!is.na(ai_product_exposure)) %>%
  arrange(desc(ai_product_exposure)) %>%
  print(n = Inf) # used in manuscript

# decile ----
model_decile <- feols(
  delta_mentions_log ~ exposure_decile | isco_level_3 + idcountry,
  data = skill_delta %>%
    mutate(
      exposure_decile = factor(ntile(ai_product_exposure, 10))
    ),
  cluster = c(
    "idcountry", 
    "isco_level_3"
  )
)

summary(model_decile)

plot_decile_coefficients <- function(model, exposure_var, conf_level = 0.95) {
  # Get the name for the exposure variable
  var_name <- "AI Product Exposure"
  
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
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
}

plot_decile_coefficients(model_decile, "ai_product_exposure")

results$model_decile <- model_decile

# save results ------------------------------------------------------------
saveRDS(results, file = "results/RDS/skill_models.RDS")

ggsave(
  file.path("results/plots", "event_study_skills.eps"),
  results$even_study_plot,
  width = 10,
  height = 6,
  device = cairo_ps
)

