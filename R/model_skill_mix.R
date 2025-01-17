library(tidyverse)
library(lubridate)

source("R/helpers.R")

t0 <- as.Date("2022-11-30") # chatgpt release date

results <- list()

# read data ---------------------------------------------------------------
skills <- read_skill_mentions(
  "data/cedefop_skills_ovate_skill_demand/csv/05_esco_skill_skill_across_occupations_hyper"
)

ai_occupational_exposure <- list(
  level_2 = read_ai_exposure_file(
    "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
    level = 2
  ),
  level_3 = read_ai_exposure_file(
    "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
    level = 3
  )
)

ai_skill_exposure <- read_csv(
  "data/ai_exposure_scores/scored_esco_skills.csv"
)

# skill mix --------------------------------------------------------------

# derive skill mix for first period
cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

find_skill_mix <- function(skills, which_date) {
  skills %>%
    filter(dmax %in% which_date) %>%
    group_by(isco_level_3, esco_skill_level_3) %>%
    summarise(n = n(), mentions = mean(mentions)) %>% 
    group_by(isco_level_3) %>%
    mutate(
      relative_mentions = mentions / sum(mentions)
    ) 
} 

calculate_skill_change <- function(
  skills, base_period, target_period
) {
  skills_base <- find_skill_mix(skills, base_period)
  skills_target <- find_skill_mix(skills, target_period)
  
  skills_base %>%
    rename(relative_mentions_base_period = relative_mentions) %>%
    full_join(skills_target, by = c("isco_level_3", "esco_skill_level_3")) %>%
    mutate(
      relative_mentions_base_period = ifelse(
        is.na(relative_mentions_base_period),
        0,
        relative_mentions_base_period
      ),
      relative_mentions = ifelse(
        is.na(relative_mentions),
        0,
        relative_mentions
      )
    ) %>%
    group_by(isco_level_3) %>%
    summarise(
      similarity = cosine_similarity(
        relative_mentions_base_period / sum(relative_mentions_base_period),
        relative_mentions / sum(relative_mentions) # renormalize to ensure sum = 1
      ),
      distance = sum(
        (
          (relative_mentions/sum(relative_mentions)) - 
            (relative_mentions_base_period/sum(relative_mentions_base_period))
        )^2
      ),
      added_skills = sum(
        (relative_mentions -
          relative_mentions_base_period) > 0.05
      ),
      removed_skills = sum(
        (relative_mentions_base_period -
          relative_mentions) > 0.05
      )
    ) %>%
    mutate(date = max(target_period)) %>%
    select(
      date,
      isco_level_3,
      similarity,
      distance,
      added_skills,
      removed_skills
    )
}

skill_change_total <- calculate_skill_change(
  skills, 
  unique(skills$dmax[skills$dmax <= t0]), 
  unique(skills$dmax[skills$dmax > t0])
)
skill_change_by_month <- map(
  unique(skills$dmax),
  ~ calculate_skill_change(skills, min(skills$dmax), .x)
) %>%
  bind_rows() %>%
  arrange(date, isco_level_3)

# relationship to AI exposure ---------------------------------------------
skill_change_total <- skill_change_total %>%
  left_join(
    ai_occupational_exposure$level_3,
    by = c("isco_level_3" = "isco_level_3")
  )

skill_change_by_month <- skill_change_by_month %>%
  left_join(
    ai_occupational_exposure$level_3,
    by = c("isco_level_3" = "isco_level_3")
  )

skill_change_by_month %>%
  filter(!is.na(beta_eloundou)) %>%
  mutate(
    beta_eloundou = scale_zero_to_one(beta_eloundou)
  ) %>%
  ggplot(aes(x = date, y = similarity, group = isco_level_3, color = beta_eloundou)) +
  geom_line() +
  #scale_color_gradient(low = "grey90", high = "black") +
  scale_color_gradient(low = "grey90", high = "black") +
  labs(
    title = "Skill Change by Month",
    x = "Date",
    y = "Similarity",
    color = "AI Product\nExposure Score"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

skill_change_total %>%
  filter(!is.na(beta_eloundou)) %>%
  #ggplot(aes(y = distance, x = beta_eloundou)) +
  #ggplot(aes(y = similarity, x = beta_eloundou)) +
  #ggplot(aes(y = added_skills, x = beta_eloundou)) +
  ggplot(aes(y = distance, x = beta_eloundou)) +
  geom_point() +
  geom_smooth(method = "lm")
