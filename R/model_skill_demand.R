library(tidyverse)
library(lubridate)
library(fixest)

source("R/helpers.R")

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
  ) %>%
  right_join(skills, by = c("esco_skill_level_2", "esco_skill_level_3"))


# plots -------------------------------------------------------------------
skills_exposure %>%
  filter(dmax == max(dmax)) %>%
  group_by(
    esco_skill_level_3, 
    #idcountry,  
    #isco_level_3,
    ai_product_exposure
  ) %>%
  summarise(
    mentions_end = sum(mentions)
  ) %>%
  inner_join(
    skills_exposure %>% 
      filter(dmax == min(dmax)) %>%
      group_by(
        esco_skill_level_3, 
        #idcountry,  
        #isco_level_3,
        ai_product_exposure
      ) %>%
      summarise(
        mentions_start = sum(mentions)
      ),
    by = c("esco_skill_level_3", "ai_product_exposure")
  ) %>%
  mutate(
    mentions_change = abs(mentions_end - mentions_start) / mentions_start
  ) %>%
  filter(mentions_change < 10) %>% # what's going on here?
  ggplot(aes(x = ai_product_exposure, y = mentions_change)) +
  geom_point() +
  geom_smooth(method = "lm") + # 0.03 unsig
  labs(
    x = "AI product exposure",
    y = "Change in mentions"
  ) +
  theme_minimal()
  
skills_exposure %>%
  group_by(esco_skill_level_3, ai_product_exposure, dmax) %>%
  filter(!is.na(ai_product_exposure)) %>%
  summarise(
    mentions = sum(mentions),
    .groups = "drop"
  ) %>%
  mutate(
    score_percentile = ntile(ai_product_exposure, 7)
  ) %>%
  mutate(score_percentile = factor(score_percentile, levels = 1:7)) %>%
  group_by(score_percentile, dmax) %>%
  arrange(dmax) %>%
  summarise(
    mentions = sum(mentions)
  ) %>%
  group_by(score_percentile) %>%
  mutate(
    mentions_index = mentions / first(mentions) * 100
  ) %>%
  ggplot(
    aes(
      x = dmax, 
      #y = mentions,
      y = mentions_index, 
      color = score_percentile, 
      lty = score_percentile
    )
  ) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-11-30"), linetype = "dashed") +
  labs(
    title = paste("Mentions Index by Exposure Quintiles"),
    x = "Date",
    y = "Mentions Index",
    color = "-tiles",
    linetype = "-tiles"
  ) +
  theme_minimal()


# twfe --------------------------------------------------------------------
t0 <- as.Date("2022-11-30")

skills_twfe <- skills_exposure %>%
  group_by(
    esco_skill_level_3, 
    #idcountry,
    dmax,
    ai_product_exposure
  ) %>%
  summarise(
    mentions = sum(mentions)
  ) %>%
  mutate(
    log_mentions = log(mentions + 1),
    post_chatgpt = (dmax > t0)*1,
    event_time = as.integer((year(dmax) - year(t0)) * 4 + (quarter(dmax) - quarter(t0)))
  )

model_event_study <- feols(
  log_mentions ~ ai_product_exposure:i(event_time, ref = 0) | esco_skill_level_3, #+ idcountry,
  data = skills_twfe,
  cluster = c(
    #"idcountry", 
    "esco_skill_level_3"
  )
)

summary(model_event_study)

model_twfe <- feols(
  log_mentions ~ ai_product_exposure:post_chatgpt | esco_skill_level_3, #+ idcountry,
  data = skills_twfe,
  cluster = c(
    #"idcountry", 
    "esco_skill_level_3"
  )
)

summary(model_twfe)
