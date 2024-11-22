library(tidyverse)
library(fixest)
library(broom)
library(lubridate)

source("R/helpers.R")

# read data ---------------------------------------------------------------
oja <- list.files("data/cedefop_skills_ovate_oja/csv", full.names = TRUE) %>%
  map_dfr(read_csv)

oja_l3 <- list.files(
  "data/cedefop_skills_ovate_skill_demand/csv/05_occupation_skill_across_occupations_hyper", 
  full.names = TRUE
) %>%
  map_dfr(read_csv) %>%
  mutate(
    idcountry = ifelse(is.na(idcountry), countryset, idcountry),
    esco_level_3_short = esco_level_3 # to match fromat of l2
  ) %>%
  select(-c(countryset, esco_level_3))

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

ai_exposure <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
  level = 2
) 
ai_exposure_l3 <- read_ai_exposure_file(
  "data/ai_exposure_scores/scored_esco_occupations_matched.csv",
  level = 3
)

oja_twfe <- format_twfe_oja_data(oja, ai_exposure, level = 2)
oja_twfe_l3 <- format_twfe_oja_data(oja_l3, ai_exposure_l3, level = 3)
oja_l3_twfe <- oja_l3 %>%
  rename(
    idesco_level_2 = idesco_level_3,
    esco_level_2_short = esco_level_3
  ) %>%
  format_twfe_oja_data(
    ai_exposure_l3 %>%
      rename(
        isco_level_2 = isco_level_3
      )
  ) %>%
  rename(
    esco_level_3_short = esco_level_2_short,
    idesco_level_3 = idesco_level_2
  ) # the function expects level 2, so rename isntead of rewriting it

#skills_total <- 
  skills %>%
  filter(idcountry == "EU27_2020 + UK") %>%
  filter(esco_hier_level_0 == "skills")

skills %>% 
  select(
    dmax, idcountry, idesco_level_3, esco_hier_level_0,
    esco_hier_level_1, esco_hier_level_2, esco_hier_level_3,
    mentions = Mention_hier
  ) %>%
  filter(
    esco_hier_level_0 == "skills"
  ) %>%
  filter(
    !str_detect(idcountry, "EU2")
  ) %>%
  mutate(
    idesco_level_2 = substr(idesco_level_3, 1, 4)
  ) %>%
  # left_join(
  #   oja %>%
  #     select(
  #       idcountry, idesco_level_2, dmax, OJA
  #     ),
  #   by = c("idcountry", "idesco_level_2", "dmax")
  # ) %>%
  # filter(!is.na(OJA)) %>% # UK
  group_by(dmax, idcountry, idesco_level_2, esco_hier_level_3) %>%
  summarise(
    mentions = sum(mentions)
  ) %>%
  mutate(
    mentions_perc = mentions / sum(mentions) 
  )
