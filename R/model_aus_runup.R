library(tidyverse)
library(lubridate)
library(readxl)

source("R/helpers.R")
init_text_log("aus_runup.txt", overwrite = TRUE)

# Australian run-up control ---------------------------------------------------
# The 2022-2024 tightening unwound the hiring boom of 2021-2022, and that boom
# was concentrated in professional and tech occupations, which are also the
# most AI-exposed. A rate-sensitivity score estimated on 2006-2021 data cannot
# capture an episode-specific over-hiring unwind. This targets it directly:
# each state x occupation's own run-up into the pre-ChatGPT baseline,
#
#   runup = log(mean OJA in the delta pre-period) - log(mean OJA in 2019 [or 2020])
#
# The 2019 base measures the run-up against the last pre-pandemic year; the
# 2020 base measures it from the pandemic trough. The score is exported as a
# horse-race control (state x ANZSCO 4-digit) and raced against exposure in
# model_horse_race.R: if the exposure gradient were mean reversion from an
# AI-correlated boom, adding the run-up should attenuate it.

t0 <- as.Date("2022-11-30") # chatgpt release date
delta_window_start <- as.Date("2021-10-01") # as in model_aus.R
runup_bases <- c(runup_from_2019 = 2019, runup_from_2020 = 2020)

oja_q <- ivi_quarterly(read_ivi())

occupation_runup <- oja_q %>%
  mutate(period = case_when(
    year(dmax) == 2019 ~ "y2019",
    year(dmax) == 2020 ~ "y2020",
    dmax >= delta_window_start & dmax < t0 ~ "pre",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(state, anzsco_4digit, anzsco_title, period) %>%
  summarise(OJA = mean(OJA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = OJA) %>%
  transmute(
    idcountry = state,
    anzsco_4digit, anzsco_title,
    runup_from_2019 = log(pre + 1) - log(y2019 + 1),
    runup_from_2020 = log(pre + 1) - log(y2020 + 1)
  ) %>%
  filter(!is.na(runup_from_2019), !is.na(runup_from_2020))

log_text(
  occupation_runup %>%
    summarise(
      n_cells = n(), n_occ = n_distinct(anzsco_4digit), n_states = n_distinct(idcountry),
      across(all_of(names(runup_bases)), list(mean = mean, sd = sd))
    ) %>%
    as.data.frame(),
  "Run-up cells (state x ANZSCO 4-digit) and moments:"
)

log_text(
  occupation_runup %>%
    group_by(anzsco_4digit, anzsco_title) %>%
    summarise(across(all_of(names(runup_bases)), mean), n_states = n(), .groups = "drop") %>%
    arrange(desc(runup_from_2019)) %>%
    slice(c(1:15, (n() - 14):n())),
  "Largest and smallest run-ups from 2019 (mean across states):",
  n = Inf
)

# export ----------------------------------------------------------------------
# same values for both Australian samples: the score is native to ANZSCO
for (s in c("aus_l3", "aus_l4")) {
  write_controls(
    occupation_runup, sample = s, key = "anzsco_4digit",
    controls = names(runup_bases), file = paste0("aus_runup_", s), idcountry_col = "idcountry"
  )
}

saveRDS(list(occupation_runup = occupation_runup), "results/RDS/aus_runup.RDS")
write_csv(occupation_runup, "results/intermediate_datasets/aus_runup_anzsco.csv")
