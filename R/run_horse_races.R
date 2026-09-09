# Rebuild every horse-race control and rerun the unified race, each script in
# a fresh R session so nothing leaks between them.
#
#   Rscript R/run_horse_races.R              # scores, then the race
#   Rscript R/run_horse_races.R --race-only  # only R/model_horse_race.R
#
# The score scripts are independent of each other and only need the raw data;
# the race reads results/controls/*.csv. Logs land in results/logs/ as usual,
# and the one-table view is results/intermediate_datasets/horse_race_summary.csv.
args <- commandArgs(trailingOnly = TRUE)

score_scripts <- c(
  "R/model_rate_sensitivity_rba.R", # RBA surprise rate sensitivity (AUS + EU)
  "R/model_eu_runup.R",             # EU pre-ChatGPT hiring run-up
  "R/model_aus_runup.R",            # AUS pre-ChatGPT hiring run-up
  "R/wfh_correlation.R"             # Dingel-Neiman teleworkable share
)
scripts <- c(if (!"--race-only" %in% args) score_scripts, "R/model_horse_race.R")

for (s in scripts) {
  message("\n==== ", s, " ====")
  status <- system2("Rscript", s)
  if (status != 0) stop(s, " failed with status ", status)
}
message("\ndone; see results/logs/horse_race.txt")
