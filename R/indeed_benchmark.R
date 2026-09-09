# Indeed Job Postings Index benchmark -----------------------------------------
# The end-of-sample event-study coefficients in the paper compare the single
# most exposed ISCO 3-digit group to the single least exposed one, so they are
# large by construction. This script asks whether relative moves of that size
# are unusual in raw postings data from an independent source. The Indeed
# Hiring Lab publishes daily, seasonally adjusted postings indices by
# occupational sector (roughly 45 sectors per country) for the US, UK, Germany,
# France and Australia (github.com/hiring-lab/job_postings_tracker). We compute,
# for each sector, the log change between the twelve months ending with the
# ChatGPT release window (30 Nov 2022) and the twelve months ending with the
# last CEDEFOP window (30 Jun 2025), mirroring the rolling 12-month windows of
# the Skills OVATE data, and compare software development to the aggregate and
# the best-performing sector to the worst.

library(tidyverse)
library(lubridate)

source("R/helpers.R")
init_text_log("indeed_benchmark.txt", overwrite = TRUE)

countries <- c("US", "GB", "DE", "FR", "AU")
data_dir <- "data/indeed_hiring_lab"
base_url <- "https://raw.githubusercontent.com/hiring-lab/job_postings_tracker/master"

t0 <- as.Date("2022-11-30") # end of the ChatGPT release window
t1 <- as.Date("2025-06-30") # end of the last CEDEFOP window

# download (only if missing) --------------------------------------------------
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
for (cc in countries) {
  for (stem in c("job_postings_by_sector_", "aggregate_job_postings_")) {
    fl <- file.path(data_dir, paste0(stem, cc, ".csv"))
    if (!file.exists(fl)) {
      download.file(paste0(base_url, "/", cc, "/", stem, cc, ".csv"), fl, quiet = TRUE)
    }
  }
}

# functions -------------------------------------------------------------------
window_mean <- function(x, dates, end, months = 12) {
  mean(x[dates > end %m-% months(months) & dates <= end], na.rm = TRUE)
}

log_change <- function(x, dates) {
  log(window_mean(x, dates, t1)) - log(window_mean(x, dates, t0))
}

# compute ---------------------------------------------------------------------
sector_changes <- map_dfr(countries, function(cc) {
  read_csv(
    file.path(data_dir, paste0("job_postings_by_sector_", cc, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(variable == "total postings", date <= t1) %>%
    group_by(country = jobcountry, sector = display_name) %>%
    summarise(
      dlog = log_change(indeed_job_postings_index, date),
      .groups = "drop"
    )
})

aggregate_changes <- map_dfr(countries, function(cc) {
  read_csv(
    file.path(data_dir, paste0("aggregate_job_postings_", cc, ".csv")),
    show_col_types = FALSE
  ) %>%
    filter(variable == "total postings", date <= t1) %>%
    group_by(country = jobcountry) %>%
    summarise(
      dlog_total = log_change(indeed_job_postings_index_SA, date),
      .groups = "drop"
    )
})

summary_table <- sector_changes %>%
  group_by(country) %>%
  summarise(
    n_sectors = n(),
    software = dlog[sector == "Software Development"],
    worst_sector = sector[which.min(dlog)],
    worst = min(dlog),
    best_sector = sector[which.max(dlog)],
    best = max(dlog),
    spread = best - worst,
    .groups = "drop"
  ) %>%
  left_join(aggregate_changes, by = "country") %>%
  mutate(
    software_relative = software - dlog_total,
    software_relative_pct = 100 * (exp(software_relative) - 1),
    spread_pct = 100 * (exp(-spread) - 1)
  ) %>%
  select(
    country, n_sectors, dlog_total, software, software_relative,
    software_relative_pct, worst_sector, worst, best_sector, best, spread, spread_pct
  )

log_text(
  as.data.frame(summary_table),
  "Indeed sector benchmark: log change, 12 months to 2025-06-30 vs 12 months to 2022-11-30",
  digits = 3
)

log_text(
  sector_changes %>%
    group_by(country) %>%
    arrange(dlog, .by_group = TRUE) %>%
    slice(c(1:5, (n() - 4):n())) %>%
    as.data.frame(),
  "Five worst and five best sectors per country",
  digits = 3
)

write_csv(sector_changes, "results/intermediate_datasets/indeed_sector_changes.csv")
write_csv(summary_table, "results/intermediate_datasets/indeed_benchmark_summary.csv")
