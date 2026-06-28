# Work-from-home readiness vs. AI exposure ------------------------------------
# A reviewer-style robustness question: the AI-exposure indices used throughout
# this paper pick out computer-based knowledge work - exactly the occupations
# that are also easiest to perform remotely. If the two are strongly correlated,
# then the pandemic remote-work demand cycle is a candidate confounder for the
# event-study dynamics (a WFH-driven boom-and-reversion masquerading as an AI
# effect). This script quantifies that overlap.
#
# WFH measure: Dingel & Neiman (2020) "teleworkable" indicator, a 0/1 flag per
# O*NET-SOC occupation (github.com/jdingel/DingelNeiman-workathome). We project
# it onto ESCO occupations through the official ESCO <-> O*NET-SOC crosswalk
# (the same crosswalk the Anthropic pipeline uses), then aggregate to the ISCO
# levels at which the exposure indices enter the regressions.

library(tidyverse)
library(patchwork)
library(showtext)
library(sysfonts)

source("R/helpers.R")
init_text_log("wfh_correlation.txt", overwrite = TRUE)

font_add_google("Merriweather", "merriweather")
showtext_auto()

exposure_vars <- c(
  "Demirev Exposure Score"   = "ai_product_exposure_score",
  "Felten AI Exposure Score" = "felten_exposure_score",
  "Webb AI Exposure Score"   = "webb_exposure_score",
  "Eloundou Exposure Score"  = "beta_eloundou",
  "Anthropic Usage Score"    = "anthropic_usage_score"
)

# SOC 2010 -> 2019 vintage remap ----------------------------------------------
# Dingel & Neiman key on SOC-2010-vintage O*NET codes; the ESCO crosswalk uses
# SOC-2019 codes. Without this the entire Computer & Mathematical cluster (the
# highest-exposure, almost-uniformly-teleworkable occupations) drops out and
# attenuates the very correlation we want to measure. This is the same hand-
# built remap used in data/anthropic/build_exposure.py.
soc_base <- c(
  "15-1111" = "15-1211", "15-1121" = "15-1211", "15-1122" = "15-1212",
  "15-1131" = "15-1251", "15-1132" = "15-1252", "15-1133" = "15-1252",
  "15-1134" = "15-1254", "15-1141" = "15-1242", "15-1142" = "15-1244",
  "15-1143" = "15-1241", "15-1151" = "15-1232", "15-1152" = "15-1231",
  "15-1199" = "15-1299", "43-9111" = "19-4099"
)
soc_exact <- c(
  "15-1199.01" = "15-1253.00", "15-1199.02" = "15-1299.02",
  "15-1199.03" = "15-1299.03", "25-3099.02" = "25-3041.00"
)

remap_soc <- function(code) {
  out <- code
  base <- sub("\\..*$", "", code)
  suffix <- sub("^[^.]*", "", code) # ".00" or ""
  hit <- base %in% names(soc_base)
  out[hit] <- paste0(soc_base[base[hit]], suffix[hit])
  exact <- code %in% names(soc_exact)
  out[exact] <- soc_exact[code[exact]]
  out
}

# Build a per-ESCO-occupation teleworkable score -----------------------------
dn <- read_csv("data/dingel_neiman/occupations_workathome.csv", show_col_types = FALSE) %>%
  transmute(onet_soc = remap_soc(onetsoccode), teleworkable) %>%
  group_by(onet_soc) %>%
  summarise(teleworkable = mean(teleworkable), .groups = "drop") # collapse merged codes

crosswalk <- read_csv("data/anthropic/ONET_(Occupations)_0_updated.csv", skip = 16, show_col_types = FALSE) %>%
  transmute(onet_soc = `O*NET Id`, occupation_uri = `ESCO or ISCO URI`) %>%
  filter(str_detect(occupation_uri, "/esco/occupation/"))

# An ESCO occupation that the crosswalk maps to several O*NET codes gets the
# simple mean teleworkable of those codes (a "share teleworkable" in [0,1]).
wfh_by_uri <- crosswalk %>%
  left_join(dn, by = "onet_soc") %>%
  group_by(occupation_uri) %>%
  summarise(wfh_teleworkable = mean(teleworkable, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(wfh_teleworkable))

# Attach exposure scores (occupation level) ----------------------------------
exp_occ <- read_csv("data/ai_exposure_scores/scored_esco_occupations_matched.csv", show_col_types = FALSE) %>%
  select(occupation_uri, isco_group,
         ai_product_exposure_score, felten_exposure_score,
         webb_exposure_score, beta_eloundou)

anthropic_occ <- read_csv("data/anthropic/anthropic_exposure_esco.csv", show_col_types = FALSE) %>%
  select(occupation_uri, anthropic_usage_score = usage_score)

occ <- exp_occ %>%
  left_join(anthropic_occ, by = "occupation_uri") %>%
  left_join(wfh_by_uri, by = "occupation_uri")

cat(sprintf(
  "Coverage: %d ESCO occupations in exposure file, %d with a WFH score (%.1f%%)\n",
  nrow(occ), sum(!is.na(occ$wfh_teleworkable)),
  100 * mean(!is.na(occ$wfh_teleworkable))
))

# Aggregate to ISCO levels (simple mean, mirroring read_ai_exposure_file) ----
aggregate_to_isco <- function(df, level) {
  df %>%
    mutate(isco = substr(isco_group, 1, level)) %>%
    group_by(isco) %>%
    summarise(across(all_of(c(unname(exposure_vars), "wfh_teleworkable")),
                     ~ mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    filter(!is.nan(wfh_teleworkable))
}

levels_data <- list(
  `ESCO occupation` = occ,
  `ISCO 3-digit`    = aggregate_to_isco(occ, 3),
  `ISCO 4-digit`    = aggregate_to_isco(occ, 4)
)

# Correlations ---------------------------------------------------------------
cor_table <- imap_dfr(levels_data, function(df, unit) {
  imap_dfr(exposure_vars, function(col, nm) {
    d <- df %>% filter(!is.na(.data[[col]]), !is.na(wfh_teleworkable))
    pe <- cor.test(d[[col]], d$wfh_teleworkable, method = "pearson")
    sp <- suppressWarnings(cor.test(d[[col]], d$wfh_teleworkable, method = "spearman"))
    tibble(
      unit = unit, measure = nm, n = nrow(d),
      pearson = unname(pe$estimate), pearson_p = pe$p.value,
      spearman = unname(sp$estimate)
    )
  })
}) %>%
  mutate(unit = factor(unit, levels = names(levels_data)))

log_text(
  cor_table %>%
    mutate(across(c(pearson, spearman), ~ round(.x, 3)),
           pearson_p = signif(pearson_p, 2)) %>%
    arrange(unit, desc(pearson)),
  "Correlation of WFH-readiness (Dingel-Neiman teleworkable) with AI exposure:",
  n = Inf
)

# Plots ----------------------------------------------------------------------
# (1) Heatmap of Pearson correlations across measures x aggregation level.
heatmap_plot <- cor_table %>%
  mutate(measure = factor(measure, levels = names(exposure_vars))) %>%
  ggplot(aes(x = unit, y = fct_rev(measure), fill = pearson)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", pearson)), family = "merriweather", size = 3.2) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
  labs(x = NULL, y = NULL,
       title = "WFH-readiness vs. AI exposure",
       subtitle = "Dingel-Neiman teleworkable share, Pearson correlation") +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "merriweather"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))

# (2) Scatter at the ISCO 3-digit level (the EU/AUS-l3 regression unit).
scatter_df <- levels_data[["ISCO 3-digit"]]
scatter_plots <- imap(exposure_vars, function(col, nm) {
  ggplot(scatter_df, aes(x = .data[[col]], y = wfh_teleworkable)) +
    geom_point(color = "gray40", alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(x = nm, y = "Share teleworkable") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal() +
    theme(text = element_text(family = "merriweather"))
})
scatter_combined <- (scatter_plots[[1]] + scatter_plots[[2]]) /
  (scatter_plots[[3]] + scatter_plots[[4]]) /
  (scatter_plots[[5]] + (ggplot() + theme_minimal())) +
  plot_annotation(
    title = "WFH-readiness vs. AI exposure (ISCO 3-digit occupations)",
    theme = theme_minimal()
  )

save_plot("wfh_exposure_heatmap.eps", heatmap_plot, width = 8, height = 6, device = cairo_ps)
save_plot("wfh_exposure_scatter_l3.eps", scatter_combined, width = 10, height = 8, device = cairo_ps)

saveRDS(list(cor_table = cor_table, levels_data = levels_data),
        "results/RDS/wfh_correlation.RDS")
