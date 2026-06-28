---
name: aus-robustness-analysis
description: Design and findings of the Australian (IVI) robustness check in R/model_aus.R
metadata:
  type: project
---

`R/model_aus.R` re-runs the two headline labour-demand specs (delta + event study, no TWFE/decile/eures/sensitivity) on Jobs and Skills Australia's Internet Vacancy Index, as an out-of-sample robustness check for the EU analysis.

Key design decisions (agreed with the user, 2026-06-25):
- **Geography = state** (8 states/territories) plays the `idcountry` role: both the panel dimension and the fixed effect. `AUST` national total dropped (like EU27).
- **ANZSCO→ISCO via `data/aus/anzsco_isco08_correspondence.csv`** (ABS 1220.0, downloaded from Internet Archive item 12200-2013). Exposure is **simple-averaged across all matched ISCO categories** when one ANZSCO 4-digit maps to several. Run at both **ISCO 4-digit** (`l4`, native) and **ISCO 3-digit** (`l3`, matches EU). Observation unit stays ANZSCO 4-digit for both; only exposure granularity differs.
- **Decoupled windows**: event study uses `event_window_start = 2016-01-01` (long pre-period for a strong pre-trends test, ~-27..+14 quarters); delta uses `delta_window_start = 2021-10-01` (EU-comparable: PRE = 4 quarters, POST = all after) so a multi-year average doesn't pollute the delta baseline. `aus_fmt` (event study) spans 2016+; `aus_delta` filters `aus_fmt` to the recent window before `format_delta_data`.
- Monthly 3-month-MA IVI collapsed to **quarterly mean levels** so the EU quarterly `event_time` logic in helpers carries over unchanged. Reuses `read_ai_exposure_file`, `format_delta_data`, `run_event_study_model`, `plot_event_study`, `save_plot`.
- Outputs: `results/RDS/aus_models.RDS` (gitignored), log `results/logs/aus_models.txt`, plots `results/plots/aus_*` + `tex/img/aus_*`.

Findings (delta, headline): Demirev (−0.15***) and Anthropic usage (−0.36***) negative & significant, consistent with EU; **Felten positive (+0.12*) and Webb positive/ns — opposite to EU**. Automation/augmentation split is clean: automation strongly negative, augmentation positive in both indices/levels. Caveat: only **8 state clusters** → clustered SEs unreliable (fixest "VCOV not positive definite" warnings); point estimates fine. See [[caveat-fixest-print-truncates-coefs]].
