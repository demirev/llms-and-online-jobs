# Generative AI and Job Displacement: README

This repository contains the materials for the working paper **Generative AI and Job Displacement: Initial Evidence from EU Online Job Markets** by Georgi Demirev (Sofia University).

**DISCLAIMER**: This project is a work in progress which is yet to be submitted for peer review. It may undergo slight or even significant changes prior to eventual publication.

## Overview

- **Objective**: Quantify the early impact of large language models (e.g., ChatGPT) on labor demand and productivity in the European Union (2021–2024).
- **Data**:
  - Four separate occupational AI-exposure indices ([Felten 2018](https://github.com/AIOE-Data/AIOE), [Webb 2022](https://www.notion.so/michaelwebb/Data-for-The-Impact-of-Artificial-Intelligence-on-the-Labor-Market-3b52b281505a48b8be107d11d8d0c363), [Eloundou et al 2023](https://github.com/openai/GPTs-are-GPTs/), [Demirev 2024](https://github.com/demirev/ai-products)) mapped to ESCO occupations.
  - CEDEFOP's Skills OVATE dataset (100+ million online job postings grouped by ESCO level 3 occupations) as a proxy for labor demand.
  - Eurostat industry-level productivity metrics.
- **Method**: Difference-in-differences approach (continuous treatment) using ChatGPT's public release (Nov 2022) as the event date.
- **Key Findings**:
  1. Occupations with higher AI exposure saw a 13–18% larger drop in job postings relative to the least-exposed.
  2. Skills most similar to AI capabilities declined in mention frequency by about 10%.
  3. No clear evidence of significant productivity gains at the industry level so far.

## Key plots and tables

These plots illustrate the relationship between changes in job postings (post vs. pre-ChatGPT) and AI-exposure measures (after partialling out country fixed effects):

![Partial Correlation (All)](results/plots/partial_plots_all.svg)

Each dot is a country-occupation observation. X-axis is the given exposure measure , rescaled 0 to 1. Y-axis is the change (in logs) in the number of job postings in the CEDEFOP dataset. Cross markers are occupation means. The negative slopes show higher AI exposure correlates with a steeper decline in online job ads.

## Full text

[Working paper](working_paper.pdf)

## Reproducability

0. Download all tableu files published by CEDEFOP each quarter from the links listed in `data/cedefop_skills_ovate_oja/links.txt` and put them in `data/cedefop_skills_ovate_oja/twbx`
1. Run `py/convert_tableau_files.py` - this script converts the tableu files to csv and saves them in `data/cedefop_skills_ovate_oja/csv`
2. Run `py/collect_occupations_by_sector.py`. This will collect data on occupational employemnt by sector and save it to `data/cedefop_skills_intelligence/cedefop_sectoral_employment_data.csv`. This is used to calculate the AI-exposure indices on the sectoral level.
3. Run `R/descriptive.R`. This will generate various plots and tables containing descriptive statistics.
4. Run `R/model_oja.R`. This will run the main models on the effect of ChatGPT on job listings.
5. Run `R/model_skill_demand.R`. This will run the models on the effect of ChatGPT on skill mention frequency.
6. Run `R/model_nama.R`. This will calculate exposure scores on the NACE Rev. 2 level and run the models on the effect of ChatGPT on NACE Rev. 2 level productivity.


## Citation

If you use this repository for your research, please cite the working paper as follows:

```
@article{demirev2025,
  title={Generative AI and Job Displacement: Initial Evidence from EU Online Job Markets},
  author={Demirev, Georgi},
  year={2025},
  journal={Working Paper, Sofia University}
}
```

## Contact

For any questions or comments, please contact Georgi Demirev at \[first_initial\]\[surname\]\[at\]\[uni-sofia\]\[dot\]\[bg].
