# ABCD Sleep Variability and Mental Health

Reproducible analysis code and manuscript source for:

**Jaffe SN, McGlinchey EL. “The Association Between Sleep Variability and Psychological Functioning in the ABCD Study”**

The analysis uses ABCD Study release 7.0 Fitbit data and six Bayesian multilevel location-scale models to examine sleep duration, sleep timing, and their variability in relation to CBCL and YSR scores.

Participant-level data are not included. Reproduction requires approved access to ABCD release 7.0. Results, figures, fitted models, and rendered documents are generated locally.

## Requirements

- R (tested with 4.5.2).
- CmdStan 2.37.0 and a supported C++ toolchain.
- Quarto (tested with 1.10.18).
- XeLaTeX with `amsmath`, `fontspec`, `unicode-math`, and Latin Modern Math for the path diagrams.

Install the R packages listed in `config.R` from the project root:

``` r
install.packages(c("here", "dplyr"))
source("config.R")
install.packages(
  setdiff(required_packages, rownames(installed.packages())),
  repos = c("https://stan-dev.r-universe.dev", "https://cloud.r-project.org")
)
```

Install CmdStan if needed:

``` r
cmdstanr::install_cmdstan(version = "2.37.0")
```

Scripts 14–16 download Open Sans from Google Fonts on first use and save it in `working/fonts/`.

## Data and Local Paths

Obtain the following Parquet files through the [NBDC Data Hub](https://www.nbdc-datahub.org/). Paths below are relative to the ABCD release directory.

| File | Contents |
|------------------------------------|------------------------------------|
| `concatenated/novel_technologies/fitbit/fitbit_ss_sleep_day.parquet` | Daily sleep duration and timestamps |
| `rawdata/phenotype/mh_p_cbcl.parquet` | Parent-reported mental health |
| `rawdata/phenotype/mh_y_ysr.parquet` | Youth-reported mental health |
| `rawdata/phenotype/ab_g_dyn.parquet` | Visit age and household income |
| `rawdata/phenotype/ab_g_stc.parquet` | Birth date, sex, race, and ethnicity |

In `config.R`, set `abcd_dir` to the release directory and `model_fit_dir` to storage for the full Stan chains. `working_dir` defaults to the local `working/` folder. Leave all six models selected to reproduce the manuscript; sampling settings and the random seed are also in `config.R`.

## Running the Analysis

| Script | Purpose |
|----------------------------------|--------------------------------------|
| `01_prepare_sleep.R` | Prepare nightly Fitbit sleep data |
| `02_prepare_mental_health.R` | Prepare CBCL, YSR, and household income |
| `03_prepare_demographics.R` | Prepare participant demographics |
| `04_build_analysis_data.R` | Link sleep sessions, assessments, and demographics |
| `05_sample_summary.R` | Summarize sample inclusion and missingness |
| `06_prepare_model_inputs.R` | Create inputs for the six Stan models |
| `07_fit_models.R` | Fit the Bayesian models |
| `08_extract_results.R` | Extract posterior draws and predictive checks |
| `09_generate_outputs.R` | Produce estimates, diagnostics, and the forest figure |
| `10_fit_sensitivity_models.R` | Fit the frequentist sensitivity models |
| `11_descriptive_statistics.R` | Calculate sample descriptives and correlations |
| `12_posterior_summaries.R` | Calculate posterior intervals and percentile contrasts |
| `13_correlation_table.R` | Prepare correlation-table descriptives and empirical percentiles |
| `14_sleep_variability_figure.R` | Draw examples of nightly sleep variability |
| `15_statistical_path_diagram.R` | Draw the statistical model |
| `16_conceptual_path_diagram.R` | Draw the conceptual model |

All numbered scripts are in `scripts/`. Step 7 fits the models sequentially and requires new fit-folder names when refitting. Keep the full chains available through steps 8 and 12; after those steps, the saved draws and summaries are sufficient to regenerate the Bayesian results.

Prepared data and selected draws stay in `working/`. Research tables and diagnostics are saved in `working/results/`, and manuscript figures in `figures/`. The QMD files read full-precision results from `working/results/data/` and use the generated figures.

## Rendering the Manuscript

After scripts 1–16 finish, run:

``` bash
quarto render manuscript.qmd --to typst
quarto render supporting-information.qmd --to typst
```

These commands create `manuscript.pdf` and `supporting-information.pdf`. Rendering uses the saved results and does not refit the models. The supporting information contains the model specification, complete parameter estimates, and sensitivity analyses.

## Repository Contents

``` text
.
├── .gitignore
├── .here
├── README.md
├── apa.csl
├── apa7-template.typ
├── config.R
├── manuscript.qmd
├── models/
│   └── sleep_iiv_model_cbcl_decomp.stan
├── references.bib
├── scripts/
│   ├── 01_prepare_sleep.R
│   ├── 02_prepare_mental_health.R
│   ├── 03_prepare_demographics.R
│   ├── 04_build_analysis_data.R
│   ├── 05_sample_summary.R
│   ├── 06_prepare_model_inputs.R
│   ├── 07_fit_models.R
│   ├── 08_extract_results.R
│   ├── 09_generate_outputs.R
│   ├── 10_fit_sensitivity_models.R
│   ├── 11_descriptive_statistics.R
│   ├── 12_posterior_summaries.R
│   ├── 13_correlation_table.R
│   ├── 14_sleep_variability_figure.R
│   ├── 15_statistical_path_diagram.R
│   ├── 16_conceptual_path_diagram.R
│   ├── apa-headings.lua
│   ├── diagnostic_helpers.R
│   ├── figure_fonts.R
│   ├── helpers.R
│   ├── manuscript-reference-format.lua
│   ├── manuscript_tables.R
│   ├── model_helpers.R
│   ├── path_diagram_helpers.R
│   ├── ppc_helpers.R
│   ├── sleep_helpers.R
│   ├── stan_csv_helpers.R
│   ├── supporting-format.lua
│   └── templates/
│       └── supporting/
│           └── typst-show.typ
├── supporting-information.bib
├── supporting-information.qmd
└── typst-show.typ
```