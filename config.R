# Analysis Configuration -------------------------------------------------------

## Packages --------------------------------------------------------------------

required_packages <- c(
  "arrow",
  "bayesplot",
  "cmdstanr",
  "dplyr",
  "ggforce",
  "ggplot2",
  "ggtext",
  "HDInterval",
  "here",
  "knitr",
  "lme4",
  "lmerTest",
  "lubridate",
  "posterior",
  "readr",
  "rmarkdown",
  "ragg",
  "stringr",
  "systemfonts",
  "tibble",
  "tidyr",
  "xdvir"
)
library(here)
library(dplyr)

## Paths -----------------------------------------------------------------------

abcd_dir <- "/path/to/abcd/7.0"
working_dir <- here("working")
model_fit_dir <- "/path/to/model-fits"

sleep_path <- file.path(
  abcd_dir,
  "concatenated",
  "novel_technologies",
  "fitbit",
  "fitbit_ss_sleep_day.parquet"
)
cbcl_path <- file.path(abcd_dir, "rawdata", "phenotype", "mh_p_cbcl.parquet")
ysr_path <- file.path(abcd_dir, "rawdata", "phenotype", "mh_y_ysr.parquet")
income_path <- file.path(abcd_dir, "rawdata", "phenotype", "ab_g_dyn.parquet")
demographics_path <- file.path(
  abcd_dir,
  "rawdata",
  "phenotype",
  "ab_g_stc.parquet"
)

data_sets_dir <- file.path(working_dir, "processed")
model_inputs_dir <- file.path(working_dir, "model-inputs")
summaries_dir <- file.path(working_dir, "summaries")
local_fits_dir <- file.path(working_dir, "fits")
outputs_dir <- file.path(working_dir, "results")
report_data_dir <- file.path(outputs_dir, "data")

## Outcomes --------------------------------------------------------------------

minimum_nights <- 1L
maximum_ysr_linkage_days <- 365

outcomes <- tibble(
  instrument = rep(c("CBCL", "YSR"), each = 3),
  outcome_key = rep(c("externalizing", "internalizing", "total"), 2),
  outcome_var = c(
    "mh_p_cbcl__synd__ext_tscore",
    "mh_p_cbcl__synd__int_tscore",
    "mh_p_cbcl_tscore",
    "mh_y_ysr__synd__ext_tscore",
    "mh_y_ysr__synd__int_tscore",
    "mh_y_ysr_tscore"
  ),
  outcome_name = c(
    "CBCL Externalizing Problems",
    "CBCL Internalizing Problems",
    "CBCL Total Problems",
    "YSR Externalizing Problems",
    "YSR Internalizing Problems",
    "YSR Total Problems"
  ),
  model_name = paste(tolower(instrument), outcome_key, "detrended", sep = "_")
)

models <- outcomes |>
  mutate(fit_folder = model_name)

selected_models <- models

## Sampling --------------------------------------------------------------------

sampling_settings <- list(
  seed = 12345L,
  chains = 4L,
  iter_warmup = 2000L,
  iter_sampling = 2000L,
  adapt_delta = 0.9,
  max_treedepth = 12L
)
