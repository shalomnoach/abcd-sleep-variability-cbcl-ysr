# Extract Model Results --------------------------------------------------------

library(dplyr)
library(readr)

source("config.R")
source(file.path("scripts", "stan_csv_helpers.R"))
source(file.path("scripts", "ppc_helpers.R"))

## Selected Draws --------------------------------------------------------------

parameters <- c(
  "beta_sleep_between",
  "beta_sleep_within",
  "gamma_sleep_mean",
  "gamma_sleep_age",
  "tau",
  "L_Omega",
  "sigma_participant",
  "sigma_outcome",
  "beta0",
  "beta_race",
  "beta_ethnicity",
  "beta_income",
  "divergent__",
  "treedepth__",
  "energy__"
)

for (i in seq_len(nrow(selected_models))) {
  model_info <- selected_models[i, ]
  fit_dir <- file.path(model_fit_dir, model_info$fit_folder)
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)

  stan_data <- readRDS(file.path(fit_dir, "stan_data.rds"))
  chain_files <- readRDS(file.path(fit_dir, "chain_files.rds"))
  fit_settings <- readRDS(file.path(fit_dir, "sampling_settings.rds"))

  draws <- read_stan_draws(chain_files, parameters)
  diagnostics <- sampler_diagnostics(draws, fit_settings$max_treedepth)

  saveRDS(draws, file.path(local_dir, "selected_draws.rds"))
  saveRDS(stan_data, file.path(local_dir, "stan_data.rds"))
  write_csv(diagnostics, file.path(local_dir, "sampler_diagnostics.csv"))
  print(diagnostics)
}

## Posterior Predictive Checks -------------------------------------------------

for (i in seq_len(nrow(selected_models))) {
  model_info <- selected_models[i, ]
  fit_dir <- file.path(model_fit_dir, model_info$fit_folder)
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  stan_data <- readRDS(file.path(fit_dir, "stan_data.rds"))
  chain_files <- readRDS(file.path(fit_dir, "chain_files.rds"))

  posterior_predictive_summary <- decomp_ppc(
    chain_files,
    stan_data,
    n_draws = 100L,
    seed = 12345L
  )
  write_csv(
    posterior_predictive_summary,
    file.path(local_dir, "posterior_predictive_summary.csv")
  )
  print(posterior_predictive_summary)
}
