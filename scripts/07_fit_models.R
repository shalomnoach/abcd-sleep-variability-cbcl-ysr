# Fit Models -------------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "model_helpers.R"))

library(cmdstanr)

print(selected_models)

model <- cmdstan_model(here("models", "sleep_iiv_model_cbcl_decomp.stan"))

for (i in seq_len(nrow(selected_models))) {
  model_info <- selected_models[i, ]
  stan_data <- readRDS(file.path(
    model_inputs_dir,
    model_info$model_name,
    "stan_data.rds"
  ))
  fit_dir <- file.path(model_fit_dir, model_info$fit_folder)
  dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(stan_data, file.path(fit_dir, "stan_data.rds"))
  saveRDS(sampling_settings, file.path(fit_dir, "sampling_settings.rds"))

  set.seed(sampling_settings$seed)
  fit <- model$sample(
    data = stan_data,
    chains = sampling_settings$chains,
    parallel_chains = sampling_settings$chains,
    iter_warmup = sampling_settings$iter_warmup,
    iter_sampling = sampling_settings$iter_sampling,
    seed = sampling_settings$seed,
    init = function() model_initial_values(stan_data),
    refresh = 100,
    output_dir = fit_dir,
    adapt_delta = sampling_settings$adapt_delta,
    max_treedepth = sampling_settings$max_treedepth,
    show_exceptions = TRUE,
    diagnostics = NULL,
    save_warmup = FALSE
  )

  saveRDS(fit, file.path(fit_dir, "fit.rds"))
  saveRDS(fit$output_files(), file.path(fit_dir, "chain_files.rds"))
}
