# Posterior Summaries ----------------------------------------------------------

library(dplyr)
library(readr)

source("config.R")
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path("scripts", "stan_csv_helpers.R"))
source(file.path("scripts", "diagnostic_helpers.R"))

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
probabilities <- c(.10, .25, .75, .90)
percentile_names <- c("p10", "p25", "p75", "p90")
coefficient_name <- "beta_sleep_between[2]"

## Percentile Draws ------------------------------------------------------------

for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  cache_file <- file.path(local_dir, "variability_percentile_draws.rds")
  if (file.exists(cache_file)) {
    next
  }

  fit_dir <- file.path(model_fit_dir, model_info$fit_folder)
  chain_files <- readRDS(file.path(fit_dir, "chain_files.rds"))
  selected <- readRDS(file.path(local_dir, "selected_draws.rds"))
  chain_ids <- vapply(
    attr(selected, "cmdstan_metadata"),
    function(x) {
      as.integer(x$id)
    },
    integer(1)
  )

  compact <- vector("list", length(chain_files))
  for (j in seq_along(chain_files)) {
    draws <- read_stan_draws(
      chain_files[j],
      c("dur_logvar_bp_participant", coefficient_name)
    )
    chain_id <- as.integer(attr(draws, "cmdstan_metadata")[[1]]$id)
    chain <- match(chain_id, chain_ids)
    between_variables <- grep(
      "^dur_logvar_bp_participant\\[",
      posterior::variables(draws),
      value = TRUE
    )
    coefficient <- as.numeric(draws[, 1, coefficient_name])
    between <- draws[, 1, between_variables, drop = TRUE]

    percentiles <- t(apply(
      between,
      1,
      quantile,
      probs = probabilities,
      type = 7,
      names = FALSE
    ))
    colnames(percentiles) <- percentile_names
    compact[[j]] <- bind_cols(
      tibble(
        chain = chain,
        chain_id = chain_id,
        iteration = seq_along(coefficient),
        coefficient = coefficient
      ),
      as_tibble(percentiles)
    )
    rm(draws, between, percentiles)
    gc(verbose = FALSE)
  }
  compact <- bind_rows(compact) |>
    arrange(chain, iteration)
  saveRDS(compact, cache_file)
}

## Parameter Summaries ---------------------------------------------------------

parameters <- list()
for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  draws <- readRDS(file.path(local_dir, "selected_draws.rds"))
  summary <- as_tibble(summarise_parameters(draws))
  hdis <- lapply(summary$variable, function(variable) {
    interval <- HDInterval::hdi(as.numeric(draws[,, variable]), credMass = .95)
    tibble(
      variable = variable,
      hdi_lower_95 = unname(interval[1]),
      hdi_upper_95 = unname(interval[2])
    )
  }) |>
    bind_rows()
  parameters[[i]] <- summary |>
    left_join(hdis, by = "variable", relationship = "one-to-one") |>
    mutate(
      instrument = model_info$instrument,
      outcome = model_info$outcome_name,
      outcome_var = model_info$outcome_var,
      run_id = model_info$fit_folder,
      .before = 1
    )
}
saveRDS(
  bind_rows(parameters),
  file.path(report_data_dir, "manuscript_parameter_summary.rds")
)
write_csv(
  bind_rows(parameters) |>
    parameter_table(),
  file.path(outputs_dir, "manuscript_parameter_summary.csv"),
  na = ""
)

## Percentiles And Paired Contrasts --------------------------------------------

posterior_interval <- function(x) {
  hdi <- HDInterval::hdi(x, credMass = .95)
  tibble(
    mean = mean(x),
    median = median(x),
    posterior_sd = sd(x),
    q2.5 = unname(quantile(x, .025)),
    q97.5 = unname(quantile(x, .975)),
    hdi_lower_95 = unname(hdi[1]),
    hdi_upper_95 = unname(hdi[2])
  )
}

percentile_summaries <- effects <- list()
for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  compact <- readRDS(file.path(local_dir, "variability_percentile_draws.rds"))

  model_labels <- tibble(
    instrument = model_info$instrument,
    outcome = sub("^(CBCL|YSR) ", "", model_info$outcome_name)
  )
  for (j in seq_along(probabilities)) {
    isd <- exp(compact[[percentile_names[j]]] / 2)
    isd_summary <- posterior_interval(isd) |>
      rename_with(~ paste0("isd_hours_", .x))
    percentile_summaries[[length(percentile_summaries) + 1L]] <- bind_cols(
      model_labels,
      tibble(
        percentile = paste0(100 * probabilities[j], "%")
      ),
      isd_summary
    )
  }

  for (pair in list(c("p25", "p75"), c("p10", "p90"))) {
    low <- compact[[pair[1]]]
    high <- compact[[pair[2]]]

    difference <- compact$coefficient * (high - low)
    effect <- posterior_interval(difference) |>
      rename(
        predicted_tscore_difference = mean,
        predicted_tscore_difference_median = median
      )
    effects[[length(effects) + 1L]] <- bind_cols(
      model_labels,
      tibble(
        comparison = if (pair[1] == "p25") {
          "25th vs 75th"
        } else {
          "10th vs 90th"
        },
        isd_low = median(exp(low / 2)),
        isd_high = median(exp(high / 2))
      ),
      effect,
      tibble(
        normative_sd_difference = mean(difference) / 10,
        normative_sd_q2.5 = unname(quantile(difference, .025)) / 10,
        normative_sd_q97.5 = unname(quantile(difference, .975)) / 10
      )
    )
  }
}
write_csv(
  bind_rows(percentile_summaries) |>
    transmute(
      Instrument = instrument,
      Outcome = outcome,
      Percentile = percentile,
      `Nightly SD (hours): posterior mean` = round(isd_hours_mean, 3),
      `Nightly SD (hours): posterior median` = round(isd_hours_median, 3),
      `Posterior SD (hours)` = round(isd_hours_posterior_sd, 3),
      `95% CrI lower (hours, equal-tailed)` = round(isd_hours_q2.5, 3),
      `95% CrI upper (hours, equal-tailed)` = round(isd_hours_q97.5, 3),
      `95% HDI lower (hours)` = round(isd_hours_hdi_lower_95, 3),
      `95% HDI upper (hours)` = round(isd_hours_hdi_upper_95, 3)
    ),
  file.path(outputs_dir, "sleep_variability_percentiles.csv"),
  na = ""
)
write_csv(
  bind_rows(effects) |>
    transmute(
      Instrument = instrument,
      Outcome = outcome,
      Comparison = recode(
        comparison,
        "25th vs 75th" = "75th minus 25th percentile",
        "10th vs 90th" = "90th minus 10th percentile"
      ),
      `Lower percentile nightly SD (hours)` = round(isd_low, 3),
      `Upper percentile nightly SD (hours)` = round(isd_high, 3),
      `T-score difference` = round(predicted_tscore_difference, 3),
      `Posterior SD` = round(posterior_sd, 3),
      `95% CrI lower (equal-tailed)` = round(q2.5, 3),
      `95% CrI upper (equal-tailed)` = round(q97.5, 3),
      `95% HDI lower` = round(hdi_lower_95, 3),
      `95% HDI upper` = round(hdi_upper_95, 3),
      `Difference in normative SD units` = round(normative_sd_difference, 3),
      `95% CrI lower (normative SD units)` = round(normative_sd_q2.5, 3),
      `95% CrI upper (normative SD units)` = round(normative_sd_q97.5, 3)
    ),
  file.path(outputs_dir, "percentile_effect_sizes.csv"),
  na = ""
)
