# Generate Tables And Figures --------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

source("config.R")
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path("scripts", "diagnostic_helpers.R"))

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

## Parameter Summaries ---------------------------------------------------------

predictor_labels <- c(
  "Duration Mean",
  "Duration Variability (log variance)",
  "Midpoint Mean",
  "Midpoint Variability (log variance)"
)
effects <- parameters <- diagnostics <- ppc_summaries <- list()
for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  local_dir <- file.path(local_fits_dir, model_info$fit_folder)
  stan_data <- readRDS(file.path(local_dir, "stan_data.rds"))
  draws <- readRDS(file.path(local_dir, "selected_draws.rds"))
  model_max_treedepth <- vapply(
    attr(draws, "cmdstan_metadata"),
    function(metadata) as.integer(metadata$max_depth),
    integer(1)
  )
  model_max_treedepth <- unique(model_max_treedepth)

  parameters[[i]] <- summarise_parameters(draws) |>
    as_tibble() |>
    mutate(
      instrument = model_info$instrument,
      outcome = model_info$outcome_name,
      outcome_var = model_info$outcome_var,
      run_id = model_info$fit_folder,
      .before = 1
    )
  effects[[i]] <- parameters[[i]] |>
    filter(str_detect(variable, "^beta_sleep_(between|within)\\[")) |>
    mutate(
      component = if_else(str_detect(variable, "between"), "Between", "Within"),
      predictor_index = as.integer(str_extract(
        variable,
        "(?<=\\[)\\d+(?=\\])"
      )),
      predictor = predictor_labels[predictor_index],
      outcome_key = model_info$outcome_key,
      detrended = TRUE,
      time_metric = "sleep-window midpoint age",
      N_participants = stan_data$N_participants,
      N_sessions = stan_data$N_ps,
      N_nights = stan_data$N_obs
    )
  diagnostics[[i]] <- read_csv(
    file.path(local_dir, "sampler_diagnostics.csv"),
    show_col_types = FALSE
  ) |>
    mutate(
      instrument = model_info$instrument,
      outcome = model_info$outcome_name,
      outcome_var = model_info$outcome_var,
      run_id = model_info$fit_folder,
      max_treedepth = model_max_treedepth,
      .before = 1
    )
  ppc_summaries[[i]] <- read_csv(
    file.path(local_dir, "posterior_predictive_summary.csv"),
    show_col_types = FALSE
  ) |>
    mutate(
      instrument = model_info$instrument,
      outcome = model_info$outcome_name,
      outcome_var = model_info$outcome_var,
      run_id = model_info$fit_folder,
      .before = 1
    )
}
effects <- bind_rows(effects) |>
  mutate(
    outcome_key = factor(
      outcome_key,
      levels = c("externalizing", "internalizing", "total")
    ),
    instrument = factor(instrument, levels = c("CBCL", "YSR")),
    component = factor(component, levels = c("Between", "Within")),
    significant = (q2.5 > 0 & q97.5 > 0) | (q2.5 < 0 & q97.5 < 0)
  )
parameter_summary <- bind_rows(parameters)
covariate_effects <- parameter_summary |>
  inner_join(covariate_labels(), by = "variable", relationship = "many-to-one")
sampler_summary <- bind_rows(diagnostics)
posterior_predictive_summary <- bind_rows(ppc_summaries)

saveRDS(
  parameter_summary,
  file.path(report_data_dir, "global_parameter_summary.rds")
)
write_csv(
  parameter_summary |>
    parameter_table(),
  file.path(outputs_dir, "global_parameter_summary.csv"),
  na = ""
)
write_csv(
  covariate_effects |>
    transmute(
      Outcome = outcome,
      Covariate = covariate,
      Category = category,
      Reference = reference,
      `T-score difference` = round(mean, 3),
      `Posterior SD` = round(sd, 3),
      `95% CrI lower (equal-tailed)` = round(q2.5, 3),
      `95% CrI upper (equal-tailed)` = round(q97.5, 3)
    ),
  file.path(outputs_dir, "covariate_effects.csv"),
  na = ""
)
saveRDS(sampler_summary, file.path(report_data_dir, "sampler_diagnostics.rds"))
write_csv(
  sampler_summary |>
    transmute(
      Outcome = outcome,
      Chain = chain,
      `Post-warmup draws` = post_warmup_draws,
      Divergences = divergences,
      `Maximum tree depth` = max_treedepth,
      `Tree depth hits` = treedepth_saturations,
      `E-BFMI` = round(ebfmi, 3)
    ),
  file.path(outputs_dir, "sampler_diagnostics.csv"),
  na = ""
)
write_csv(
  posterior_predictive_summary |>
    transmute(
      Outcome = outcome,
      Statistic = recode(
        statistic,
        outcome_mean = "Outcome mean (T-score)",
        outcome_sd = "Outcome SD (T-score)",
        outcome_q10 = "Outcome 10th percentile (T-score)",
        outcome_q90 = "Outcome 90th percentile (T-score)",
        duration_mean = "Mean sleep duration (hours)",
        midpoint_mean = "Mean sleep midpoint (hours)",
        duration_mean_session_sd = "Mean within-session duration SD (hours)",
        midpoint_mean_session_sd = "Mean within-session midpoint SD (hours)",
        outcome_repeat_correlation = "Correlation between repeated outcome scores"
      ),
      Observed = round(observed, 3),
      `Replicated mean` = round(replicated_mean, 3),
      `Replicated 2.5th percentile` = round(replicated_q025, 3),
      `Replicated 97.5th percentile` = round(replicated_q975, 3),
      `Posterior predictive p` = posterior_predictive_p
    ),
  file.path(outputs_dir, "posterior_predictive_summary.csv"),
  na = ""
)
print(sampler_summary, n = Inf)
print(posterior_predictive_summary, n = Inf)

## Fixed Contrasts -------------------------------------------------------------

effect_sizes <- effects |>
  select(-significant) |>
  filter(component == "Between") |>
  mutate(
    contrast = if_else(predictor_index %in% c(2L, 4L), 2 * log(2), 1),
    contrast_description = if_else(
      predictor_index %in% c(2L, 4L),
      "Multiply geometric nightly SD across age-adjusted windows by two",
      "Increase age-adjusted person-average mean by one hour"
    ),
    predicted_tscore_difference = mean * contrast,
    lower_tscore_difference = q2.5 * contrast,
    upper_tscore_difference = q97.5 * contrast,
    normative_sd_difference = predicted_tscore_difference / 10
  )
write_csv(
  effect_sizes |>
    transmute(
      Outcome = outcome,
      Predictor = predictor,
      Contrast = contrast_description,
      `T-score difference` = round(predicted_tscore_difference, 3),
      `95% CrI lower (equal-tailed)` = round(lower_tscore_difference, 3),
      `95% CrI upper (equal-tailed)` = round(upper_tscore_difference, 3),
      `Difference in normative SD units` = round(normative_sd_difference, 3)
    ),
  file.path(outputs_dir, "effect_sizes.csv"),
  na = ""
)

## Sample Summaries ------------------------------------------------------------

sample_files <- c(
  "cohort_summary.csv",
  "outcome_eligibility.csv",
  "session_summary.csv",
  "covariate_missingness.csv",
  "model_input_summary.csv",
  "assessment_sleep_linkage_summary.csv",
  "income_source_summary.csv"
)
file.copy(file.path(summaries_dir, sample_files), outputs_dir, overwrite = TRUE)

## Primary Results Table -------------------------------------------------------

primary_table <- effects |>
  filter(component == "Between") |>
  arrange(outcome_key, instrument, predictor) |>
  transmute(
    instrument,
    outcome = as.character(outcome_key),
    predictor,
    estimate = mean,
    posterior_sd = sd,
    lower_95 = q2.5,
    upper_95 = q97.5,
    significant,
    rhat,
    ess_bulk,
    ess_tail,
    N_participants,
    N_sessions,
    N_nights
  )
write_csv(
  primary_table |>
    transmute(
      Instrument = instrument,
      Outcome = paste(str_to_title(outcome), "Problems"),
      Predictor = predictor,
      Estimate = round(estimate, 3),
      `Posterior SD` = round(posterior_sd, 3),
      `95% CrI lower (equal-tailed)` = round(lower_95, 3),
      `95% CrI upper (equal-tailed)` = round(upper_95, 3),
      Participants = N_participants,
      Sessions = N_sessions,
      Nights = N_nights
    ),
  file.path(outputs_dir, "table3_abcd70.csv"),
  na = ""
)
write_csv(
  effects |>
    transmute(
      Outcome = outcome,
      Component = paste0(component, "-person"),
      Predictor = predictor,
      Estimate = round(mean, 3),
      `Posterior SD` = round(sd, 3),
      `95% CrI lower (equal-tailed)` = round(q2.5, 3),
      `95% CrI upper (equal-tailed)` = round(q97.5, 3)
    ),
  file.path(outputs_dir, "all_between_within_effects.csv"),
  na = ""
)
print(primary_table, n = Inf)

typst_rows <- primary_table |>
  mutate(
    estimate = sprintf("%.2f", estimate),
    interval = sprintf("[%.2f, %.2f]", lower_95, upper_95),
    across(
      c(estimate, interval),
      ~ if_else(significant, paste0("*", .x, "*"), .x)
    ),
    row = sprintf(
      "  [%s], [%s], [%s], [%s], [%s],",
      instrument,
      str_to_title(outcome),
      predictor,
      estimate,
      interval
    )
  ) |>
  pull(row)

writeLines(
  c(
    "#figure(kind: table, caption: [ABCD 7.0 age-detrended sleep models])[",
    "#table(",
    "  columns: (auto, auto, 1fr, auto, auto),",
    "  [*Instrument*], [*Outcome*], [*Sleep parameter*], [*Estimate*], [*95% CrI*],",
    typst_rows,
    ")",
    "#text(size: 9pt, style: \"italic\")[Note. Primary coefficients are age-detrended between-person effects. Bold credible intervals exclude zero.]",
    "] <tbl-abcd70>"
  ),
  file.path(outputs_dir, "table3_abcd70.typ")
)

## Trace And Rank Plots --------------------------------------------------------

for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  draws <- readRDS(file.path(
    local_fits_dir,
    model_info$fit_folder,
    "selected_draws.rds"
  ))
  save_diagnostic_plots(
    draws,
    model_info$outcome_name,
    file.path(outputs_dir, "diagnostics", model_info$model_name)
  )
}
