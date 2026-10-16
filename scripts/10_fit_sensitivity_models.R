# Frequentist Sensitivity Models -----------------------------------------------

library(here)
source(here("config.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(lme4)
library(lmerTest)
library(readr)
library(tidyr)

minimum_sensitivity_nights <- 5L

## Helpers ---------------------------------------------------------------------

prepare_session_data <- function(data, outcome_var) {
  paired_sessions <- data |>
    filter(
      model_sleep_eligible %in% TRUE,
      is.finite(sleep_duration_hours),
      is.finite(midpoint_hours)
    ) |>
    group_by(participant_id, sleep_session_id) |>
    summarise(
      participant_session_id = first(participant_session_id),
      n_paired_nights = n(),
      duration_mean = mean(sleep_duration_hours),
      duration_isd = stats::sd(sleep_duration_hours),
      midpoint_mean = mean(midpoint_hours),
      midpoint_isd = stats::sd(midpoint_hours),
      age_mid_ps = as.numeric(difftime(
        min(dt) + (max(dt) - min(dt)) / 2,
        first(dob),
        units = "days"
      )) /
        365.25,
      outcome = first(.data[[outcome_var]]),
      race_nih = first(race_nih),
      ethnicity = first(ethnicity),
      income_3lvl = first(income_3lvl),
      .groups = "drop"
    )

  available_sessions <- data |>
    distinct(participant_id, sleep_session_id, participant_session_id) |>
    nrow()
  empirical_sessions <- paired_sessions |>
    filter(n_paired_nights >= minimum_sensitivity_nights)
  outcome_sessions <- empirical_sessions |>
    filter(is.finite(outcome), is.finite(age_mid_ps))
  analysis_sessions <- outcome_sessions |>
    filter(
      !is.na(race_nih),
      !is.na(ethnicity),
      !is.na(income_3lvl)
    )

  list(
    data = analysis_sessions,
    counts = tibble(
      sessions_available = available_sessions,
      sessions_paired_duration_midpoint = nrow(paired_sessions),
      sessions_minimum_nights = nrow(empirical_sessions),
      sessions_outcome_age_complete = nrow(outcome_sessions),
      sessions_analysis_complete = nrow(analysis_sessions),
      excluded_fewer_than_minimum_nights = nrow(paired_sessions) -
        nrow(empirical_sessions),
      excluded_missing_outcome_or_age = nrow(empirical_sessions) -
        nrow(outcome_sessions),
      excluded_missing_covariate = nrow(outcome_sessions) -
        nrow(analysis_sessions)
    )
  )
}

fit_sensitivity_model <- function(data) {
  feature_names <- c(
    "duration_mean",
    "duration_isd",
    "midpoint_mean",
    "midpoint_isd"
  )
  data <- data |>
    mutate(
      age_time_c = age_mid_ps - mean(age_mid_ps),
      race_nih = factor(
        case_when(
          race_nih == "White" ~ "White",
          race_nih == "Black (African American)" ~ "Black (African American)",
          race_nih == "Asian" ~ "Asian",
          race_nih == "More than One Race" ~ "More than One Race",
          race_nih %in%
            c(
              "American Indian/Alaska Native",
              "Native Hawaiian or Other Pacific Islander",
              "Other (Unknown or not reported)"
            ) ~ "Other",
          TRUE ~ NA_character_
        ),
        levels = c(
          "White",
          "Black (African American)",
          "Asian",
          "More than One Race",
          "Other"
        )
      ),
      ethnicity = factor(
        ethnicity,
        levels = c("Non-Hispanic", "Hispanic")
      ),
      income_3lvl = factor(
        income_3lvl,
        levels = c(
          "> $100K",
          "$50K-$100K",
          "< $50K",
          "Don't know",
          "Decline to answer"
        )
      )
    ) |>
    droplevels()

  slopes <- vapply(
    feature_names,
    function(feature) {
      age_model <- stats::lm(
        stats::reformulate("age_time_c", feature),
        data = data
      )
      stats::coef(age_model)[["age_time_c"]]
    },
    numeric(1)
  )

  for (feature in feature_names) {
    detrended <- data[[feature]] - slopes[[feature]] * data$age_time_c
    person_mean <- ave(detrended, data$participant_id, FUN = mean)
    data[[paste0(feature, "_between")]] <- person_mean
    data[[paste0(feature, "_within")]] <- detrended - person_mean
  }

  model <- lmerTest::lmer(
    outcome ~ duration_mean_between +
      duration_mean_within +
      duration_isd_between +
      duration_isd_within +
      midpoint_mean_between +
      midpoint_mean_within +
      midpoint_isd_between +
      midpoint_isd_within +
      race_nih +
      ethnicity +
      income_3lvl +
      (1 | participant_id),
    data = data,
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 100000L)
    )
  )

  list(
    model = model,
    data = data,
    age_slopes = slopes
  )
}

extract_coefficients <- function(fit, model_info) {
  sleep_labels <- c(
    duration_mean_between = "Sleep Duration (Mean)",
    duration_isd_between = "Sleep Duration (iSD)",
    midpoint_mean_between = "Sleep Midpoint (Mean)",
    midpoint_isd_between = "Sleep Midpoint (iSD)",
    duration_mean_within = "Sleep Duration (Mean)",
    duration_isd_within = "Sleep Duration (iSD)",
    midpoint_mean_within = "Sleep Midpoint (Mean)",
    midpoint_isd_within = "Sleep Midpoint (iSD)"
  )

  tibble::as_tibble(
    summary(fit$model)$coefficients,
    rownames = "term"
  ) |>
    transmute(
      term,
      estimate = Estimate,
      std_error = `Std. Error`,
      df = df,
      statistic = `t value`,
      p_value = `Pr(>|t|)`,
      conf_low = estimate - stats::qt(0.975, df) * std_error,
      conf_high = estimate + stats::qt(0.975, df) * std_error
    ) |>
    mutate(
      instrument = model_info$instrument,
      outcome_key = model_info$outcome_key,
      outcome_name = model_info$outcome_name,
      component = case_when(
        term %in% names(sleep_labels) & grepl("_between$", term) ~ "Between",
        term %in% names(sleep_labels) & grepl("_within$", term) ~ "Within",
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ "Covariate"
      ),
      predictor = case_when(
        term %in% names(sleep_labels) ~ unname(sleep_labels[term]),
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ term
      )
    ) |>
    select(
      instrument,
      outcome_key,
      outcome_name,
      component,
      predictor,
      term,
      estimate,
      std_error,
      conf_low,
      conf_high,
      df,
      statistic,
      p_value
    )
}

extract_diagnostics <- function(fit, model_info) {
  variance <- as.data.frame(lme4::VarCorr(fit$model))

  tibble(
    instrument = model_info$instrument,
    outcome_key = model_info$outcome_key,
    outcome_name = model_info$outcome_name,
    observations = stats::nobs(fit$model),
    participant_sessions = nrow(fit$data),
    participants = n_distinct(fit$data$participant_id),
    participants_multiple_sessions = sum(table(fit$data$participant_id) > 1L),
    singular = as.integer(lme4::isSingular(fit$model, tol = 1e-4)),
    optimizer_code = as.numeric(fit$model@optinfo$conv$opt),
    convergence_messages = length(fit$model@optinfo$conv$lme4$messages),
    residual_sd = variance$sdcor[variance$grp == "Residual"],
    participant_intercept_sd = variance$sdcor[variance$grp == "participant_id"],
    log_likelihood = as.numeric(stats::logLik(fit$model)),
    aic = stats::AIC(fit$model),
    bic = stats::BIC(fit$model)
  )
}

## Model Fits ------------------------------------------------------------------

coefficient_results <- list()
diagnostic_results <- list()
sample_results <- list()

for (i in seq_len(nrow(models))) {
  model_info <- models[i, ]
  data_path <- file.path(
    data_sets_dir,
    paste0("analysis_", tolower(model_info$instrument), ".rds")
  )
  input_data <- readRDS(data_path)
  prepared <- prepare_session_data(input_data, model_info$outcome_var)
  fit <- fit_sensitivity_model(prepared$data)
  coefficient_results[[i]] <- extract_coefficients(fit, model_info)
  diagnostic_results[[i]] <- extract_diagnostics(fit, model_info)
  sample_results[[i]] <- bind_cols(
    tibble(
      instrument = model_info$instrument,
      outcome_key = model_info$outcome_key,
      outcome_name = model_info$outcome_name,
      minimum_paired_nights = minimum_sensitivity_nights,
      age_detrending = "equal_session_ols"
    ),
    prepared$counts,
    tibble(
      analysis_participants = n_distinct(fit$data$participant_id),
      analysis_nights = sum(fit$data$n_paired_nights),
      repeated_participants = sum(table(fit$data$participant_id) > 1L),
      duration_mean_age_slope = unname(fit$age_slopes[["duration_mean"]]),
      duration_isd_age_slope = unname(fit$age_slopes[["duration_isd"]]),
      midpoint_mean_age_slope = unname(fit$age_slopes[["midpoint_mean"]]),
      midpoint_isd_age_slope = unname(fit$age_slopes[["midpoint_isd"]])
    )
  )
}

coefficients <- bind_rows(coefficient_results)
diagnostics <- bind_rows(diagnostic_results)
sample_summary <- bind_rows(sample_results)

## Save Outputs ----------------------------------------------------------------

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  coefficients,
  file.path(report_data_dir, "sensitivity_coefficients.rds")
)
write_csv(
  coefficients |>
    transmute(
      Outcome = outcome_name,
      Component = recode(
        component,
        Between = "Between-person",
        Within = "Within-person"
      ),
      Predictor = case_when(
        grepl("^race_nih", term) ~ paste(
          sub("^race_nih", "Race: ", term),
          "vs White"
        ),
        grepl("^ethnicity", term) ~ paste(
          sub("^ethnicity", "Ethnicity: ", term),
          "vs Non-Hispanic"
        ),
        term == "income_3lvl.L" ~ "Income: linear polynomial contrast",
        term == "income_3lvl.Q" ~ "Income: quadratic polynomial contrast",
        term == "income_3lvl.C" ~ "Income: cubic polynomial contrast",
        term == "income_3lvl^4" ~ "Income: quartic polynomial contrast",
        grepl("^income_3lvl", term) ~ paste(
          sub("^income_3lvl", "Income: ", term),
          "vs > $100K"
        ),
        TRUE ~ predictor
      ),
      Estimate = round(estimate, 3),
      SE = round(std_error, 3),
      `95% CI lower` = round(conf_low, 3),
      `95% CI upper` = round(conf_high, 3),
      df = round(df, 1),
      t = round(statistic, 3),
      p = if_else(p_value < .001, "< .001", sprintf("%.3f", p_value))
    ),
  file.path(outputs_dir, "sensitivity_coefficients.csv"),
  na = ""
)
write_csv(
  diagnostics |>
    transmute(
      Outcome = outcome_name,
      Participants = participants,
      Sessions = participant_sessions,
      `Singular fit` = as.logical(singular),
      `Optimizer code` = optimizer_code,
      `Convergence messages (n)` = convergence_messages,
      `Residual SD` = round(residual_sd, 3),
      `Participant intercept SD` = round(participant_intercept_sd, 3),
      `Log likelihood` = round(log_likelihood, 2),
      AIC = round(aic, 2),
      BIC = round(bic, 2)
    ),
  file.path(outputs_dir, "sensitivity_diagnostics.csv"),
  na = ""
)
saveRDS(
  sample_summary,
  file.path(report_data_dir, "sensitivity_sample_summary.rds")
)
write_csv(
  sample_summary |>
    transmute(
      Outcome = outcome_name,
      `Available sessions` = sessions_available,
      `Sessions with paired sleep measures` = sessions_paired_duration_midpoint,
      `Minimum paired nights` = minimum_paired_nights,
      `Sessions meeting night minimum` = sessions_minimum_nights,
      `Sessions with complete outcome and age` = sessions_outcome_age_complete,
      `Analysis sessions` = sessions_analysis_complete,
      `Excluded: too few nights` = excluded_fewer_than_minimum_nights,
      `Excluded: missing outcome or age` = excluded_missing_outcome_or_age,
      `Excluded: missing covariate` = excluded_missing_covariate,
      `Analysis participants` = analysis_participants,
      `Analysis nights` = analysis_nights,
      `Participants with repeated sessions` = repeated_participants,
      `Duration mean age slope (hours/year)` = round(
        duration_mean_age_slope,
        4
      ),
      `Duration iSD age slope (hours/year)` = round(duration_isd_age_slope, 4),
      `Midpoint mean age slope (hours/year)` = round(
        midpoint_mean_age_slope,
        4
      ),
      `Midpoint iSD age slope (hours/year)` = round(midpoint_isd_age_slope, 4)
    ),
  file.path(outputs_dir, "sensitivity_sample_summary.csv"),
  na = ""
)

print(coefficients, n = Inf)
print(diagnostics, n = Inf)
print(sample_summary, n = Inf)
