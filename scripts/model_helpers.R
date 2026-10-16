# Model Inputs And Initial Values ----------------------------------------------

library(dplyr)
library(tidyr)

## Model Inputs ----------------------------------------------------------------

category_probabilities <- function(code, levels) {
  observed <- tibble(code = code) |>
    filter(code > 0L) |>
    count(code) |>
    complete(code = seq_len(levels), fill = list(n = 0L)) |>
    arrange(code)
  observed$n / sum(observed$n)
}

prepare_detrended_stan_data <- function(outcome_var, data_path) {
  data_analysis <- readRDS(data_path) |>
    mutate(
      participant_session_id = paste(
        participant_id,
        sleep_session_id,
        sep = "_"
      )
    ) |>
    group_by(participant_id, sleep_session_id) |>
    mutate(
      age_mid_ps = as.numeric(difftime(
        min(dt, na.rm = TRUE) +
          (max(dt, na.rm = TRUE) - min(dt, na.rm = TRUE)) / 2,
        dob,
        units = "days"
      )) /
        365.25
    ) |>
    ungroup() |>
    filter(
      is.finite(sleep_duration_hours),
      is.finite(midpoint_hours),
      is.finite(age_mid_ps),
      is.finite(.data[[outcome_var]])
    ) |>
    arrange(participant_session_id, dt) |>
    group_by(participant_session_id) |>
    filter(n() >= minimum_nights) |>
    mutate(day_index = day, n_nights = n()) |>
    ungroup()

  ps_data <- data_analysis |>
    group_by(participant_session_id) |>
    slice(1L) |>
    ungroup() |>
    arrange(participant_session_id) |>
    mutate(
      race_nih = as.character(race_nih),
      ethnicity = as.character(ethnicity),
      income_3lvl = as.character(income_3lvl)
    )

  age_mean_ps <- mean(ps_data$age_mid_ps)
  ps_data <- ps_data |>
    mutate(
      age_time_c = age_mid_ps - age_mean_ps,
      race_code = case_when(
        race_nih == "White" ~ 1L,
        race_nih == "Black (African American)" ~ 2L,
        race_nih == "Asian" ~ 3L,
        race_nih == "More than One Race" ~ 4L,
        race_nih %in%
          c(
            "American Indian/Alaska Native",
            "Native Hawaiian or Other Pacific Islander",
            "Other (Unknown or not reported)"
          ) ~ 5L,
        TRUE ~ 0L
      ),
      ethnicity_code = case_when(
        ethnicity == "Non-Hispanic" ~ 1L,
        ethnicity == "Hispanic" ~ 2L,
        TRUE ~ 0L
      ),
      income_code = case_when(
        income_3lvl == "> $100K" ~ 1L,
        income_3lvl == "$50K-$100K" ~ 2L,
        income_3lvl == "< $50K" ~ 3L,
        income_3lvl == "Don't know" ~ 4L,
        income_3lvl == "Decline to answer" ~ 5L,
        TRUE ~ 0L
      )
    )

  data_analysis <- data_analysis |>
    mutate(
      ps_id = match(participant_session_id, ps_data$participant_session_id)
    )

  participant_lookup <- ps_data |>
    distinct(participant_id) |>
    arrange(participant_id) |>
    mutate(participant_idx = row_number())
  ps_data <- ps_data |>
    left_join(
      participant_lookup,
      by = "participant_id",
      relationship = "many-to-one"
    )

  stan_data <- list(
    N_obs = nrow(data_analysis),
    N_ps = nrow(ps_data),
    N_participants = nrow(participant_lookup),
    ps_id = as.integer(data_analysis$ps_id),
    sleep_duration = data_analysis$sleep_duration_hours,
    sleep_midpoint = data_analysis$midpoint_hours,
    outcome = ps_data[[outcome_var]],
    participant_id_ps = as.integer(ps_data$participant_idx),
    age_time_c = ps_data$age_time_c,
    K_race = 5L,
    race_code = as.integer(ps_data$race_code),
    pi_race = category_probabilities(
      ps_data$race_code[!duplicated(ps_data$participant_idx)],
      5L
    ),
    K_ethnicity = 2L,
    ethnicity_code = as.integer(ps_data$ethnicity_code),
    pi_ethnicity = category_probabilities(
      ps_data$ethnicity_code[!duplicated(ps_data$participant_idx)],
      2L
    ),
    K_income = 5L,
    income_code = as.integer(ps_data$income_code),
    pi_income = category_probabilities(ps_data$income_code, 5L)
  )

  metadata <- ps_data |>
    transmute(
      participant_id,
      participant_session_id,
      sleep_session_id,
      assessment_session_id,
      assessment_dtt,
      assessment_sleep_lag_days,
      match_rule,
      age_mid_ps,
      age_time_c,
      outcome = .data[[outcome_var]],
      n_nights,
      race_code,
      ethnicity_code,
      income_code,
      income_source_session,
      income_source_rule
    )

  list(
    stan_data = stan_data,
    metadata = metadata,
    age_mean_ps = age_mean_ps,
    nightly_metadata = data_analysis |>
      select(
        source_row,
        participant_id,
        sleep_session_id,
        participant_session_id,
        dt,
        day,
        day_index,
        ps_id,
        sleep_duration_hours,
        midpoint_clock_hours,
        midpoint_noon_hours,
        midpoint_hours,
        midpoint_session_shift_days,
        midpoint_person_shift_days,
        midpoint_shift_days,
        midpoint_session_ambiguous,
        midpoint_person_ambiguous
      ),
    outcome_var = outcome_var,
    data_path = data_path
  )
}

## Initial Values --------------------------------------------------------------

model_initial_values <- function(stan_data) {
  list(
    gamma_sleep_mean = c(
      rnorm(1, 8, 0.5),
      rnorm(1, 0, 0.2),
      rnorm(1, 3, 0.5),
      rnorm(1, 0, 0.2)
    ),
    gamma_sleep_age = rnorm(4, 0, 0.1),
    L_Omega = diag(4),
    tau = abs(rnorm(4, 0.8, 0.2)),
    beta0 = rnorm(1, 50, 5),
    beta_sleep_between = rnorm(4, 0, 1),
    beta_sleep_within = rnorm(4, 0, 1),
    beta_race = rnorm(stan_data$K_race - 1L, 0, 1),
    beta_ethnicity = rnorm(stan_data$K_ethnicity - 1L, 0, 1),
    beta_income = rnorm(stan_data$K_income - 1L, 0, 1),
    u_participant_raw = rnorm(stan_data$N_participants, 0, 0.1),
    sigma_participant = abs(rnorm(1, 1, 0.25)),
    sigma_outcome = abs(rnorm(1, 10, 2)),
    eta_raw = lapply(seq_len(stan_data$N_ps), function(i) rnorm(4, 0, 0.1))
  )
}
