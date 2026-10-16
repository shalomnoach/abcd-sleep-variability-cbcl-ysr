# Sample Summary ---------------------------------------------------------------

library(here)
source(here("config.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(readr)
library(tidyr)

## Analysis Data ---------------------------------------------------------------

analysis_data <- bind_rows(
  readRDS(file.path(data_sets_dir, "analysis_cbcl.rds")),
  readRDS(file.path(data_sets_dir, "analysis_ysr.rds"))
)

sessions <- analysis_data |>
  distinct(instrument, participant_id, sleep_session_id, .keep_all = TRUE)

## Cohort And Sessions ---------------------------------------------------------

cohort_summary <- analysis_data |>
  group_by(instrument, participant_id, participant_session_id) |>
  summarise(
    nights = n(),
    flagged_nights = sum(flg_slp %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(instrument) |>
  summarise(
    total_nights = sum(nights),
    participant_sessions = n(),
    participants = n_distinct(participant_id),
    participants_multiple_sessions = sum(table(participant_id) > 1L),
    median_nights_per_session = median(.data$nights),
    flagged_nights = sum(flagged_nights),
    .groups = "drop"
  ) |>
  select(
    instrument,
    nights = total_nights,
    participant_sessions,
    participants,
    participants_multiple_sessions,
    median_nights_per_session,
    flagged_nights
  )
print(cohort_summary, width = Inf)

session_summary <- sessions |>
  count(
    instrument,
    sleep_session_id,
    assessment_session_id,
    name = "participant_sessions"
  )
print(session_summary, n = Inf)

## Outcome Eligibility ---------------------------------------------------------

outcome_data <- sessions |>
  select(
    instrument,
    participant_id,
    sleep_session_id,
    all_of(outcomes$outcome_var),
    dob,
    race_nih,
    ethnicity,
    income_3lvl
  ) |>
  pivot_longer(
    all_of(outcomes$outcome_var),
    names_to = "outcome_var",
    values_to = "score"
  ) |>
  filter(!is.na(score)) |>
  inner_join(
    outcomes |> select(instrument, outcome_var, outcome = outcome_name),
    by = c("instrument", "outcome_var"),
    relationship = "many-to-one"
  )

outcome_counts <- outcome_data |>
  group_by(instrument, outcome, outcome_var) |>
  summarise(
    participant_sessions = n(),
    participants = n_distinct(participant_id),
    participants_multiple_sessions = sum(table(participant_id) > 1L),
    .groups = "drop"
  )
print(outcome_counts, n = Inf, width = Inf)

## Covariate Missingness -------------------------------------------------------

missingness <- outcome_data |>
  group_by(instrument, outcome_var) |>
  summarise(
    across(c(dob, race_nih, ethnicity, income_3lvl), ~ sum(is.na(.x))),
    participant_sessions = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    c(dob, race_nih, ethnicity, income_3lvl),
    names_to = "variable",
    values_to = "missing"
  ) |>
  select(instrument, outcome_var, variable, missing, participant_sessions)
print(missingness, n = Inf)

## Save Summaries --------------------------------------------------------------

dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(cohort_summary, file.path(report_data_dir, "cohort_summary.rds"))
write_csv(
  cohort_summary |>
    transmute(
      Instrument = instrument,
      Participants = participants,
      `Participants with repeated sessions` = participants_multiple_sessions,
      Sessions = participant_sessions,
      Nights = nights,
      `Median nights per session` = median_nights_per_session,
      `Flagged nights` = flagged_nights
    ),
  file.path(summaries_dir, "cohort_summary.csv"),
  na = ""
)
saveRDS(outcome_counts, file.path(report_data_dir, "outcome_eligibility.rds"))
write_csv(
  outcome_counts |>
    transmute(
      Outcome = outcome,
      Participants = participants,
      Sessions = participant_sessions,
      `Participants with repeated sessions` = participants_multiple_sessions
    ),
  file.path(summaries_dir, "outcome_eligibility.csv"),
  na = ""
)
saveRDS(session_summary, file.path(report_data_dir, "session_summary.rds"))
write_csv(
  session_summary |>
    transmute(
      Instrument = instrument,
      `Sleep visit` = sub("ses-", "", sleep_session_id),
      `Assessment visit` = sub("ses-", "", assessment_session_id),
      Sessions = participant_sessions
    ),
  file.path(summaries_dir, "session_summary.csv"),
  na = ""
)
saveRDS(missingness, file.path(report_data_dir, "covariate_missingness.rds"))
write_csv(
  missingness |>
    left_join(
      outcomes |> select(outcome_var, outcome_name),
      by = "outcome_var"
    ) |>
    transmute(
      Outcome = outcome_name,
      Sessions = participant_sessions,
      Covariate = recode(
        variable,
        dob = "Birth date",
        race_nih = "Race",
        ethnicity = "Ethnicity",
        income_3lvl = "Income"
      ),
      `Missing (n)` = missing,
      `Missing (%)` = round(100 * missing / participant_sessions, 2)
    ),
  file.path(summaries_dir, "covariate_missingness.csv"),
  na = ""
)
