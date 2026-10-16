# Prepare Model Inputs ---------------------------------------------------------

library(here)
source(here("config.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)
source(here("scripts", "model_helpers.R"))

library(dplyr)
library(readr)

## Outcomes --------------------------------------------------------------------

model_input_summary <- vector("list", nrow(outcomes))

for (i in seq_len(nrow(outcomes))) {
  model_info <- outcomes[i, ]
  data_path <- file.path(
    data_sets_dir,
    paste0("analysis_", tolower(model_info$instrument), ".rds")
  )
  prepared <- prepare_detrended_stan_data(model_info$outcome_var, data_path)
  input_dir <- file.path(model_inputs_dir, model_info$model_name)
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(prepared$stan_data, file.path(input_dir, "stan_data.rds"))
  write_csv(
    prepared$metadata,
    file.path(input_dir, "participant_session_metadata.csv")
  )
  saveRDS(
    prepared$nightly_metadata,
    file.path(input_dir, "nightly_metadata.rds")
  )

  model_input_summary[[i]] <- tibble(
    instrument = model_info$instrument,
    outcome = model_info$outcome_name,
    outcome_var = model_info$outcome_var,
    N_obs = prepared$stan_data$N_obs,
    N_participant_sessions = prepared$stan_data$N_ps,
    N_participants = prepared$stan_data$N_participants,
    participants_multiple_sessions = sum(
      table(prepared$metadata$participant_id) > 1L
    ),
    mean_sleep_window_age = prepared$age_mean_ps
  )
}

model_input_summary <- bind_rows(model_input_summary)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  model_input_summary,
  file.path(report_data_dir, "model_input_summary.rds")
)
write_csv(
  model_input_summary |>
    transmute(
      Outcome = outcome,
      Participants = N_participants,
      Sessions = N_participant_sessions,
      Nights = N_obs,
      `Participants with repeated sessions` = participants_multiple_sessions,
      `Mean age at sleep monitoring (years)` = round(mean_sleep_window_age, 2)
    ),
  file.path(summaries_dir, "model_input_summary.csv"),
  na = ""
)
print(model_input_summary, n = Inf)
