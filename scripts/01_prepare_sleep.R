# Prepare Daily Sleep ----------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "sleep_helpers.R"))

library(arrow)
library(dplyr)
library(readr)

## Released Records ------------------------------------------------------------

raw_sleep <- read_parquet(sleep_path)

unique_sleep <- raw_sleep |>
  mutate(source_row = row_number()) |>
  distinct(across(-source_row), .keep_all = TRUE)

sleep_release <- unique_sleep |> filter(day != 0)

sleep_release <- derive_release_sleep_values(sleep_release)
sleep_standard <- sleep_release |>
  filter(model_sleep_eligible) |>
  arrange(participant_id, sleep_session_id, dt)

## Sample Counts ---------------------------------------------------------------

count_nights <- function(data, stage) {
  data |>
    summarise(
      stage = stage,
      nights = n(),
      participant_sessions = n_distinct(participant_id, session_id),
      participants = n_distinct(participant_id)
    )
}

sleep_summary <- bind_rows(
  count_nights(raw_sleep, "Downloaded records"),
  count_nights(unique_sleep, "Exact duplicates removed"),
  count_nights(sleep_release, "Day zero removed"),
  count_nights(sleep_standard, "Observed duration and timestamps")
)
print(sleep_summary)

## Save Data -------------------------------------------------------------------

dir.create(data_sets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(sleep_release, file.path(data_sets_dir, "sleep_release.rds"))
saveRDS(sleep_standard, file.path(data_sets_dir, "sleep_standard.rds"))
write_csv(
  sleep_summary |>
    transmute(
      Stage = stage,
      Nights = nights,
      Sessions = participant_sessions,
      Participants = participants
    ),
  file.path(summaries_dir, "sleep_preparation_flow.csv"),
  na = ""
)
