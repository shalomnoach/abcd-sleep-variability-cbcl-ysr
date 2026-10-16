# Prepare Demographics ---------------------------------------------------------

library(here)
source(here("config.R"))

library(arrow)
library(dplyr)
library(readr)

demographics <- read_parquet(
  demographics_path,
  col_select = c(
    "participant_id",
    "ab_g_stc__cohort_dob",
    "ab_g_stc__cohort_sex",
    "ab_g_stc__cohort_race__nih",
    "ab_g_stc__cohort_ethn"
  )
) |>
  transmute(
    participant_id = as.character(participant_id),
    dob = as.Date(ab_g_stc__cohort_dob),
    sex = factor(
      as.character(ab_g_stc__cohort_sex),
      levels = c("1", "2"),
      labels = c("Male", "Female")
    ),
    race_nih = factor(
      as.character(ab_g_stc__cohort_race__nih),
      levels = c("2", "3", "4", "5", "6", "8", "13"),
      labels = c(
        "White",
        "Black (African American)",
        "Asian",
        "American Indian/Alaska Native",
        "Native Hawaiian or Other Pacific Islander",
        "More than One Race",
        "Other (Unknown or not reported)"
      )
    ),
    ethnicity = factor(
      as.character(ab_g_stc__cohort_ethn),
      levels = c("1", "2"),
      labels = c("Hispanic", "Non-Hispanic")
    )
  ) |>
  arrange(participant_id)

demographics_summary <- demographics |>
  summarise(
    Participants = n(),
    `Missing birth date (n)` = sum(is.na(dob)),
    `Missing sex (n)` = sum(is.na(sex)),
    `Missing race (n)` = sum(is.na(race_nih)),
    `Missing ethnicity (n)` = sum(is.na(ethnicity))
  )
print(demographics_summary)

## Save Data -------------------------------------------------------------------

dir.create(data_sets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(demographics, file.path(data_sets_dir, "demographics.rds"))
write_csv(
  demographics_summary,
  file.path(summaries_dir, "demographics_preparation_summary.csv"),
  na = ""
)
