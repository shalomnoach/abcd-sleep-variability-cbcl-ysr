# Build Analysis Data ----------------------------------------------------------

library(here)
source(here("config.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(readr)

## Prepared Inputs -------------------------------------------------------------

sleep <- readRDS(file.path(data_sets_dir, "sleep_standard.rds"))
mental <- readRDS(file.path(data_sets_dir, "mental_health.rds"))
demographics <- readRDS(file.path(data_sets_dir, "demographics.rds"))

## Sleep Windows And Income ----------------------------------------------------

sleep_windows <- sleep |>
  group_by(participant_id, sleep_session_id) |>
  summarise(
    n_nights = n(),
    sleep_window_start_dt = min(dt),
    sleep_window_end_dt = max(dt),
    sleep_window_mid_dt = min(dt) + (max(dt) - min(dt)) / 2,
    .groups = "drop"
  )

income_available <- mental |>
  filter(!is.na(income_3lvl)) |>
  transmute(
    participant_id,
    income_source_session = assessment_session_id,
    income_3lvl,
    income_wave = as.integer(sub(
      "^ses-([0-9]+).*$",
      "\\1",
      assessment_session_id
    ))
  )

income_lookup <- sleep_windows |>
  select(participant_id, sleep_session_id) |>
  mutate(
    sleep_wave = as.integer(sub("^ses-([0-9]+).*$", "\\1", sleep_session_id))
  ) |>
  left_join(
    income_available,
    by = "participant_id",
    relationship = "many-to-many"
  ) |>
  mutate(
    exact = income_source_session == sleep_session_id,
    wave_distance = abs(income_wave - sleep_wave)
  ) |>
  arrange(
    participant_id,
    sleep_session_id,
    desc(exact),
    wave_distance,
    income_wave,
    income_source_session
  ) |>
  group_by(participant_id, sleep_session_id) |>
  slice_head(n = 1L) |>
  ungroup() |>
  mutate(
    income_source_rule = case_when(
      is.na(income_3lvl) ~ "genuinely_missing",
      exact ~ "same_sleep_session",
      TRUE ~ "nearest_available_wave"
    )
  ) |>
  select(
    participant_id,
    sleep_session_id,
    income_3lvl,
    income_source_session,
    income_source_rule
  )

## Assessment Linkage ----------------------------------------------------------

### CBCL -----------------------------------------------------------------------

cbcl_assessments <- mental |>
  filter(
    assessment_session_id %in% unique(sleep$sleep_session_id),
    if_any(starts_with("mh_p_cbcl") & ends_with("tscore"), ~ !is.na(.x))
  ) |>
  transmute(
    participant_id,
    sleep_session_id = assessment_session_id,
    assessment_session_id,
    assessment_dtt = mh_p_cbcl_dtt,
    assessment_date = as.Date(mh_p_cbcl_dtt),
    assessment_age = mh_p_cbcl_age,
    visit_age,
    mh_p_cbcl__synd__ext_tscore,
    mh_p_cbcl__synd__int_tscore,
    mh_p_cbcl_tscore,
    match_rule = "exact_participant_session"
  ) |>
  inner_join(
    sleep_windows,
    by = c("participant_id", "sleep_session_id"),
    relationship = "one-to-one"
  ) |>
  mutate(
    assessment_sleep_lag_days = as.numeric(
      assessment_date - sleep_window_mid_dt
    )
  )

### YSR ------------------------------------------------------------------------

ysr_assessments <- mental |>
  filter(
    assessment_session_id %in% c("ses-04M", "ses-06A"),
    if_any(starts_with("mh_y_ysr") & ends_with("tscore"), ~ !is.na(.x))
  ) |>
  mutate(
    sleep_session_id = case_when(
      assessment_session_id == "ses-04M" ~ "ses-04A",
      assessment_session_id == "ses-06A" ~ "ses-06A",
      TRUE ~ NA_character_
    )
  ) |>
  transmute(
    participant_id,
    sleep_session_id,
    assessment_session_id,
    assessment_dtt = mh_y_ysr_dtt,
    assessment_date = as.Date(mh_y_ysr_dtt),
    assessment_age = mh_y_ysr_age,
    visit_age,
    mh_y_ysr__synd__ext_tscore,
    mh_y_ysr__synd__int_tscore,
    mh_y_ysr_tscore,
    match_rule = case_when(
      assessment_session_id == "ses-04M" ~ "ysr_04M_to_sleep_04A",
      assessment_session_id == "ses-06A" ~ "exact_06A",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(assessment_date)) |>
  inner_join(
    sleep_windows,
    by = c("participant_id", "sleep_session_id"),
    relationship = "one-to-one"
  ) |>
  mutate(
    assessment_sleep_lag_days = as.numeric(
      assessment_date - sleep_window_mid_dt
    )
  ) |>
  filter(abs(assessment_sleep_lag_days) <= maximum_ysr_linkage_days)

## Nightly Analysis Data -------------------------------------------------------

build_nightly_data <- function(assessments, instrument) {
  sleep |>
    inner_join(
      assessments,
      by = c("participant_id", "sleep_session_id"),
      relationship = "many-to-one"
    ) |>
    left_join(
      income_lookup,
      by = c("participant_id", "sleep_session_id"),
      relationship = "many-to-one"
    ) |>
    left_join(
      demographics,
      by = "participant_id",
      relationship = "many-to-one"
    ) |>
    mutate(
      instrument = instrument,
      participant_session_id = paste(
        participant_id,
        sleep_session_id,
        sep = "_"
      )
    ) |>
    arrange(participant_id, sleep_session_id, dt)
}

analysis_cbcl <- build_nightly_data(cbcl_assessments, "CBCL")
analysis_ysr <- build_nightly_data(ysr_assessments, "YSR")

## Sample Summaries ------------------------------------------------------------

income_summary <- bind_rows(analysis_cbcl, analysis_ysr) |>
  distinct(participant_id, sleep_session_id, instrument, income_source_rule) |>
  count(instrument, income_source_rule, name = "participant_sessions")
print(income_summary)

linkage_summary <- bind_rows(
  cbcl_assessments |> mutate(instrument = "CBCL"),
  ysr_assessments |> mutate(instrument = "YSR")
) |>
  group_by(instrument, sleep_session_id, assessment_session_id, match_rule) |>
  summarise(
    participant_sessions = n(),
    participants = n_distinct(participant_id),
    median_lag_days = median(assessment_sleep_lag_days, na.rm = TRUE),
    p05_lag_days = quantile(assessment_sleep_lag_days, 0.05, na.rm = TRUE),
    p95_lag_days = quantile(assessment_sleep_lag_days, 0.95, na.rm = TRUE),
    maximum_allowed_abs_lag_days = if_else(
      first(match_rule) == "ysr_04M_to_sleep_04A",
      as.numeric(maximum_ysr_linkage_days),
      NA_real_
    ),
    mean_abs_lag_days = mean(abs(assessment_sleep_lag_days), na.rm = TRUE),
    median_abs_lag_days = median(abs(assessment_sleep_lag_days), na.rm = TRUE),
    sd_abs_lag_days = sd(abs(assessment_sleep_lag_days), na.rm = TRUE),
    .groups = "drop"
  )
print(linkage_summary, n = Inf, width = Inf)

## Save Data -------------------------------------------------------------------

dir.create(data_sets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(analysis_cbcl, file.path(data_sets_dir, "analysis_cbcl.rds"))
saveRDS(analysis_ysr, file.path(data_sets_dir, "analysis_ysr.rds"))
saveRDS(cbcl_assessments, file.path(data_sets_dir, "linkage_cbcl.rds"))
saveRDS(ysr_assessments, file.path(data_sets_dir, "linkage_ysr.rds"))
saveRDS(income_summary, file.path(report_data_dir, "income_source_summary.rds"))
write_csv(
  income_summary |>
    transmute(
      Instrument = instrument,
      `Income source` = recode(
        income_source_rule,
        same_sleep_session = "Same sleep session",
        nearest_available_wave = "Nearest available wave",
        genuinely_missing = "Missing"
      ),
      Sessions = participant_sessions
    ),
  file.path(summaries_dir, "income_source_summary.csv"),
  na = ""
)
saveRDS(
  linkage_summary,
  file.path(report_data_dir, "assessment_sleep_linkage_summary.rds")
)
write_csv(
  linkage_summary |>
    transmute(
      Instrument = instrument,
      `Sleep visit` = sub("ses-", "", sleep_session_id),
      `Assessment visit` = sub("ses-", "", assessment_session_id),
      Sessions = participant_sessions,
      Participants = participants,
      `Median lag (days)` = round(median_lag_days, 2),
      `5th percentile lag (days)` = round(p05_lag_days, 2),
      `95th percentile lag (days)` = round(p95_lag_days, 2),
      `Mean absolute lag (days)` = round(mean_abs_lag_days, 2),
      `SD absolute lag (days)` = round(sd_abs_lag_days, 2),
      `Median absolute lag (days)` = round(median_abs_lag_days, 2)
    ),
  file.path(summaries_dir, "assessment_sleep_linkage_summary.csv"),
  na = ""
)
