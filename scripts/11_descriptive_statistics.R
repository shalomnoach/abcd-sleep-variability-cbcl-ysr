# Descriptive Statistics -------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(readr)
library(tidyr)

## Inputs ----------------------------------------------------------------------

analysis_cbcl <- readRDS(file.path(data_sets_dir, "analysis_cbcl.rds"))
analysis_ysr <- readRDS(file.path(data_sets_dir, "analysis_ysr.rds"))

analysis_data <- bind_rows(analysis_cbcl, analysis_ysr)

## Helpers ---------------------------------------------------------------------

add_session_age <- function(data) {
  data |>
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
    ungroup()
}

prepare_model_sessions <- function(model_info) {
  analysis_data |>
    filter(instrument == model_info$instrument) |>
    add_session_age() |>
    mutate(
      participant_session_id = paste(
        participant_id,
        sleep_session_id,
        sep = "_"
      )
    ) |>
    filter(
      is.finite(sleep_duration_hours),
      is.finite(midpoint_hours),
      is.finite(age_mid_ps),
      is.finite(.data[[model_info$outcome_var]])
    ) |>
    arrange(participant_session_id, dt) |>
    group_by(participant_session_id) |>
    filter(n() >= minimum_nights) |>
    summarise(
      participant_id = first(participant_id),
      sleep_session_id = first(sleep_session_id),
      n_nights = n(),
      mean_duration = mean(sleep_duration_hours),
      sd_duration = if_else(n() >= 2L, sd(sleep_duration_hours), NA_real_),
      mean_midpoint = mean(midpoint_hours),
      sd_midpoint = if_else(n() >= 2L, sd(midpoint_hours), NA_real_),
      score = first(.data[[model_info$outcome_var]]),
      age_mid_ps = first(age_mid_ps),
      sex = first(sex),
      race_nih = first(race_nih),
      ethnicity = first(ethnicity),
      income_3lvl = first(income_3lvl),
      .groups = "drop"
    ) |>
    mutate(
      instrument = model_info$instrument,
      model_name = model_info$model_name,
      outcome = model_info$outcome_name
    ) |>
    select(
      instrument,
      model_name,
      outcome,
      participant_id,
      sleep_session_id,
      participant_session_id,
      n_nights,
      mean_duration,
      sd_duration,
      mean_midpoint,
      sd_midpoint,
      score,
      age_mid_ps,
      sex,
      race_nih,
      ethnicity,
      income_3lvl
    )
}

calculate_correlation <- function(data, variable1, variable2) {
  complete <- data |>
    select(all_of(c(variable1, variable2))) |>
    drop_na()

  if (
    nrow(complete) < 3L ||
      sd(complete[[variable1]]) == 0 ||
      sd(complete[[variable2]]) == 0
  ) {
    return(tibble(
      variable1 = variable1,
      variable2 = variable2,
      n = nrow(complete),
      correlation = NA_real_,
      conf_low_95 = NA_real_,
      conf_high_95 = NA_real_,
      p_value = NA_real_
    ))
  }

  test <- cor.test(
    complete[[variable1]],
    complete[[variable2]],
    method = "pearson"
  )
  tibble(
    variable1 = variable1,
    variable2 = variable2,
    n = nrow(complete),
    correlation = unname(test$estimate),
    conf_low_95 = test$conf.int[[1]],
    conf_high_95 = test$conf.int[[2]],
    p_value = test$p.value
  )
}

## Model-Eligible Sessions -----------------------------------------------------

model_sessions <- bind_rows(lapply(
  seq_len(nrow(outcomes)),
  function(i) prepare_model_sessions(outcomes[i, ])
))

model_counts <- model_sessions |>
  group_by(instrument, model_name, outcome) |>
  summarise(
    nights = sum(n_nights),
    participant_sessions = n(),
    participants = n_distinct(participant_id),
    .groups = "drop"
  )

prepare_total_reference_scores <- function(
  data,
  total_variable,
  score_variables
) {
  add_session_age(data) |>
    mutate(
      participant_session_id = paste(
        participant_id,
        sleep_session_id,
        sep = "_"
      )
    ) |>
    filter(
      is.finite(sleep_duration_hours),
      is.finite(midpoint_hours),
      is.finite(age_mid_ps),
      is.finite(.data[[total_variable]])
    ) |>
    group_by(participant_session_id) |>
    filter(n() >= minimum_nights) |>
    summarise(
      across(all_of(score_variables), first),
      .groups = "drop"
    )
}

total_reference_scores <- bind_rows(
  prepare_total_reference_scores(
    analysis_cbcl,
    "mh_p_cbcl_tscore",
    c(
      "mh_p_cbcl__synd__ext_tscore",
      "mh_p_cbcl__synd__int_tscore",
      "mh_p_cbcl_tscore"
    )
  ) |>
    transmute(
      instrument = "CBCL",
      cbcl_ext = mh_p_cbcl__synd__ext_tscore,
      cbcl_int = mh_p_cbcl__synd__int_tscore,
      cbcl_tot = mh_p_cbcl_tscore
    ),
  prepare_total_reference_scores(
    analysis_ysr,
    "mh_y_ysr_tscore",
    c(
      "mh_y_ysr__synd__ext_tscore",
      "mh_y_ysr__synd__int_tscore",
      "mh_y_ysr_tscore"
    )
  ) |>
    transmute(
      instrument = "YSR",
      ysr_ext = mh_y_ysr__synd__ext_tscore,
      ysr_int = mh_y_ysr__synd__int_tscore,
      ysr_tot = mh_y_ysr_tscore
    )
)

total_reference_counts <- model_counts |>
  filter(grepl("_total_detrended$", model_name)) |>
  mutate(
    model_name = "total_problems_reference",
    outcome = "Total Problems Reference"
  )

## Descriptive Statistics ------------------------------------------------------

descriptive_statistics <- model_sessions |>
  select(
    instrument,
    model_name,
    outcome,
    participant_id,
    n_nights,
    mean_duration,
    sd_duration,
    mean_midpoint,
    sd_midpoint,
    score
  ) |>
  pivot_longer(
    cols = c(
      n_nights,
      mean_duration,
      sd_duration,
      mean_midpoint,
      sd_midpoint,
      score
    ),
    names_to = "measure",
    values_to = "measure_value"
  ) |>
  mutate(
    measure = recode(
      measure,
      n_nights = "nights",
      mean_duration = "mean_duration",
      sd_duration = "sd_duration",
      mean_midpoint = "mean_midpoint",
      sd_midpoint = "sd_midpoint",
      score = recode(
        outcome,
        "CBCL Externalizing Problems" = "cbcl_ext",
        "CBCL Internalizing Problems" = "cbcl_int",
        "CBCL Total Problems" = "cbcl_tot",
        "YSR Externalizing Problems" = "ysr_ext",
        "YSR Internalizing Problems" = "ysr_int",
        "YSR Total Problems" = "ysr_tot"
      )
    )
  ) |>
  group_by(instrument, model_name, outcome, measure) |>
  summarise(
    mean = mean(measure_value, na.rm = TRUE),
    median = median(measure_value, na.rm = TRUE),
    sd = sd(measure_value, na.rm = TRUE),
    min = min(measure_value, na.rm = TRUE),
    max = max(measure_value, na.rm = TRUE),
    n = sum(!is.na(measure_value)),
    .groups = "drop"
  ) |>
  left_join(model_counts, by = c("instrument", "model_name", "outcome"))

total_reference_descriptives <- total_reference_scores |>
  pivot_longer(
    cols = -instrument,
    names_to = "measure",
    values_to = "measure_value"
  ) |>
  filter(!is.na(measure_value)) |>
  group_by(instrument, measure) |>
  summarise(
    mean = mean(measure_value, na.rm = TRUE),
    median = median(measure_value, na.rm = TRUE),
    sd = sd(measure_value, na.rm = TRUE),
    min = min(measure_value, na.rm = TRUE),
    max = max(measure_value, na.rm = TRUE),
    n = sum(!is.na(measure_value)),
    .groups = "drop"
  ) |>
  mutate(
    model_name = "total_problems_reference",
    outcome = "Total Problems Reference"
  ) |>
  left_join(
    total_reference_counts,
    by = c("instrument", "model_name", "outcome")
  )

descriptive_statistics <- bind_rows(
  descriptive_statistics,
  total_reference_descriptives
)

## Demographic Summaries -------------------------------------------------------

total_reference_sessions <- model_sessions |>
  filter(grepl("_total_detrended$", model_name)) |>
  mutate(
    model_name = "total_problems_reference",
    outcome = "Total Problems Reference"
  )

total_reference_people <- total_reference_sessions |>
  arrange(instrument, participant_id, age_mid_ps, sleep_session_id) |>
  group_by(instrument, participant_id) |>
  slice_head(n = 1L) |>
  ungroup() |>
  mutate(unit = "participant")

demographic_cohorts <- bind_rows(
  model_sessions |> mutate(unit = "participant_session"),
  total_reference_sessions |> mutate(unit = "participant_session"),
  total_reference_people
)

demographic_denominators <- demographic_cohorts |>
  group_by(instrument, model_name, outcome, unit) |>
  summarise(
    denominator = n_distinct(participant_session_id),
    participants = n_distinct(participant_id),
    participant_sessions = n_distinct(participant_session_id),
    .groups = "drop"
  )

demographics_long <- demographic_cohorts |>
  mutate(
    across(c(sex, race_nih, ethnicity, income_3lvl), as.character)
  ) |>
  pivot_longer(
    cols = c(sex, race_nih, ethnicity, income_3lvl),
    names_to = "variable",
    values_to = "level"
  ) |>
  mutate(level = coalesce(level, "Missing")) |>
  group_by(instrument, model_name, outcome, unit, variable, level) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  left_join(
    demographic_denominators,
    by = c("instrument", "model_name", "outcome", "unit")
  ) |>
  mutate(proportion = n / denominator, mean = NA_real_, sd = NA_real_)

age_summary <- demographic_cohorts |>
  group_by(instrument, model_name, outcome, unit) |>
  summarise(
    n = sum(!is.na(age_mid_ps)),
    denominator = n_distinct(participant_session_id),
    proportion = NA_real_,
    participants = n_distinct(participant_id),
    participant_sessions = n_distinct(participant_session_id),
    mean = mean(age_mid_ps, na.rm = TRUE),
    sd = sd(age_mid_ps, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(variable = "age_mid_ps", level = NA_character_) |>
  select(names(demographics_long))

demographics_summary <- bind_rows(demographics_long, age_summary) |>
  mutate(
    table_row = case_when(
      variable == "age_mid_ps" ~ "Age (years)",
      variable == "sex" ~ paste0("Sex: ", level),
      variable == "race_nih" ~ paste0("Race: ", level),
      variable == "ethnicity" ~ paste0("Ethnicity: ", level),
      variable == "income_3lvl" ~ paste0("Family income: ", level)
    ),
    row_type = if_else(variable == "age_mid_ps", "continuous", "category"),
    display = if_else(
      variable == "age_mid_ps",
      sprintf("%.2f (%.2f)", mean, sd),
      sprintf("%d (%.1f%%)", n, 100 * proportion)
    )
  )

## Bivariate Correlations ------------------------------------------------------

correlation_nights <- analysis_data |>
  filter(is.finite(sleep_duration_hours), is.finite(midpoint_hours)) |>
  distinct(participant_id, sleep_session_id, dt, .keep_all = TRUE) |>
  group_by(participant_id) |>
  summarise(
    n_nights = n(),
    sleep_duration_mean = mean(sleep_duration_hours),
    sleep_duration_isd = if_else(n() >= 2L, sd(sleep_duration_hours), NA_real_),
    midpoint_mean = mean(midpoint_hours),
    midpoint_isd = if_else(n() >= 2L, sd(midpoint_hours), NA_real_),
    .groups = "drop"
  )

correlation_age <- add_session_age(analysis_data) |>
  distinct(participant_id, sleep_session_id, .keep_all = TRUE) |>
  group_by(participant_id) |>
  summarise(age = mean(age_mid_ps), .groups = "drop")

cbcl_person_outcomes <- analysis_cbcl |>
  distinct(participant_id, assessment_session_id, .keep_all = TRUE) |>
  group_by(participant_id) |>
  summarise(
    cbcl_total = mean(mh_p_cbcl_tscore, na.rm = TRUE),
    cbcl_internalizing = mean(mh_p_cbcl__synd__int_tscore, na.rm = TRUE),
    cbcl_externalizing = mean(mh_p_cbcl__synd__ext_tscore, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(-participant_id, ~ if_else(is.nan(.x), NA_real_, .x)))

ysr_person_outcomes <- analysis_ysr |>
  distinct(participant_id, assessment_session_id, .keep_all = TRUE) |>
  group_by(participant_id) |>
  summarise(
    ysr_total = mean(mh_y_ysr_tscore, na.rm = TRUE),
    ysr_internalizing = mean(mh_y_ysr__synd__int_tscore, na.rm = TRUE),
    ysr_externalizing = mean(mh_y_ysr__synd__ext_tscore, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(-participant_id, ~ if_else(is.nan(.x), NA_real_, .x)))

correlation_people <- correlation_nights |>
  left_join(correlation_age, by = "participant_id") |>
  left_join(cbcl_person_outcomes, by = "participant_id") |>
  left_join(ysr_person_outcomes, by = "participant_id")

correlation_variables <- c(
  "sleep_duration_mean",
  "sleep_duration_isd",
  "midpoint_mean",
  "midpoint_isd",
  "cbcl_total",
  "cbcl_internalizing",
  "cbcl_externalizing",
  "ysr_total",
  "ysr_internalizing",
  "ysr_externalizing",
  "age"
)

correlations <- combn(correlation_variables, 2L, simplify = FALSE) |>
  lapply(function(pair) {
    calculate_correlation(correlation_people, pair[[1]], pair[[2]])
  }) |>
  bind_rows()

## Save Results ----------------------------------------------------------------

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  demographics_summary,
  file.path(report_data_dir, "demographics_summary.rds")
)
saveRDS(
  descriptive_statistics,
  file.path(report_data_dir, "descriptive_statistics.rds")
)
saveRDS(correlation_people, file.path(data_sets_dir, "correlation_people.rds"))
saveRDS(correlations, file.path(report_data_dir, "correlations.rds"))

write_csv(
  demographics_summary |>
    filter(model_name == "total_problems_reference", unit == "participant") |>
    arrange(match(
      variable,
      c("age_mid_ps", "sex", "race_nih", "ethnicity", "income_3lvl")
    )) |>
    mutate(
      instrument = paste0(instrument, " (N = ", participants, ")"),
      table_row = if_else(
        variable == "age_mid_ps",
        "Age (years), M (SD)",
        paste0(table_row, ", n (%)")
      )
    ) |>
    select(Characteristic = table_row, instrument, display) |>
    pivot_wider(names_from = instrument, values_from = display),
  file.path(outputs_dir, "demographics_summary.csv"),
  na = ""
)
write_csv(
  demographics_summary |>
    transmute(
      Instrument = instrument,
      Sample = outcome,
      Unit = recode(
        unit,
        participant = "Participants",
        participant_session = "Sessions"
      ),
      Characteristic = table_row,
      N = n,
      Denominator = denominator,
      Percent = round(100 * proportion, 2),
      Mean = round(mean, 2),
      SD = round(sd, 2)
    ),
  file.path(outputs_dir, "demographics_by_sample.csv"),
  na = ""
)
write_csv(
  descriptive_statistics |>
    transmute(
      Instrument = instrument,
      Sample = outcome,
      Measure = unname(measure_labels[measure]),
      `Available sessions` = n,
      Mean = round(mean, 2),
      SD = round(sd, 2),
      Median = round(median, 2),
      Minimum = round(min, 2),
      Maximum = round(max, 2)
    ),
  file.path(outputs_dir, "descriptive_statistics.csv"),
  na = ""
)
write_csv(
  correlations |>
    transmute(
      `First measure` = unname(measure_labels[variable1]),
      `Second measure` = unname(measure_labels[variable2]),
      N = n,
      r = round(correlation, 3),
      `95% CI lower` = round(conf_low_95, 3),
      `95% CI upper` = round(conf_high_95, 3),
      p = if_else(p_value < .001, "< .001", sprintf("%.3f", p_value))
    ),
  file.path(outputs_dir, "correlations.csv"),
  na = ""
)

print(model_counts, n = Inf, width = Inf)
print(descriptive_statistics, n = Inf, width = Inf)
print(demographics_summary, n = Inf, width = Inf)
print(correlations, n = Inf, width = Inf)
