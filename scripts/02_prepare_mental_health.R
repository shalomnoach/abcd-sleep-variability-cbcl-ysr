# Prepare Mental Health --------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))

library(arrow)
library(dplyr)
library(readr)

## Phenotype Inputs ------------------------------------------------------------

cbcl <- read_parquet(
  cbcl_path,
  col_select = c(
    "participant_id",
    "session_id",
    "mh_p_cbcl_dtt",
    "mh_p_cbcl_age",
    "mh_p_cbcl__synd__ext_tscore",
    "mh_p_cbcl__synd__int_tscore",
    "mh_p_cbcl_tscore"
  )
) |>
  mutate(across(c(participant_id, session_id), as.character))

ysr <- read_parquet(
  ysr_path,
  col_select = c(
    "participant_id",
    "session_id",
    "mh_y_ysr_dtt",
    "mh_y_ysr_age",
    "mh_y_ysr__synd__ext_tscore",
    "mh_y_ysr__synd__int_tscore",
    "mh_y_ysr_tscore"
  )
) |>
  mutate(across(c(participant_id, session_id), as.character))

income <- read_parquet(
  income_path,
  col_select = c(
    "participant_id",
    "session_id",
    "ab_g_dyn__visit_age",
    "ab_g_dyn__cohort_income__hhold__3lvl"
  )
) |>
  mutate(across(c(participant_id, session_id), as.character))

mental_health <- cbcl |>
  full_join(
    ysr,
    by = c("participant_id", "session_id"),
    relationship = "one-to-one"
  ) |>
  full_join(
    income,
    by = c("participant_id", "session_id"),
    relationship = "one-to-one"
  ) |>
  transmute(
    participant_id,
    assessment_session_id = session_id,
    mh_p_cbcl_dtt = parse_abcd_datetime(mh_p_cbcl_dtt),
    mh_p_cbcl_age = as.numeric(mh_p_cbcl_age),
    mh_p_cbcl__synd__ext_tscore = as.numeric(mh_p_cbcl__synd__ext_tscore),
    mh_p_cbcl__synd__int_tscore = as.numeric(mh_p_cbcl__synd__int_tscore),
    mh_p_cbcl_tscore = as.numeric(mh_p_cbcl_tscore),
    mh_y_ysr_dtt = parse_abcd_datetime(mh_y_ysr_dtt),
    mh_y_ysr_age = as.numeric(mh_y_ysr_age),
    mh_y_ysr__synd__ext_tscore = as.numeric(mh_y_ysr__synd__ext_tscore),
    mh_y_ysr__synd__int_tscore = as.numeric(mh_y_ysr__synd__int_tscore),
    mh_y_ysr_tscore = as.numeric(mh_y_ysr_tscore),
    visit_age = as.numeric(ab_g_dyn__visit_age),
    income_3lvl = recode_income_3lvl(ab_g_dyn__cohort_income__hhold__3lvl)
  ) |>
  arrange(participant_id, assessment_session_id)

outcome_summary <- mental_health |>
  select(participant_id, all_of(outcomes$outcome_var)) |>
  tidyr::pivot_longer(
    -participant_id,
    names_to = "outcome_var",
    values_to = "score"
  ) |>
  left_join(
    outcomes |> select(outcome_var, outcome_name),
    by = "outcome_var"
  ) |>
  group_by(outcome_name) |>
  summarise(
    `Available assessments` = sum(!is.na(score)),
    Participants = n_distinct(participant_id[!is.na(score)]),
    .groups = "drop"
  ) |>
  rename(Outcome = outcome_name)
print(outcome_summary, width = Inf)

## Save Data -------------------------------------------------------------------

dir.create(data_sets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(mental_health, file.path(data_sets_dir, "mental_health.rds"))
write_csv(
  outcome_summary,
  file.path(summaries_dir, "mental_health_preparation_summary.csv"),
  na = ""
)
