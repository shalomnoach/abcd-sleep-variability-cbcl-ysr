# Correlation Table ------------------------------------------------------------

library(here)
source(here("config.R"))
source(here("scripts", "helpers.R"))
dir.create(report_data_dir, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(readr)
library(tidyr)

## Inputs ----------------------------------------------------------------------

correlation_people <- readRDS(file.path(data_sets_dir, "correlation_people.rds"))

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

## Descriptive Statistics ------------------------------------------------------

correlation_descriptives <- correlation_people |>
  select(all_of(correlation_variables)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(variable, correlation_variables))

empirical_sleep_variability_percentiles <- tibble(
  probability = c(.10, .25, .75, .90),
  percentile = c("10%", "25%", "75%", "90%"),
  isd_hours = as.numeric(quantile(
    correlation_people$sleep_duration_isd,
    probs = c(.10, .25, .75, .90),
    na.rm = TRUE,
    type = 7,
    names = FALSE
  )),
  n = sum(is.finite(correlation_people$sleep_duration_isd)),
  basis = "Empirical person-level iSD across pooled unique eligible nights"
)

## Save Results ----------------------------------------------------------------

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  correlation_descriptives,
  file.path(report_data_dir, "correlation_descriptives.rds")
)
write_csv(
  correlation_descriptives |>
    transmute(
      Variable = unname(measure_labels[variable]),
      N = n,
      Mean = round(mean, 2),
      SD = round(sd, 2)
    ),
  file.path(outputs_dir, "correlation_descriptives.csv"),
  na = ""
)
saveRDS(
  empirical_sleep_variability_percentiles,
  file.path(report_data_dir, "empirical_sleep_variability_percentiles.rds")
)
write_csv(
  empirical_sleep_variability_percentiles |>
    transmute(
      Percentile = percentile,
      `Sleep duration iSD (hours)` = round(isd_hours, 3),
      Participants = n
    ),
  file.path(outputs_dir, "empirical_sleep_variability_percentiles.csv"),
  na = ""
)

print(correlation_descriptives, n = Inf, width = Inf)
print(empirical_sleep_variability_percentiles, n = Inf, width = Inf)
