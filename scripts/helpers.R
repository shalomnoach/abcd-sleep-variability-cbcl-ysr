# Shared Helpers ---------------------------------------------------------------

parse_abcd_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd"),
    tz = "UTC",
    quiet = TRUE
  ))
}

recode_income_3lvl <- function(x) {
  factor(
    as.character(x),
    levels = c("1", "2", "3", "777", "999"),
    labels = c(
      "< $50K",
      "$50K-$100K",
      "> $100K",
      "Decline to answer",
      "Don't know"
    ),
    ordered = TRUE
  )
}

## Descriptive Table Labels ----------------------------------------------------

measure_labels <- c(
  nights = "Nights per session",
  mean_duration = "Session mean sleep duration (hours)",
  sd_duration = "Session sleep duration iSD (hours)",
  mean_midpoint = "Session mean sleep midpoint (hours)",
  sd_midpoint = "Session sleep midpoint iSD (hours)",
  cbcl_ext = "CBCL Externalizing Problems (T-score)",
  cbcl_int = "CBCL Internalizing Problems (T-score)",
  cbcl_tot = "CBCL Total Problems (T-score)",
  ysr_ext = "YSR Externalizing Problems (T-score)",
  ysr_int = "YSR Internalizing Problems (T-score)",
  ysr_tot = "YSR Total Problems (T-score)",
  sleep_duration_mean = "Mean sleep duration (hours)",
  sleep_duration_isd = "Sleep duration iSD (hours)",
  midpoint_mean = "Mean sleep midpoint (hours)",
  midpoint_isd = "Sleep midpoint iSD (hours)",
  cbcl_total = "CBCL Total Problems (T-score)",
  cbcl_internalizing = "CBCL Internalizing Problems (T-score)",
  cbcl_externalizing = "CBCL Externalizing Problems (T-score)",
  ysr_total = "YSR Total Problems (T-score)",
  ysr_internalizing = "YSR Internalizing Problems (T-score)",
  ysr_externalizing = "YSR Externalizing Problems (T-score)",
  age = "Age (years)"
)
