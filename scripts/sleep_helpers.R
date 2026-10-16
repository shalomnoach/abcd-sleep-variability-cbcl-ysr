# Sleep Helpers ----------------------------------------------------------------

## Midpoint Coordinates --------------------------------------------------------

align_release_midpoints <- function(data, reference = 3, tolerance = 1e-9) {
  n <- nrow(data)
  data$midpoint_session_shift_days <- integer(n)
  data$midpoint_person_shift_days <- integer(n)
  data$midpoint_session_ambiguous <- rep(FALSE, n)
  data$midpoint_person_ambiguous <- rep(FALSE, n)
  data$midpoint_hours <- data$midpoint_noon_hours
  usable <- which(data$model_sleep_eligible)
  short_arc <- function(values) {
    clocks <- sort(unique(values %% 24))
    gaps <- diff(c(clocks, clocks[1] + 24))
    gap <- which.max(gaps)
    start <- clocks[if (gap == length(clocks)) 1L else gap + 1L]
    list(width = 24 - gaps[gap], start = start)
  }
  lift_arc <- function(values, start) {
    clocks <- values %% 24
    lifted <- clocks + 24 * (clocks < start)
    lifted + 24 * floor((reference - mean(sort(lifted))) / 24 + .5)
  }
  if (length(usable)) {
    keys <- paste(data$participant_id[usable], data$sleep_session_id[usable], sep = "\r")
    session_rows <- split(usable, keys)
    session_person <- vapply(session_rows, function(j) as.character(data$participant_id[j[1]]), "")
    session_means <- numeric(length(session_rows))
    for (s in seq_along(session_rows)) {
      j <- session_rows[[s]]
      original <- data$midpoint_noon_hours[j]
      arc <- short_arc(original)
      crosses <- diff(range(original)) > 12 + tolerance
      if (crosses && arc$width < 12 - tolerance) {
        shifted <- lift_arc(original, arc$start)
        days <- as.integer(round((shifted - original) / 24))
        data$midpoint_session_shift_days[j] <- days
        data$midpoint_hours[j] <- original + 24 * days
      }
      data$midpoint_session_ambiguous[j] <- crosses && arc$width >= 12 - tolerance
      session_means[s] <- mean(sort(data$midpoint_hours[j]))
    }
    person_sessions <- split(seq_along(session_rows), session_person)
    for (ss in person_sessions) {
      means <- session_means[ss]
      arc <- short_arc(means)
      crosses <- diff(range(means)) > 12 + tolerance
      if (crosses && arc$width < 12 - tolerance) {
        shifted <- lift_arc(means, arc$start)
        days <- as.integer(round((shifted - means) / 24))
        for (k in seq_along(ss)) {
          j <- session_rows[[ss[k]]]
          data$midpoint_person_shift_days[j] <- days[k]
          data$midpoint_hours[j] <- data$midpoint_hours[j] + 24 * days[k]
        }
      } else if (crosses) {
        for (s in ss) data$midpoint_person_ambiguous[session_rows[[s]]] <- TRUE
      }
    }
  }
  data$midpoint_shift_days <- data$midpoint_session_shift_days + data$midpoint_person_shift_days
  data
}

## Released Sleep Values -------------------------------------------------------

derive_release_sleep_values <- function(data, align_midpoints = TRUE) {
  start <- as.numeric(data$dtt_start_slp)
  end <- as.numeric(data$dtt_end_slp)
  span <- end - start
  midpoint <- start + span / 2
  data <- data |>
    dplyr::mutate(
      sleep_session_id = as.character(session_id),
      sleep_onset_time = dtt_start_slp,
      wake_time = dtt_end_slp,
      elapsed_span_hours = span / 3600,
      released_duration_hours = min_total_slp / 60,
      sleep_duration_hours = released_duration_hours,
      span_discrepancy_hours = elapsed_span_hours - released_duration_hours,
      midpoint_datetime = as.POSIXct(midpoint, origin = "1970-01-01", tz = "UTC"),
      midpoint_clock_hours = (midpoint %% 86400) / 3600,
      midpoint_noon_hours = ifelse(
        midpoint_clock_hours > 12, midpoint_clock_hours - 24, midpoint_clock_hours
      ),
      midpoint_hours = midpoint_noon_hours,
      qc_300min_corrected = min_total_slp >= 300,
      model_sleep_eligible = !is.na(participant_id) &
        nzchar(as.character(participant_id)) & !is.na(session_id) &
        nzchar(as.character(session_id)) & !is.na(dt) &
        !is.na(day) & day != 0 &
        is.finite(sleep_duration_hours) & sleep_duration_hours >= 0 &
        is.finite(span) & is.finite(midpoint_hours)
    )
  if (align_midpoints) align_release_midpoints(data) else data
}
