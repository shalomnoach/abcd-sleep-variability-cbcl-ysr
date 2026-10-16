# Sleep Variability Figure -----------------------------------------------------

library(here)
source(here("config.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

source(here("scripts", "figure_fonts.R"))

## Inputs ----------------------------------------------------------------------

analysis_data <- bind_rows(
  readRDS(file.path(data_sets_dir, "analysis_cbcl.rds")),
  readRDS(file.path(data_sets_dir, "analysis_ysr.rds"))
) |>
  filter(
    is.finite(sleep_duration_hours),
    is.finite(midpoint_hours),
    !is.na(sleep_onset_time),
    !is.na(wake_time)
  ) |>
  distinct(participant_id, sleep_session_id, dt, .keep_all = TRUE) |>
  mutate(
    onset_clock = hour(sleep_onset_time) +
      minute(sleep_onset_time) / 60 +
      second(sleep_onset_time) / 3600,
    onset_noon = if_else(onset_clock < 12, onset_clock + 12, onset_clock - 12),
    elapsed_hours = as.numeric(difftime(
      wake_time,
      sleep_onset_time,
      units = "hours"
    )),
    sleep_day = as.Date(sleep_onset_time) - as.integer(onset_clock < 12)
  )

percentile_values <- readRDS(
  file.path(report_data_dir, "empirical_sleep_variability_percentiles.rds")
) |>
  filter(percentile %in% c("10%", "25%", "75%", "90%")) |>
  mutate(
    percentile_number = as.integer(sub("%", "", percentile)),
    percentile_label = paste0(percentile_number, "th Percentile")
  ) |>
  arrange(match(percentile_number, c(10L, 25L, 75L, 90L)))

## Illustrative Participants ---------------------------------------------------

selected_participants <- analysis_data |>
  group_by(participant_id) |>
  summarise(
    n_nights = n(),
    n_sessions = n_distinct(sleep_session_id),
    duration_isd = sd(sleep_duration_hours),
    first_sleep_day = min(sleep_day),
    last_sleep_day = max(sleep_day),
    sleep_day_span = as.integer(last_sleep_day - first_sleep_day) + 1L,
    minimum_elapsed_hours = min(elapsed_hours),
    maximum_elapsed_hours = max(elapsed_hours),
    age = first(assessment_age),
    sex = as.character(first(sex)),
    .groups = "drop"
  ) |>
  filter(
    n_sessions == 1L,
    between(n_nights, 15L, 30L),
    sleep_day_span <= 30L,
    minimum_elapsed_hours >= 3,
    maximum_elapsed_hours <= 14
  ) |>
  cross_join(
    percentile_values |>
      select(percentile, percentile_number, percentile_isd_hours = isd_hours)
  ) |>
  mutate(distance = abs(duration_isd - percentile_isd_hours)) |>
  group_by(percentile, percentile_number) |>
  arrange(distance, desc(n_nights), participant_id, .by_group = TRUE) |>
  slice(2L) |>
  ungroup() |>
  arrange(percentile_number)

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(
  selected_participants |>
    transmute(
      Percentile = percentile,
      `Target duration iSD (hours)` = round(percentile_isd_hours, 3),
      Participant = participant_id,
      `Observed duration iSD (hours)` = round(duration_isd, 3),
      Nights = n_nights,
      `Age (years)` = round(age, 2),
      Sex = sex,
      `First sleep date` = first_sleep_day,
      `Last sleep date` = last_sleep_day
    ),
  file.path(outputs_dir, "figure1_exemplars.csv"),
  na = ""
)

## Noon-To-Noon Windows --------------------------------------------------------

panel_data <- analysis_data |>
  inner_join(
    selected_participants |>
      select(
        participant_id,
        percentile,
        percentile_number,
        percentile_isd_hours,
        selected_age = age,
        selected_sex = sex
      ),
    by = "participant_id"
  ) |>
  mutate(
    age = coalesce(selected_age, assessment_age),
    sex = coalesce(selected_sex, as.character(sex)),
    panel_title = paste0(
      "<b>",
      percentile_number,
      "th Percentile (<i>iSD</i> = ",
      sprintf("%.2f", percentile_isd_hours),
      ")</b><br><span style='font-size:10pt;font-weight:normal'>Age ",
      sprintf("%.2f", age),
      ", ",
      sex,
      "</span>"
    )
  ) |>
  mutate(
    panel_title = factor(
      panel_title,
      levels = unique(panel_title[order(percentile_number)])
    )
  ) |>
  select(-selected_age, -selected_sex)

date_rows <- panel_data |>
  group_by(panel_title) |>
  summarise(
    monitoring_start = first(sleep_day - (day - 1L)),
    .groups = "drop"
  ) |>
  cross_join(tibble(day = 1:21)) |>
  mutate(sleep_day = monitoring_start + day - 1L) |>
  group_by(panel_title) |>
  arrange(day, .by_group = TRUE) |>
  mutate(y = 22L - day) |>
  ungroup() |>
  mutate(
    date_label = format(sleep_day, "%m/%d/%y"),
    weekend = wday(sleep_day) %in% c(1L, 7L)
  )

panel_data <- panel_data |>
  left_join(
    date_rows |> select(panel_title, day, y),
    by = c("panel_title", "day")
  ) |>
  mutate(wake_noon_unwrapped = onset_noon + elapsed_hours)

segments <- bind_rows(
  panel_data |>
    transmute(
      panel_title,
      y,
      xmin = onset_noon,
      xmax = pmin(wake_noon_unwrapped, 24)
    ),
  panel_data |>
    filter(wake_noon_unwrapped > 24) |>
    transmute(
      panel_title,
      y,
      xmin = 0,
      xmax = wake_noon_unwrapped - 24
    )
) |>
  filter(xmax > xmin)

## Plot ------------------------------------------------------------------------

figure <- ggplot() +
  geom_rect(
    data = date_rows |> distinct(panel_title),
    xmin = 0,
    xmax = 24,
    ymin = 0.5,
    ymax = 21.5,
    fill = "#F4F4F4",
    color = NA
  ) +
  geom_vline(
    xintercept = seq(0, 24, 1),
    color = "#D9D9D9",
    linetype = "dashed",
    linewidth = 0.20
  ) +
  geom_vline(
    xintercept = seq(0, 24, 4),
    color = "#ADADAD",
    linetype = "dashed",
    linewidth = 0.30
  ) +
  geom_rect(
    data = segments,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = y - 0.42,
      ymax = y + 0.42
    ),
    fill = "#4169E1",
    color = NA
  ) +
  geom_text(
    data = date_rows,
    aes(
      x = -0.35,
      y = y,
      label = date_label,
      color = weekend
    ),
    hjust = 1,
    size = 3.05,
    family = figure_font,
    show.legend = FALSE
  ) +
  facet_wrap(
    vars(panel_title),
    ncol = 2
  ) +
  scale_color_manual(values = c(`FALSE` = "grey20", `TRUE` = "#B3261E")) +
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    labels = c("Noon", "4 PM", "8 PM", "Midnight", "4 AM", "8 AM", "Noon"),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    limits = c(0.5, 21.5),
    expand = expansion(mult = 0)
  ) +
  coord_cartesian(xlim = c(0, 24), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = figure_font, base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = ggtext::element_markdown(
      hjust = 0,
      family = figure_font,
      face = "plain",
      size = 12,
      lineheight = 1.2,
      margin = margin(b = 5)
    ),
    axis.text.x = element_text(
      family = figure_font,
      size = 10
    ),
    panel.spacing.x = grid::unit(2.0, "cm"),
    panel.spacing.y = grid::unit(0.8, "cm"),
    plot.margin = margin(t = 8, r = 28, b = 8, l = 62)
  )

dir.create(here("figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(
  here("figures", "sleep_variability_figure.png"),
  figure,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

ggsave(
  here("figures", "sleep_variability_figure.pdf"),
  figure,
  width = 12,
  height = 10,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

print(selected_participants)
