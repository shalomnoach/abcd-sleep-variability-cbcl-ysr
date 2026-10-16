# Path Diagram Helpers ---------------------------------------------------------

library(ggplot2)
library(ggforce)
library(xdvir)
library(tibble)
source(here::here("scripts", "figure_fonts.R"))

diagram_ink <- "#111111"
diagram_panel_fill <- "#EFEFEC"
diagram_plate_ink <- "#3B3B3B"
diagram_stroke <- 2.5
diagram_font <- figure_font

options(xdvir.quiet = TRUE)

math_font <- function(file, range) {
  sprintf("\\setmathfont{%s}[Path=%s/, range={%s}]", file, font_dir, range)
}

open_sans_math <- LaTeXpackage(
  name = "opensansmath",
  preamble = c(
    "\\usepackage{amsmath}",
    "\\usepackage{fontspec}",
    "\\usepackage{unicode-math}",
    "\\setmathfont{latinmodern-math.otf}",
    math_font(
      "Open Sans-regular.ttf",
      paste0(
        "up/{latin,Latin,num,greek,Greek},it/{greek,Greek},",
        r"("2212,"003D,"0028,"0029,"002C,"002B,"2032,"005B,"005D,"2211)"
      )
    ),
    math_font("Open Sans-italic.ttf", r"(it/{latin,Latin},"2113)"),
    math_font("Open Sans-700.ttf", "bfup/{latin,Latin,greek,Greek},bfit/{greek,Greek}"),
    math_font("Open Sans-700italic.ttf", "bfit/{latin,Latin}"),

    "\\makeatletter",
    paste0(
      "\\newcommand{\\smallsup}[1]{\\text{\\fontsize",
      "{\\strip@pt\\dimexpr\\f@size pt*6/7\\relax}{0}\\selectfont #1}}"
    ),
    "\\makeatother"
  )
)

## Scale -----------------------------------------------------------------------

diagram_spec <- function(left_px, top_px, width_px, height_px, width_in) {
  list(
    left_px = left_px,
    top_px = top_px,
    width_px = width_px,
    height_px = height_px,
    width_in = width_in,
    height_in = width_in * height_px / width_px,
    pt_per_px = width_in * 72 / width_px
  )
}

px_unit <- function(px, spec) grid::unit(px * spec$pt_per_px, "bigpts")

text_size <- function(px, spec) px * spec$pt_per_px / .pt

line_width <- function(px, spec) px * spec$pt_per_px * 96 / 72 / .pt

## Geometry --------------------------------------------------------------------

circle_point <- function(cx, cy, r, angle) {
  c(cx + r * cos(angle * pi / 180), cy + r * sin(angle * pi / 180))
}

circle_arc <- function(cx, cy, r, from, to, n = 120) {
  t <- seq(from, to, length.out = n) * pi / 180
  cbind(cx + r * cos(t), cy + r * sin(t))
}

covariance_arc <- function(from, to, angle = 45) {
  half <- (to[1] - from[1]) / 2
  height <- half * tan(angle / 2 * pi / 180)
  r <- (half^2 + height^2) / (2 * height)
  cx <- from[1] + half
  cy <- from[2] - height + r
  circle_arc(
    cx,
    cy,
    r,
    atan2(from[2] - cy, from[1] - cx) * 180 / pi,
    atan2(to[2] - cy, to[1] - cx) * 180 / pi
  )
}

variance_loop <- function(
  cx,
  cy,
  r,
  angle,
  loop_r = 17,
  offset = 9,
  gap = diagram_stroke
) {
  d <- r + offset
  centre <- circle_point(cx, cy, d, angle)
  inside <- acos((d^2 + loop_r^2 - (r + gap)^2) / (2 * d * loop_r)) * 180 / pi
  circle_arc(
    centre[1],
    centre[2],
    loop_r,
    angle + 180 + inside,
    angle + 540 - inside
  )
}

## Shapes ----------------------------------------------------------------------

box_layer <- function(
  boxes,
  spec,
  fill = "#FFFFFF",
  colour = diagram_ink,
  stroke_px = diagram_stroke,
  radius_px = 0
) {
  inset <- if (is.na(colour)) 0 else stroke_px / 2
  corners <- dplyr::reframe(
    dplyr::mutate(boxes, id = dplyr::row_number()),
    x = c(left, left + width, left + width, left) + inset * c(1, -1, -1, 1),
    y = c(top, top, top + height, top + height) + inset * c(1, 1, -1, -1),
    .by = id
  )
  geom_shape(
    data = corners,
    aes(x = x, y = y, group = id),
    fill = fill,
    colour = colour,
    linewidth = line_width(stroke_px, spec),
    radius = px_unit(max(radius_px - inset, 0), spec)
  )
}

plate_layer <- function(plates, spec) {
  box_layer(
    plates,
    spec,
    fill = NA,
    colour = diagram_plate_ink,
    stroke_px = 2,
    radius_px = 14
  )
}

panel_layer <- function(panels, spec) {
  box_layer(panels, spec, fill = diagram_panel_fill, colour = NA, radius_px = 18)
}

circle_layer <- function(
  circles,
  spec,
  fill = "#FFFFFF",
  colour = diagram_ink,
  stroke_px = diagram_stroke
) {
  inset <- if (is.na(colour)) 0 else stroke_px / 2
  geom_circle(
    data = circles,
    aes(x0 = cx, y0 = cy, r = r - inset),
    fill = fill,
    colour = colour,
    linewidth = line_width(stroke_px, spec)
  )
}

ellipse_layer <- function(
  ellipses,
  spec,
  fill = "#FFFFFF",
  stroke_px = diagram_stroke
) {
  geom_ellipse(
    data = ellipses,
    aes(x0 = cx, y0 = cy, a = rx - stroke_px / 2, b = ry - stroke_px / 2, angle = 0),
    fill = fill,
    colour = diagram_ink,
    linewidth = line_width(stroke_px, spec)
  )
}

double_circle_layers <- function(circles, spec, border_px = 6) {
  stroke_px <- border_px / 3
  list(
    circle_layer(circles, spec, stroke_px = stroke_px),
    circle_layer(
      transform(circles, r = r - border_px + stroke_px),
      spec,
      stroke_px = stroke_px
    )
  )
}

double_ellipse_layers <- function(ellipses, spec, border_px = 6) {
  stroke_px <- border_px / 3
  list(
    ellipse_layer(ellipses, spec, stroke_px = stroke_px),
    ellipse_layer(
      transform(
        ellipses,
        rx = rx - border_px + stroke_px,
        ry = ry - border_px + stroke_px
      ),
      spec,
      stroke_px = stroke_px
    )
  )
}

triangle_layer <- function(triangles, spec, stroke_px = diagram_stroke) {
  points <- dplyr::reframe(
    dplyr::mutate(triangles, id = dplyr::row_number()),
    x = cx + c(0, width / 2, -width / 2),
    y = top + c(0, height, height),
    .by = id
  )
  geom_polygon(
    data = points,
    aes(x = x, y = y, group = id),
    fill = "#FFFFFF",
    colour = diagram_ink,
    linewidth = line_width(stroke_px, spec),
    linejoin = "round"
  )
}

## Paths -----------------------------------------------------------------------

arrow_head <- function(spec, stroke_px = diagram_stroke, ends = "last") {
  grid::arrow(
    angle = atan(0.5) * 180 / pi,
    length = px_unit((4 - 1.618) * stroke_px, spec),
    ends = ends,
    type = "closed"
  )
}

tip_shift <- function(stroke_px) (1 / (2 * sin(atan(0.5))) - 0.8) * stroke_px

pull_back <- function(x_from, y_from, x_to, y_to, distance) {
  length <- sqrt((x_to - x_from)^2 + (y_to - y_from)^2)
  list(
    x = x_to - (x_to - x_from) * distance / length,
    y = y_to - (y_to - y_from) * distance / length
  )
}

segment_layer <- function(
  segments,
  spec,
  stroke_px = diagram_stroke,
  head = TRUE,
  linetype = "solid"
) {
  if (head) {
    tip <- pull_back(
      segments$x1,
      segments$y1,
      segments$x2,
      segments$y2,
      tip_shift(stroke_px)
    )
    segments$x2 <- tip$x
    segments$y2 <- tip$y
  }
  geom_segment(
    data = segments,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    colour = diagram_ink,
    linewidth = line_width(stroke_px, spec),
    linetype = linetype,
    arrow = if (head) arrow_head(spec, stroke_px) else NULL,
    arrow.fill = diagram_ink,
    lineend = "round",
    linejoin = "mitre"
  )
}

dashed_arrow_layers <- function(segments, spec, stroke_px = diagram_stroke) {
  head_px <- 4 * stroke_px + 1
  dx <- segments$x2 - segments$x1
  dy <- segments$y2 - segments$y1
  shaft_x <- segments$x2 - dx * head_px / sqrt(dx^2 + dy^2)
  shaft_y <- segments$y2 - dy * head_px / sqrt(dx^2 + dy^2)
  list(
    segment_layer(
      transform(segments, x2 = shaft_x, y2 = shaft_y),
      spec,
      stroke_px,
      head = FALSE,
      linetype = "43"
    ),
    segment_layer(transform(segments, x1 = shaft_x, y1 = shaft_y), spec, stroke_px)
  )
}

curve_layer <- function(curves, spec, stroke_px = diagram_stroke) {
  points <- do.call(rbind, lapply(seq_along(curves), function(i) {
    tibble(id = i, x = curves[[i]][, 1], y = curves[[i]][, 2])
  }))
  geom_path(
    data = points,
    aes(x = x, y = y, group = id),
    colour = diagram_ink,
    linewidth = line_width(stroke_px, spec),
    arrow = arrow_head(spec, stroke_px, ends = "both"),
    arrow.fill = diagram_ink,
    lineend = "round",
    linejoin = "mitre"
  )
}

## Text ------------------------------------------------------------------------

latex_layer <- function(labels, spec) {
  if (!"hjust" %in% names(labels)) labels$hjust <- 0.5
  labels$size <- text_size(labels$px, spec)
  labels$baseline <- labels$y + 0.388 * labels$px
  geom_latex(
    data = labels,
    aes(x = x, y = baseline, label = label, size = size, hjust = hjust),
    vjust = "baseline",
    family = diagram_font,
    colour = diagram_ink,
    packages = open_sans_math,
    engine = "xetex"
  )
}

label_background_layer <- function(boxes) {
  geom_rect(
    data = boxes,
    aes(
      xmin = x - width / 2,
      xmax = x + width / 2,
      ymin = y - height / 2,
      ymax = y + height / 2
    ),
    fill = "#FFFFFF",
    colour = NA
  )
}

## Canvas ----------------------------------------------------------------------

diagram_canvas <- function(spec) {
  list(
    scale_x_continuous(
      limits = spec$left_px + c(0, spec$width_px),
      expand = expansion(0)
    ),
    scale_y_reverse(
      limits = spec$top_px + c(spec$height_px, 0),
      expand = expansion(0)
    ),
    scale_size_identity(),
    coord_fixed(clip = "off"),
    theme_void(),
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA)
    )
  )
}

save_diagram <- function(plot, path_stem, spec, dpi = 600) {
  dir.create(dirname(path_stem), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    paste0(path_stem, ".png"),
    plot,
    width = spec$width_in,
    height = spec$height_in,
    units = "in",
    dpi = dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  ggsave(
    paste0(path_stem, ".pdf"),
    plot,
    width = spec$width_in,
    height = spec$height_in,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white"
  )
}
