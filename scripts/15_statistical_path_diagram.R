# Statistical Path Diagram -----------------------------------------------------

library(here)
library(dplyr)
library(ggplot2)
source(here("scripts", "path_diagram_helpers.R"))

spec <- diagram_spec(64, 172, 1788, 1516, width_in = 6.5)

## Nodes -----------------------------------------------------------------------

plates <- tibble(
  left = c(72, 104, 136), top = c(180, 212, 244),
  width = c(1772, 1708, 860), height = c(1500, 960, 316)
)

panels <- tibble(
  left = c(160, 1116, 1116), top = c(600, 400, 1456),
  width = c(850, 640, 640), height = c(316, 188, 196)
)

observed <- tribble(
  ~cx, ~cy, ~width, ~height, ~px, ~label,
  540, 320, 240, 68, 32, r"(duration$_{ti}$)",
  540, 472, 240, 68, 32, r"(midpoint$_{ti}$)",
  870, 1092, 120, 68, 35, r"($a^{\mathrm{c}}_{i}$)",
  1436, 942, 240, 100, 40, r"($y_{i}$)",
  1718, 918, 100, 60, 35, r"($I_{i}$)",
  1732, 1226, 84, 62, 35, r"($R_{p}$)",
  1732, 1310, 84, 62, 35, r"($E_{p}$)"
) |>
  mutate(left = cx - width / 2, top = cy - height / 2)

latent <- tribble(
  ~cx, ~cy, ~r, ~px, ~label,
  790, 320, 30, 25, r"($\varepsilon^{\text{dur}}_{ti}$)",
  790, 472, 30, 25, r"($\varepsilon^{\text{mid}}_{ti}$)",
  885, 320, 38, 24, r"($(\sigma^{\text{dur}}_{i})^{2}$)",
  885, 472, 38, 24, r"($(\sigma^{\text{mid}}_{i})^{2}$)",
  325, 356.5, 29, 25, r"($\mu^{\text{dur}}_{i}$)",
  327.5, 440.5, 29, 25, r"($\mu^{\text{mid}}_{i}$)",
  270, 842, 58, 38, r"($\mu^{\text{dur}}_{i}$)",
  480, 842, 58, 38, r"($\ell^{\text{dur}}_{i}$)",
  690, 842, 58, 38, r"($\mu^{\text{mid}}_{i}$)",
  900, 842, 58, 38, r"($\ell^{\text{mid}}_{i}$)"
)

components <- tibble(
  cx = rep(c(1196, 1356, 1516, 1676), 2),
  cy = rep(c(510, 1530), each = 4), r = 54, px = 33,
  label = sprintf(
    r"($%s^{\smallsup{%s,%s}}_{%s}$)",
    c(r"(\mu)", r"(\ell)", r"(\mu)", r"(\ell)"),
    c("dur", "dur", "mid", "mid"),
    rep(c("W", "B"), each = 4), rep(c("i", "p"), each = 4)
  )
)

between_outcome <- tibble(
  cx = 1436, cy = 1310, rx = 120, ry = 50, px = 40,
  label = r"($y^{\mathrm{B}}_{p}$)"
)

constants <- tibble(
  cx = c(212, 270, 1290), top = c(366, 1060, 1196),
  width = c(60, 60, 48), height = c(56, 56, 42),
  x = c(212, 272, 1290), y = c(403, 1096.5, 1225),
  px = c(30, 30, 25), label = "1"
)

## Paths -----------------------------------------------------------------------

paths <- tribble(
  ~path, ~x1, ~y1, ~x2, ~y2,
  "Duration mean", 325, 356.5, 418, 321,
  "Midpoint mean", 327.5, 440.5, 418, 471,
  "Duration residual", 757.5, 320, 662, 320,
  "Midpoint residual", 757.5, 472, 662, 472,
  "Duration mean residual", 208.8, 780.8, 227.2, 799.2,
  "Duration variance residual", 418.8, 780.8, 437.2, 799.2,
  "Midpoint mean residual", 751.2, 780.8, 732.8, 799.2,
  "Midpoint variance residual", 961.2, 780.8, 942.8, 799.2,
  "Duration mean intercept", 270, 1068, 270, 902.5,
  "Duration variance intercept", 269.8, 1075.6, 439.5, 887,
  "Midpoint mean intercept", 273.5, 1083.4, 637.7, 872.3,
  "Midpoint variance intercept", 278.2, 1092.1, 843.9, 864.6,
  "Age to duration mean", 817.2, 1099.6, 324.7, 867.8,
  "Age to duration variance", 816.6, 1074.5, 529.8, 876.4,
  "Age to midpoint mean", 837.7, 1064.7, 723.4, 892.4,
  "Age to midpoint variance", 858, 1065.9, 888.9, 901.5,
  "Within duration mean", 1217, 560, 1352, 890,
  "Within duration variance", 1363, 564, 1404, 890,
  "Within midpoint mean", 1509, 564, 1468, 890,
  "Within midpoint variance", 1655, 560, 1520, 890,
  "Between duration mean", 1231, 1488.9, 1351.1, 1348,
  "Between duration variance", 1370.6, 1478, 1403.7, 1360.7,
  "Between midpoint mean", 1501.4, 1478, 1468.3, 1360.7,
  "Between midpoint variance", 1641, 1488.9, 1520.9, 1348,
  "Outcome residual", 1268, 942, 1313.5, 942,
  "Income", 1668, 918, 1558, 918,
  "Between-person outcome", 1436, 1257.5, 1436, 994.5,
  "Participant residual", 1268, 1310, 1313.5, 1310,
  "Race", 1690, 1226, 1529, 1275.6,
  "Ethnicity", 1690, 1310, 1558.5, 1310,
  "Outcome intercept", 1307, 1224, 1366.5, 1266.6
)

mean_paths <- tibble(
  x1 = c(225.6, 237), y1 = c(394.4, 410),
  x2 = c(325, 327.5), y2 = c(356.5, 440.5)
)

decomposition_paths <- tibble(
  x1 = 1012, y1 = c(780, 904), x2 = 1114, y2 = c(570, 1474)
)

covariances <- list(
  covariance_arc(c(324.4, 815.5), c(425.6, 815.5)),
  covariance_arc(c(534.4, 815.5), c(635.6, 815.5)),
  covariance_arc(c(744.4, 815.5), c(845.6, 815.5)),
  covariance_arc(c(307.2, 794.3), c(652.8, 794.3)),
  covariance_arc(c(517.2, 794.3), c(862.8, 794.3)),
  covariance_arc(c(282.6, 782.8), c(887.4, 782.8)),
  variance_loop(790, 320, 30, 0),
  variance_loop(790, 472, 30, 0)
)

## Labels ----------------------------------------------------------------------

path_labels <- bind_rows(
  tibble(
    x = c(1268, 1378, 1494, 1604), y = c(685, 688, 688, 685),
    width = 78, height = 42, px = 30,
    label = sprintf(r"($\beta^{\mathrm{W}}_{%d}$)", 1:4)
  ),
  tibble(
    x = c(1290, 1387.4, 1484.6, 1582), y = c(1419.7, 1418.7, 1418.7, 1419.7),
    width = 78, height = 42, px = 30,
    label = sprintf(r"($\beta^{\mathrm{B}}_{%d}$)", 1:4)
  ),
  tibble(
    x = c(270, 339.7, 420.8, 512.1), y = 998,
    width = 36, height = 39, px = 28,
    label = sprintf(r"($\gamma_{%d}$)", 1:4)
  ),
  tibble(
    x = c(601.4, 705.8, 793.5, 870.8), y = 1004, text_y = 998,
    width = 66, height = 38, px = 28,
    label = sprintf(r"($\gamma_{\text{age},%d}$)", 1:4)
  ),
  tibble(x = 1436, y = 1083, width = 34, height = 40, px = 30, label = "1")
) |>
  mutate(text_y = coalesce(text_y, y))

annotations <- bind_rows(
  tribble(
    ~x, ~y, ~px, ~hjust, ~label,
    710, 298, 28, 0.5, "1",
    710, 450, 28, 0.5, "1",
    1356, 1214, 28, 0.5, r"($\beta_{0}$)",
    1606, 1210, 30, 0.5, r"($b^{\text{race}}$)",
    1622, 1338, 30, 0.5, r"($b^{\text{ethnicity}}$)",
    1613, 890, 30, 0.5, r"($b^{\text{income}}$)",
    1262, 937.5, 30, 1, r"($\sigma^{2}_{y}$)",
    1262, 1305.5, 30, 1, r"($\sigma^{2}_{p}$)",
    176, 630, 35, 0, r"($\symbfup{\eta}_{i}$)",
    152, 535, 28, 0, r"(Level 1: Nights $t$ = 1, …, $T_{i}$)",
    120, 1149, 28, 0, r"(Level 2: Sessions $i \in S_{p}$)",
    88, 1657, 28, 0, r"(Level 3: Participants $p$ = 1, …, $N$)"
  ),
  tibble(
    x = 1132, y = c(427, 1615), px = 30, hjust = 0,
    label = c(
      paste(r"(Within-person:\enspace $\symbfup{\eta}^{\mathrm{W}}_{i} =)",
            r"(\tilde{\symbfup{\eta}}_{i} - \symbfup{\eta}^{\mathrm{B}}_{p(i)}$)"),
      paste(r"(Between-person:\enspace $\symbfup{\eta}^{\mathrm{B}}_{p} =)",
            r"(n^{-1}_{p} \sum_{i \in S_{p}} \tilde{\symbfup{\eta}}_{i}$)")
    )
  ),
  tibble(
    x = c(191.8, 401.8, 768.2, 978.2), y = 761.2, px = 30, hjust = 0.5,
    label = sprintf(r"($\tau^{2}_{%d}$)", 1:4)
  ),
  tibble(
    x = c(375, 585, 795, 480, 690, 585),
    y = c(832, 832, 832, 700, 700, 630), px = 28, hjust = 0.5,
    label = sprintf(r"($\omega_{%s}$)", c("12", "23", "34", "13", "24", "14"))
  )
)

## Plot ------------------------------------------------------------------------

figure <- ggplot() +
  plate_layer(plates, spec) +
  panel_layer(panels, spec) +
  segment_layer(mean_paths, spec, head = FALSE) +
  segment_layer(paths, spec) +
  dashed_arrow_layers(decomposition_paths, spec) +
  curve_layer(covariances, spec) +
  box_layer(observed, spec) +
  circle_layer(latent, spec) +
  double_circle_layers(components, spec) +
  ellipse_layer(between_outcome, spec) +
  triangle_layer(constants, spec) +
  label_background_layer(path_labels) +
  latex_layer(
    bind_rows(observed, latent, components, between_outcome) |>
      transmute(x = cx, y = cy, px, label),
    spec
  ) +
  latex_layer(constants, spec) +
  latex_layer(path_labels |> mutate(y = text_y), spec) +
  latex_layer(annotations, spec) +
  diagram_canvas(spec)

save_diagram(figure, here("figures", "statistical_path_diagram"), spec)
