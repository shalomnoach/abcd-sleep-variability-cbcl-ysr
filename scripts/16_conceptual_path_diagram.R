# Conceptual Path Diagram ------------------------------------------------------

library(here)
source(here("scripts", "path_diagram_helpers.R"))

library(dplyr)
library(ggplot2)
library(tidyr)

spec <- diagram_spec(
  left_px = 64,
  top_px = 176,
  width_px = 2032,
  height_px = 894,
  width_in = 6.5
)

latent_y <- c(494, 632, 832, 970)
derived_x <- c(1181.3, 1416.7)
derived_y <- c(507, 613, 884, 990)

## Panels ----------------------------------------------------------------------

panels <- tribble(
  ~left , ~top , ~width , ~height ,
     72 ,  344 ,    346 ,     635 ,
    510 ,  344 ,    440 ,     718 ,
   1042 ,  344 ,    514 ,     341 ,
   1042 ,  721 ,    514 ,     341
)

## Nodes -----------------------------------------------------------------------

observed <- tribble(
  ~left , ~top , ~width , ~height ,
    658 ,  184 ,    144 ,      76 ,
    101 ,  515 ,    288 ,      96 ,
    101 ,  853 ,    288 ,      96 ,
   1648 ,  605 ,    440 ,     196 ,
   1648 ,  907 ,    440 ,     116
)

latent <- tibble(
  cx = 730, cy = latent_y, rx = 138, ry = 62,
  first = c("Mean sleep", "Sleep duration", "Mean sleep", "Sleep midpoint"),
  second = c("duration", "variability", "midpoint", "variability")
)

derived <- tibble(
  cx = rep(derived_x, 4),
  cy = rep(derived_y, each = 2),
  rx = 96,
  ry = 46,
  first = rep(c("Mean", "Duration", "Mean", "Midpoint"), 2),
  second = rep(c("duration", "variability", "midpoint", "variability"), 2)
)

## Paths -----------------------------------------------------------------------

paths <- bind_rows(
  tibble(x1 = 589.5, y1 = latent_y, x2 = 391.5, y2 = c(541, 585, 879, 923)),
  tribble(
    ~x1  , ~y1   , ~x2    , ~y2   ,
     730 , 260   ,  730   , 341.5 ,
    1556 , 514.5 , 1645.5 , 669   ,
    1556 , 891.5 , 1645.5 , 737   ,
    1868 , 907   , 1868   , 803.5
  )
)

decomposition_paths <- tribble(
  ~x1 , ~y1 , ~x2    , ~y2   ,
  950 , 619 , 1039.5 , 514.5 ,
  950 , 787 , 1039.5 , 891.5
)

## Labels ----------------------------------------------------------------------

labels <- bind_rows(
  tribble(
    ~x   , ~y    , ~px , ~hjust , ~label                                  ,
     730 , 222   ,  29 , 0.5    , "Age"                                   ,
     245 , 563   ,  31 , 0.5    , "Sleep duration"                        ,
     245 , 901   ,  31 , 0.5    , "Sleep midpoint"                        ,
     101 , 387   ,  31 , 0      , r"(\textbf{Nightly sleep})"             ,
     539 , 387   ,  31 , 0      , r"(\textbf{Latent sleep parameters})"   ,
    1071 , 387   ,  31 , 0      , r"(\textbf{Within-person components})"  ,
    1071 , 426.5 ,  25 , 0      , "deviation from the child’s own mean"   ,
    1071 , 764   ,  31 , 0      , r"(\textbf{Between-person components})" ,
    1071 , 803.5 ,  25 , 0      , "child’s mean across waves"             ,
    1868 , 655   ,  30 , 0.5    , r"(\textbf{Mental health outcomes})"    ,
    1868 , 691.5 ,  24 , 0.5    , r"(CBCL or YSR \textit{T}-score:)"      ,
    1868 , 723.5 ,  24 , 0.5    , "Externalizing, Internalizing,"         ,
    1868 , 755.5 ,  24 , 0.5    , "or Total Problems"                     ,
    1868 , 949   ,  29 , 0.5    , r"(\textbf{Covariates})"                ,
    1868 , 984   ,  24 , 0.5    , "race, ethnicity, family income"
  ),
  latent |>
    pivot_longer(c(first, second), names_to = "line", values_to = "label") |>
    transmute(x = cx, y = cy + if_else(line == "first", -17, 17),
              px = 28, hjust = 0.5, label),
  derived |>
    pivot_longer(c(first, second), names_to = "line", values_to = "label") |>
    transmute(x = cx, y = cy + if_else(line == "first", -13, 13),
              px = 22, hjust = 0.5, label)
)

## Plot ------------------------------------------------------------------------

figure <- ggplot() +
  panel_layer(panels, spec) +
  segment_layer(paths, spec) +
  dashed_arrow_layers(decomposition_paths, spec) +
  box_layer(observed, spec) +
  ellipse_layer(latent, spec) +
  double_ellipse_layers(derived, spec) +
  latex_layer(labels, spec) +
  diagram_canvas(spec)

save_diagram(figure, here("figures", "conceptual_path_diagram"), spec)
