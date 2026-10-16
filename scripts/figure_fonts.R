font_dir <- here::here("working", "fonts")
figure_font <- "Open Sans Figure"
font_files <- file.path(
  font_dir,
  paste0("Open Sans-", c("regular", "700", "italic", "700italic"), ".ttf")
)

if (!all(file.exists(font_files))) {
  systemfonts::get_from_google_fonts("Open Sans", dir = font_dir)
}

systemfonts::register_font(
  name = figure_font,
  plain = font_files[1],
  bold = font_files[2],
  italic = font_files[3],
  bolditalic = font_files[4]
)
