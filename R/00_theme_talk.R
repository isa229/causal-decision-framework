# ==============================================================================
# 00_theme_talk.R -- shared visual identity for all talk figures
# ==============================================================================
# ixpantia visual identity: official palette ONLY (see skills/VISUAL_IDENTITY.md),
# Lato typography (Black for titles, Regular for body, Light for footnotes),
# and a black (#1B1B1B) canvas so figures sit seamlessly on the dark slides.
#
# Usage: source this file at the top of any figure-producing script, then add
# `theme_talk()` to every ggplot and save with ggsave(..., bg = ix_black).
#
# Figures are deliberately minimal: a short plain title, axis labels when
# needed, and NO subtitles or captions -- the narrative lives in the talk,
# not on the slide.

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  options(ix_talk_theme_loaded = TRUE)

  library(ggplot2)

  # --- Official ixpantia palette (the ONLY colors allowed) --------------------
  ix_blue   <- "#3F72FF"
  ix_orange <- "#E85600"
  ix_green  <- "#009B65"
  ix_red    <- "#F1204C"
  ix_purple <- "#9959D8"
  ix_black  <- "#1B1B1B"
  ix_white  <- "#FFFFFF"

  # Subtle grid / reference lines: white at low opacity (readable on black,
  # and stays within the black/white system of the identity).
  ix_grid  <- grDevices::adjustcolor(ix_white, alpha.f = 0.14)
  ix_faint <- grDevices::adjustcolor(ix_white, alpha.f = 0.35)

  # --- Lato via showtext (renders the .ttf files shipped in fonts/) ----------
  font_ok <- FALSE
  font_dir <- "fonts"
  if (!dir.exists(font_dir) && requireNamespace("here", quietly = TRUE)) {
    font_dir <- as.character(here::here("fonts"))
  }
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    f_reg  <- file.path(font_dir, "Lato-Regular.ttf")
    f_bold <- file.path(font_dir, "Lato-Bold.ttf")
    f_black <- file.path(font_dir, "Lato-Black.ttf")
    f_light <- file.path(font_dir, "Lato-Light.ttf")
    if (file.exists(f_reg)) {
      sysfonts::font_add(
        "lato", regular = f_reg,
        bold = if (file.exists(f_bold)) f_bold else f_reg
      )
      if (file.exists(f_black)) sysfonts::font_add("lato-black", regular = f_black)
      if (file.exists(f_light)) sysfonts::font_add("lato-light", regular = f_light)
      font_ok <- TRUE
    }
  }
  if (font_ok) {
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 300)  # match ggsave(dpi = 300)
    ix_font       <- "lato"        # body text
    ix_font_title <- if (file.exists(f_black)) "lato-black" else "lato"
    ix_font_light <- if (file.exists(f_light)) "lato-light" else "lato"
  } else {
    warning("Lato fonts not found in fonts/ -- falling back to default sans.")
    ix_font       <- "sans"
    ix_font_title <- "sans"
    ix_font_light <- "sans"
  }

  # --- The theme: black canvas, white Lato text, minimal furniture ----------
  theme_talk <- function(base_size = 22) {
    theme(
      line               = element_line(colour = ix_faint, linewidth = 0.5),
      rect               = element_rect(fill = ix_black, colour = ix_black),
      text               = element_text(family = ix_font, face = "plain",
                                       colour = ix_white, size = base_size),
      panel.background   = element_rect(fill = ix_black, colour = ix_black),
      plot.background    = element_rect(fill = ix_black, colour = ix_black),
      panel.grid         = element_line(colour = ix_grid, linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = ix_white, size = rel(0.80)),
      axis.title         = element_text(colour = ix_white, size = rel(0.90)),
      axis.ticks         = element_line(colour = ix_faint, linewidth = 0.5),
      plot.title         = element_text(family = ix_font_title, face = "plain",
                                        colour = ix_white, size = rel(1.15),
                                        hjust = 0,
                                        margin = margin(b = 16, t = 6)),
      plot.subtitle      = element_blank(),
      plot.caption       = element_blank(),
      plot.tag           = element_text(family = ix_font_title, colour = ix_white,
                                        size = rel(1.4)),
      legend.position    = "top",
      legend.text        = element_text(colour = ix_white, size = rel(0.80)),
      legend.title       = element_blank(),
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      strip.text         = element_text(colour = ix_white, size = rel(0.90)),
      complete           = TRUE
    )
  }

  # --- Convenience: save with the black canvas baked in ----------------------
  ggsave_talk <- function(filename, plot, width = 10, height = 6,
                           dpi = 300, units = "in") {
    ggsave(filename, plot, width = width, height = height,
           dpi = dpi, units = units, bg = ix_black)
  }
}
