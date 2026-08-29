
set.seed(42)

base_size <- 18
base_family <- "Lato Regular"


# Define a custom ggplot theme with larger text
theme_large_text <- function(base_size = 18) {
  see::theme_modern(base_size = base_size) +
    theme(
      text = element_text(size = base_size + 4),          # Base text size
      axis.title = element_text(size = base_size + 6),    # Axis title text size
      axis.text = element_text(size = base_size + 4),     # Axis tick text size
      plot.title = element_text(size = base_size + 10, face = "bold"), # Plot title size
      plot.subtitle = element_text(size = base_size + 8), # Subtitle text size
      legend.text = element_text(size = base_size + 4),   # Legend text size
      legend.title = element_text(size = base_size + 6)   # Legend title text size
    )
}



ggplot2::theme_set(see::theme_modern())

knitr::opts_chunk$set(tidy = FALSE,
                      width = 60,
                      fig.retina = 2,
                      max.print = 100,
                      fig.dpi = 300,
                      warning = FALSE,
                      out.width = "70%", # enough room to breath
                      fig.width = 6,     # reasonable size
                      fig.asp = 0.618,   # golden ratio
                      fig.align = "center", # mostly what I want
                      dev = "cairo_pdf" # R's default pdf() device falls back to
                      # non-embeddable base-14 fonts (Helvetica); cairo_pdf
                      # embeds the actual fonts so KDP/PDF validators accept them
)


options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  digits = 2,
  width = 77 # 80 - 3 for #> comment
)

   

ycol <- "#E69F00"
modelcol <- "#56B4E9"
errorcol <- "#009E73"
beta0col <- "#D55E00"
beta1col <- "#0072B2"
xcol <- "#CC79A7"

yellow <- "#F0E442FF"
blue <- "#0072B2FF"
orange <- "#E69F00FF"
green <- errorcol




#ggplot2::theme_set(see::theme_modern(axis.title.size = 18))

labeltextsize <- 8





if (knitr:::is_latex_output()) {

  # add font for plots in PDF output:

  showtext::showtext_auto(TRUE)  # use "showtext" automatically

  # Sucht eine Schriftart ueber die systemweite Fontregistrierung (funktioniert
  # plattformunabhaengig auf macOS/Linux/Windows), statt einen festen Pfad
  # anzunehmen -- Schriftpfade unterscheiden sich je nach Betriebssystem und
  # Rechner. Ist die Schrift auf der aktuellen Maschine nicht installiert,
  # wird registrierung uebersprungen (mit Hinweis), statt mit einem Fehler
  # abzubrechen.
  add_system_font <- function(sysfonts_name, family) {
    hit <- systemfonts::system_fonts()
    hit <- hit[tolower(hit$family) == tolower(family) &
                 tolower(hit$style) %in% c("regular", "normal"), ]
    if (nrow(hit) == 0) {
      hit <- systemfonts::system_fonts()
      hit <- hit[tolower(hit$family) == tolower(family), ]
    }
    if (nrow(hit) == 0) {
      message("Schrift '", family, "' nicht gefunden -- ",
              "ueberspringe font_add() fuer '", sysfonts_name, "'.")
      return(invisible(NULL))
    }
    sysfonts::font_add(sysfonts_name, regular = hit$path[1])
  }

  add_system_font("Lato Regular", "Lato")
  add_system_font("Lato", "Lato")
  add_system_font("Font Awesome", "FontAwesome")
  add_system_font("Roboto Regular", "Roboto")

  }
