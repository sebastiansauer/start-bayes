
set.seed(42)

base_size <- 18
base_family <- "Lato Regular"


# Grossschrift-Variante (z.B. fuer Abbildungen mit viel Text): nutzt das
# Folien-Preset von theme_med() (base_size 19), siehe .claude/skills/ggplot-diagramme.
theme_large_text <- function() {
  theme_med("folien")
}



# ggplot-Diagramme-Skill: zentrales Theme + Mediumerkennung (HTML/PDF/Folien),
# siehe .claude/skills/ggplot-diagramme. Ersetzt das bisherige see::theme_modern().
theme_med <- function(medium = c("html", "pdf", "folien"),
                       grid = c("both", "h", "v", "none")) {
  medium <- match.arg(medium)
  grid <- match.arg(grid)
  bs <- switch(medium, html = 13, pdf = 10.5, folien = 19)
  lw <- if (medium == "folien") 0.6 else 0.4
  gl <- ggplot2::element_line(color = "#D9D9D9", linewidth = lw)
  off <- ggplot2::element_blank()

  ggplot2::theme_minimal(base_size = bs) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = off,
      panel.grid.major.x = if (grid %in% c("both", "v")) gl else off,
      panel.grid.major.y = if (grid %in% c("both", "h")) gl else off,
      axis.line = off,
      axis.ticks = off,
      legend.position = "bottom"
    )
}

# Medium erkennen: PDF-Lauf -> "pdf", sonst Option "plot_medium" (Default "html").
# Fuer Folien im jeweiligen Setup-Chunk: options(plot_medium = "folien")
plot_medium <- function() {
  if (knitr::is_latex_output()) "pdf" else getOption("plot_medium", "html")
}

ggplot2::theme_set(theme_med(plot_medium()))

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
                      # Bewusst KEIN "dev ="-Eintrag hier: _quarto.yml setzt
                      # dev = cairo_pdf bereits selbst, aber nur format-scoped
                      # unter format.titlepage-pdf.knitr.opts_chunk (s. dortiger
                      # Kommentar) -- dieser YAML-Block wird beim
                      # Dokument-Setup NACH dem R-Start eingemischt und gewinnt
                      # daher zuverlaessig fuer den PDF-Export. Ein "dev ="-Wert
                      # hier in _common.R laeuft dagegen schon beim R-Start (via
                      # .Rprofile) und wird fuers PDF-Format wirkungslos wieder
                      # ueberschrieben -- fuers HTML-Format gibt es aber keinen
                      # solchen Override, sodass ein hier gesetztes
                      # dev = "cairo_pdf" ungehindert durchschlaegt: Abbildungen
                      # werden dann als PDF erzeugt, die Quarto per <embed>
                      # statt <img> einbettet -- der Browser zeigt dann seine
                      # eigene PDF-Viewer-Leiste samt Scrollbalken um jede
                      # Abbildung an. Deshalb bleibt dev hier auf dem
                      # knitr-Standard (png fuers HTML), das PDF-Format regelt
                      # sich ueber _quarto.yml selbst.
                      cache.extra = knitr::is_latex_output()
                      # Rendert "quarto render" (ohne --to) beide Formate in
                      # einem Lauf, teilen sich HTML- und PDF-Pass denselben
                      # knitr-Cache pro Chunk-Code-Hash -- "dev" ist dabei
                      # KEIN Teil dieses Hashes (es ist ein globaler
                      # opts_chunk-Default, kein chunk-lokal gesetzter Wert),
                      # weshalb ohne dieses cache.extra der zuerst laufende
                      # Format-Pass seine Abbildungen in den Cache schreibt
                      # und der zweite Pass sie unveraendert (im falschen
                      # Grafikformat) uebernimmt, statt fuer sein eigenes
                      # Format neu zu rendern. cache.extra nimmt den
                      # PDF/HTML-Status explizit mit in den Hash auf und
                      # erzwingt so pro Format einen eigenen Cache-Eintrag.
)


# Cache-Schutz fuer Abbildungen: knitr erneuert einen gecachten Chunk nur,
# wenn sich SEIN Code aendert -- nicht, wenn sich eine per source()
# eingebundene Hilfsdatei oder das Theme hier in _common.R aendert. Folge war
# z. B., dass Abbildungen nach einer Umstellung von Farben/Theme weiter im
# alten Stil aus dem Cache kamen. Daher geht fuer Abbildungs-Chunks ein Hash
# ueber alle Hilfsdateien in den Cache-Schluessel (cache.extra) ein: Aendert
# sich eine davon, werden alle Abbildungen neu gezeichnet. Chunks ohne
# Abbildung (z. B. Modellschaetzung mit stan_glm) bleiben unberuehrt, damit
# nicht jede Stil-Aenderung alle Modelle neu rechnen laesst.
helper_files_hash <- local({
  files <- c(Sys.glob(c("funs/*.R", "R-Code/*.R", "children/*.R")), "_common.R")
  paste(unname(tools::md5sum(sort(files))), collapse = "")
})

knitr::opts_hooks$set(label = function(options) {
  code <- paste(options$code, collapse = "\n")
  is_fig_chunk <-
    grepl("^fig-", options$label) ||
    !is.null(options$fig.cap) ||
    grepl(paste0("\\bgg[a-zA-Z0-9_.]*\\s*\\(",      # ggplot(), gg_exp(), ggdag() ...
                 "|plot[_a-zA-Z0-9]*\\s*\\(",        # plot(), plot_rope_hdi() ...
                 "|[a-zA-Z0-9]_?plot\\b|plot_[a-zA-Z0-9_]+",  # uniform_plot, plot_maschine_schrott
                 "|pp_check|\\bgeom_|\\blabs\\(",
                 "|(?m)^\\s*p[0-9a-z_]*\\s*$"),      # gespeicherter Plot, z. B. "p3"
          code, perl = TRUE)
  if (is_fig_chunk) {
    options$cache.extra <- list(options$cache.extra, helper_files_hash)
  }
  options
})


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

# ggplot-Diagramme-Skill: feste Farbwerte fuer 1/2 Gruppen, Punkte, Flaechen,
# Gitter -- siehe .claude/skills/ggplot-diagramme.
dunkelgrau <- "#4D4D4D"   # Punkte bei 1 Gruppe
mittelgrau <- "#999999"   # Fuellung bei 1 Gruppe
gitterfarbe <- "#D9D9D9"  # Gitterlinien

# Linien-/Text-Farbfolgen ab 3 Gruppen (Reihenfolge: Orange, Blau, ...)
okabe_linie <- c("#E69F00", "#0072B2", "#009E73", "#56B4E9", "#D55E00", "#CC79A7")
okabe_text  <- c("#B36B00", "#0072B2", "#007A59", "#2B7FB0", "#D55E00", "#A5527F")

# Fuegt Linien- und Textfarbe als Spalten hinzu (Reihenfolge = Faktorstufen),
# fuer Linien, die direkt am Linienende beschriftet werden (ggrepel), statt
# ueber eine Legende.
add_farben <- function(d, gruppe) {
  g <- as.integer(factor(d[[deparse(substitute(gruppe))]]))
  d$farbe_linie <- okabe_linie[g]
  d$farbe_text  <- okabe_text[g]
  d
}


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
