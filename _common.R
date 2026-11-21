
set.seed(42)

base_size <- 18
base_family <- "Lato Regular"


# Grossschrift-Variante (z.B. fuer Abbildungen mit viel Text): nutzt das
# Folien-Preset von theme_med() (base_size 19), siehe globaler Skill ggplot-diagramme (~/.claude/skills).
theme_large_text <- function() {
  theme_med("folien")
}



# ggplot-Diagramme-Skill: zentrales Theme + Mediumerkennung (HTML/PDF/Folien),
# siehe globaler Skill ggplot-diagramme (~/.claude/skills). Ersetzt das bisherige see::theme_modern().
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

# Tufte-Stil: Y-Achsenbeschriftung horizontal ueber dem Panel (V1) statt vertikal
# daneben. Technik: Beim Bauen jedes ggplots wird der Y-Titel als erste Zeile
# des Untertitels (bzw. als Untertitel) direkt ueber das Panel gesetzt und der
# normale Y-Achsentitel ausgeblendet. Ausnahmen (bleiben unveraendert):
# coord_flip(), Plots ohne/mit bereits ausgeblendetem Y-Titel.
# Abschalten: options(y_label_oben = FALSE) bzw. pro Plot theme(axis.title.y = ...)
# Guard: _common.R wird mehrfach gesourct (.Rprofile + Kapitel); Original nur einmal sichern.
if (!exists(".ggplot_build_orig", envir = globalenv(), inherits = FALSE)) {
  assign(".ggplot_build_orig", utils::getS3method("ggplot_build", "ggplot2::ggplot", envir = asNamespace("ggplot2")), envir = globalenv())
}
.y_label_busy <- new.env()  # Re-Entranz-Schutz: get_labs() baut den Plot selbst
y_label_nach_oben <- function(plot) {
  if (!isTRUE(getOption("y_label_oben", TRUE))) return(plot)
  if (isTRUE(.y_label_busy$on)) return(plot)
  .y_label_busy$on <- TRUE
  on.exit(.y_label_busy$on <- FALSE)
  if (inherits(plot@coordinates, "CoordFlip")) return(plot)
  ti <- plot@theme$axis.title.y
  if (is.null(ti)) ti <- ggplot2::theme_get()$axis.title.y
  if (inherits(ti, "ggplot2::element_blank")) return(plot)
  ylab <- ggplot2::get_labs(plot)$y
  if (is.null(ylab) || !is.character(ylab) || !nzchar(ylab[1])) return(plot)
  sub <- ggplot2::get_labs(plot)$subtitle
  # y-Titel mit HTML (z. B. gruppen_schluessel()) per ggtext rendern
  md <- .ist_markdown(ylab[1])
  el <- if (md) ggtext::element_markdown else ggplot2::element_text
  plot +
    ggplot2::labs(subtitle = if (is.null(sub)) ylab else
                    paste0(sub, if (md) "<br>" else "\n", ylab)) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   plot.title.position = "panel",
                   plot.subtitle = el(hjust = 0, margin = ggplot2::margin(b = 6)))
}
.ist_markdown <- function(x) is.character(x) && grepl("<b|<span|&#", x)
# Tufte-Stil fuer Streu- und Liniendiagramme: keine Gitterlinien, Achsenlinien
# nur im Datenbereich (Range-Frame, ggthemes::geom_rangeframe) und ein grauer
# Rug (Datenpunkte als Striche statt klassischer Ticks).
# - Streudiagramme: Rohdaten als Punkte (geom_point), X und Y kontinuierlich,
#   mind. 10 Zeilen. Rug an beiden Achsen.
# - Liniendiagramme: geom_line/geom_path, mind. 10 Zeilen, X numerisch oder
#   Datum, Y numerisch. Rug pro Achse nur, wenn er etwas zeigt: X nur bei
#   unregelmaessigen X-Abstaenden oder < 50 Punkten (ein dichtes, regelmaessiges
#   Raster waere nur ein grauer Balken), Y immer.
# Ausnahmen (bleiben unveraendert): coord_flip(), Plots, die schon geom_rug()/
# geom_rangeframe() enthalten, Heatmaps (geom_raster/tile); geom_hex bekommt Range-Frame ohne Rug (Fuellung viridis), Punkt-Layer mit
# < 10 Zeilen (Hervorhebungspunkte). Dichten, Flaechen und stat_function sind
# keine Punkt-/Linien-Layer und werden nicht beruehrt.
# Abschalten: options(tufte_streu = FALSE) (alles) bzw.
# options(tufte_linie = FALSE) (nur Liniendiagramme), z. B. pro Chunk fuer eine
# geom_line, die eine berechnete Kurve statt Beobachtungen zeigt.
.tufte_streu_min_n <- 10
.tufte_rug_x_max_n <- 50
# Ab so vielen Punkten verschmilzt der Rug zu einem grauen Balken und zeigt
# nichts mehr: dann nur Range-Frame, kein Rug (Streu- und Liniendiagramme)
.tufte_rug_max_n <- 500

# Erster Layer der angegebenen Geoms mit genug Zeilen und passenden Daten
# (X numerisch oder Datum/Zeit, Y numerisch). NULL, wenn keiner passt.
.tufte_layer_daten <- function(plot, geoms) {
  tryCatch({
    for (l in plot@layers) {
      if (!inherits(l$geom, geoms)) next
      d <- l$data
      if (inherits(d, "waiver") || is.null(d)) d <- plot@data
      else if (is.function(d)) d <- d(plot@data)
      if (!is.data.frame(d) || nrow(d) < .tufte_streu_min_n) next
      m <- if (isFALSE(l$inherit.aes)) l$mapping else
        ggplot2::aes(!!!utils::modifyList(as.list(plot@mapping), as.list(l$mapping)))
      if (is.null(m$x) || is.null(m$y)) next
      x <- rlang::eval_tidy(m$x, d); y <- rlang::eval_tidy(m$y, d)
      x_ok <- is.numeric(x) || inherits(x, c("Date", "POSIXct"))
      if (!x_ok || !is.numeric(y) || length(x) < .tufte_streu_min_n) next
      return(list(d = d, m = ggplot2::aes(x = !!m$x, y = !!m$y), x = x, y = y))
    }
    NULL
  }, error = function(e) NULL)
}

tufte_streu <- function(plot) {
  if (!isTRUE(getOption("tufte_streu", TRUE))) return(plot)
  if (inherits(plot@coordinates, "CoordFlip")) return(plot)
  gtypes <- vapply(plot@layers, function(l) class(l$geom)[1], character(1))
  if (any(gtypes %in% c("GeomRug", "GeomRangeFrame"))) return(plot)
  # Heatmaps/Flaechen-Layer (Raster, Tile, Konturflaechen): kein Streudiagramm
  if (any(gtypes %in% c("GeomRaster", "GeomTile", "GeomContourFilled",
                        "GeomDensity2dFilled"))) return(plot)
  # Hex-Fliesen: Range-Frame + Ticks im Datenbereich, ohne Rug (die Fliesen sind
  # schon die Dichte); Fuellung bleibt viridis
  hex <- any(gtypes == "GeomHex")
  # Symbole per geom_text (z. B. ✓/✗ als Marker) zaehlen als Streudiagramm,
  # wenn es keinen Punkt-Layer gibt (sonst sind Texte nur Beschriftungen)
  res <- if (hex) .tufte_layer_daten(plot, "GeomHex") else {
    r <- .tufte_layer_daten(plot, "GeomPoint")
    if (is.null(r) && !"GeomPoint" %in% gtypes)
      r <- .tufte_layer_daten(plot, "GeomText")
    r
  }
  rug_seiten <- if (hex) "" else "bl"
  if (hex && !is.null(res) && is.null(plot@scales$get_scales("fill")))
    plot <- plot + ggplot2::scale_fill_viridis_c()
  # Liniendiagramm nur ohne Flaechen-Layer (Flaechen/Dichten = berechnete Formen)
  # und ohne Opt-out per "+ tufte_aus()"
  linie_ok <- isTRUE(getOption("tufte_linie", TRUE)) &&
    !isTRUE(plot@meta$tufte_aus) &&
    !any(gtypes %in% c("GeomArea", "GeomRibbon", "GeomDensity", "GeomPolygon"))
  if (is.null(res) && linie_ok) {
    res <- .tufte_layer_daten(plot, c("GeomLine", "GeomPath"))
    if (!is.null(res)) {
      xu <- sort(unique(as.numeric(res$x)))
      # "regelmaessig": Abstaende schwanken um < 15 % (auch Monatsdaten, 28-31 Tage)
      regelmaessig <- length(xu) > 2 && {
        dx <- diff(xu); max(dx) <= 1.15 * min(dx)
      }
      rug_x <- length(xu) < .tufte_rug_x_max_n || !regelmaessig
      rug_seiten <- if (rug_x) "bl" else "l"
    }
  }
  if (is.null(res)) return(plot)
  if (length(res$x) > .tufte_rug_max_n) rug_seiten <- ""

  grau <- "#4D4D4D"
  lw <- if (identical(plot_medium(), "folien")) 0.6 else 0.4
  # Breaks nur innerhalb des Datenbereichs (runde Breaks, aber keine Ticks
  # ueber die Achsenlinie hinaus); nur wenn der Plot keine eigene Skala setzt.
  nur_im_bereich <- function(v) {
    r <- range(v, na.rm = TRUE)
    function(lims) {
      b <- scales::extended_breaks()(lims)
      b[b >= r[1] - 1e-9 & b <= r[2] + 1e-9]
    }
  }
  if (is.null(plot@scales$get_scales("x")) && is.numeric(res$x))
    plot <- plot + ggplot2::scale_x_continuous(breaks = nur_im_bereich(res$x))
  if (is.null(plot@scales$get_scales("y")))
    plot <- plot + ggplot2::scale_y_continuous(breaks = nur_im_bereich(res$y))

  plot +
    ggthemes::geom_rangeframe(data = res$d, mapping = res$m, inherit.aes = FALSE,
                              color = grau, size = lw) +
    (if (nzchar(rug_seiten))
      ggplot2::geom_rug(data = res$d, mapping = res$m, inherit.aes = FALSE,
                        sides = rug_seiten, color = grau, alpha = 0.3, linewidth = 0.3,
                        length = grid::unit(0.02, "npc"))
     else NULL) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(color = grau, linewidth = lw),
                   axis.ticks.length = grid::unit(3, "pt"))
}

# Opt-out pro Plot: "+ tufte_aus()" laesst Streu-/Linien-Stil fuer diesen Plot weg
# (z. B. geom_line, die eine berechnete Kurve statt Beobachtungen zeigt).
tufte_aus <- function() structure(list(), class = "tufte_aus")
base::registerS3method("ggplot_add", "tufte_aus",
                       function(object, plot, object_name) {
                         plot@meta$tufte_aus <- TRUE
                         plot
                       },
                       envir = asNamespace("ggplot2"))

base::registerS3method("ggplot_build", "ggplot2::ggplot",
                 function(plot) .ggplot_build_orig(tufte_streu(y_label_nach_oben(plot))),
                 envir = asNamespace("ggplot2"))

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
                "|source\\(\\s*[\"'](\\./)?(R-Code|funs|children)/",  # eingelesene Plot-Skripte
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
# Gitter -- siehe globaler Skill ggplot-diagramme (~/.claude/skills).
dunkelgrau <- "#4D4D4D"   # Punkte bei 1 Gruppe
mittelgrau <- "#999999"   # Fuellung bei 1 Gruppe
gitterfarbe <- "#D9D9D9"  # Gitterlinien

# Tufte-Stil: Punkte und Linien (1 Gruppe) standardmaessig dunkelgrau; Hervorhebung
# nur bewusst in Orange/Dunkelblau (Okabe-Ito). Gilt nur fuer Layer ohne eigene
# Farbe; explizit gesetzte Farben bleiben unveraendert.
ggplot2::update_geom_defaults("point", list(colour = dunkelgrau))
ggplot2::update_geom_defaults("line",  list(colour = dunkelgrau))
ggplot2::update_geom_defaults("path",  list(colour = dunkelgrau))
# Regressionsgerade/Trend (geom_smooth, 1 Gruppe) ist die Kernaussage: als
# Hervorhebung in Okabe-Ito-Dunkelblau, etwas kraeftiger, Band blau getoent.
# So hebt sie sich von der grauen Punktwolke ab (Pop-out), auch dort, wo sich
# halbtransparente Punkte zu fast schwarzen Flaechen addieren.
trendfarbe <- "#0072B2"
ggplot2::update_geom_defaults("smooth", list(colour = trendfarbe, fill = trendfarbe,
                                             alpha = 0.2, linewidth = 1.2))
# Fuellung (1 Gruppe) standardmaessig mittelgrau. Bewusst NICHT fuer geom_density:
# dessen Default ist fill = NA (reine Linie); viele Dichten im Buch sind
# Umrisse. Gefuellte Dichten setzen fill = mittelgrau explizit.
ggplot2::update_geom_defaults("area", list(fill = mittelgrau))
ggplot2::update_geom_defaults("col",  list(fill = mittelgrau))
ggplot2::update_geom_defaults("bar",  list(fill = mittelgrau))

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

# Gruppen-Schluessel fuer den y-Titel (statt Legende, Skill ggplot-diagramme):
# je Gruppe Symbol + Name in der Textfarbe der Gruppe, fett. Liefert HTML;
# y_label_nach_oben() rendert es per ggtext::element_markdown. Beispiel:
#   labs(y = paste0("IQ des Kindes; Mutter ",
#                   gruppen_schluessel(c("ohne", "mit"), c(blue, orange)),
#                   " Schulabschluss"))
# `formen` wie in scale_shape_manual (16 Kreis, 17 Dreieck, 15 Quadrat, 18 Raute);
# Linienfarben werden automatisch auf die lesbare Textfarbe (okabe_text) abgebildet.
gruppen_formen <- c(16, 17, 15, 18)
.form_zeichen <- c(`16` = "&#9679;", `19` = "&#9679;", `17` = "&#9650;",
                   `15` = "&#9632;", `18` = "&#9670;")
textfarbe <- function(farbe) {
  f <- toupper(sub("^(#[0-9A-Fa-f]{6})FF$", "\\1", farbe))
  i <- match(f, okabe_linie)
  ifelse(is.na(i), f, okabe_text[i])
}
gruppen_schluessel <- function(namen, farben,
                               formen = gruppen_formen[seq_along(namen)],
                               trenner = " / ") {
  paste0("<b style='color:", textfarbe(farben), "'>",
         .form_zeichen[as.character(formen)], " ", namen, "</b>",
         collapse = trenner)
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
