
set.seed(42)

# --- Farben (Okabe-Ito, farbenblind-sicher; Gelb #F0E442 bewusst nicht) ---
# Folge ab 3 Gruppen (Reihenfolge: Orange, Blau, Gruen, Himmelblau, Zinnober,
# Rotviolett) und die abgedunkelten Textvarianten (fett, Kontrast auf Weiss).
okabe_linie <- c("#E69F00", "#0072B2", "#009E73", "#56B4E9", "#D55E00", "#CC79A7")
okabe_text  <- c("#B36B00", "#0072B2", "#007A59", "#2B7FB0", "#D55E00", "#A5527F")

# Kurznamen fuer die Okabe-Ito-Farben
orange <- okabe_linie[1]
blue   <- okabe_linie[2]
green  <- okabe_linie[3]

# Bedeutungsfarben fuer Modell-Abbildungen (Regression): feste Rolle je Farbe,
# alle aus okabe_linie
ycol     <- okabe_linie[1]  # beobachtetes Y
beta1col <- okabe_linie[2]  # Steigung beta1
errorcol <- okabe_linie[3]  # Fehler/Residuen
modelcol <- okabe_linie[4]  # Modellvorhersage
beta0col <- okabe_linie[5]  # Achsenabschnitt beta0
xcol     <- okabe_linie[6]  # Praediktor X

# ggplot-Diagramme-Skill: feste Farbwerte fuer 1/2 Gruppen, Punkte, Flaechen,
# Gitter -- siehe globaler Skill ggplot-diagramme (~/.claude/skills).
dunkelgrau <- "#4D4D4D"   # Punkte, Linien, Achsen bei 1 Gruppe
mittelgrau <- "#999999"   # Fuellung bei 1 Gruppe, Referenzlinien
gitterfarbe <- "#D9D9D9"  # Gitterlinien
hellgrau <- "#E5E5E5"     # helles Ende der grauen Sequenzskala, Hintergrundflaechen
fast_schwarz <- "#1A1A1A" # Text auf farbiger Flaeche (auf Dunkelblau: weiss)
trendfarbe <- okabe_linie[2]  # Trend-/Regressionsgerade ueber Punkten
trend_linienbreite <- 1.2     # Trend-/Modellgerade als Kernaussage (Pop-out)

# Weitere Kurznamen (Okabe-Ito) und die lesbare Textvariante von Orange
himmelblau <- okabe_linie[4]
zinnober   <- okabe_linie[5]
rotviolett <- okabe_linie[6]
orange_text <- okabe_text[1]





# Grossschrift-Variante (z.B. fuer Abbildungen mit viel Text): nutzt das
# Folien-Preset von theme_med() (base_size 19), siehe globaler Skill ggplot-diagramme (~/.claude/skills).
theme_large_text <- function() {
  theme_med("folien")
}



# ggplot-Diagramme-Skill: zentrales Theme + Mediumerkennung (HTML/PDF/Folien),
# siehe globaler Skill ggplot-diagramme (~/.claude/skills). Ersetzt das bisherige see::theme_modern().
# Schriftgroesse und duenne Linienstaerke je Medium (an einer Stelle)
.base_size <- function(medium = plot_medium()) {
  switch(medium, html = 13, pdf = 10.5, folien = 19)
}
.duenne_linie <- function(medium = plot_medium()) {
  if (identical(medium, "folien")) 0.6 else 0.4
}

theme_med <- function(medium = c("html", "pdf", "folien"),
                       grid = c("both", "h", "v", "none")) {
  medium <- match.arg(medium)
  grid <- match.arg(grid)
  gl <- ggplot2::element_line(color = gitterfarbe, linewidth = .duenne_linie(medium))
  off <- ggplot2::element_blank()

  ggplot2::theme_minimal(base_size = .base_size(medium)) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = off,
      panel.grid.major.x = if (grid %in% c("both", "v")) gl else off,
      panel.grid.major.y = if (grid %in% c("both", "h")) gl else off,
      axis.line = off,
      axis.ticks = off,
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      panel.spacing = grid::unit(1.2, "lines")
    )
}

# Leeres Theme (keine Achsen, kein Gitter) fuer Schemazeichnungen, Raster,
# Kruschke-Diagramme; gleiche Schriftgroesse wie theme_med() (statt
# theme_void() mit dessen fester Groesse 11)
theme_leer <- function(medium = plot_medium()) {
  ggplot2::theme_void(base_size = .base_size(medium)) +
    ggplot2::theme(legend.position = "bottom")
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
  # wirksames Element (auch geerbt, z. B. von theme_leer()/theme_void())
  ti <- tryCatch(ggplot2::calc_element("axis.title.y",
                                       ggplot2::theme_get() + plot@theme),
                 error = function(e) NULL)
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
# geom_rangeframe() enthalten, Heatmaps (geom_raster/tile); geom_hex bekommt Range-Frame ohne Rug (Fuellung grau), Punkt-Layer mit
# < 10 Zeilen (Hervorhebungspunkte). Dichten, Flaechen und stat_function sind
# keine Punkt-/Linien-Layer und werden nicht beruehrt.
# Abschalten: options(tufte_streu = FALSE) (alles) bzw.
# options(tufte_linie = FALSE) (nur Liniendiagramme), z. B. pro Chunk fuer eine
# geom_line, die eine berechnete Kurve statt Beobachtungen zeigt.
# --- Schwellenwerte des Diagrammstils (alle an einer Stelle) ---
# Ab so vielen Zeilen gilt ein Punkt-/Linien-Layer als Streu-/Liniendiagramm
.tufte_streu_min_n <- 10
# Liniendiagramm: X-Rug nur unter so vielen Punkten (oder bei unregelmaessigen X)
.tufte_rug_x_max_n <- 50
# Ab so vielen Punkten verschmilzt der Rug zu einem grauen Balken und zeigt
# nichts mehr: dann nur Range-Frame, kein Rug (Streu- und Liniendiagramme)
.tufte_rug_max_n <- 500
# Overplotting-Alpha fuer Punkt-Layer: Zeilen im Layer -> alpha (darunter alpha 1)
.tufte_alpha_stufen <- c(`100` = 0.6, `300` = 0.35, `1000` = 0.15)

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
  # DAGs/Netzwerke (ggdag, ggraph): Koordinaten sind Layout, keine Daten
  if (any(grepl("^Geom(Dag|DAG|Edge|Node)", gtypes))) return(plot)
  # Hex-Fliesen: Range-Frame + Ticks im Datenbereich, ohne Rug (die Fliesen sind
  # schon die Dichte); Fuellung als graue Sequenzskala (hell = wenige, dunkel =
  # viele Punkte), passend zum Grau der Punkte und ohne Gelb
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
  # (graue Fuellskala fuer Hex-Fliesen: globale Default-Skala, s. "Standard-Skalen")
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

  grau <- dunkelgrau
  lw <- .duenne_linie()
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
      tufte_rug(data = res$d, mapping = res$m, inherit.aes = FALSE,
                sides = rug_seiten)
     else NULL) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(color = grau, linewidth = lw),
                   axis.ticks.length = grid::unit(3, "pt"))
}

# Rug im Buchstil (graue, halbtransparente Striche an der Achse); auch fuer
# Dichten mit Rohdaten nutzbar: geom_density(...) + tufte_rug()
tufte_rug <- function(sides = "b", ...) {
  do.call(ggplot2::geom_rug, utils::modifyList(
    list(sides = sides, color = dunkelgrau, alpha = 0.3, linewidth = 0.3,
         length = grid::unit(0.02, "npc")),
    list(...)))
}

# Wilke-Gitter nur an der Werteachse (Skill Abschnitt 3): Histogramme,
# berechnete Kurven, Boxplots, Balken bekommen nur Gitterlinien quer zur
# Werteachse. Werteachse = y, ausser bei coord_flip() oder diskretem y (liegende
# Darstellung), dann x. Greift nur, wenn der Plot das Gitter nicht selbst setzt
# (theme(panel.grid...)); Streu-/Liniendiagramme regelt tufte_streu() (kein
# Gitter). Abschalten: options(wilke_gitter = FALSE).
.y_diskret <- function(plot) {
  tryCatch({
    sk <- plot@scales$get_scales("y")
    if (!is.null(sk)) return(sk$is_discrete())
    for (l in plot@layers) {
      m <- if (isFALSE(l$inherit.aes)) l$mapping else
        utils::modifyList(as.list(plot@mapping), as.list(l$mapping))
      if (is.null(m$y)) next
      d <- l$data
      if (inherits(d, "waiver") || is.null(d)) d <- plot@data
      else if (is.function(d)) d <- d(plot@data)
      y <- rlang::eval_tidy(m$y, d)
      return(is.character(y) || is.factor(y) || is.logical(y))
    }
    FALSE
  }, error = function(e) FALSE)
}
wilke_gitter <- function(plot) {
  if (!isTRUE(getOption("wilke_gitter", TRUE))) return(plot)
  th <- plot@theme
  eigen <- c("panel.grid", "panel.grid.major", "panel.grid.major.x",
             "panel.grid.major.y")
  if (any(vapply(eigen, function(e) !is.null(th[[e]]), logical(1)))) return(plot)
  # Theme-Elemente beziehen sich auf die angezeigte Lage (auch bei coord_flip)
  liegend <- inherits(plot@coordinates, "CoordFlip") || .y_diskret(plot)
  off <- ggplot2::element_blank()
  if (liegend) plot + ggplot2::theme(panel.grid.major.y = off)
  else plot + ggplot2::theme(panel.grid.major.x = off)
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

# Overplotting: Punkt-Layer (geom_point/geom_jitter) mit vielen Zeilen werden
# leicht durchsichtig, damit dichte Bereiche dunkler erscheinen. Gilt nur, wenn
# alpha weder fest gesetzt (geom_point(alpha = ...)) noch gemappt ist.
# Stufen (Zeilen im Layer -> alpha): < 100 -> 1 (unveraendert), ab 100 -> 0.6,
# ab 300 -> 0.35, ab 1000 -> 0.15 (.tufte_alpha_stufen, oben bei den
# Schwellenwerten). Abschalten: options(tufte_alpha = FALSE).
tufte_alpha <- function(plot) {
  if (!isTRUE(getOption("tufte_alpha", TRUE))) return(plot)
  for (i in seq_along(plot@layers)) {
    l <- plot@layers[[i]]
    if (!inherits(l$geom, "GeomPoint")) next
    if (!is.null(l$aes_params$alpha) || !is.null(l$mapping$alpha) ||
        (!isFALSE(l$inherit.aes) && !is.null(plot@mapping$alpha))) next
    d <- tryCatch({
      d <- l$data
      if (inherits(d, "waiver") || is.null(d)) plot@data
      else if (is.function(d)) d(plot@data) else d
    }, error = function(e) NULL)
    if (!is.data.frame(d)) next
    grenzen <- as.numeric(names(.tufte_alpha_stufen))
    stufe <- sum(nrow(d) >= grenzen)
    if (stufe == 0) next
    plot@layers[[i]] <- .layer_mit_alpha(l, unname(.tufte_alpha_stufen[stufe]))
  }
  plot
}
# Kopie des Layers (ggproto-Kind), damit das Plot-Objekt selbst unveraendert
# bleibt. Eigene Funktion, weil ggproto() den Eltern-Layer erst spaeter in der
# aufrufenden Umgebung auswertet -- in einer Schleife zeigte er sonst auf den
# zuletzt durchlaufenen Layer.
.layer_mit_alpha <- function(eltern, alpha) {
  ggplot2::ggproto(NULL, eltern, aes_params = c(eltern$aes_params, list(alpha = alpha)))
}

base::registerS3method("ggplot_build", "ggplot2::ggplot",
                 function(plot) .ggplot_build_orig(tufte_alpha(tufte_streu(wilke_gitter(y_label_nach_oben(plot))))),
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
  # Teilbilder (Skill Abschnitt 8): Stehen Abbildungen nebeneinander
  # (layout-ncol >= 2), wird das Bild kleiner gerechnet (2/3 der Breite), damit
  # Schrift, Punkte und Linien im halbbreiten Bild so gross wirken wie im
  # Einzelbild -- statt die Schrift pro Abbildung hochzusetzen. Nur, wenn der
  # Chunk keine eigene fig-width setzt. Abschalten pro Chunk: "#| teilbild: false";
  # erzwingen (z. B. bei Spalten per ::: {.column}): "#| teilbild: true".
  ncol <- suppressWarnings(as.numeric(options[["layout-ncol"]]))
  teilbild <- options$teilbild
  if (is.null(teilbild))
    teilbild <- length(ncol) == 1 && !is.na(ncol) && ncol >= 2 &&
      identical(options$fig.width, knitr::opts_chunk$get("fig.width"))
  if (isTRUE(teilbild) && !isTRUE(options$.teilbild_done)) {
    options$fig.width <- options$fig.width * 2 / 3
    options$.teilbild_done <- TRUE
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

   

# Tufte-Stil: Punkte und Linien (1 Gruppe) standardmaessig dunkelgrau; Hervorhebung
# nur bewusst in Orange/Dunkelblau (Okabe-Ito). Gilt nur fuer Layer ohne eigene
# Farbe; explizit gesetzte Farben bleiben unveraendert.
ggplot2::update_geom_defaults("point", list(colour = dunkelgrau))
ggplot2::update_geom_defaults("line",  list(colour = dunkelgrau))
ggplot2::update_geom_defaults("path",  list(colour = dunkelgrau))
# Bezugslinien (geom_vline/hline/abline) ebenfalls dunkelgrau statt schwarz
for (g in c("vline", "hline", "abline"))
  ggplot2::update_geom_defaults(g, list(colour = dunkelgrau))
# Regressionsgerade/Trend (geom_smooth, 1 Gruppe) ist die Kernaussage: als
# Hervorhebung in Okabe-Ito-Dunkelblau, etwas kraeftiger, Band blau getoent.
# So hebt sie sich von der grauen Punktwolke ab (Pop-out), auch dort, wo sich
# halbtransparente Punkte zu fast schwarzen Flaechen addieren.
ggplot2::update_geom_defaults("smooth", list(colour = trendfarbe, fill = trendfarbe,
                                             alpha = 0.2, linewidth = trend_linienbreite))
# Fuellung (1 Gruppe) standardmaessig mittelgrau. Bewusst NICHT fuer geom_density:
# dessen Default ist fill = NA (reine Linie); viele Dichten im Buch sind
# Umrisse. Gefuellte Dichten setzen fill = mittelgrau explizit.
ggplot2::update_geom_defaults("area", list(fill = mittelgrau))
# Balken und Histogrammsaeulen mit weissem Trennrand (Skill Abschnitt 6)
ggplot2::update_geom_defaults("col",  list(fill = mittelgrau, colour = "white"))
ggplot2::update_geom_defaults("bar",  list(fill = mittelgrau, colour = "white"))

# Violinen (1 Gruppe) wie andere Flaechen mittelgrau
ggplot2::update_geom_defaults("violin", list(fill = mittelgrau))
# Beschriftungskaesten (geom_label, annotate("label")): ohne Rahmen auf leicht
# durchsichtigem Weiss, damit sie ueber Punkten lesbar bleiben (Skill Abschnitt 5).
# Rahmen als Ausnahme: geom_label(..., linewidth = 0.25)
ggplot2::update_geom_defaults("label", list(fill = scales::alpha("white", 0.85),
                                            linewidth = 0))

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


# =====================================================================
# Zentrale Stil-Bausteine: Wiederkehrende Gestaltungsentscheidungen stehen
# hier, nicht in den einzelnen Abbildungen. In Kapiteln nur aufrufen bzw.
# fuer begruendete Ausnahmen ueberschreiben (jeder Baustein nimmt "..." an).
# =====================================================================

# --- Standard-Skalen ---
# Diskrete Farb-/Fuellskala: Okabe-Ito-Folge (Orange, Blau, Gruen, ...), ohne
# Gelb. Gilt fuer jedes gemappte color/fill ohne eigene Skala; ab 7 Gruppen
# faellt ggplot2 auf seine Standardpalette zurueck (dann ohnehin hervorheben
# oder facettieren). Kontinuierliche Fuellung (Hexbin, Dichte-Raster):
# graue Sequenzskala hell -> dunkelgrau.
options(
  ggplot2.discrete.colour = okabe_linie,
  ggplot2.discrete.fill = okabe_linie,
  ggplot2.continuous.fill = function(...)
    ggplot2::scale_fill_gradient(low = hellgrau, high = dunkelgrau, ...)
)

# Abweichende Reihenfolge der Okabe-Ito-Farben fuer eine Abbildung, z. B.
# scale_fill_gruppen(c(2, 1)) = Blau, Orange (Indizes in okabe_linie).
scale_colour_gruppen <- function(reihenfolge = seq_along(okabe_linie), ...) {
  ggplot2::scale_colour_manual(values = unname(okabe_linie[reihenfolge]), ...)
}
scale_color_gruppen <- scale_colour_gruppen
scale_fill_gruppen <- function(reihenfolge = seq_along(okabe_linie), ...) {
  ggplot2::scale_fill_manual(values = unname(okabe_linie[reihenfolge]), ...)
}

# ggpubr-Funktionen (ggline, gghistogram, ggbarplot, ...) setzen per Default
# ihr eigenes theme_pubr() und uebergingen so das Buch-Theme: hier durch ein
# leeres Theme ersetzen, sodass das globale Theme (theme_med) samt Automatik
# (z. B. wilke_gitter) greift.
# Ebenso deren Farb-Defaults (schwarze Linien, weisse/hellgraue Fuellung) auf
# die Buchfarben: Linien/Punkte dunkelgrau, Flaechen mittelgrau, Balken mit
# weissem Rand. Muss vor library(ggpubr) laufen (das Anhaengen kopiert dann die
# geaenderten Funktionen) -- _common.R wird per .Rprofile vorab geladen.
if (requireNamespace("ggpubr", quietly = TRUE)) {
  utils::assignInNamespace("theme_pubr", function(...) ggplot2::theme(),
                           ns = "ggpubr")
  # Die oeffentlichen Funktionen reichen nur vom Nutzer gesetzte Argumente an
  # interne *_core-Funktionen weiter; deren Defaults zaehlen daher mit.
  .ggpubr_defaults <- function(fn, color, fill = NULL) {
    for (f_name in c(fn, paste0(fn, "_core"))) {
      if (!exists(f_name, envir = asNamespace("ggpubr"), inherits = FALSE)) next
      f <- get(f_name, envir = asNamespace("ggpubr"))
      fm <- formals(f)
      fm$color <- color
      if (!is.null(fill) && "fill" %in% names(fm)) fm$fill <- fill
      formals(f) <- fm
      utils::assignInNamespace(f_name, f, ns = "ggpubr")
    }
  }
  for (fn in c("ggline", "ggscatter", "ggdensity", "ggboxplot", "ggviolin",
               "ggstripchart", "ggdotchart", "ggecdf", "ggqqplot"))
    .ggpubr_defaults(fn, color = dunkelgrau, fill = mittelgrau)
  for (fn in c("ggbarplot", "gghistogram"))
    .ggpubr_defaults(fn, color = "white", fill = mittelgrau)
  # Der gemeinsame interne Zeichner .plotter() hat eigene Defaults und reicht
  # sie explizit weiter: Balken/Histogramm weisser Rand, sonst dunkelgrau.
  .ggpubr_defaults(".plotter",
                   color = bquote(if (isTRUE(fun_name == "barplot") ||
                                      identical(fun, gghistogram_core))
                                    "white" else .(dunkelgrau)),
                   fill = mittelgrau)
}

# Intervall-Plots von see/bayestestR (plot(hdi()), plot(eti()), plot(rope())):
# Blau/Orange statt Orange/Blau -- einheitlich fuer alle Intervall-Abbildungen
scale_fill_intervall <- function(...) scale_fill_gruppen(c(2, 1), ...)

# --- Referenzlinien (Skill Abschnitt 5): duenn, gestrichelt, grau ---
# referenzlinie(yintercept = 1/6), referenzlinie(xintercept = c(a, b)),
# referenzlinie(intercept = b0, slope = b1). Abweichungen per "...".
referenzlinie <- function(xintercept = NULL, yintercept = NULL,
                          intercept = NULL, slope = NULL, ...) {
  stil <- utils::modifyList(
    list(linetype = "dashed", colour = mittelgrau, linewidth = .duenne_linie()),
    list(...))
  if (!is.null(xintercept))
    return(do.call(ggplot2::geom_vline, c(list(xintercept = xintercept), stil)))
  if (!is.null(yintercept))
    return(do.call(ggplot2::geom_hline, c(list(yintercept = yintercept), stil)))
  do.call(ggplot2::geom_abline, c(list(intercept = intercept, slope = slope), stil))
}
# Wert einer Referenzlinie als Text daneben (kein Label-Kasten)
referenz_text <- function(x, y, label, ...) {
  do.call(ggplot2::annotate, utils::modifyList(
    list(geom = "text", x = x, y = y, label = label, colour = dunkelgrau),
    list(...)))
}

# --- Gitter und Achsen je Diagrammtyp (Skill Abschnitt 3) ---
# Kein Gitter (z. B. Kurven mit eigenem Range-Frame, Schemazeichnungen)
ohne_gitter <- function() {
  off <- ggplot2::element_blank()
  ggplot2::theme(panel.grid = off, panel.grid.major = off,
                 panel.grid.major.x = off, panel.grid.major.y = off,
                 panel.grid.minor = off)
}
# Wilke-Gitter nur an der Werteachse (Histogramm, berechnete Kurve: "y";
# liegende Boxplots/Intervalle: "x")
gitter_nur <- function(achse = c("y", "x")) {
  achse <- match.arg(achse)
  off <- ggplot2::element_blank()
  if (achse == "y") ggplot2::theme(panel.grid.major.x = off, panel.grid.minor = off)
  else ggplot2::theme(panel.grid.major.y = off, panel.grid.minor = off)
}
# Keine y-Achse (Dichte, deren Hoehe nicht abgelesen werden soll)
ohne_y_achse <- function() {
  off <- ggplot2::element_blank()
  list(ggplot2::scale_y_continuous(NULL, breaks = NULL),
       ggplot2::theme(axis.title.y = off, axis.text.y = off,
                      axis.ticks.y = off, axis.line.y = off))
}
# Range-Frame (Tufte) fuer Kurven/Flaechen, die tufte_streu() bewusst
# auslaesst (z. B. Posterior als Flaeche): Achsenlinie nur im Datenbereich,
# Ticks statt Gitter.
tufte_rahmen <- function(data, mapping) {
  list(
    ggthemes::geom_rangeframe(data = data, mapping = mapping, inherit.aes = FALSE,
                              color = dunkelgrau, size = .duenne_linie()),
    ohne_gitter(),
    ggplot2::theme(axis.ticks = ggplot2::element_line(color = dunkelgrau,
                                                      linewidth = .duenne_linie()),
                   axis.ticks.length = grid::unit(3, "pt"))
  )
}
# Groessere Achsenbeschriftung, wenn die Achsentexte selbst die Aussage tragen
# (z. B. Mengendiagramme mit "A"/"nicht A")
achsentext_gross <- function(faktor = 1.4) {
  ggplot2::theme(axis.text = ggplot2::element_text(size = ggplot2::rel(faktor)))
}

# --- Balkendiagramme (Skill Abschnitt 6) ---
# Werte stehen am Balken: keine y-Achsenbeschriftung, kein Gitter, keine
# Legende (der y-Titel bleibt, falls gesetzt)
theme_balken <- function() {
  off <- ggplot2::element_blank()
  ggplot2::theme(legend.position = "none",
                 axis.text.y = off,
                 panel.grid.major.x = off, panel.grid.major.y = off,
                 panel.grid.minor = off)
}
# Gestapelte/kategoriale Balken mit kraeftiger weisser Trennlinie
geom_balken <- function(..., width = 0.85) {
  ggplot2::geom_col(..., colour = "white", linewidth = 1.2, width = width)
}
# Lesbare Textfarbe auf einer Balkenfuellung: weiss auf Dunkelblau, sonst fast schwarz
balken_textfarbe <- function(fuellung) {
  ifelse(toupper(substr(fuellung, 1, 7)) == toupper(blue), "white", fast_schwarz)
}






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
