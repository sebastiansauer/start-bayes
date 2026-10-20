# Bewusst zwei getrennte Quarto-CLI-Aufrufe (ein Aufruf pro Format) statt
# eines einzigen "quarto render" ohne "--to": Wird ein Buchprojekt mit
# mehreren konfigurierten Formaten (hier: html UND titlepage-pdf, s.
# _quarto.yml) in EINEM Aufruf gerendert, fuehrt Quarto den R-Code jedes
# Kapitels nur EINMAL aus und teilt das Ergebnis (inkl. der bereits
# erzeugten Abbildungen) zwischen den Formaten -- dabei setzt sich der
# Kontext des zuerst in _quarto.yml gelisteten Formats durch (titlepage-pdf
# steht vor html), wodurch knitr::is_latex_output() faelschlich TRUE
# liefert, selbst waehrend die HTML-Ausgabe erzeugt wird. Sichtbare Folge:
# Abbildungen werden als PDF statt PNG erzeugt und landen im HTML als
# <embed>-PDF statt <img> -- der Browser zeigt dafuer seine eigene
# PDF-Viewer-Leiste samt Scrollbalken um jede Abbildung an. Getrennte
# Aufrufe je Format vermeiden das zuverlaessig, da jeder Aufruf eine
# eigene, formatspezifische R-Ausfuehrung anstoesst.
#
# Bewusst system2("quarto", ...) statt quarto::quarto_render(): Der
# R-Wrapper quarto::quarto_render(output_format = "html") kopiert beim
# HTML-Format nicht alle Core-Assets (site_libs/bootstrap,
# site_libs/quarto-html, site_libs/quarto-nav, site_libs/quarto-search,
# site_libs/quarto-contrib, site_libs/quarto-diagram, site_libs/quarto-ojs)
# nach docs/site_libs/ -- sichtbare Folge: die veroeffentlichte Seite laedt
# ohne Bootstrap-CSS/JS und ist optisch "zerschossen" (404 auf diese
# Dateien). Der direkte CLI-Aufruf "quarto render --to <format>" erzeugt
# docs/site_libs/ vollstaendig und korrekt.
#
# Reihenfolge bewusst PDF VOR html (nicht umgekehrt): Jeder Render-Aufruf
# synchronisiert docs/site_libs/ projektweit auf genau die Assets, die das
# GERADE gerenderte Format braucht -- titlepage-pdf braucht kein
# Bootstrap/quarto-html/quarto-nav/quarto-search/quarto-contrib/
# quarto-diagram/quarto-ojs und entfernt diese Ordner wieder aus
# docs/site_libs/, selbst mit --no-clean (das verhindert nur das Loeschen
# der schon erzeugten *.html-/*.pdf-Ausgabedateien, nicht die
# formatspezifische site_libs-Synchronisation). Rendert man zuletzt html,
# bringt dieser letzte Aufruf alle fuer HTML noetigen Assets wieder zurueck
# -- der publizierte Stand in docs/site_libs/ ist dann korrekt fuer HTML.

# Freeze-Schutz: _quarto.yml setzt "freeze: auto" -- beim Projekt-Render
# fuehrt Quarto den R-Code eines Kapitels nur aus, wenn sich dessen .qmd
# geaendert hat, sonst kommen Ergebnisse UND Abbildungen aus _freeze/.
# Aenderungen an Hilfsdateien (funs/, R-Code/, children/*.R, _common.R)
# kaeme dann weder beim knitr-Cache-Schutz in _common.R an, noch wuerden
# Abbildungen neu gezeichnet. Daher: Hat sich der Hash dieser Dateien seit
# dem letzten erfolgreichen Render geaendert, _freeze/ verwerfen. Teure
# Modell-Chunks kommen danach weiter aus dem knitr-Cache (*_cache/).
if (!exists("helper_files_hash")) source("_common.R")
freeze_stamp <- "_freeze/.helper-files-hash"
old_hash <- if (file.exists(freeze_stamp)) readLines(freeze_stamp) else ""
if (!identical(old_hash, helper_files_hash)) {
  message("Hilfsdateien geaendert -> _freeze/ wird verworfen, alle Kapitel werden ausgefuehrt.")
  unlink("_freeze", recursive = TRUE)
}

status_pdf <- system2(
  "quarto",
  c("render", "--to", "titlepage-pdf"),
  stdout = "", stderr = ""
)
if (status_pdf != 0) stop("quarto render --to titlepage-pdf ist fehlgeschlagen")

# --no-clean: ein Buchprojekt-Render raeumt output-dir (docs/) standardmaessig
# zu Beginn auf -- ohne dieses Flag wuerde dieser zweite (HTML-)Aufruf das
# gerade erzeugte PDF wieder loeschen.
status_html <- system2(
  "quarto",
  c("render", "--to", "html", "--no-clean"),
  stdout = "", stderr = ""
)
if (status_html != 0) stop("quarto render --to html ist fehlgeschlagen")

# Erst nach erfolgreichem Render festhalten, auf welchem Stand _freeze/ ist.
dir.create("_freeze", showWarnings = FALSE)
writeLines(helper_files_hash, freeze_stamp)
