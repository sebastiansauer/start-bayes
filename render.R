# Bewusst zwei getrennte quarto_render()-Aufrufe (ein Aufruf pro Format)
# statt eines einzigen quarto_render() ohne as_output_format/"--to": Wird ein
# Buchprojekt mit mehreren konfigurierten Formaten (hier: html UND
# titlepage-pdf, s. _quarto.yml) in EINEM Aufruf gerendert, fuehrt Quarto den
# R-Code jedes Kapitels nur EINMAL aus und teilt das Ergebnis (inkl. der
# bereits erzeugten Abbildungen) zwischen den Formaten -- dabei setzt sich
# der Kontext des zuerst in _quarto.yml gelisteten Formats durch
# (titlepage-pdf steht vor html), wodurch knitr::is_latex_output() faelschlich
# TRUE liefert, selbst waehrend der HTML-Ausgabe erzeugt wird. Sichtbare Folge:
# Abbildungen werden als PDF statt PNG erzeugt und landen im HTML als
# <embed>-PDF statt <img> -- der Browser zeigt dafuer seine eigene
# PDF-Viewer-Leiste samt Scrollbalken um jede Abbildung an. Getrennte Aufrufe
# je Format vermeiden das zuverlaessig, da jeder Aufruf eine eigene,
# formatspezifische R-Ausfuehrung anstoesst.
quarto::quarto_render(
  execute = TRUE,
  cache = TRUE,
  as_job = FALSE,
  output_format = "html"
)

# --no-clean: ein Buchprojekt-Render raeumt output-dir (docs/) standardmaessig
# zu Beginn auf -- ohne dieses Flag wuerde der zweite (PDF-)Aufruf die vom
# ersten (HTML-)Aufruf gerade erzeugten *.html-Dateien wieder loeschen.
quarto::quarto_render(
  execute = TRUE,
  cache = TRUE,
  as_job = FALSE,
  output_format = "titlepage-pdf",
  quarto_args = "--no-clean"
)
