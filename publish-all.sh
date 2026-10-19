#!/bin/bash
# Rendert das Buch und die RevealJS-Foliendecks und veröffentlicht beides
# gemeinsam auf GitHub Pages (Branch gh-pages).
#
# Ablauf:
#   1. Buch rendern (Rscript render.R) -> docs/
#   2. Folien: Mermaid-Diagramme (nur geaenderte), alle Decks als HTML,
#      danach alle Decks als PDF (decktape). Reihenfolge wichtig: Die Decks
#      verweisen auf Abbildungen aus <kapitel>_files/, die Schritt 1 erzeugt.
#   3. img/ und die fertigen Foliendecks (slides/*.html, *.pdf, site_libs,
#      mermaid/*.png) nach docs/ spiegeln (keine Quelltexte/Skripte)
#   4. Pruefen, dass jedes Bild, auf das ein Deck verweist, in docs/ liegt
#      -- sonst Abbruch VOR dem Veroeffentlichen
#   5. docs/ 1:1 auf gh-pages veröffentlichen, ohne erneut zu rendern
#      (--no-render), da Schritt 1-3 den Stand bereits hergestellt haben.
#
# Option --skip-slide-pdfs: Schritt 2 ohne PDF-Export (spart einige
# Minuten, wenn sich an den Folien nichts geaendert hat; die vorhandenen
# slides/*.pdf werden dann unveraendert veroeffentlicht).
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

skip_slide_pdfs=false
[[ "${1:-}" == "--skip-slide-pdfs" ]] && skip_slide_pdfs=true

echo "==> Buch rendern..."
# NICHT "quarto render" (bare CLI): Das Buch definiert zwei Formate
# (html + titlepage-pdf, s. _quarto.yml). Ein einzelner "quarto render"-Lauf
# fuehrt den R-Code jedes Kapitels nur einmal aus und teilt das Ergebnis
# zwischen beiden Formaten -- dabei setzt sich der Kontext des zuerst
# gelisteten Formats (titlepage-pdf) durch, wodurch knitr::is_latex_output()
# auch waehrend der HTML-Ausgabe faelschlich TRUE liefert. Folge: Abbildungen
# werden als PDF statt PNG erzeugt und landen im HTML als <embed>-PDF statt
# <img>. render.R vermeidet das durch zwei getrennte quarto_render()-Aufrufe.
Rscript render.R

echo "==> Folien: Mermaid-Diagramme rendern (nur geänderte)..."
slides/render_mermaid.sh

echo "==> Folien: Decks als HTML rendern..."
(cd slides && quarto render)

if $skip_slide_pdfs; then
  echo "==> Folien: PDF-Export übersprungen (--skip-slide-pdfs)"
else
  echo "==> Folien: Decks als PDF exportieren..."
  slides/export_pdf.sh
fi

echo "==> Bilder für Foliendecks nach docs/img/ ergänzen..."
# quarto render kopiert nach docs/img nur Bilder, die vom Buch referenziert
# werden. Foliendecks referenzieren teils weitere Bilder aus img/ (z. B. das
# Logo) - diese ergänzend (additiv, ohne Löschen) nach docs/img/ spiegeln.
rsync -a img/ docs/img/

echo "==> Foliendecks nach docs/slides/ spiegeln..."
mkdir -p docs/slides
rsync -a --delete \
  --exclude='_quarto.yml' \
  --exclude='.quarto' \
  --exclude='.gitignore' \
  --exclude='*.qmd' \
  --exclude='*.mmd' \
  --exclude='*.sh' \
  --exclude='*.py' \
  slides/ docs/slides/

echo "==> Prüfen, ob alle Folien-Bilder in docs/ liegen..."
python3 slides/check_slide_assets.py

echo "==> Veröffentlichen auf gh-pages..."
quarto publish gh-pages --no-render --no-prompt

echo "==> Fertig: https://sebastiansauer.github.io/start-bayes/"
