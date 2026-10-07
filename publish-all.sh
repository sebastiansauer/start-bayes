#!/bin/bash
# Rendert das Buch und die RevealJS-Foliendecks und veröffentlicht beides
# gemeinsam auf GitHub Pages (Branch gh-pages).
#
# Ablauf:
#   1. Buch rendern (Rscript render.R) -> docs/
#   2. Foliendecks (slides/*.html + slides/site_libs) nach docs/slides/
#      spiegeln (nur die fertigen Ausgaben, keine Quelltexte/Cache)
#   3. docs/ 1:1 auf gh-pages veröffentlichen, ohne erneut zu rendern
#      (--no-render), da Schritt 1+2 den Stand bereits hergestellt haben.
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

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
  slides/ docs/slides/

echo "==> Veröffentlichen auf gh-pages..."
quarto publish gh-pages --no-render --no-prompt

echo "==> Fertig: https://sebastiansauer.github.io/start-bayes/"
