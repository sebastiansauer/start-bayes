#!/bin/bash
# Rendert das Buch und die RevealJS-Foliendecks und veröffentlicht beides
# gemeinsam auf GitHub Pages (Branch gh-pages).
#
# Ablauf:
#   1. Buch rendern (quarto render) -> docs/
#   2. Foliendecks (slides/*.html + slides/site_libs) nach docs/slides/
#      spiegeln (nur die fertigen Ausgaben, keine Quelltexte/Cache)
#   3. docs/ 1:1 auf gh-pages veröffentlichen, ohne erneut zu rendern
#      (--no-render), da Schritt 1+2 den Stand bereits hergestellt haben.
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

echo "==> Buch rendern..."
quarto render

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
