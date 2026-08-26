#!/bin/bash
# Rendert das Buch als PDF im titlepage-pdf-Format (nmfs-opensci/titlepage
# Extension) statt im gewoehnlichen HTML-Format aus _quarto.yml.
#
# Format-Key MUSS "titlepage-pdf" heissen, siehe Kommentar in _quarto.yml,
# damit die Titelseiten-Zwischenseiten der Extension gezogen werden.
#
# Nutzung:
#   ./render-pdf.sh                    # ganzes Buch
#   ./render-pdf.sh 0250-inferenz.qmd  # einzelnes Kapitel
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

quarto render "$@" --to titlepage-pdf
