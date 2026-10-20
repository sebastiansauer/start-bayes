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

# --- Fortschrittsanzeige ---------------------------------------------------
# Jeder Schritt meldet sich als "[n/N] Text" mit der bisher verstrichenen
# Zeit; am Ende (oder bei Abbruch) die Gesamtdauer und ggf. der Schritt, in
# dem es schiefging. Farbig nur, wenn die Ausgabe ein Terminal ist.
TOTAL_STEPS=8
current_step=0
current_label="Start"
start_time=$(date +%s)
if [[ -t 1 ]]; then c_on=$'\e[1;34m'; c_ok=$'\e[1;32m'; c_err=$'\e[1;31m'; c_off=$'\e[0m'
else c_on=""; c_ok=""; c_err=""; c_off=""; fi

elapsed() {
  local s=$(( $(date +%s) - start_time ))
  printf '%dm%02ds' $((s / 60)) $((s % 60))
}

step() {
  current_step=$((current_step + 1))
  current_label="$1"
  printf '\n%s==> [%d/%d] %s%s  (bisher %s)\n' \
    "$c_on" "$current_step" "$TOTAL_STEPS" "$current_label" "$c_off" "$(elapsed)"
}

on_exit() {
  local rc=$?
  if [[ $rc -eq 0 ]]; then
    printf '\n%s==> Fertig nach %s: https://sebastiansauer.github.io/start-bayes/%s\n' \
      "$c_ok" "$(elapsed)" "$c_off"
  else
    printf '\n%s==> ABGEBROCHEN in Schritt %d/%d (%s) nach %s, Exit-Code %d%s\n' \
      "$c_err" "$current_step" "$TOTAL_STEPS" "$current_label" "$(elapsed)" "$rc" "$c_off" >&2
  fi
}
trap on_exit EXIT

step "Buch rendern (PDF, dann HTML)"
# NICHT "quarto render" (bare CLI): Das Buch definiert zwei Formate
# (html + titlepage-pdf, s. _quarto.yml). Ein einzelner "quarto render"-Lauf
# fuehrt den R-Code jedes Kapitels nur einmal aus und teilt das Ergebnis
# zwischen beiden Formaten -- dabei setzt sich der Kontext des zuerst
# gelisteten Formats (titlepage-pdf) durch, wodurch knitr::is_latex_output()
# auch waehrend der HTML-Ausgabe faelschlich TRUE liefert. Folge: Abbildungen
# werden als PDF statt PNG erzeugt und landen im HTML als <embed>-PDF statt
# <img>. render.R vermeidet das durch zwei getrennte quarto_render()-Aufrufe.
Rscript render.R

step "Folien: Mermaid-Diagramme rendern (nur geänderte)"
slides/render_mermaid.sh

step "Folien: Decks als HTML rendern"
(cd slides && quarto render)

if $skip_slide_pdfs; then
  step "Folien: PDF-Export übersprungen (--skip-slide-pdfs)"
else
  step "Folien: Decks als PDF exportieren"
  slides/export_pdf.sh
fi

step "Bilder für Foliendecks nach docs/img/ ergänzen"
# quarto render kopiert nach docs/img nur Bilder, die vom Buch referenziert
# werden. Foliendecks referenzieren teils weitere Bilder aus img/ (z. B. das
# Logo) - diese ergänzend (additiv, ohne Löschen) nach docs/img/ spiegeln.
rsync -a img/ docs/img/

step "Foliendecks nach docs/slides/ spiegeln"
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

step "Prüfen, ob alle Folien-Bilder in docs/ liegen"
python3 slides/check_slide_assets.py

step "Veröffentlichen auf gh-pages"
quarto publish gh-pages --no-render --no-prompt

