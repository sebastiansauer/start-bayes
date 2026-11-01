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
#
# Option --verbose: komplette Ausgabe von R/Quarto/decktape anzeigen.
# Ohne diese Option zeigt das Skript nur das Wesentliche (Kapitel-Fortschritt
# "[n/N] datei.qmd", "Output created", Warnungen und Fehler, jeweils mit
# Zeitstempel). Die vollstaendige Ausgabe landet immer in der Logdatei
# (Pfad steht am Anfang der Ausgabe), z. B. zum Nachlesen von Warnungen.
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

skip_slide_pdfs=false
verbose=false
for arg in "$@"; do
  case "$arg" in
    --skip-slide-pdfs) skip_slide_pdfs=true ;;
    --verbose) verbose=true ;;
    *) echo "Unbekannte Option: $arg (erlaubt: --skip-slide-pdfs, --verbose)" >&2; exit 2 ;;
  esac
done

logfile="${TMPDIR:-/tmp}/publish-all-$(date +%Y%m%d-%H%M%S).log"
echo "Vollstaendiges Log: $logfile"

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

# Filtert die Ausgabe der Teilschritte auf das Wesentliche (siehe --verbose).
# Quarto faerbt seine Ausgabe ein; die Escape-Sequenzen werden vor dem
# Abgleich entfernt. Gezeigt werden: Kapitel-Fortschritt "[ 3/20] x.qmd",
# "Output created", Warnungen/Fehler von Quarto, R und knitr.
show_progress() {
  if $verbose; then cat; return; fi
  local line clean
  local re='^[[:space:]]*(\[[[:space:]]*[0-9]+/[0-9]+\]|Output created|WARN|Warning|Warnmeld|ERROR|Error|Fehler|Quitting from|Execution halted|Unable to|unable to)'
  while IFS= read -r line; do
    clean=$(printf '%s' "$line" | sed -E $'s/\x1b\\[[0-9;]*[A-Za-z]//g; s/\r//g')
    if [[ "$clean" =~ $re ]]; then
      printf '    [%s] %s\n' "$(elapsed)" "$clean"
    fi
  done
}

# Fuehrt einen Befehl aus: komplette Ausgabe ins Log, gefilterte Ausgabe auf
# den Bildschirm. Der Exit-Code des Befehls bleibt erhalten (pipefail).
run() {
  printf '\n$ %s\n' "$*" >> "$logfile"
  "$@" 2>&1 | tee -a "$logfile" | show_progress
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
run Rscript render.R

step "Folien: Mermaid-Diagramme rendern (nur geänderte)"
run slides/render_mermaid.sh

step "Folien: Decks als HTML rendern"
run bash -c 'cd slides && quarto render'

if $skip_slide_pdfs; then
  step "Folien: PDF-Export übersprungen (--skip-slide-pdfs)"
else
  step "Folien: Decks als PDF exportieren"
  run slides/export_pdf.sh
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
run quarto publish gh-pages --no-render --no-prompt

