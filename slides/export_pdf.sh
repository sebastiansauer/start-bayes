#!/usr/bin/env bash
# Erzeugt aus jedem gerenderten HTML-Foliensatz in docs/slides/ ein PDF
# (docs/slides/<kapitel>.pdf) via decktape. Manuell auszufuehren, sobald
# sich ein Foliensatz inhaltlich geaendert hat (nicht Teil von render.R).
#
# Voraussetzung: docs/slides/*.html ist aktuell (vorher ggf. in slides/
# "quarto render" laufen lassen).
set -euo pipefail

cd "$(dirname "$0")/.."
SLIDES_DIR="docs/slides"
PORT=8743

decks=(
  0200-zielarten
  0250-inferenz
  0300-Wskt
  0350-wskt2
  0400-Verteilungen
  0500-Globusversuch
  0600-Post
  0800-gauss
  0900-lineare-modelle
  1000-metrische-AV
  1050-Schaetzen-Testen
  1150-konfundierung
  1180-kausalatome
  1200-abschluss
)

npx --yes http-server "$SLIDES_DIR" -p "$PORT" -s &
SERVER_PID=$!
trap 'kill "$SERVER_PID" 2>/dev/null || true' EXIT

sleep 2

for deck in "${decks[@]}"; do
  echo "=== $deck ==="
  # --chrome-arg=--no-sandbox: das gebuendelte Puppeteer-Chromium startet in
  # dieser Umgebung sonst mit "No usable sandbox!" (FATAL) ab.
  npx --yes decktape --chrome-arg=--no-sandbox reveal \
    "http://localhost:${PORT}/${deck}.html" "${SLIDES_DIR}/${deck}.pdf"
done
