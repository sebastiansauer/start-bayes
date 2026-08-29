#!/usr/bin/env bash
# Konvertiert ein PDF in Grauskalen (für den KDP-Print-Innenteil).
# Das Cover NICHT damit konvertieren - KDP erwartet ein farbiges Cover.
#
# Nutzung:
#   ./convert-pdf-grayscale.sh [input.pdf] [output.pdf]
#
# Ohne Argumente wird "docs/Start-Bayes!.pdf" eingelesen und
# "docs/Start-Bayes!-grayscale.pdf" erzeugt.

set -euo pipefail

INPUT="${1:-docs/Start-Bayes!.pdf}"
OUTPUT="${2:-${INPUT%.pdf}-grayscale.pdf}"

if ! command -v gs >/dev/null 2>&1; then
  echo "Fehler: Ghostscript (gs) ist nicht installiert." >&2
  echo "Installation z. B. mit: sudo apt install ghostscript" >&2
  exit 1
fi

if [[ ! -f "$INPUT" ]]; then
  echo "Fehler: Eingabedatei nicht gefunden: $INPUT" >&2
  exit 1
fi

echo "Konvertiere '$INPUT' -> '$OUTPUT' (Grauskalen, druckoptimiert)..."

gs \
  -sOutputFile="$OUTPUT" \
  -sDEVICE=pdfwrite \
  -sColorConversionStrategy=Gray \
  -sColorConversionStrategyForImages=Gray \
  -dProcessColorModel=/DeviceGray \
  -dCompatibilityLevel=1.4 \
  -dNOPAUSE \
  -dBATCH \
  -dSAFER \
  -dAutoRotatePages=/None \
  -q \
  "$INPUT"

echo "Fertig: $OUTPUT"
