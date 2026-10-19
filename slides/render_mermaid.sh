#!/usr/bin/env bash
# Rendert jedes Mermaid-Diagramm slides/mermaid/<name>.mmd zu
# slides/mermaid/<name>.png (via mermaid-cli). Die Foliendecks binden diese
# PNGs als normale Bilder ein, statt ```{mermaid}-Bloecke clientseitig
# rendern zu lassen.
#
# Warum: In RevealJS misst Mermaid die Knotentexte, waehrend Reveal die Folie
# per CSS-Transform skaliert. Die Textfelder werden dadurch zu klein
# berechnet, und die Beschriftungen erscheinen abgeschnitten -- im Browser
# wie im PDF-Export (decktape). Vorab gerenderte Bilder haben das Problem
# nicht.
#
# Aufruf: slides/render_mermaid.sh            (nur geaenderte .mmd)
#         slides/render_mermaid.sh --force    (alle neu)
# Danach die betroffenen Decks neu rendern ("quarto render" in slides/) und
# ggf. export_pdf.sh laufen lassen.
set -euo pipefail

cd "$(dirname "$0")/mermaid"

# Mermaid-Version passend zu der in Quarto gebuendelten (Quarto 1.9: 11.12.0),
# damit Layout und KaTeX-Formeln ($$...$$ in Labels) wie im Buch aussehen.
MMDC_PKG="@mermaid-js/mermaid-cli@11.12.0"

# Das gebuendelte Puppeteer-Chromium startet hier sonst mit
# "No usable sandbox!" ab (wie bei decktape in export_pdf.sh).
PUPPETEER_CFG="$(mktemp)"
trap 'rm -f "$PUPPETEER_CFG"' EXIT
echo '{"args":["--no-sandbox"]}' > "$PUPPETEER_CFG"

force=false
[[ "${1:-}" == "--force" ]] && force=true

for mmd in *.mmd; do
  png="${mmd%.mmd}.png"
  if ! $force && [[ -f "$png" && "$png" -nt "$mmd" ]]; then
    continue
  fi
  echo "=== $mmd -> $png ==="
  # -s 3: dreifache Aufloesung, damit die Bilder auch im PDF scharf sind.
  npx --yes -p "$MMDC_PKG" mmdc -q -p "$PUPPETEER_CFG" \
    -t neutral -b white -s 3 -i "$mmd" -o "$png"
done
