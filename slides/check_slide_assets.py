#!/usr/bin/env python3
"""Prueft, ob alle Bilder/Videos, auf die die gerenderten Foliendecks
(docs/slides/*.html) verweisen, im Veroeffentlichungsordner docs/ liegen.

Hintergrund: Die Decks verweisen per "../img/..." und
"../<kapitel>_files/figure-html/..." auf Dateien ausserhalb von slides/.
Lokal existieren diese oft noch (alte Render-Reste), auf GitHub Pages aber
nur, wenn das Buch-Render sie nach docs/ kopiert hat -- z. B. fehlen dort
Abbildungen aus auskommentierten Buchabschnitten. Dieses Skript findet
solche Luecken, bevor veroeffentlicht wird.

Aufruf (aus dem Repo-Root, nachdem slides/ nach docs/slides/ gespiegelt
wurde): python3 slides/check_slide_assets.py
Exit-Code 0 = alles da, 1 = mindestens ein Verweis fehlt.
"""
import glob
import os
import re
import sys

DOCS = "docs"
PATTERN = re.compile(
    r'(?:src|href|data-src)="([^"#?]+\.(?:png|jpe?g|svg|gif|webp|mp4|webm|pdf))"',
    re.IGNORECASE,
)

missing = []
decks = sorted(glob.glob(os.path.join(DOCS, "slides", "*.html")))
if not decks:
    sys.exit(f"Keine Decks in {DOCS}/slides/ gefunden -- erst slides/ spiegeln.")

for deck in decks:
    html = open(deck, encoding="utf-8").read()
    for ref in sorted(set(PATTERN.findall(html))):
        if re.match(r"^[a-z]+:", ref) or ref.startswith("//"):
            continue  # externe URL
        target = os.path.normpath(os.path.join(os.path.dirname(deck), ref))
        if not os.path.exists(target):
            missing.append((deck, ref))

for deck, ref in missing:
    print(f"FEHLT: {deck} -> {ref}")
print(f"{len(decks)} Decks geprueft, {len(missing)} fehlende Verweise.")
sys.exit(1 if missing else 0)
