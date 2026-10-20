#!/usr/bin/env python3
"""Erzeugt netlify-redirect/_redirects: leitet die alte Netlify-Kopie des
Buchs (start-bayes.netlify.app, seit 2026 nicht mehr aktualisiert) dauerhaft
(301) auf GitHub Pages um.

Netlify lieferte Seiten mit "Pretty URLs" kleingeschrieben und ohne .html aus
(z. B. /0400-verteilungen), GitHub Pages unterscheidet aber Gross-/Klein-
schreibung (/0400-Verteilungen.html). Daher je Seite eigene Regeln fuer alle
Schreibweisen; alles Uebrige (Bilder, Daten, ...) per Sammelregel 1:1.

Aufruf aus dem Repo-Root, nach einem Render (liest docs/):
    python3 netlify-redirect/make_redirects.py
Danach den Ordner netlify-redirect/ auf Netlify deployen (s. README.md dort).
"""
import glob
import os

TARGET = "https://sebastiansauer.github.io/start-bayes"
OUT = os.path.join(os.path.dirname(__file__), "_redirects")

pages = sorted(
    os.path.relpath(p, "docs")
    for p in glob.glob("docs/*.html") + glob.glob("docs/slides/*.html")
)

lines = [
    "# Automatisch erzeugt von make_redirects.py -- nicht von Hand bearbeiten.",
    f"/  {TARGET}/  301!",
]
for page in pages:
    stem = page[: -len(".html")]
    target = f"{TARGET}/{page}"
    variants = {f"/{stem.lower()}", f"/{stem}", f"/{page}", f"/{page.lower()}"}
    if stem == "index":
        continue  # durch "/" und die Sammelregel abgedeckt
    for v in sorted(variants):
        lines.append(f"{v}  {target}  301!")
lines.append(f"/*  {TARGET}/:splat  301!")

with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(lines) + "\n")
print(f"{OUT}: {len(pages)} Seiten, {len(lines) - 1} Regeln")
