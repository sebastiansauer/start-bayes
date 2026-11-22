#!/usr/bin/env bash
# Kopiert das zuletzt gerenderte Video nach img/ und erzeugt das Poster
# (letztes Frame = Überblick wie Abb. 9.4; für Folien-PDF und vor dem Abspielen).
# Aufruf aus dem Repo-Root:  videos/kung-modell-priori/scripts/export.sh
# Vorher rendern:  (cd videos/kung-modell-priori && npm run render)
set -euo pipefail
SRC=$(ls -t videos/kung-modell-priori/renders/*.mp4 | head -1)
cp "$SRC" img/kung-modell-priori.mp4
ffmpeg -y -loglevel error -sseof -0.1 -i "$SRC" -frames:v 1 -update 1 img/kung-modell-priori-poster.png
ls -la img/kung-modell-priori.mp4 img/kung-modell-priori-poster.png
