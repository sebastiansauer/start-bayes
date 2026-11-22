#!/usr/bin/env bash
# Kopiert das gerenderte Video nach img/ und schneidet es in die sechs
# Phasen-Clips für die Folien (je mit Poster = letztes Frame).
# Aufruf aus dem Repo-Root:  videos/post-mu-regression/scripts/cut-clips.sh
set -euo pipefail
SRC=videos/post-mu-regression/renders/post-mu-regression.mp4
OUT=img/post-mu-regression
mkdir -p "$OUT"

cp "$SRC" img/post-mu-regression.mp4
# Standbild (Endzustand) als Poster des Buch-Videos
ffmpeg -y -loglevel error -sseof -0.1 -i "$SRC" -frames:v 1 img/post-mu-regression-poster.png

# Clip-Grenzen in Sekunden (wie CUTS in index.html); letzter Clip bis 18 s
CUTS=(0 3 5.5 9.5 12.5 15 18)
for i in 0 1 2 3 4 5; do
  a=${CUTS[$i]}; b=${CUTS[$((i+1))]}; n=$((i+1))
  ffmpeg -y -loglevel error -ss "$a" -to "$b" -i "$SRC" -an \
    -c:v libx264 -pix_fmt yuv420p -crf 20 -preset slow -movflags +faststart \
    "$OUT/clip-$n.mp4"
  # Poster: letztes Frame des Clips (für PDF-Export und vor dem Abspielen)
  ffmpeg -y -loglevel error -sseof -0.05 -i "$OUT/clip-$n.mp4" -frames:v 1 -update 1 "$OUT/clip-$n-end.png"
done
ls -la "$OUT" img/post-mu-regression.mp4
