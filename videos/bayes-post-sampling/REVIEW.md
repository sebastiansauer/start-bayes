# REVIEW — bayes-post-sampling (Stand 2026-11-11, nach der Vertonung)

## Was gebaut wurde

Acht Szenen als Sub-Kompositionen (`compositions/*.html`), ein dünner Orchestrator
(`index.html`), Daten aus `scripts/build-data.mjs` (fester Seed), Zeitmarken aus
`scripts/build-timing.mjs`. Alle Cues hängen an Ankerwörtern des Skripts
(`scripts/anchors.json`); solange keine Azure-Wortzeiten vorliegen, werden sie aus
der Zeichenzahl geschätzt (15 Zeichen/s). Seit der Vertonung mit Azure
(de-DE-ConradNeural, Tempo −5 %) kommen alle Cues aus den echten Wortzeiten; jeder
Anker wurde gefunden. Aussprache per IPA im SSML: „Bayes“ = ˈbɛːjəs, „Likelihood“ =
ˈlaɪklihʊd (`PRON` in `scripts/tts-azure.mjs`). Gesamtlänge 6:19 min inkl. 9 s Abspann
mit KI-Hinweis und Impressum; mit Tempo 0 % wären es
etwa 5:55 min (`RATE` in `scripts/tts-azure.mjs`).

## Kreative Entscheidungen

- Dunkler Grund (Wunsch des Autors), dagegen gehalten: große Flächen (Kurve als
  gefüllte Landschaft, Tabellen, Würfel), keine Glows, keine Partikel.
- Die Zeitlupe des Wanderers (drei Schritte: bergauf, bergab, tief) ist eine
  **konstruierte Demonstration** (0,30 → 0,37 → 0,33 → Vorschlag 0,12 abgelehnt);
  der Zeitraffer danach läuft auf der **echten Metropolis-Kette** aus
  `build-data.mjs` (10 000 Schritte, Akzeptanzrate 0,83). Das Histogramm am Ende
  ist das echte Ergebnis dieser Kette, die blaue Kurve die Beta(7,4)-Dichte.
- Stichproben aus der Box: 10 000 gewichtete Ziehungen mit Seed; „über 50 %" ergibt
  7 378 ≈ 74 % (Gitterpunkt 0,5 zählt nicht mit, bleibt sichtbar blau).
- Regressionsdaten (Lernstunden → Klausurpunkte) und die Stan-Tabelle sind
  **illustrativ** (im Bild nicht als echte Daten ausgewiesen; das Skript sagt es
  nicht, der Kontext ist ein erfundenes Beispiel).
- Nicht gebaut: Sound-Effekte (Ticks/Whoosh). Bewusst zurückgestellt, bis die
  Stimme da ist; nachrüstbar über `npx hyperframes media-use resolve --type sfx`.

## Gemessen vs. nur angesehen

- `npx hyperframes check --json` mit echter Tonspur: 0 Fehler, 0 Kontrast-, 0
  Audio-Warnungen. 36 Warnungen `dead_stretch`, die längste jetzt 3,1 s.
- Beim Zoom-Schnappschuss gefunden und behoben: Balken wuchsen unter die Achse, weil
  GSAP ein `transform-origin`-Attribut bei SVG relativ zum Element liest. Jetzt
  `svgOrigin` in absoluten Koordinaten (Szenen Bayes-Box und Lostrommel).
- Render `renders/video.mp4`: 20,6 MB, 379,0 s (Root-Dauer 379,023 s), 1920×1080, 30 fps,
  H.264 + AAC; Render in 50 s (Hardware-GPU). Ton gemessen mit `hyperframes audio` und
  ffmpeg ebur128: −17,2 LUFS integriert, Spitze −2,1 dBFS, kein Clipping.
  Zehn dekodierte Frames (0,5 s bis 372,7 s) entsprechen den Snapshots.
- Angesehen: `snapshot --review` (Beat-Bogen, Handy-Bogen 360 px) und ein
  Kontaktbogen mit acht Einzelframes nach den Korrekturen. Alle Titel auf dem
  Handy-Bogen lesbar; Zahlen in der Bayes-Box geprüft (Post 0,7 = 0,267).
- Nicht geprüft: der Ton nach Gehör, die Nahtstellen in Bewegung
  (nur als Standbilder: alle Szenen blenden über denselben Grund, kein Sprung).

## Warnungen, die bleiben, und warum

- `dead_stretch` (36×, max. 3,1 s): Das Video ist ein narrierter Erklärfilm; die
  Stimme trägt diese Strecken, das Bild hält still, damit man lesen und mitdenken
  kann. Wer sie verkürzen will: zusätzliche Anker in `scripts/anchors.json` + kleine
  Einblendungen in der jeweiligen Szene.
- Die SRT bildet Azure-Wörter auf die Skript-Tokens ab (Satzzeichen aus dem Skript,
  Zeiten von Azure); „M-C-M-C" und „der Weg" werden dabei zusammengeführt.
- Der Ton wurde gemessen (Dauer, Wortzeiten), nicht gehört. Hörprobe:
  `assets/voice/probe.wav`.

## Schwächste Stelle

Szene 6 (Wanderer): Die Figur ist mit 54 px × 1,35 noch klein in einem 1600 px
breiten Panel; die Zeitlupe lebt von Pfeil und Höhenbalken. Mit mehr Zeit: Kamera-
Punch-in (Wrapper-Scale) auf den Wanderer während der drei Zeitlupen-Schritte.

## Nächste Schritte

1. `renders/video.mp4` ansehen und anhören (Stimme, Tempo, Pausen).
2. Bei Textänderungen: `SCRIPT.md` → `npm run tts && npm run timing && npm run srt`
   → `npm run check` → Render.
3. YouTube-Upload nach `youtube.md` (Titel, Beschreibung mit Kapiteln, Tags, Einstellungen,
   Vorschaubilder in `renders/`), dann Platzhalter-URL in `0600-Post.qmd` ersetzen.
