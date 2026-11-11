# Video: Die Post befragen – Stichproben statt Bayes-Box

Erklärvideo zu Kapitel 0600 („Die Post befragen") von *Start:Bayes!*.
Zielgruppe: Psychologiestudierende im 1./2. Semester.

## Inhalt in einem Satz

Warum die Bayes-Box bei größeren Modellen zu lang wird und wie man die
Posteriori-Verteilung stattdessen durch Stichproben beschreibt – inklusive
einer rein intuitiven Erklärung von MCMC als „Wanderer im Nebel".

## Dateien

| Datei / Ordner             | Zweck                                                              |
| -------------------------- | ------------------------------------------------------------------ |
| `SCRIPT.md`                | Sprechertext, zeilenweise; die eingerückten Zeilen gehen an TTS    |
| `STORYBOARD.md`            | Shot-Plan: Szenen, Dauer, Bildbeschreibung                         |
| `SPEC.md`                  | Look, Motion, Shots und Verbote als Zahlen (HyperFrames-Konvention)|
| `REVIEW.md`                | Was gemessen, was nur angesehen wurde; offene Punkte               |
| `index.html`               | Orchestrator: Szenen-Hosts und Sprachclips (Zeiten werden generiert)|
| `compositions/*.html`      | acht Szenen, je eine Sub-Komposition mit eigener GSAP-Timeline     |
| `assets/data.js`           | alle Zahlen (Bayes-Box, Lose, Wanderer-Kette, Regression), generiert|
| `assets/cues.js`           | Zeitmarken je Szene, generiert                                      |
| `assets/voice/`            | `line-N.words.json` versioniert; `line-N.wav` (Azure) nicht, per `npm run tts` neu erzeugbar |
| `scripts/build-data.mjs`   | erzeugt `assets/data.js` (fester Seed)                              |
| `scripts/anchors.json`     | Ankerwörter je Szene → Cue-Namen                                   |
| `scripts/build-timing.mjs` | Wortzeiten (Azure oder Schätzung) → `cues.js`, patcht `index.html` |
| `scripts/tts-azure.mjs`    | Vertonung mit Azure AI Speech, schreibt WAV + Wortzeiten           |
| `scripts/build-srt.mjs`    | `captions.srt` aus den Wortzeiten                                  |
| `captions.srt`             | Untertitel für YouTube (nicht eingebrannt)                         |
| `youtube.md`               | Upload-Paket: Titel, Beschreibung mit Kapiteln, Tags, Einstellungen|
| `renders/`                 | MP4-Ausgabe, nicht versioniert                                     |

## Ablauf bis zum fertigen Video

```bash
# einmalig
npm install
# .env anlegen (nie committen):
#   AZURE_SPEECH_KEY=...
#   AZURE_SPEECH_REGION=germanywestcentral
npm run tts -- --probe        # Hörprobe: assets/voice/probe.wav
npm run tts                   # alle acht Zeilen
npm run timing && npm run srt # Zeitmarken, index.html, captions.srt
npm run check                 # 0 Fehler erwartet
npx hyperframes@0.9.33 render --quality delivery -o renders/video.mp4
```

Nach dem YouTube-Upload: Platzhalter-URL in `0600-Post.qmd` (📺-Link beim Abschnitt
„Bayesbox geht nur für einfache Modelle") ersetzen.

Skript geändert? `SCRIPT.md` anpassen, Ankerwörter in `scripts/anchors.json` prüfen,
dann `npm run tts && npm run timing && npm run srt`.

## Festgelegte Entscheidungen (Stand 2026-11-11)

- Länge 4–6 min, 16:9 (1920×1080), dunkler Hintergrund
- Palette: Okabe-Ito auf Dunkel – Hellblau `#56B4E9` (Hauptmotiv/Kurve),
  Orange `#E69F00` (Akzent/Wanderer), Grün `#009E73` (Bestätigung),
  Hintergrund `#16181D`, Text `#F2F2F2`, gedämpft `#9A9DA6`; Font Inter
- Roter Faden: Bayes-Box → Stichproben aus der Box (Zwischenschritt, ehrlich
  benannt) → Box wird zu lang (Regression Lernstunden → Klausurpunkte,
  10³/10⁶/10²⁰) → MCMC als Wanderer im Nebel → Teaser `stan_glm()`
- MCMC-Tiefe: Schrittregel ja („bergauf immer, bergab manchmal, je tiefer
  desto seltener"), kein Warum, keine Formel
- Stimme: Azure AI Speech, `de-DE-ConradNeural`; Key in `.env`
  (`AZURE_SPEECH_KEY`, `AZURE_SPEECH_REGION`), nie committen
- Keine Musik, nur dezente CC0-Geräusche bei Stapel-Animationen
- Untertitel nicht eingebrannt; `captions.srt` wird beigelegt
- MP4 nicht im Repo; Link im Kapitel 0600 mit Platzhalter nach dem Render
- Anrede: Sie (wie das Buch); Wanderer als schlichte Figur ohne Gesicht
