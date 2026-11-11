# YouTube-Upload: Stichproben statt Bayes-Box

Dateien für den Upload (alle in diesem Ordner):

| Was | Datei |
| --- | --- |
| Video | `renders/video.mp4` (1920×1080, 30 fps, H.264/AAC, ca. 20 MB, 6:19 min) |
| Untertitel | `captions.srt` (Deutsch, nicht eingebrannt; bei „Untertitel" hochladen) |
| Vorschaubild | `renders/thumbnail-a.jpg` (Titelkarte) oder `renders/thumbnail-b.jpg` (Wanderer und Stapel), je 1280×720 |

## Titel (max. 100 Zeichen)

Stichproben statt Bayes-Box: Wie ein Wanderer im Nebel die Posteriori-Verteilung findet

Alternative, kürzer:
MCMC intuitiv erklärt: Warum die Bayes-Box zu lang wird (Start:Bayes!)

## Beschreibung

Wie kommt man bei Bayes zu einer approximierten Posteriori-Verteilung, wenn man sie nicht mehr ausrechnen kann? Dieses Video zeigt in sechs Minuten, warum die Bayes-Box (Gitter-Methode) bei größeren Modellen zu lang wird, wie man eine Post-Verteilung stattdessen durch Stichproben beschreibt, und wie MCMC dabei hilft: ein Wanderer im Nebel, der nur seine eigene Höhe kennt. Für Studierende der Psychologie im ersten oder zweiten Semester, ohne Formeln.

Kapitel:
0:00 Worum es geht
0:32 Zur Erinnerung: die Bayes-Box
1:13 Stichproben aus der Box: die Lostrommel
2:09 Der Einwand: dafür brauchten wir doch die Box
2:37 Warum die Bayes-Box zu lang wird
3:40 Der Wanderer im Nebel (MCMC, intuitiv)
4:52 Warum das reicht: MCMC
5:37 In R: Stan, und der Merksatz

Zum Buchkapitel: https://sebastiansauer.github.io/start-bayes/0600-Post.html
Das Buch „Start:Bayes!": https://sebastiansauer.github.io/start-bayes
Impressum: https://sebastiansauer.github.io/start-bayes/imprint.html

Der Globusversuch folgt Richard McElreath, Statistical Rethinking (2. Aufl., 2020), Kap. 2 und 3.

Lizenz: CC-BY-NC-SA 4.0 (wie das Buch). Prof. Dr. Sebastian Sauer, Hochschule Ansbach.
Hinweis: Die Sprecherstimme ist KI-synthetisiert (Azure AI Speech, Stimme „Conrad"). Grafiken und Animationen sind programmatisch erzeugt (HyperFrames); die Zahlen stammen aus einer reproduzierbaren Simulation mit festem Seed.

## Tags

Bayes, Bayes-Statistik, Posteriori-Verteilung, MCMC, Markov Chain Monte Carlo, Metropolis, Bayes-Box, Gitter-Methode, Stichproben, Statistik Psychologie, Statistical Rethinking, Globusversuch, Stan, rstanarm, Start:Bayes, Sebastian Sauer, Erklärvideo

## Einstellungen beim Upload

- Sprache des Videos: Deutsch. Titel und Beschreibung: Deutsch.
- Kategorie: Bildung.
- Zielgruppe: nicht speziell für Kinder.
- „Veränderte oder synthetische Inhalte": **Ja** ankreuzen (KI-Stimme). YouTube zeigt dann den Hinweis; der Abspann nennt ihn zusätzlich.
- Untertitel: `captions.srt` als Deutsch hochladen, nicht die automatische Transkription verwenden.
- Kapitel: entstehen automatisch aus den Zeitmarken in der Beschreibung (erste Marke muss 0:00 sein, mindestens drei Marken, je ≥ 10 s).
- Playlist: „QM2" (die Playlist, auf die das Kapitel 0600 verweist), falls das Video dort hingehört.
- Lizenz-Feld bei YouTube: „Standard-YouTube-Lizenz" (YouTube bietet nur CC-BY an, nicht BY-NC-SA; die Lizenz steht in Beschreibung und Abspann).
- Kommentare und Einbettung: wie bei den anderen Kanalvideos.

## Nach dem Upload

1. Die Video-URL in `0600-Post.qmd` beim 📺-Link eintragen (Platzhalter `PLATZHALTER` ersetzen).
2. Buch neu rendern und veröffentlichen (`./publish-all.sh`).
