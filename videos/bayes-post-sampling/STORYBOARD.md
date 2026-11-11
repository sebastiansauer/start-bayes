---
format: 1920x1080
duration: 340s
message: "Die Bayes-Box wird zu lang – also lassen wir einen Wanderer Stichproben aus der Post sammeln"
arc: Einstieg → Erinnerung Bayes-Box → Stichproben aus der Box → Einwand → Box zu lang → Wanderer im Nebel → Warum das reicht → Stan-Teaser
audience: Psychologiestudierende, 1./2. Semester, Kapitel 0600 „Die Post befragen" (Start:Bayes!)
mode: collaborative
fps: 30
palette:
  bg: "#16181D"
  text: "#F2F2F2"
  muted: "#9A9DA6"
  blue: "#56B4E9"
  orange: "#E69F00"
  green: "#009E73"
  grid: "#2A2D35"
font: Inter
style: "Dunkler Hintergrund, flaches Lehrbuch-Design, viel Ruhe. Eine Grafik pro Szene als Hauptmotiv, Stichwörter sparsam. Keine Stockfotos, keine Gesichter. Wanderer = schlichte Strichfigur/Punkt in Orange. Alle Zahlen und Kurven aus dem Globusversuch (W=6, N=9, Prior flach)."
audio: "Voice-over aus SCRIPT.md (Azure, de-DE-ConradNeural). Keine Musik. Dezente CC0-SFX: leises 'tick' pro gelegtem Los (gebündelt, nicht 10.000×), weiches 'whoosh' bei Gitter→Würfel, kurzer Bestätigungston beim Überlagern der Kurven."
---

## Frame 1 — Einstieg: zwei Fragen

- scene: Titel „Die Post befragen" über einer leeren Dichtekurve, die sich beim Wort „Post" blau füllt; dann zwei nummerierte Stichwörter
- duration: 20s
- poster: 12s
- transition_in: cut
- status: outline
- voiceover: "Stellen Sie sich vor, Sie haben Daten gesammelt … bei größeren Modellen nicht mehr ausreicht."
- src: compositions/frames/01-einstieg.html

Bild: Oben links klein „Start:Bayes! · Kapitel: Die Post befragen". Mitte: eine
unbeschriftete glockenähnliche Kurve (Beta(7,4)-Form, Achse 0 bis 1) als Linie
in `blue`, die sich beim Wort „Post" von links nach rechts zeichnet und darunter
leicht füllt. Bei „Erstens" erscheint rechts ein Stichwort „1 · Stichproben statt
Rechnen", bei „zweitens" darunter „2 · Warum die Bayes-Box nicht reicht". Beide
in `text`, Nummern in `orange`.

## Frame 2 — Zur Erinnerung: die Bayes-Box

- scene: Globus-Icon mit 9 Wurfergebnissen (W/L), daneben baut sich die Bayes-Box als Tabelle mit 11 Zeilen auf, rechts entsteht die Post-Kurve
- duration: 38s
- poster: 30s
- transition_in: crossfade
- status: outline
- voiceover: "Zur Erinnerung: der Globusversuch … Werte um sechzig bis siebzig Prozent sind am wahrscheinlichsten."
- src: compositions/frames/02-bayesbox.html

Bild: Links oben ein schematischer Globus (Kreis mit blauen Flächen, kein Foto),
darunter die Wurfreihe `W L W W L W L W W` als Kacheln, W in `blue`, L in
`muted`. Beim Satz „sechs Mal Wasser" zählen die W-Kacheln kurz hoch.
Mitte: Tabelle mit Spalten `p`, `Prior`, `Likelihood`, `Post`, 11 Zeilen
(p = 0,0; 0,1; … 1,0). Zeilen erscheinen gestaffelt beim Aufzählen „null Prozent,
zehn Prozent …". Bei „Prior mal Likelihood" leuchten die beiden Spalten kurz
`orange`, bei „teilen durch die Summe" die Spalte `Post` in `blue`.
Rechts: Balkendiagramm der Post-Werte (11 Balken), das sich bei „Als Kurve …"
in eine glatte Kurve verwandelt (gleiche Kurve wie Frame 1). Werte, Rundung auf
3 Stellen, flacher Prior, Binomial-Likelihood für W=6, N=9:

| p   | Post  |
| --- | ----- |
| 0.0 | 0.000 |
| 0.1 | 0.000 |
| 0.2 | 0.003 |
| 0.3 | 0.021 |
| 0.4 | 0.074 |
| 0.5 | 0.164 |
| 0.6 | 0.251 |
| 0.7 | 0.267 |
| 0.8 | 0.176 |
| 0.9 | 0.045 |
| 1.0 | 0.000 |

(Phase 2 berechnet die Werte im Build-Skript, nicht aus dieser Tabelle abtippen.)

## Frame 3 — Stichproben aus der Box

- scene: Die Tabellenzeilen wandern als Lose in eine Trommel (hohe Post = viele Lose), 10.000 Ziehungen türmen Stapel auf, die die Kurve nachzeichnen; dann wird rechts von 0,5 gezählt
- duration: 54s
- poster: 35s
- transition_in: crossfade
- status: outline
- voiceover: "Jetzt ein Trick, der zuerst merkwürdig wirkt … Keine Formel. Nur zählen."
- src: compositions/frames/03-lostrommel.html

Bild: Die Tabelle aus Frame 2 schrumpft nach links. Mitte: eine schematische
Lostrommel (Kreis mit Öffnung). Bei „nicht jede Zeile einmal" fliegen aus jeder
Tabellenzeile Lose in die Trommel: Anzahl proportional zur Post (z. B. 0 bis 27
Lose, Rundung auf Post × 100), Lose als kleine Kacheln mit dem p-Wert.
Bei „zehntausend Mal" läuft ein Zähler `n = 1 … 10 000` hoch (beschleunigt,
3 s), während rechts auf einer Achse 0 bis 1 Stapel wachsen: zuerst einzelne
Lose sichtbar (die ersten ~50 mit tick-SFX), dann Übergang in Balken.
Bei „zeichnen die Kurve nach" legt sich die Kurve aus Frame 2 als dünne Linie
über die Stapel. Einblendung oben: „Wahrscheinlichkeit → Häufigkeit".
Bei „über fünfzig Prozent" senkrechte Linie bei 0,5; die Stapel rechts davon
färben sich `orange`, Zähler darunter: „7 4## von 10 000 ≈ 74 %" (Wert exakt
in Phase 2 aus den Samples berechnen; Erwartung P(p > 0,5) = 0,74 beim Gitter
mit 11 Werten, da der Gitterpunkt 0,5 selbst mit 16 % nicht mitzählt; die
Stapel bei 0,5 bleiben sichtbar `blue`, damit das klar ist). Letzte
Einblendung: „Keine Formel. Nur zählen."

## Frame 4 — Der berechtigte Einwand

- scene: Sprechblase mit dem Einwand, die Bayes-Box dahinter leuchtet auf; dann das Ziel als Satz: „Stichproben aus der Post – ohne die Box aufzuschreiben"
- duration: 20s
- poster: 14s
- transition_in: cut
- status: outline
- voiceover: "Vielleicht denken Sie jetzt: Moment … Warum das wichtig ist, sehen wir jetzt."
- src: compositions/frames/04-einwand.html

Bild: Reduzierte Szene. Links die Tabelle aus Frame 2 gedimmt, rechts die
Stapel aus Frame 3. Bei „Moment" erscheint eine Sprechblase (ohne Figur) mit
dem Text „Dafür brauchten wir doch die fertige Box?!", die Tabelle leuchtet
kurz `orange`. Bei „Übung" wandert ein Haken in `green` an die Stapel.
Bei „Das eigentliche Ziel" wird alles bis auf einen Satz ausgeblendet:
„Ziel: Stichproben aus der Post – ohne die Box je ganz aufzuschreiben."
Das Wort „ohne" in `orange`.

## Frame 5 — Die Box wird zu lang

- scene: Streudiagramm Lernstunden→Klausurpunkte mit Gerade; drei Parameter werden markiert; die Bayes-Box wächst von Reihe zu Gitter zu Würfel, Zahlen 1 000 → 1 000 000 → 10²⁰
- duration: 73s
- poster: 55s
- transition_in: crossfade
- status: outline
- voiceover: "Nehmen wir ein etwas größeres Modell … Sie wird nur zu lang."
- src: compositions/frames/05-box-zu-lang.html

Bild, Teil A (0–22 s): Links ein kleines Streudiagramm, x „Lernstunden", y
„Klausurpunkte", ca. 20 Punkte in `muted`, Gerade in `blue`. Drei Marker
erscheinen nacheinander mit Label: „Start der Geraden" (Achsenabschnitt, Punkt
auf der y-Achse), „Steigung" (kleines Dreieck an der Geraden), „Streuung"
(zwei gestrichelte Linien parallel zur Geraden). Keine griechischen Buchstaben
im Bild; optional klein darunter in `muted`: „β₀ · β₁ · σ".

Teil B (22–60 s): Rechts die Box als Kästchen. „ein Parameter": eine Reihe von
10 Kästchen. „zwei Parameter": Reihe klappt zu einem 10×10-Gitter auf
(whoosh). „drei Parameter": Gitter extrudiert zu einem 10×10×10-Würfel in
isometrischer Ansicht. Zahlen groß darunter: „10 × 10 × 10 = 1 000 Zeilen".
„hundert Werte": Würfel wird feiner, Zahl „1 000 000". „zehn Parameter":
Würfel wird zu einem Stapel/Unschärfe, Zahl zählt hoch zu
„100 000 000 000 000 000 000" und bekommt daneben klein „10²⁰".

Teil C (60–73 s): Alles ausblenden bis auf zwei Zeilen Text, nacheinander:
„Die Bayes-Box ist nicht falsch." / „Sie wird nur zu lang." Zweite Zeile in
`orange`.

## Frame 6 — Der Wanderer im Nebel

- scene: Die Post-Kurve wird zur Hügellandschaft im Nebel; ein Wanderer macht Zufallsschritte nach der Regel „bergauf immer, bergab manchmal"; sein Standort-Stapel wächst und deckt sich mit der Kurve
- duration: 75s
- poster: 50s
- transition_in: crossfade
- status: outline
- voiceover: "Hier kommt die Idee, die das Problem löst … Sie passen zusammen."
- src: compositions/frames/06-wanderer.html

Bild, Teil A (0–18 s): Die Globus-Post-Kurve (Frame 2) wird zur Silhouette
einer Hügellandschaft: Fläche darunter in dunklem `blue`, Achse 0 bis 1 unten
als „Parameterwert p", links eine Höhenskala „Posteriori-Wahrscheinlichkeit".
Bei „dichter Nebel" legt sich ein halbtransparenter Verlauf über die
Landschaft, sodass die Kontur nur noch schwach zu sehen ist. Bei „Wanderer"
erscheint eine kleine Figur in `orange` (Kreis-Kopf, Strichkörper) bei p ≈
0,3. Bei „Höhenmesser" ein kleines Instrument-Icon mit der aktuellen Höhe als
Zahl.

Teil B (18–45 s), Schrittregel, in Zeitlupe: Vom Standort geht ein
gestrichelter Pfeil zu einem Vorschlag (zufällige Richtung, Schrittweite ca.
0,05–0,1). Zwei Höhenbalken nebeneinander: „hier" und „dort". Fall 1 („bergauf,
immer"): dort höher → Haken `green`, Figur rückt vor. Fall 2 („bergab, nur
manchmal"): dort niedriger → eine Münze/Würfel-Animation, manchmal Haken,
manchmal Kreuz; Figur bleibt oder geht. Fall 3 („je tiefer, desto seltener"):
Vorschlag weit unten im Tal → Kreuz, mit Einblendung „selten". Bei „notiert er"
fällt bei jedem Standort ein kleiner Punkt in `orange` nach unten auf eine
zweite Achse unter der Landschaft (Standort-Stapel).

Teil C (45–75 s): „tausende Male": Zeitraffer, die Figur zuckt durch die
Landschaft, Zähler „Schritte: 1 … 10 000", der Stapel unten wächst zu einem
Histogramm in `orange`. Nebel lichtet sich langsam. Bei „sehen Sie hier" zoomt
das Histogramm groß in die Mitte. Bei „Legen wir die Kurve … darüber" legt
sich die Bayes-Box-Kurve in `blue` darüber, Bestätigungston, Einblendung
„Stapel des Wanderers ≈ Post-Verteilung". Phase 2: den Wanderer wirklich per
Metropolis-Regel auf der Beta(7,4)-Dichte simulieren (fester Seed), damit das
Histogramm ehrlich ist; die ersten ~200 Schritte in Teil B aus derselben Kette.

## Frame 7 — Warum das die Lösung ist

- scene: Die Landschaft wird wieder vernebelt und ausgegraut, nur zwei Höhenmesser-Werte bleiben; daneben kippt die Linie zu einem Gebirge mit vielen Dimensionen; der Name MCMC wird aufgeschlüsselt
- duration: 32s
- poster: 22s
- transition_in: cut
- status: outline
- voiceover: "Der entscheidende Punkt: Der Wanderer hat die Landschaft nie gesehen … ohne dass jemand die Post ausgerechnet hat."
- src: compositions/frames/07-mcmc.html

Bild: Links die Landschaft aus Frame 6, wieder voll im Nebel, nur der Wanderer
und zwei Höhenbalken „hier"/„einen Schritt weiter" bleiben sichtbar. Bei
„Millionen Zeilen hat niemand aufgeschrieben" blitzt der Würfel aus Frame 5
kurz auf und wird durchgestrichen. Bei „Gebirge mit zehn Dimensionen" wird die
1-D-Silhouette zu einer kleinen 3-D-Höhenkarte (isometrisches Gitter mit
Hügel), der Wanderer darauf unverändert. Rechts baut sich die Aufschlüsselung
auf, Wort für Wort mit dem Text:
„**M**arkov-**C**hain **M**onte **C**arlo" → darunter zwei Zeilen:
„Monte Carlo = Zufall in den Schritten" und „Markov-Kette = jeder Schritt hängt
nur vom vorigen ab". Großbuchstaben M C M C in `orange`.

## Frame 8 — Teaser: Stan in R, und Schluss

- scene: Eine Zeile R-Code, daraus entsteht eine Tabelle mit Spalten pro Parameter (eine Zeile pro Standort); Rückblende Lostrommel; Merksatz; Abspann
- duration: 28s
- poster: 15s
- transition_in: crossfade
- status: outline
- voiceover: "In R müssen Sie den Wanderer nicht selbst bauen … Nicht ausrechnen, sondern wandern lassen und zählen."
- src: compositions/frames/08-stan.html

Bild: Oben in Monospace, Syntax leicht gefärbt, eine Zeile:
`fit <- stan_glm(punkte ~ lernstunden, data = d)`. Darunter kleines Stan-Logo
nachempfunden als Text „Stan" in `blue` (kein Originallogo). Bei „Tabelle
zurück" wächst darunter eine Tabelle mit Kopf `(Intercept)`, `lernstunden`,
`sigma` und ca. 8 sichtbaren Zeilen mit plausiblen Zahlen (z. B. 42.1 · 2.3 ·
8.7), eine Zeilennummer links; rechts ein Hinweis „… 4 000 Zeilen". Bei „genau
wie mit der Lostrommel" erscheint rechts klein die Trommel aus Frame 3 mit
den drei Stichwörtern „zählen · Anteile · Intervalle".
Bei „Das ist der Weg" alles ausblenden; Merksatz mittig in zwei Zeilen:
„Nicht ausrechnen." / „Wandern lassen und zählen." Zweite Zeile `orange`.
Abspann ca. 8 s: „Start:Bayes! · sebastiansauer.github.io/start-bayes · Kapitel
‚Die Post befragen'", darunter klein „Globusversuch nach McElreath, Statistical
Rethinking", dann zwei Impressumszeilen: „📚 start-bayes • Prof. Dr. Sebastian Sauer •
Lizenz: CC-BY-NC-SA 4.0 (Open Access)" und „🤖 Stimme KI-synthetisiert (EU AI
Act) • Impressum: sebastiansauer.github.io/start-bayes/imprint.html".
