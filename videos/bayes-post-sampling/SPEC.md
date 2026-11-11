# SPEC — bayes-post-sampling

GOAL:    Am Ende kann eine Erstsemester-Studentin in einem Satz sagen, warum die Bayes-Box
         bei großen Modellen scheitert und wie ein „Wanderer" trotzdem Stichproben aus der
         Post liefert. Merksatz: „Nicht ausrechnen, wandern lassen und zählen."
OUTPUT:  HyperFrames, 1920 x 1080, 30 fps, Voice-over (Azure de-DE-ConradNeural), keine Musik
TIME:    ca. 340 s (≈ 10 200 Frames); die Stimme ist die Uhr, jede Szene = eine Skriptzeile
         (SCRIPT.md), Cues aus Wortzeiten (scripts/build-timing.mjs → assets/cues.js)

LOOK:    Ground #16181D flach, ohne Textur, ohne Gradient (Dunkel bewusst gewählt, siehe
         README; Gegenmaßnahme gegen „Void": der Held füllt ≥ 45 % der Höhe, Flächen statt
         Striche). Ink #F2F2F2, gedämpft #9A9DA6, Gitter/Linien #2A2D35.
         Akzente (Okabe-Ito): Blau #56B4E9 = die Post (Kurve, Stapel aus der Box, Stan),
         Orange #E69F00 = der Wanderer und das eine betonte Wort pro Szene,
         Grün #009E73 = Bestätigung (Haken, „passen zusammen"); Rot wird nicht verwendet.
         Display Inter 700, Titel 88 px, Tracking −0.02 em; Fließtext Inter 400/500, 40 px;
         Zahlen Inter 700 tabular-nums; Code JetBrains Mono 36 px.
         Medium: Tafelbild aus Flächen – Balken, Kacheln, Kurven als gefüllte Flächen mit
         1,5 px hellerer Kante; keine Schatten, kein Glow, 0 % Grain, 0 % Vignette.
OBJECTS: Globus = Kreis #2A2D35 mit vier blauen Flächen (Kontinente negativ), 9 Wurfkacheln
         W (blau) / L (grau); Bayes-Box = Tabelle mit sichtbaren Zeilenlinien, Kopfzeile
         fett; Lostrommel = liegende Trommel (abgerundetes Rechteck) mit Kurbel und Öffnung;
         Los = kleine Kachel 28 x 20 px mit p-Wert; Würfel = isometrisches Gitter 10³;
         Wanderer = Kreis-Kopf + Strichkörper in Orange, 54 px hoch, ohne Gesicht;
         Höhenmesser = rundes Instrument mit Zeiger und Zahl; Nebel = weiße Fläche
         Opazität 0.55 über der Landschaft; Stan-Tabelle = Monospace-Tabelle.
MOTION:  enter `power4.out` 0.5–0.7 s; move `power2.inOut`; exit `power3.in` 0.3 s;
         Balken wachsen `power3.out` 0.6 s, Stagger 40 ms; Kurve zeichnet mit
         stroke-dashoffset 1.2 s `power2.inOut`; Zahlen zählen 1.4 s linear;
         Wanderer-Schritt 0.35 s `power2.out`, Zeitraffer linear;
         Wort-Highlights: Farbe ohne Bewegung, 0.2 s. Nichts atmet, nichts driftet nach
         dem Landen.
SHOTS:   (Frames bei 30 fps, Szenenstart = Cue-Null; genaue Sekunden in assets/cues.js)
         1 einstieg   0–20 s   Titel steht ab 0.3 s; Kurve zeichnet bei „Post";
                               zwei Punkte landen bei „Erstens"/„zweitens". Cut.
         2 bayesbox   20–58 s  Globus+Kacheln links, Tabelle baut sich zeilenweise, Spalten
                               leuchten bei Prior·Likelihood, Balken→Kurve bei „Als Kurve".
         3 lostrommel 58–112 s Tabelle schrumpft, Lose fliegen in Trommel, Zähler 1…10 000,
                               Stapel wachsen, Kurve legt sich darüber, Zählen rechts von 0,5.
         4 einwand    112–132 s Sprechblase, Tabelle leuchtet, Haken, Zielsatz mit „ohne".
         5 box        132–205 s Streudiagramm + 3 Marker; Reihe → Gitter → Würfel; Zahlen
                               1 000 → 1 000 000 → 10²⁰; Schlussätze.
         6 wanderer   205–280 s Kurve wird Landschaft, Nebel, Wanderer, Schrittregel in
                               Zeitlupe (3 Fälle), Zeitraffer, Histogramm, Kurve darüber.
         7 mcmc       280–312 s Nebel zurück, zwei Höhen, Würfel durchgestrichen, Gebirge,
                               MCMC-Aufschlüsselung.
         8 stan       312–340 s Codezeile, Tabelle, Trommel-Rückblende, Merksatz, Abspann.
BANNED:  Glow/Bloom/Neon, Partikel, Gradient-Hintergründe, Glaskarten, drei Karten in
         einer Reihe, KPI-Kacheln, Ecklabels/Rahmen/HUD, Logo in der Ecke, alles zentriert
         einfaden, Titel die nach dem Landen weiterdriften, atmende Loops, Emoji als Figur,
         Stockfotos, Gesichter, Bullet-Folien, Legenden (Labels sitzen am Objekt),
         Sprechertext wortgleich auf dem Bild.
QA:      t=0.3 Titel + Kicker lesbar; t=Szene2 Mitte: Tabelle 11 Zeilen, Zahlen korrekt
         (Post 0.7 = 0.267); Szene3 Ende: Zähler „≈ 74 %" und Stapel bei 0,5 blau;
         Szene5 „10²⁰" lesbar; Szene6 Ende: Histogramm und Kurve decken sich sichtbar;
         Szene8 letzte 4 s: Abspann mit URL; 360-px-Sheet: alle Titel lesbar.
