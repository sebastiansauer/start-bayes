# Drehbuch & Storyboard: Bayes-Inferenz mit Stichproben & MCMC
**Zielgruppe:** Psychologiestudierende im 1. oder 2. Semester (Vorkenntnisse: Globusversuch, Grundlagen Bayes-Box)  
**Gesamtlänge:** Exakt 6:00 Minuten (ca. 780 gesprochene Wörter bei moderatem Tempo: 130 WpM)  
**Bezug:** [start-bayes, Kapitel 6: Die Post befragen](https://sebastiansauer.github.io/start-bayes/0600-Post.html)

---

## Übersicht der 4 Akte

| Szene | Titel | Timecode | Dauer | Wortanzahl |
|:---|:---|:---:|:---:|:---:|
| **Szene 1** | Die geniale Bayes-Box und ihr Kollaps | 0:00 – 1:15 | 1:15 min | ~160 Wörter |
| **Szene 2** | Der Trick: Häufigkeiten statt Wahrscheinlichkeiten | 1:15 – 2:30 | 1:15 min | ~165 Wörter |
| **Szene 3** | MCMC: Stan, der Bergwanderer im Nebel | 2:30 – 4:45 | 2:15 min | ~295 Wörter |
| **Szene 4** | Zusammenfassung & Ausblick auf R | 4:45 – 6:00 | 1:15 min | ~160 Wörter |

---

## Detailliertes Storyboard (Zweispaltig)

### Szene 1: Die geniale Bayes-Box und ihr Kollaps (0:00 – 1:15)

| Timecode & Visuals | On-Screen-Text / Grafiken | Gesprochener Text (Voice-Over) |
|:---|:---|:---|
| **0:00 – 0:25**<br>*Animation:* Eine animierte Weltkugel dreht sich kurz. Daneben erscheint die bekannte Bayes-Box-Tabelle mit 11 Zeilen ($p = 0.0, 0.1, \ldots, 1.0$), Likelihood ($W=6, N=9$) und der Post-Spalte. Ein schöner Glockengraph verbindet die Punkte. | **Folie 1: Unser Ausgangspunkt**<br>• Globusversuch: 6-mal Wasser bei 9 Würfen<br>• Gittermethode („Bayes-Box“)<br>• Ergebnis: Posteriori-Verteilung | *„Erinnert ihr euch an unseren Globusversuch? 9 Würfe, 6-mal Wasser. Mit der Bayes-Box konnten wir die Posteriori-Verteilung direkt Zeile für Zeile berechnen: Gitter anlegen, Likelihood mal Prior, standardisieren – fertig! Für einfache Modelle mit nur einem einzigen Parameter wie dem Wasseranteil $p$ ist diese Gittermethode absolut genial, anschaulich und präzise.“* |
| **0:25 – 0:50**<br>*Visual:* Die Tabelle zoomt heraus. Daneben taucht eine einfache lineare Regressionsformel auf ($y = \beta_0 + \beta_1 \cdot x + \epsilon$, Parameter: $\beta_0, \beta_1, \sigma$). Die Tabelle teilt sich in drei Dimensionen auf und wächst rasend schnell in die Länge. Ein animierter Laptop fängt an zu rauchen. | **Folie 2: Der Fluch der Dimensionen**<br>• Einfache Regression: 3 Parameter ($\beta_0, \beta_1, \sigma$)<br>• Bei nur 100 Werten pro Parameter:<br>  **$100 \times 100 \times 100 = 1.000.000$ Zeilen!**<br>• Multiple Regression (10 Parameter):<br>  **$100^{10} = 10^{20}$ Zeilen!** | *„Aber was passiert, wenn wir psychologische Phänomene untersuchen – zum Beispiel in einer Regression mit Prädiktoren? Plötzlich schätzen wir nicht mehr einen, sondern drei, fünf oder zehn Parameter gleichzeitig! Wenn wir für jeden Parameter nur 100 Gitterwerte annehmen, braucht unsere Bayes-Box bei 3 Parametern schon eine Million Zeilen. Bei einer multiplen Regression mit 10 Parametern sind es $10^{20}$ Zeilen. Selbst ein Supercomputer rechnet daran bis zum jüngsten Tag.“* |
| **0:50 – 1:15**<br>*Visual:* Ein cartoonhafter Roboter („Stan“) erscheint mit erhobenen Händen und Sprechblase: „Bitte tu mir das nicht an!“. Die Tabelle zerbröselt zu Staub. Die Leitfrage erscheint groß. | **Folie 3: Die Sackgasse**<br>„Die Bayes-Box wird schlicht zu lang.“<br><br>👉 **Die Kernfrage:**<br>*Wie bekommen wir die Posteriori-Verteilung, wenn wir die Tabelle gar nicht berechnen können?* | *„Kurz gesagt: Die Bayes-Box funktioniert für echte psychologische Modelle schlicht nicht mehr. Die Mathematik wird zu riesig. Wir stecken in einer scheinbaren Sackgasse. Doch zum Glück gibt es einen genialen Ausweg: Wir verabschieden uns davon, die Kurve exakt ausrechnen zu wollen – und lassen uns stattdessen Stichproben liefern.“* |

---

### Szene 2: Der Trick: Häufigkeiten statt Wahrscheinlichkeiten (1:15 – 2:30)

| Timecode & Visuals | On-Screen-Text / Grafiken | Gesprochener Text (Voice-Over) |
|:---|:---|:---|
| **1:15 – 1:45**<br>*Visual:* Eine Urne oder Lostrommel erscheint. Darin liegen Zettel mit Zahlen für $p$. Zettel mit Werten um 0.67 sind riesig und häufig, Zettel mit 0.1 sind winzig. Wir ziehen Zettel um Zettel heraus und stapeln sie auf einem Tisch. | **Folie 4: Der Stichproben-Trick**<br>• Wahrscheinlichkeiten sind abstrakt.<br>• **Häufigkeiten sind intuitiv.**<br>• Idee: Ziehe Werte proportional zu ihrer Plausibilität! | *„Hier kommt der zentrale Trick moderner Bayes-Statistik: Wir arbeiten ab jetzt nicht mehr mit abstrakten Wahrscheinlichkeitsformeln, sondern mit handfesten Häufigkeiten. Stellt euch vor, wir haben eine Lostrommel. Darin liegen mögliche Parameterwerte. Und je plausibler ein Wert nach den Daten ist, desto häufiger liegt er in der Trommel. Werte um 67 % Wasser ziehen wir ständig; Werte unter 20 % so gut wie nie.“* |
| **1:45 – 2:30**<br>*Visual:* Split-Screen:<br>Links: Die theoretische, glatte Posteriori-Kurve aus der Bayes-Box ($g=101$).<br>Rechts: 10.000 gezogene Einzelwerte ordnen sich in einem Histogramm an (animierter Aufbau von Balken). Beide Grafiken schieben sich übereinander – sie decken sich fast perfekt! | **Folie 5: Stichproben rekonstruieren die Verteilung**<br>Links: Exakte Kurve (Bayes-Box)<br>Rechts: Histogramm von 10.000 Stichproben<br>👉 *Die Form ist praktisch identisch!* | *„Schaut euch diesen Vergleich an: Links die mühsam berechnete, exakte Kurve. Rechts ein einfaches Histogramm aus 10.000 gezogenen Stichproben. Seht ihr den Unterschied? Es gibt praktisch keinen! Mit Stichproben können wir alle psychologischen Fragen kinderleicht beantworten: Wie wahrscheinlich ist ein Wasseranteil über 70 %? Wir zählen einfach, wie viele unserer 10.000 Zettel über 0.7 liegen. Kein Integrieren, kein Formelchaos – reines Zählen. Aber: Wie kommen wir an diese Stichproben, wenn wir doch gar keine Tabelle haben, aus der wir ziehen können?“* |

---

### Szene 3: MCMC: Stan, der Bergwanderer im Nebel (2:30 – 4:45)

| Timecode & Visuals | On-Screen-Text / Grafiken | Gesprochener Text (Voice-Over) |
|:---|:---|:---|
| **2:30 – 3:10**<br>*Visual:* Schnitt auf eine stilisierte Gebirgslandschaft. Die Form des Berges entspricht exakt unserer Posteriori-Verteilung (hoher Gipfel bei 0.67, flache Ausläufer zu den Rändern). Doch dichter Nebel verdeckt fast alles. Ein kleiner Roboter mit Rucksack und Notizblock („Stan“) steht am Hang. | **Folie 6: Die Gebirgslandschaft im Nebel**<br>• Höhe des Berges = Posteriori-Wahrscheinlichkeit<br>• Problem: Dichter Nebel (keine Landkarte / keine Tabelle!)<br>• Die Lösung: Ein Erkunder vor Ort (**MCMC**) | *„Genau hier betritt die Geheimwaffe der Bayes-Statistik die Bühne: MCMC – kurz für Monte-Carlo-Markov-Ketten. Klingt furchteinflößend, ist aber bildlich ganz einfach. Stellt euch die gesuchte Posteriori-Verteilung wie eine Gebirgslandschaft vor: Wo die Plausibilität hoch ist, ragen mächtige Berge auf; unwahrscheinliche Werte sind flache Täler. Das Problem: Die Landschaft liegt im stockdichten Nebel. Wir haben keine Landkarte und können das Gebirge nicht aus der Vogelperspektive vermessen.“* |
| **3:10 – 3:55**<br>*Visual:* Nahaufnahme von Roboter Stan. Er testet mit dem Fuß die Steigung nach links und rechts.<br>1. Pfeil zeigt nach oben: Stan geht sofort los.<br>2. Pfeil zeigt nach unten: Stan holt einen Würfel heraus, wirft, zuckt mit den Schultern und geht nur manchmal. | **Folie 7: Stans zwei einfache Schritt-Regeln**<br>Stan kennt nur den Punkt, auf dem er gerade steht!<br>1. **Geht der Schritt bergauf?**<br>   ➡️ Er geht **immer**!<br>2. **Geht der Schritt bergab?**<br>   ➡️ Er wirft eine Münze / einen Würfel und geht **nur manchmal** (proportional zur Höhe). | *„Deshalb schicken wir einen blinden Kundschafter ins Gebirge: Nennen wir ihn Stan. Stan sieht nichts im Nebel, aber er kann an seiner aktuellen Position den Boden abtasten. Er schlägt zufällig eine Richtung für den nächsten Schritt vor und folgt zwei simplen Regeln: Geht der Schritt bergauf in Richtung Gipfel? Dann macht er ihn sofort und immer! Geht der vorgeschlagene Schritt bergab in ein Tal? Dann zögert er: Er wirft eine Münze und geht nur mit einer gewissen Wahrscheinlichkeit – je tiefer der Abgrund, desto seltener.“* |
| **3:55 – 4:45**<br>*Visual:* Zeitraffer-Animation: Stan macht 10.000 Schritte. Nach jedem Schritt drückt er einen Stempel in den Boden (Hinterlassen von Fußspuren) und schreibt die Koordinate in sein Notizbuch. Am Gipfel entsteht ein dichter Teppich aus Punkten, am Fuß des Berges sind nur vereinzelte Spuren. Die Punkte formen das Histogramm von vorhin. | **Folie 8: Die Kette der Schritte**<br>• Stan verbringt automatisch die meiste Zeit oben.<br>• Seine Standorte = **Unsere Stichproben**.<br>• Histogramm seiner Schritte = **Posteriori-Verteilung**! | *„Was passiert durch diese simplen Regeln über Tausende von Schritten? Stan wird nicht für immer auf dem Gipfel festgenagelt, aber er hält sich ganz automatisch viel häufiger in den Höhenlagen auf als unten im Tal. An jedem Schritt notiert Stan seinen aktuellen Standort. Und das Geniale ist: Wenn wir nach 10.000 Schritten Stans Notizbuch aufschlagen und ein Histogramm all seiner Standorte zeichnen, erhalten wir exakt die gesuchte Posteriori-Verteilung! Stan hat das Gebirge für uns abgetastet – ganz ohne jemals eine gigantische Bayes-Box berechnen zu müssen.“* |

---

### Szene 4: Zusammenfassung & Ausblick auf R (4:45 – 6:00)

| Timecode & Visuals | On-Screen-Text / Grafiken | Gesprochener Text (Voice-Over) |
|:---|:---|:---|
| **4:45 – 5:25**<br>*Visual:* Eine übersichtliche Zusammenfassungsfolie mit drei Icon-Boxen baut sich auf (1. Gitter-Icon mit Kreuz durchgestrichen, 2. Roboter Stan mit Berg, 3. Histogramm mit Prozentbereich). | **Folie 9: Die drei Takeaways**<br>1. **Bayes-Box scheitert an Komplexität:** Zu viele Zeilen bei mehreren Parametern.<br>2. **MCMC tastet ab:** Stan erkundet die Verteilung Schritt für Schritt im Nebel.<br>3. **Stichproben genügen:** Aus Stans Schritten lesen wir Mittelwert, HDI und Intervalle ab. | *„Fassen wir die drei wichtigsten Punkte zusammen: Erstens: Die Bayes-Box ist super zum Verstehen, aber bei echten psychologischen Modellen explodiert die Zeilenzahl. Zweitens: MCMC löst dieses Problem elegant. Statt das gesamte Gelände analytisch zu berechnen, schicken wir Stan als Bergwanderer los, der das Terrain stichprobenartig erkundet. Und drittens: Wir brauchen keine komplizierten Formeln. Wir fassen einfach Stans Schritte zusammen, um Schätzbereiche, Mittelwerte und Wahrscheinlichkeiten zu bestimmen.“* |
| **5:25 – 6:00**<br>*Visual:* Ein Screenshot von RStudio blendet sanft ein. Man sieht den Aufruf von `stan_glm(...)` aus dem Paket `rstanarm` und daneben ein schönes `plot(...)` mit Verteilungskurve. Stan winkt freundlich von der Folie. Abspann mit Kapitellink. | **Folie 10: Ausblick auf die Praxis**<br>In R: Paket `rstanarm`<br>`stan_glm(y ~ x, data = meine_daten)`<br>👉 *Stan wandert für euch im Hintergrund!*<br><br>[start-bayes: Kapitel 6](https://sebastiansauer.github.io/start-bayes/0600-Post.html) | *„In den nächsten Kapiteln lernt ihr, wie ihr Stan in R selbst an die Arbeit schickt – mit dem Paket rstanarm. Ihr gebt einfach euer Modell ein, drückt Enter, und Stan wandert für euch los und liefert euch fertige Stichproben frei Haus. Ihr müsst MCMC also nicht selbst programmieren – ihr müsst nur wissen, wie ihr Stans Notizbuch richtig lest. Viel Erfolg beim Ausprobieren!“* |

---

## Produktions-Anhang: R-Code zur Erstellung der Folien-Grafiken

Mit diesem R-Skript können Sie die exakten Grafiken für die Folien 1, 4 und 5 direkt aus dem Kapitel 6 generieren und als hochauflösende PNGs abspeichern.

```r
library(tidyverse)
library(ggpubr)

# -------------------------------------------------------------
# 1. Datenbasis: Bayes-Box für Globusversuch (W = 6, N = 9)
# -------------------------------------------------------------
n_success <- 6
n_trials  <- 9

# Grobes Gitter (g = 11) für Folie 1
p_grid_11 <- seq(from = 0, to = 1, by = 0.1)
bayesbox_11 <- tibble(
  p_grid = p_grid_11,
  prior  = 1,
  likelihood = dbinom(n_success, size = n_trials, prob = p_grid_11)
) |> 
  mutate(
    unstand_post = likelihood * prior,
    post = unstand_post / sum(unstand_post)
  )

# Feines Gitter (g = 101) für Folie 5
p_grid_101 <- seq(from = 0, to = 1, length.out = 101)
bayesbox_101 <- tibble(
  p_grid = p_grid_101,
  prior  = 1,
  likelihood = dbinom(n_success, size = n_trials, prob = p_grid_101)
) |> 
  mutate(
    unstand_post = likelihood * prior,
    post = unstand_post / sum(unstand_post)
  )

# -------------------------------------------------------------
# 2. Stichproben ziehen (10.000 Samples)
# -------------------------------------------------------------
set.seed(42)
samples_10k <- bayesbox_101 |> 
  slice_sample(n = 1e4, weight_by = post, replace = TRUE) |> 
  select(p_grid)

# -------------------------------------------------------------
# 3. Plots für Folien erstellen
# -------------------------------------------------------------

# Plot A (Folie 1): Bayes-Box Kurve mit Punkten
plot_bayesbox <- ggplot(bayesbox_11, aes(x = p_grid, y = post)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 3.5) +
  geom_point(data = filter(bayesbox_11, p_grid == 0.7), color = "#E69F00", size = 5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Exakte Posteriori-Verteilung (Bayes-Box)",
    subtitle = "W = 6 Wasser bei N = 9 Würfen (g = 11)",
    x = "Parameterwert p (Wasseranteil)",
    y = "Posteriori-Wahrscheinlichkeit"
  )

# Plot B (Folie 5 - Rechts): Histogramm der Stichproben
plot_samples <- ggplot(samples_10k, aes(x = p_grid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#56B4E9", color = "white", alpha = 0.8) +
  geom_density(color = "#0072B2", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Stichproben-Approximation (10.000 Ziehungen)",
    subtitle = "Häufigkeitsverteilung spiegelt die Post-Dichte perfekt wider",
    x = "Gezogene Parameterwerte p",
    y = "Dichte (relative Häufigkeit)"
  )

# Grafiken speichern
ggsave("folie1_bayesbox.png", plot_bayesbox, width = 7, height = 4.5, dpi = 300)
ggsave("folie5_samples.png", plot_samples, width = 7, height = 4.5, dpi = 300)
```
