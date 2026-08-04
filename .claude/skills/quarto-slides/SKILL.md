---
name: quarto-slides
description: Erstellt oder aktualisiert ein RevealJS-Foliendeck (Quarto) in slides/ aus einem Buchkapitel dieses Repos (NNNN-titel.qmd). Benutze diesen Skill, wenn der Nutzer "Folien für Kapitel X erstellen", "Foliendeck aktualisieren/neu erstellen" (z. B. weil sich Bilder/Text im Kapitel geändert haben), oder "Überlauf auf Folien prüfen/beheben" verlangt.
---

# Quarto-Foliendecks aus Buchkapiteln erstellen

Dieser Skill destilliert ein Kapitel des Buchs "Start:Bayes!" (`NNNN-titel.qmd` im Repo-Root)
zu einem eigenständigen RevealJS-Foliendeck unter `slides/NNNN-titel.qmd`.

## 0. Vorbereitung

- `slides/index.qmd` ist das Stil-Referenzdeck. Existiert es noch nicht, lege es zuerst nach dem
  YAML-Template in Schritt 1 an (Inhalt: Kapitel `index.qmd`).
- Ermittle den Kapiteltitel per `grep -m1 '^# ' <kapitel>.qmd`.
- Bei mehreren Kapiteln gleichzeitig: für jedes Kapitel einen eigenen Agent (Tool `Agent`,
  `run_in_background`, mehrere parallel in einer Nachricht) mit einem vollständig
  selbst-enthaltenen Prompt starten, der die Regeln aus diesem Skill (Schritte 1–5) enthält —
  das ist deutlich schneller als sequenziell zu arbeiten und war die Vorgehensweise, mit der
  erfolgreich alle 14 Kapitel dieses Buchs bebildert wurden.

## 1. YAML-Header-Template

```yaml
---
title: "Start:Bayes!"
subtitle: "<Kapiteltitel>"
author: "Sebastian Sauer"
lang: de
format:
  revealjs:
    theme: default
    lightbox: true
    slide-number: true
    chalkboard: true
    incremental: false
    toc: false
    logo: ../img/Golem_hex-small.png
    footer: "Start:Bayes! — <Kapiteltitel>"
    css: ../specifics/styles.css
    html-math-method:
      method: mathjax
      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"
mermaid:
  theme: neutral
bibliography: ../specifics/bib-local.bib
csl: ../specifics/apa7.csl
---
```

`lang: de` sorgt dafür, dass unbetitelte Callouts (z. B. `callout-important`) automatisch die
deutsche Quarto-Standardübersetzung bekommen ("Wichtig", "Hinweis", "Tipp" …) — keine eigenen
Titel für diese Fälle erfinden.

**`html-math-method` (MathJax v3 statt v2):** Quartos RevealJS-Format lädt für Formeln standardmäßig
MathJax **v2** (Hub-API, `TeX-AMS_HTML-full`-Konfiguration). Diese Kombination unterstützt Befehle
wie `\textcolor{}{}` bzw. `\color{}{}` (aus dem `color`-Paket) NICHT — sie werden dann als
unverarbeiteter Rohtext angezeigt (sichtbares `\textcolor` statt farbiger Formel), UND der lange
Rohtext kann dabei die Folie horizontal sprengen. Das Buch selbst (normales `html`-Format) nutzt
bereits MathJax **v3** (`tex-chtml-full.js`), das `\textcolor`/`\color` korrekt darstellt — die
obige `html-math-method`-Override im YAML-Header schaltet die Foliendecks auf dieselbe v3-Engine
um. Ein rein CSS-basierter Nachbesserungsversuch (z. B. eine globale Regel für die MathJax-Ausgabe)
funktioniert hier NICHT, siehe die Erläuterung zu Formel-Font-Size weiter unten — das Problem liegt
an der fehlenden TeX-Extension, nicht an der Darstellungsgröße. Farbige Formeln (`\textcolor`,
`\color`) sind in diesem Buch üblich (siehe z. B. 0800-gauss.qmd, 0900-lineare-modelle.qmd), also
immer mit v3-Override arbeiten, auch wenn ein neues Kapitel aktuell keine farbigen Formeln enthält
— falls später welche ergänzt werden, funktionieren sie dann direkt.

`theme: default` ist Quartos helles/weißes RevealJS-Theme (aktueller Stand — war zwischenzeitlich
`night`/dunkel, aber wieder auf hell zurückgestellt). `lightbox: true` aktiviert Klick-zum-Vergrößern
für alle Bilder (über glightbox) — funktioniert in RevealJS genauso wie im HTML-Buch.

**Achtung — Nebenwirkung von `lightbox: true`:** Quarto wandelt dadurch jedes Bild mit Alt-Text in
eine vollständige `<figure>` mit sichtbarer `<figcaption>` unter dem Bild um (statt eines simplen
`<img>`). Das kostet auf eng bemessenen Folien zusätzlichen vertikalen Platz und hat beim Umstellen
auf `lightbox: true` in praktisch jedem Kapitel neue Überlauf-Folien erzeugt. Abgefangen wird das
bereits global in `specifics/styles.css` (`.reveal .slides figcaption { display: none; }` plus
reduzierter `margin-bottom` auf `.quarto-figure`) — die Caption bleibt nur im `title`-Attribut
erhalten und wird beim Aufklappen der Lightbox weiterhin angezeigt. Trotzdem nach JEDER
Neuerstellung/Aktualisierung eines Decks den Überlaufcheck (Abschnitt 4) laufen lassen, da einzelne
besonders bild- oder textlastige Folien trotz dieser globalen Abfederung noch überlaufen können
(dann wie gewohnt mit `{.smaller}` / Bildbreite reduzieren beheben).

**Bilder ohne Bildunterschrift werden sonst linksbündig:** Ein Markdown-Bild mit Caption
(`![Text](...)`) bekommt von Quarto automatisch die zentrierende Hülle `.quarto-figure-center`.
Ein Bild **ohne** Caption (`![](...)`) bekommt diese Hülle NICHT und landet als reines
`<p><img></p>` bzw. `<p><a><img></a></p>` linksbündig auf der Folie. Dafür gibt es bereits eine
globale Gegenmaßnahme in `specifics/styles.css`
(`.reveal .slides p:has(> img), .reveal .slides p:has(> a > img) { text-align: center; }`), die
gezielt nur Absätze zentriert, die ausschließlich aus einem (oder mehreren nebeneinander
gesetzten) Bild(ern) bestehen — Side-by-side-Bildpaare/-tripel im selben Absatz (z. B.
`![](a.png) ![](b.png)`) bleiben dadurch als Gruppe zusammen und werden gemeinsam zentriert, statt
auseinandergerissen zu werden. Beim Einfügen neuer Bilder ist daher **kein** manuelles
`fig-align="center"` nötig — nur bei Bildern, die absichtlich NICHT zentriert werden sollen, auf
diese Regel achten.

**Golem-Bild:** Nur in `slides/index.qmd` darf die erste Inhaltsfolie
`![Bildquelle: Klara Schaumann](../img/Golem_hex.png){width="40%"}` zeigen. In allen anderen
Kapiteldecks NICHT auf der Startfolie einbinden — das kleine Golem-Logo erscheint über
`logo: ../img/Golem_hex-small.png` ohnehin dauerhaft in der Fußzeile.

## 2. Inhalt destillieren

1. Lies das Quellkapitel vollständig (bei > 1000 Zeilen mit `offset` in mehreren `Read`-Aufrufen).
2. Übernimm die grobe Gliederung (`##`-Überschriften) als Folienstruktur — didaktisch verdichtet,
   **keine 1:1-Textkopie**. Faustregel: ca. 1 Folie pro 30–50 Zeilen Quelltext, mehr bei sehr
   dichten Kapiteln mit vielen Fallbeispielen (dann ggf. `#`-Section-Divider-Folien pro
   Fallbeispiel).
3. **Nicht übernehmen:** Quiz-/Übungsaufgaben-Abschnitte (`exams2forms(...)`-Chunks,
   "Quiz-Aufgaben"-Überschriften, `quiz_*_files`-Listen).
4. **R-Code-Chunks nicht ausführen lassen** (kein `{r}`-Chunk-Format verwenden). Für Abbildungen,
   die im Original per knitr erzeugt wurden: zuerst `ls <kapitel>_files/figure-html/` prüfen — die
   PNGs liegen dort meist schon vorgerendert (Buch wurde bereits gebaut) und können direkt per
   Markdown-Bild mit Pfad `../<kapitel>_files/figure-html/<datei>.png` eingebunden werden. Feste
   Bilder aus `img/` analog mit `../img/<datei>` übernehmen.
5. **Mermaid-Diagramme** (` ```{mermaid} `) unverändert übernehmen — sie werden clientseitig
   gerendert, nicht ausgeführt.
6. **Mathe-Formeln** ($…$, $$…$$) unverändert übernehmen. Lange `$$…$$`-Anzeigeformeln, die auf
   der Folie über den Rand hinauslaufen würden, entweder in
   `\begin{aligned} … &= … \\ &= … \end{aligned}` umbrechen, oder — wenn die Formel sich nicht
   sinnvoll umbrechen lässt (eine einzelne lange Zeile ohne natürlichen Bruchpunkt) — direkt beim
   Schreiben in einen Font-Size-Wrapper packen (siehe Abschnitt 4, Punkt 5). Bei bekanntermaßen
   langen Formeln lieber gleich mitdenken statt erst beim Überlaufcheck zu reagieren.
7. **Definitionen/Sätze** (`:::{#def-…}` / `:::{#thm-…}`) als eigene Folie mit dem Label als
   Überschrift übernehmen, formatiert als
   `:::{.callout-note title="Definition: XYZ"} … :::` bzw. `title="Satz: XYZ"` — ohne
   Quarto-Crossref-ID-Syntax (`{#def-…}` weglassen, das Deck ist kein Buchkapitel).
8. **Interne Crossrefs** (`@sec-…`, `@fig-…`, `@tbl-…`, `@def-…`, `@thm-…`), die innerhalb der
   Foliendatei nicht auflösbar sind, durch normalen Text ersetzen oder weglassen — sonst meldet
   der Render "Unable to resolve crossref".
9. Zitationen (`@key`) können unverändert übernommen werden (gleiche `bibliography`).

## 3. Rendern & Fehler beheben

```bash
cd slides && quarto render <kapitel>.qmd
```

Render muss fehler- und warnungsfrei durchlaufen (insbesondere keine
"Unable to resolve crossref"-Meldungen).

## 4. Überlauf prüfen (Pflichtschritt)

RevealJS zeigt überlaufenden Folieninhalt nicht automatisch mit Scrollbalken an — zu viel Inhalt
"fließt unten über" (vertikal) oder läuft rechts aus der Folie hinaus (horizontal, typisch bei
langen `$$…$$`-Formeln). Beides ist mit bloßem Lesen des Quelltexts nicht zuverlässig erkennbar
und muss automatisiert per Headless-Chrome geprüft werden (`check_overflow.py` misst sowohl
`scrollHeight` als auch `scrollWidth` jeder Folie gegen die konfigurierte Foliengröße):

```bash
# einmalig, z. B. in einem Scratch-Verzeichnis:
python3 -m venv /tmp/quarto-slides-venv
/tmp/quarto-slides-venv/bin/pip install --quiet websocket-client requests

# Prüfung (aus dem Skill-Verzeichnis heraus aufrufen). Braucht Internet (MathJax lädt vom CDN)
# und wartet bewusst mehrere Sekunden, bis MathJax das gesamte Deck fertig gesetzt hat — das ist
# kein Hänger, sondern nötig, damit Formelbreiten überhaupt korrekt gemessen werden können.
/tmp/quarto-slides-venv/bin/python \
  <pfad-zu-diesem-skill>/check_overflow.py slides/ <kapitel>.html
```

**Verlässlichkeit des Checkers:** `check_overflow.py` navigiert inzwischen aktiv zu JEDER einzelnen
Folie (`Reveal.slide(h, v)`) statt alle `<section>`-Elemente pauschal im DOM abzufragen, und misst
gezielt das *innerste* `.present`-Element. Beides war nötig, weil zwei subtile Bugs in früheren
Versionen einen Großteil der vertikalen Unter-Folien (Section-Divider-Stapel) faktisch nie geprüft
haben (RevealJS setzt bei inaktiven Vertikal-Folien `display:none`, und bei aktiven Stapeln landet
die `.present`-Klasse auf ZWEI Elementen gleichzeitig — dem äußeren Stapel-Container und der
eigentlichen Folie — wodurch ein naiver `querySelector` das falsche, oft mit fixer Höhe versehene
äußere Element gemessen hat). Falls der Checker nach einer künftigen Änderung an RevealJS/Quarto
wieder verdächtig viele identische Werte für verschiedene Folien meldet: erster Verdacht ist wieder
ein falsch gegriffenes Element, nicht ein echter Überlauf — mit einem gezielten Einzel-Slide-Dump
(siehe Kommentare im Skript) gegenprüfen, bevor man blind Fixes anwendet.

Exit-Code 0 = keine überlaufende Folie. Ausgabe zeigt pro überlaufender Folie Foliennummer, Titel,
und ob `height` (vertikal) und/oder `width` (horizontal) betroffen ist. Je nach Ursache in dieser
Reihenfolge beheben, jeweils leichteste Option zuerst, danach neu rendern + erneut prüfen:

**Vertikaler Überlauf (`height`) — zu viel Inhalt insgesamt:**

1. **`{.smaller}` auf die Überschrift** (`## Titel {.smaller}`) — meist ausreichend bei
   Textfolien mit vielen Bullet-Points oder Definitionen.
2. **Bildbreiten reduzieren** bei Folien mit 2–3 nebeneinander stehenden Bildern
   (`{width="X%"}` verkleinern).
3. **Zwei-Spalten-Layout** für Folien mit klar trennbaren Blöcken (z. B. Diagramm + Definitionen):
   ```markdown
   :::: {.columns}
   ::: {.column width="40%"}
   <erster Block>
   :::
   ::: {.column width="60%"}
   <zweiter Block>
   :::
   ::::
   ```
4. **`zoom`-Wrapper** für sehr große Mermaid-Baumdiagramme, die allein schon die Folie sprengen:
   ```markdown
   ::: {style="zoom: 0.7;"}
   ```{mermaid}
   ...
   ```
   :::
   ```
5. **Letztes Mittel:** Folie in zwei Folien splitten, wenn keine der obigen Optionen greift.

**Horizontaler Überlauf (`width`) — meist eine zu lange `$$…$$`-Formel oder ein langer
Inline-Term ($…$) in einem Bullet ohne Umbruchmöglichkeit:**

1. Lässt sich die Formel sinnvoll umbrechen: `\begin{aligned} … &= … \\ &= … \end{aligned}`
   verwenden.
2. Lässt sie sich nicht umbrechen (eine einzelne lange Zeile): **die Formel selbst in einen
   Font-Size-Wrapper packen**, direkt um das `$$…$$` (bzw. um die ganze Bullet-Zeile bei
   Inline-Formeln):
   ```markdown
   ::: {style="font-size: 0.7em;"}
   $$Pr(\ldots) = \ldots$$
   :::
   ```
   Je nach Länge 0.6–0.75em wählen und danach mit dem Checker verifizieren.

   **Wichtig — was NICHT funktioniert:** Eine globale CSS-Regel, die nachträglich auf die von
   MathJax erzeugten `.MathJax_Display`-Elemente zielt (z. B. in `specifics/styles.css`), hat
   **keine Wirkung**. MathJax v2 (HTML-CSS-Output, von Quarto/RevealJS standardmäßig verwendet)
   berechnet beim Typesetting einmalig absolute Pixelgrößen für die inneren Glyph-Elemente: eine
   spätere Änderung der `font-size` am äußeren Container — egal ob per CSS oder per JS — wird von
   den bereits gesetzten Kindelementen ignoriert (getestet und bestätigt). Der Font-Size-Wrapper
   funktioniert nur, weil er schon **vor** dem Rendern um den rohen `$$…$$`-Quelltext steht, sodass
   MathJax die reduzierte Schriftgröße direkt beim Typesetting berücksichtigt. Nachträgliches
   Verkleinern nach dem Rendern ist keine Option.
3. `specifics/styles.css` enthält bereits ein `!important`-Sicherheitsnetz
   (`.reveal .slides .MathJax_Display { overflow-x: auto; max-width: 100%; }`), falls doch einmal
   eine Formel durchrutscht — das verhindert nur, dass sie *unsichtbar* über den Rand hinausläuft
   (zeigt notfalls einen Scrollbalken), ersetzt aber nicht den Font-Size-Wrapper als eigentliche
   Lösung.

Wiederhole Render + Überlaufprüfung, bis der Exit-Code 0 ist und keine Folie mehr overflowt.

## 5. Bericht

Am Ende kurz zusammenfassen: Anzahl Folien, Render-Status, verwendete Bilder, ggf. behobene
Überlauf-Folien.
