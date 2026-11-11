# SCRIPT — bayes-post-sampling

**Voice:** de-DE-ConradNeural (Azure AI Speech)
**Voice settings:** Stil `neutral`, Tempo 0.95, Pitch 0
**Voice direction:** Ruhig, freundlich, erklärend. Wie ein Dozent, der im Seminarraum
an der Tafel steht, nicht wie ein Werbesprecher. Kurze Pausen an Satzenden,
etwas längere Pause vor jedem „Jetzt …"-Wendepunkt.

Konvention: Jede `## Line` gehört zu einem Frame im `STORYBOARD.md`.
Nur die eingerückten Blöcke werden an TTS übergeben. Die Zeitangaben sind
Richtwerte; das echte Timing kommt aus den Wort-Zeitstempeln der TTS.

---

## Line 1 — Einstieg: zwei Fragen (Frame 1)

**Time:** 0.0 – 20.0s
**Delivery:** Gelassen beginnen, die zwei Punkte klar voneinander absetzen.

    Stellen Sie sich vor, Sie haben Daten gesammelt und ein Modell gebaut.
    Jetzt wollen Sie wissen: Was sagt das Modell, nachdem es die Daten gesehen hat?
    Das ist die Posteriori-Verteilung, kurz: die Post.
    In diesem Video geht es um zwei Dinge.
    Erstens: Wie man die Post mit Stichproben beschreibt, statt sie auszurechnen.
    Und zweitens: Warum unser bisheriges Werkzeug, die Bayes-Box, bei größeren Modellen nicht mehr ausreicht.

## Line 2 — Zur Erinnerung: die Bayes-Box (Frame 2)

**Time:** 20.0 – 58.0s
**Delivery:** Zügig, das ist Wiederholung. Beim Satz „Als Kurve …" kurz innehalten.

    Zur Erinnerung: der Globusversuch.
    Wir werfen einen Globus neun Mal und sehen sechs Mal Wasser.
    Gesucht ist der Wasseranteil, nennen wir ihn p.
    Die Bayes-Box ist eine Tabelle.
    Jede Zeile steht für einen möglichen Wert von p: null Prozent, zehn Prozent, zwanzig Prozent, und so weiter bis hundert.
    Für jede Zeile rechnen wir Prior mal Likelihood und teilen durch die Summe.
    Das ergibt die Posteriori-Wahrscheinlichkeit jeder Zeile.
    Fertig ist die Post.
    Als Kurve sieht sie so aus: Werte um sechzig bis siebzig Prozent sind am wahrscheinlichsten.

## Line 3 — Stichproben aus der Box (Frame 3)

**Time:** 58.0 – 112.0s
**Delivery:** Neugierig, als würde man einen Trick vorführen. „Schauen Sie, was passiert" mit kleiner Pause danach.

    Jetzt ein Trick, der zuerst merkwürdig wirkt.
    Wir legen die Zeilen der Box in eine Lostrommel.
    Aber nicht jede Zeile einmal.
    Eine Zeile mit hoher Posteriori-Wahrscheinlichkeit kommt oft in die Trommel, eine mit niedriger nur selten.
    Dann ziehen wir, mit Zurücklegen, zehntausend Mal.
    Jedes gezogene Los legen wir auf einen Stapel, und zwar an der Stelle seines Werts von p.
    Schauen Sie, was passiert.
    Die Stapel zeichnen die Kurve von eben nach.
    Nur haben wir jetzt keine Wahrscheinlichkeiten mehr, sondern Häufigkeiten.
    Und Häufigkeiten kann man zählen.
    Wie wahrscheinlich ist ein Wasseranteil über fünfzig Prozent?
    Einfach die Lose rechts von fünfzig zählen und durch zehntausend teilen.
    Keine Formel. Nur zählen.

## Line 4 — Der berechtigte Einwand (Frame 4)

**Time:** 112.0 – 132.0s
**Delivery:** Den Einwand mit leicht gehobener Stimme sprechen, dann ruhig bestätigen.

    Vielleicht denken Sie jetzt: Moment, für die Lostrommel brauchten wir doch die fertige Box.
    Was haben wir damit gewonnen?
    Zu Recht gefragt.
    Dieser Schritt war eine Übung, um uns an Stichproben zu gewöhnen.
    Das eigentliche Ziel ist ein anderes: Wir wollen an Stichproben aus der Post kommen, ohne die Box jemals ganz aufzuschreiben.
    Warum das wichtig ist, sehen wir jetzt.

## Line 5 — Die Box wird zu lang (Frame 5)

**Time:** 132.0 – 205.0s
**Delivery:** Die Zahlen langsam und deutlich. „Eine Eins mit zwanzig Nullen" fast beiläufig, dann Pause. Letzter Satz ruhig und bestimmt.

    Nehmen wir ein etwas größeres Modell.
    Wir wollen Klausurpunkte durch Lernstunden vorhersagen.
    Dafür braucht das Modell drei unbekannte Zahlen, also drei Parameter:
    wo die Gerade startet, wie steil sie ist, und wie stark die Punkte um sie herum streuen.
    Beim Globus hatten wir einen Parameter, und die Box war eine Reihe von Kästchen.
    Mit zwei Parametern wird daraus ein Gitter, denn jede Kombination braucht ihre eigene Zeile.
    Mit drei Parametern ein Würfel.
    Nehmen wir nur zehn Werte pro Parameter, was sehr grob ist, sind das schon tausend Zeilen.
    Mit hundert Werten pro Parameter: eine Million.
    Und eine Regression mit zehn Parametern, in der Psychologie ganz normal, bei hundert Werten:
    eine Eins mit zwanzig Nullen.
    Diese Box rechnet kein Computer bis zum Ende.
    Die Bayes-Box ist nicht falsch.
    Sie wird nur zu lang.

## Line 6 — Der Wanderer im Nebel (Frame 6)

**Time:** 205.0 – 280.0s
**Delivery:** Erzählend, wie eine kleine Geschichte. Die Schrittregel langsam, jeder der drei Teile für sich. „Sie passen zusammen" mit Nachdruck.

    Hier kommt die Idee, die das Problem löst.
    Stellen Sie sich die Post-Verteilung als Landschaft vor.
    Jede Stelle ist ein Parameterwert, und die Höhe ist seine Posteriori-Wahrscheinlichkeit.
    Über der Landschaft liegt dichter Nebel.
    Ein Wanderer steht darin und sieht nichts.
    Er kann nur eines: mit einem Höhenmesser prüfen, wie hoch er gerade steht.
    Jetzt gehen wir so vor.
    Der Wanderer schlägt einen Schritt in eine zufällige Richtung vor und misst dort die Höhe.
    Geht es bergauf, geht er immer.
    Geht es bergab, geht er nur manchmal.
    Und je tiefer es hinunterginge, desto seltener.
    Bei jedem Schritt notiert er, wo er steht.
    Das wiederholt er tausende Male.
    Was dabei herauskommt, sehen Sie hier.
    Dort, wo die Landschaft hoch ist, war der Wanderer oft.
    Dort, wo sie niedrig ist, nur selten.
    Sein Stapel aus Standorten zeichnet die Post-Verteilung nach.
    Legen wir die Kurve aus der Bayes-Box darüber:
    Sie passen zusammen.

## Line 7 — Warum das die Lösung ist (Frame 7)

**Time:** 280.0 – 312.0s
**Delivery:** Der Kernsatz „Der Wanderer hat die Landschaft nie gesehen" langsam. Den Namen MCMC deutlich buchstabieren.

    Der entscheidende Punkt: Der Wanderer hat die Landschaft nie gesehen.
    Er brauchte immer nur zwei Höhen: hier, und einen Schritt weiter.
    Die Box mit ihren Millionen Zeilen hat niemand aufgeschrieben.
    Und ob die Landschaft eine Linie ist, wie beim Globus, oder ein Gebirge mit zehn Dimensionen, wie bei der Regression:
    Für den Wanderer ändert das nichts.
    Dieses Verfahren heißt Markov-Chain-Monte-Carlo, kurz M-C-M-C.
    Monte Carlo steht für den Zufall in den Schritten.
    Markov-Kette dafür, dass jeder Schritt nur vom vorherigen Standort abhängt.
    Das Ergebnis sind Stichproben aus der Post, ohne dass jemand die Post ausgerechnet hat.

## Line 8 — Teaser: Stan in R, und Schluss (Frame 8)

**Time:** 312.0 – 340.0s
**Delivery:** Leicht, ausblickend. Den letzten Satz als Merksatz, mit Pause davor.

    In R müssen Sie den Wanderer nicht selbst bauen.
    Das übernimmt ein Programm namens Stan.
    Eine Zeile Code, und Sie bekommen eine Tabelle zurück:
    eine Zeile pro Standort des Wanderers, eine Spalte pro Parameter.
    Mit dieser Tabelle arbeiten Sie dann genau wie mit der Lostrommel von vorhin:
    zählen, Anteile bilden, Intervalle ablesen.
    Das ist der Weg, den wir ab jetzt gehen.
    Nicht ausrechnen, sondern wandern lassen und zählen.
