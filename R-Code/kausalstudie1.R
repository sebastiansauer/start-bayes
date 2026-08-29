library(ggplot2)
library(scales)
library(see)

# Daten vorbereiten: pro Medikamentengruppe x Geschlecht x Status ("überlebt"/"nicht überlebt")
# Zahlen entsprechen Tabelle 2.1 (tbl-studie-a)
df <- data.frame(
  Gruppe = rep(
    c("Mit Medikament", "Mit Medikament", "Ohne Medikament", "Ohne Medikament"),
    each = 2
  ),
  Geschlecht = rep(c("Männer", "Frauen", "Männer", "Frauen"), each = 2),
  Status = rep(c("überlebt", "nicht überlebt"), 4),
  Anzahl = c(
    81,
    87 - 81, # Männer, mit Medikament: 81/87 überlebt
    192,
    263 - 192, # Frauen, mit Medikament: 192/263 überlebt
    234,
    270 - 234, # Männer, ohne Medikament: 234/270 überlebt
    55,
    80 - 55 # Frauen, ohne Medikament: 55/80 überlebt
  )
)

df$Status <- factor(df$Status, levels = c("nicht überlebt", "überlebt"))

# Anteile innerhalb von Gruppe x Geschlecht berechnen
df$Summe <- ave(df$Anzahl, df$Gruppe, df$Geschlecht, FUN = sum)
df$Prozent <- df$Anzahl / df$Summe

df$Label <- ifelse(
  df$Status == "überlebt",
  paste0(
    df$Anzahl,
    "/",
    df$Summe,
    "\n(",
    percent(df$Prozent, accuracy = 1),
    ")"
  ),
  ""
)

# Plot: pro Medikamentengruppe (Facette) die Überlebensrate je Geschlecht
plot_kausalstudie_a <-
  ggplot(df, aes(x = Geschlecht, y = Prozent, fill = Status)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = Label),
    color = "white",
    fontface = "bold",
    position = position_stack(0.5),
    lineheight = 0.8,
    size = 3.2
  ) +
  facet_wrap(~Gruppe) +
  scale_y_continuous(labels = scales::percent) +
  # order = c(5, 4): Blau/Amber statt Orange/Hellblau, da Letztere im
  # Graustufendruck fast identisch hell sind (schlechter S/W-Kontrast)
  scale_fill_okabeito(order = c(5, 4)) +
  labs(
    x = "",
    y = "Anteil",
    fill = "",
    title = "Überlebensrate nach Geschlecht, mit/ohne Medikament"
  )
