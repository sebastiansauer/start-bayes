library(ggplot2)
library(scales)

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

# Beschriftung direkt im Balken (statt Legende und y-Achse):
# Kategorie, Anteil und (beim Überlebt-Segment) die absoluten Zahlen
df$Label <- ifelse(
  df$Status == "überlebt",
  paste0(
    "überlebt\n", percent(df$Prozent, accuracy = 1),
    "\n(", df$Anzahl, "/", df$Summe, ")"
  ),
  paste0("nicht überlebt ", percent(df$Prozent, accuracy = 1))
)

# Textfarbe je Füllung: weiß auf Dunkelblau, fast schwarz auf Orange
df$Textfarbe <- ifelse(df$Status == "überlebt", "#FFFFFF", "#1A1A1A")

# Gestapelte Balken: weiße Trennlinie, Werte im Balken, keine Legende,
# keine y-Achse, kein Gitter (Tufte). Okabe-Ito: Dunkelblau / Orange.
plot_kausalstudie_a <-
  ggplot(df, aes(x = Geschlecht, y = Prozent, fill = Status)) +
  geom_col(position = "fill", color = "white", linewidth = 1.2, width = 0.85) +
  geom_text(
    aes(label = Label, color = Textfarbe),
    position = position_stack(vjust = 0.5),
    lineheight = 0.9,
    size = 3.4,
    fontface = "bold"
  ) +
  facet_wrap(~Gruppe) +
  scale_fill_manual(values = c("nicht überlebt" = "#E69F00", "überlebt" = "#0072B2")) +
  scale_color_identity() +
  labs(
    x = NULL, y = NULL,
    title = "Überlebensrate nach Geschlecht, mit/ohne Medikament"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    axis.text.x = element_text(size = rel(1.05)),
    strip.text = element_text(face = "bold")
  )
