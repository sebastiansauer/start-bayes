library(ggplot2)
library(scales)

# Daten vorbereiten
df <- data.frame(
  Gruppe = c(
    "Mit Medikament",
    "Mit Medikament",
    "Ohne Medikament",
    "Ohne Medikament"
  ),
  Geschlecht = c("Frauen", "Männer", "Frauen", "Männer"),
  Anzahl = c(263, 87, 80, 270),
  # SPALTE FÜR DIE ZUSÄTZLICHEN PROZENTE
  Zusatz_Prozent = c(0.73, 0.93, 0.69, 0.87) 
)

# Prozentwerte innerhalb der Gruppen berechnen (Base R-Methoden)
df$Summe <- ave(df$Anzahl, df$Gruppe, FUN = sum)
df$Prozent <- df$Anzahl / df$Summe
df$pos <- ave(df$Prozent, df$Gruppe, FUN = function(x) cumsum(x) - x / 2)
df$Symbol <- ifelse(df$Geschlecht == "Frauen", "\u2640", "\u2642")


# *** NEUES LABEL MIT HAKEN (TICK MARK) ***
# Haken: \u2713 (oder "✔")
df$Label <- paste0(scales::percent(df$Zusatz_Prozent), " OK", "\n", df$Anzahl, " ", df$Symbol)

# Plot
plot_kausalstudie_a <- 
  ggplot(df, aes(x = Gruppe, y = Prozent, fill = Geschlecht)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = Label),
    color = "white",
    fontface = "bold",
    position = position_stack(0.5),
    lineheight = 0.8
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "Anteil",
    title = "Geschlechterverteilung mit/ohne Medikament"
  ) +
  theme_minimal()