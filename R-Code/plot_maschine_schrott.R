# Pakete laden
library(ggplot2)
library(ggmosaic)
library(dplyr)

# Daten vorbereiten
df <- tibble::tibble(
  Maschine = c("M1", "M2", "M3"),
  Anteil = c(0.60, 0.10, 0.30),
  Defekt = c(0.05, 0.02, 0.04)
) %>%
  # Wahrscheinlichkeiten für defekt / nicht defekt
  rowwise() %>%
  mutate(
    Schrott = Anteil * Defekt,
    ok = Anteil * (1 - Defekt)
  ) %>%
  select(Maschine, Schrott, ok) %>%
  tidyr::pivot_longer(
    cols = c(Schrott, ok),
    names_to = "Status",
    values_to = "Prob"
  )

# Mosaicplot
plot_maschine_schrott_ALT <-
  ggplot(data = df) +
  geom_mosaic(aes(
    weight = Prob,
    x = product("Maschine"),
    fill = Status
  )) +
  labs(
    x = "Maschine",
    y = "Anteil"
  ) +
  scale_fill_manual(
    values = c("Schrott" = "#E69F00", "ok" = "#0072B2"),
    labels = c("Ausschuss", "OK")
  ) +
  theme(legend.position = "bottom")


# Pakete laden
library(ggplot2)
library(dplyr)
library(tidyr)
library(see)

# Daten vorbereiten
df <- tibble::tibble(
  Maschine = c("M1", "M2", "M3"),
  Anteil = c(0.60, 0.10, 0.30),
  Defekt = c(0.05, 0.02, 0.04)
) %>%
  # Wahrscheinlichkeiten für defekt / nicht defekt berechnen
  rowwise() %>%
  mutate(
    Schrott = Anteil * Defekt,
    ok = Anteil * (1 - Defekt)
  ) %>%
  select(Maschine, Schrott, ok) %>%
  # Daten in das "long" Format bringen, das ggplot benötigt
  pivot_longer(
    cols = c(Schrott, ok),
    names_to = "Status",
    values_to = "Prob"
  )

# Gestapelte Balken im Tufte-Stil: Kategorie und Anteil im Balken, keine Legende,
# keine y-Achse, kein Gitter, weisse Trennlinie. Okabe-Ito: Orange (Ausschuss) / Blau (OK).
df <- df %>%
  mutate(Status = factor(Status, levels = c("Schrott", "ok")))

# Ausschuss-Segmente sind sehr schmal: ihr Label steht ueber dem Balken (dunkles Orange),
# das OK-Label im Balken.
d_ok <- filter(df, Status == "ok")
d_schrott <-
  df %>%
  group_by(Maschine) %>%
  mutate(oben = sum(Prob)) %>%
  ungroup() %>%
  filter(Status == "Schrott")

plot_maschine_schrott <-
  ggplot(data = df, aes(x = Maschine, y = Prob, fill = Status)) +
  geom_col(position = "stack", color = "white", linewidth = 1.2, width = 0.85) +
  geom_text(data = d_ok, aes(label = sprintf("OK %.3f", Prob)),
            position = position_stack(vjust = 0.5),
            color = "#FFFFFF", fontface = "bold", size = 3.6) +
  geom_text(data = d_schrott, aes(y = oben, label = sprintf("Ausschuss %.3f", Prob)),
            vjust = -0.4, color = "#B36B00", fontface = "bold", size = 3.6) +
  scale_fill_manual(values = c(Schrott = "#E69F00", ok = "#0072B2")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Maschine", y = "Anteil an der Produktion") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )
