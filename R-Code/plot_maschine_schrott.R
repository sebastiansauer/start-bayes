# Pakete laden
library(ggplot2)
library(ggmosaic)
library(dplyr)

# Daten vorbereiten
df <- tibble::tibble(
  Maschine = c("M1", "M2", "M3"),
  Anteil   = c(0.60, 0.10, 0.30),
  Defekt   = c(0.05, 0.02, 0.04)
) %>%
  # Wahrscheinlichkeiten für defekt / nicht defekt
  rowwise() %>%
  mutate(
    Schrott = Anteil * Defekt,
    ok      = Anteil * (1 - Defekt)
  ) %>%
  select(Maschine, Schrott, ok) %>%
  tidyr::pivot_longer(
    cols = c(Schrott, ok),
    names_to = "Status",
    values_to = "Prob"
  )

# Mosaicplot
plot_maschine_schrott <-
  ggplot(data = df) +
  geom_mosaic(aes(
    weight = Prob,
    x = product(Maschine),
    fill = Status
  )) +
  labs(
    x = "Maschine",
    y = "Anteil"
  ) +
  scale_fill_manual(
    values = c("Schrott" = "red", "ok" = "grey"),
    labels = c("Ausschuss", "OK")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
