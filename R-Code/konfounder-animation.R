# Animation "Was heißt Kontrollieren?" für Kap. 1150 (Konfundierung).
# Nutzt d_sim aus R-Code/controlling-confounder.R.
# Erzeugt img/konfounder-kontrollieren-anim.gif; nicht Teil des Book-Renders,
# bei Bedarf von Hand ausführen:
#   Rscript R-Code/konfounder-animation.R

library(tidyverse)
library(gganimate)

source("R-Code/controlling-confounder.R")

schluessel <- gruppen_schluessel(c("A", "B"), okabe_linie[1:2], c(16, 17))

stufen <- c(
  "1\\. Rohdaten: x und y scheinen zusammenzuhängen",
  paste0("2\\. Der Konfounder: Gruppe ", schluessel),
  paste0("3\\. Kontrollieren: Gruppenmittel abziehen; Gruppe ", schluessel),
  paste0("4\\. Ergebnis: Kein Zusammenhang; Gruppe ", schluessel)
)

d_konf <-
  d_sim |>
  mutate(id = row_number()) |>
  group_by(group) |>
  mutate(x_kontr = x - mean(x),
         y_kontr = y - mean(y)) |>
  ungroup() |>
  # Gesamtmittelwert wieder addieren, damit die Punktwolke an ihrem Platz bleibt
  mutate(x_kontr = x_kontr + mean(x),
         y_kontr = y_kontr + mean(y),
         form_gruppe = c(A = 16, B = 17)[group])

d_anim <-
  bind_rows(
    # Stufe 1: Gruppen noch unbekannt, also alle Punkte gleich (graue Kreise)
    d_konf |> mutate(stufe = stufen[1], farbe = dunkelgrau, form = 16),
    d_konf |> mutate(stufe = stufen[2], farbe = farbe_linie, form = form_gruppe),
    d_konf |> mutate(stufe = stufen[3], farbe = farbe_linie, form = form_gruppe,
                     x = x_kontr, y = y_kontr),
    d_konf |> mutate(stufe = stufen[4], farbe = farbe_linie, form = form_gruppe,
                     x = x_kontr, y = y_kontr)
  ) |>
  mutate(stufe = factor(stufe, levels = stufen))

# Gesamt-Trendgerade je Stufe selbst schätzen (zwei Endpunkte, gleiche
# Gruppe über alle Stufen), damit gganimate sie flüssig mitkippen lässt;
# ein geom_smooth würde je Stufe als eigene Linie ein- und ausgeblendet.
d_trend <-
  d_anim |>
  group_by(stufe) |>
  reframe({
    fit <- lm(y ~ x)
    x_end <- range(x)
    tibble(x = x_end, y = predict(fit, tibble(x = x_end)))
  }) |>
  mutate(punkt = rep(1:2, length.out = n()))

p_konf_anim <-
  d_anim |>
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = farbe, shape = form, group = id), size = 2.5) +
  geom_line(data = d_trend, aes(group = 1),
            color = trendfarbe, linewidth = trend_linienbreite) +
  scale_color_identity() +
  scale_shape_identity() +
  # gganimate umgeht den Hook y_label_nach_oben(); daher y-Titel hier von Hand
  # als Untertitel über dem Panel. Der Gruppenschlüssel steht ab Stufe 2 im
  # (Markdown-)Titel, damit Stufe 1 die Gruppen noch nicht verrät.
  labs(title = "{closest_state}", subtitle = "y", y = NULL) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = ggtext::element_markdown(hjust = 0, face = "plain")) +
  transition_states(stufe, transition_length = 2, state_length = 2,
                    wrap = TRUE) +
  ease_aes("cubic-in-out")

anim_save(
  "img/konfounder-kontrollieren-anim.gif",
  animate(p_konf_anim, nframes = 200, fps = 20,
          width = 7, height = 7 * 0.618, units = "in", res = 120,
          renderer = magick_renderer())
)
