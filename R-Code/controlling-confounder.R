
library(ggrepel)

n <- 100

set.seed(42)

d_sim <-
  tibble(
    x = rnorm(n, 0, 0.5),
    y = rnorm(n, 0, 0.5),
    group = "A"
  ) %>%
  bind_rows(
    tibble(
      x = rnorm(n, 1, 0.5),
      y = rnorm(n, 1, 0.5),
      group = "B")
  )


p_konf1 <-
  d_sim %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = dunkelgrau) +
  geom_smooth(method = "lm", color = "#0072B2") +
  labs(title = "Ohne Kontrolle der Konfundierungsvariablen")

d_sim <- add_farben(d_sim, group)

d_sim_label <-
  d_sim %>%
  group_by(group) %>%
  slice_max(x, n = 1) %>%
  ungroup()

p_konf2 <-
  d_sim %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = farbe_linie)) +
  geom_smooth(aes(color = farbe_linie, group = group), method = "lm", se = FALSE) +
  geom_text_repel(
    data = d_sim_label,
    aes(label = group, color = farbe_text),
    fontface = "bold", direction = "y", hjust = 0, nudge_x = 0.3,
    segment.color = NA, show.legend = FALSE
  ) +
  scale_color_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
  theme(legend.position = "none")



