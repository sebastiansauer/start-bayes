library(tidyverse)
# library(beyonce)
# library(brms)
library(patchwork)
# library(tidybayes)
# library(bayesplot)

# Genutzt für Abb 8.2 (m_kung) in Start-Bayes 8.3.4

# Source: https://bookdown.org/content/3686/16.html

# bp <- beyonce_palette(127)[]
# beyonce_palette(1)
col1 <- "grey20"
col2 <- "black"
col3 <- "grey80"

# Normal density
p1 <- tibble(x = seq(from = -3, to = 3, by = 0.1)) |>
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3, color = blue) +
  annotate(
    geom = "text",
    x = 0, y = 0.2,
    label = "Normal",
    color = col1, size = 7
  ) +
  annotate(
    geom = "text",
    x = c(0, 1.5), y = 0.6,
    label = c("italic(M)", "italic(S)[mu]"),
    color = col1, family = "Times", parse = TRUE, size = 7
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col2, linewidth = 0.5))

# Half-normal density
p2 <- tibble(
  x = seq(from = 0, to = 3, by = 0.01),
  d = (dnorm(x)) / max(dnorm(x))
) |>
  ggplot(aes(x = x, y = d)) +
  geom_area(fill = col3) +
  annotate(
    geom = "text",
    x = 1.5, y = 0.2,
    label = "Halbnormal",
    color = col1, size = 7
  ) +
  annotate(
    geom = "text",
    x = 1.5, y = 0.6,
    label = "0*','*~italic(S)[sigma]",
    color = col1, family = "Times", parse = TRUE, size = 7
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col2, linewidth = 0.5))


# Exponential density
p2a <- tibble(x = seq(from = 0, to = 1, by = 0.01)) |>
  ggplot(aes(x = x, y = (dexp(x, 2) / max(dexp(x, 2))))) +
  geom_area(fill = col3, color = green) +
  annotate(
    geom = "text",
    x = 0.5, y = 0.2,
    label = "exp",
    color = col1, size = 7
  ) +
  annotate(
    geom = "text",
    x = 0.5, y = 0.6,
    label = "italic(lambda)",
    color = col1, family = "Times", parse = TRUE, size = 7
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col2, linewidth = 0.5))


## Two annotated arrows
# Save our custom arrow settings
my_arrow <- arrow(angle = 20, length = unit(0.35, "cm"), type = "closed")

p3 <- tibble(
  x = c(0.43, 1.5),
  y = c(1, 1),
  xend = c(0.43, 0.8),
  yend = c(0.2, 0.2)
) |>
  ggplot(aes(
    x = x, xend = xend,
    y = y, yend = yend
  )) +
  geom_segment(arrow = my_arrow, color = col2) +
  annotate(
    geom = "text",
    x = c(0.3, 1), y = 0.6,
    label = "'~'",
    color = col1, family = "Times", parse = TRUE, size = 10
  ) +
  xlim(0, 2) +
  theme_void()

# A second normal density
p4 <- tibble(x = seq(from = -3, to = 3, by = 0.1)) |>
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3, color = orange) +
  annotate(
    geom = "text",
    x = 0, y = 0.2,
    label = "Normal",
    color = col1, size = 7
  ) +
  annotate(
    geom = "text",
    x = c(0, 1.5), y = 0.6,
    label = c("mu", "sigma"),
    color = col1, family = "Times", parse = TRUE, size = 7
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col2, linewidth = 0.5))

# The final annotated arrow
p5 <- tibble(
  x = c(0.375, 0.625),
  y = c(1 / 3, 1 / 3),
  label = c("'~'", "italic(i)")
) |>
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(color = col1, family = "Times", parse = TRUE, size = c(10, 7)) +
  geom_segment(
    x = 0.5, xend = 0.5,
    y = 1, yend = 0,
    arrow = my_arrow, color = col2
  ) +
  xlim(0, 1) +
  theme_void()

# Some text
p6 <- tibble(
  x = 0.5,
  y = 0.5,
  label = "italic(h[i])"
) |>
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(color = col1, family = "Times", parse = TRUE, size = 7) +
  xlim(0, 1) +
  theme_void()

# Define the layout
layout <- c(
  area(t = 1, b = 2, l = 1, r = 2),
  area(t = 1, b = 2, l = 3, r = 4),
  area(t = 4, b = 5, l = 1, r = 2),
  area(t = 3, b = 4, l = 1, r = 4),
  area(t = 6, b = 6, l = 1, r = 2),
  area(t = 7, b = 7, l = 1, r = 2)
)

# Combine and plot!
(p1 + p2a + p4 + p3 + p5 + p6) +
  plot_layout(design = layout) &
  ylim(0, 1) &
  theme(plot.margin = margin(0, 5.5, 0, 5.5))
