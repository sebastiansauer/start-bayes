library(tidyverse)
library(beyonce)
library(brms)
library(patchwork)
library(tidybayes)
library(bayesplot)

bp <- beyonce_palette(126)[]
#beyonce_palette(1)

col1 <- "grey20"
col2 <- "black"
col3 <- "grey80"




# Normal density
p1 <- tibble(x = seq(from = -3, to = 3, by = 0.1)) |> 
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3) +
  annotate(geom = "text",
           x = 0, y = 0.2,
           label = "Normal",
           color = col1, size = 7) +
  annotate(geom = "text",
           x = c(0, 1.5), y = 0.6,
           label = c("italic(M)[0]", "italic(S)[0]"), 
           color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col1, linewidth = 0.5))

# A second normal density
p2 <- tibble(x = seq(from = -3, to = 3, by = 0.1)) |> 
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3) +
  annotate(geom = "text",
           x = 0, y = 0.2,
           label = "Normal",
           color = col1, size = 7) +
  annotate(geom = "text",
           x = c(0, 1.5), y = 0.6,
           label = c("italic(M)[1]", "italic(S)[1]"), 
           color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col1, linewidth = 0.5))

## Two annotated arrows
# Save our custom arrow settings
my_arrow <- arrow(angle = 20, length = unit(0.35, "cm"), type = "closed")

p3 <- tibble(x = c(0.33, 1.67),
             y = c(1, 1),
             xend = c(0.75, 1.1),
             yend = c(0, 0)) |>
  ggplot(aes(x = x, xend = xend,
             y = y, yend = yend)) +
  geom_segment(arrow = my_arrow, color = col1) +
  annotate(geom = "text",
           x = c(0.4, 1.25), y = 0.5,
           label = "'~'",
           color = col1, family = "Times", parse = TRUE, size = 10) +
  xlim(0, 2) +
  theme_void()

# Exponential density
p4 <- tibble(x = seq(from = 0, to = 1, by = 0.01)) |> 
  ggplot(aes(x = x, y = (dexp(x, 2) / max(dexp(x, 2))))) +
  geom_area(fill = col3) +
  annotate(geom = "text",
           x = 0.5, y = 0.2,
           label = "exp",
           color = col1, size = 7) +
  annotate(geom = "text",
           x = 0.5, y = 0.6,
           label = "italic(K)",
           color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col1, linewidth = 0.5))

# Likelihood formula
p5 <- tibble(x = 0.5,
             y = 0.25,
             label = "beta[0]+beta[1]*italic(x)[italic(i)]") |> 
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  ylim(0, 1) +
  theme_void()

# Half-normal density
p6 <- tibble(x = seq(from = 0, to = 3, by = 0.01)) |> 
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3) +
  annotate(geom = "text",
           x = 1.5, y = 0.2,
           label = "Halb-Normal",
           color = col1, size = 7) +
  annotate(geom = "text",
           x = 1.5, y = 0.6,
           label = "0*','*~italic(S)[sigma]", 
           color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col1, linewidth = 0.5))

# Four annotated arrows
p7 <- tibble(x = c(0.43, 0.43, 1.5, 2.5),
             y = c(1, 0.55, 1, 1),
             xend = c(0.43, 1.225, 1.5, 1.75),
             yend = c(0.8, 0.15, 0.2, 0.2)) |>
  ggplot(aes(x = x, xend = xend,
             y = y, yend = yend)) +
  geom_segment(arrow = my_arrow, color = bp[1]) +
  annotate(geom = "text",
           x = c(0.3, 0.7, 1.38, 2), y = c(0.92, 0.22, 0.65, 0.6),
           label = c("'~'", "'='", "'='", "'~'"),
           color = bp[1], family = "Times", parse = TRUE, size = 10) +
  annotate(geom = "text",
           x = 0.43, y = 0.7,
           label = "nu*minute+1",
           color = bp[1], family = "Times", parse = TRUE, size = 7) +
  xlim(0, 3) +
  theme_void()


# Normal density
p8 <- tibble(x = seq(from = -3, to = 3, by = 0.1)) |> 
  ggplot(aes(x = x, y = (dnorm(x)) / max(dnorm(x)))) +
  geom_area(fill = col3) +
  annotate(geom = "text",
           x = 0, y = 0.2,
           label = "Normal",
           color = col1, size = 7) +
  annotate(geom = "text",
           x = 0, y = 0.6,
           label = "  ~~~~~mu[italic(i)]~~~sigma",
           color = col1, family = "Times", parse = TRUE, size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(axis.line.x = element_line(color = col1, linewidth = 0.5))

# The final annotated arrow
p9 <- tibble(x = c(0.375, 0.625),
             y = c(1/3, 1/3),
             label = c("'~'", "italic(i)")) |> 
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(color = col1, family = "Times", parse = TRUE, size = c(10, 7)) +
  geom_segment(x = 0.5, xend = 0.5,
               y = 1, yend = 0, 
               arrow = my_arrow, color = col1) +
  xlim(0, 1) +
  theme_void()

# Some text
p10 <- tibble(x = 0.5,
              y = 0.5,
              label = "italic(y[i])") |> 
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(color = col1, family = "Times", parse = TRUE, size = 7) +
  xlim(0, 1) +
  theme_void()

# Define the layout
layout <- c(
  area(t = 1, b = 2, l = 3, r = 5),  # p1
  area(t = 1, b = 2, l = 7, r = 9),  # p2
  area(t = 4, b = 5, l = 5, r = 7),  # p5
#  area(t = 4, b = 5, l = 5, r = 7),
  area(t = 4, b = 5, l = 9, r = 11),
  area(t = 3, b = 4, l = 3, r = 9),
  area(t = 7, b = 8, l = 5, r = 7),
  area(t = 6, b = 7, l = 1, r = 11),
  area(t = 9, b = 9, l = 5, r = 7),
  area(t = 10, b = 10, l = 5, r = 7))

# Combine and plot!
(p1 + p2  + p5 + p6 + p3 + p8 + p7 + p9 + p10) + 
  plot_layout(design = layout) &
  ylim(0, 1) &
  theme(plot.margin = margin(0, 5.5, 0, 5.5))

