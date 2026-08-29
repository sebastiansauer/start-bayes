## Erstellt die Print-Titelseite (LaTeX/PDF-Cover) für Start:Bayes!,
## im Layout an das Statistik1-Cover angelehnt.
## Canvas: 7in x 10in @ 300dpi = 2100x3000 px (Buch-Trimgröße, siehe
## _quarto.yml, titlepage-pdf-Format, geometry: paperwidth/paperheight).

library(ggplot2)
library(patchwork)
library(magick)
library(tidyverse)

out_dir <- "img/start-bayes-cover"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

canvas_w <- 2100L
canvas_h <- 3000L
bg_col   <- "#D5D5D5"
font_fam <- "Lato"

## 1) Prior/Likelihood/Posterior-Plot (aus R-Code/img241.R), hochaufgelöst
sequence_length <- 1e3

d <-
  tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>%
  expand(probability, row = c("flat", "stepped", "Laplace")) %>%
  arrange(row, probability) %>%
  mutate(prior = ifelse(row == "flat", 1,
                         ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                                exp(-abs(probability - 0.5) / .25) / (2 * 0.25))),
         likelihood = dbinom(x = 6, size = 9, prob = probability)) %>%
  group_by(row) %>%
  mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>%
  pivot_longer(prior:posterior) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("prior", "likelihood", "posterior")),
         row  = factor(row, levels = c("flat", "stepped", "Laplace")))

base_theme <- theme_minimal(base_size = 22, base_family = font_fam) +
  theme(panel.grid = element_blank(),
        axis.text  = element_blank(),
        strip.text = element_text(face = "bold", size = 30))

p1 <- d %>% filter(row == "flat") %>%
  ggplot(aes(probability, value)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  base_theme +
  facet_wrap(~name, scales = "free_y")

p2 <- d %>% filter(row == "stepped") %>%
  ggplot(aes(probability, value)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  base_theme +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~name, scales = "free_y")

p3 <- d %>% filter(row == "Laplace") %>%
  ggplot(aes(probability, value)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  base_theme +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  facet_wrap(~name, scales = "free_y")

plot241 <- (p1 / p2 / p3) &
  theme(plot.background = element_rect(fill = bg_col, color = NA),
        panel.background = element_rect(fill = bg_col, color = NA))

plot_path <- file.path(out_dir, "prior-lik-post-cover.png")
ggsave(plot_path, plot241, width = 15, height = 13, dpi = 300, bg = bg_col)

## 2) Canvas zusammensetzen
canvas <- image_blank(canvas_w, canvas_h, color = bg_col)

## Titel + Golem-Maskottchen nebeneinander in der oberen Zone
## Beide Elemente werden mit fixer, absoluter Pixelgroesse direkt auf den
## Canvas gesetzt (kein gemeinsames Resize hinterher), damit sich ihre
## Groessen unabhaengig voneinander justieren lassen.
title_block_h <- 500
title_block_y <- 120

title_block_w <- 1250
title_img <- image_blank(title_block_w, title_block_h, color = bg_col) %>%
  image_annotate("Start:Bayes!", size = 190, font = font_fam, weight = 700,
                  gravity = "center", color = "black")
title_block_x <- (canvas_w - title_block_w) / 2
canvas <- image_composite(canvas, title_img,
                           offset = paste0("+", title_block_x, "+", title_block_y))

## Untertitel (direkt unter dem Titelblock)
subtitle_h <- 150
subtitle_y <- title_block_y + title_block_h + 20
subtitle_img <- image_blank(canvas_w - 300, subtitle_h, color = bg_col) %>%
  image_annotate("Einführung in die Bayes-Statistik mit R",
                  size = 78, font = font_fam, weight = 400,
                  gravity = "center", color = "black") %>%
  image_background(bg_col, flatten = TRUE)
canvas <- image_composite(canvas, subtitle_img,
                           offset = paste0("+150+", subtitle_y))

## Golem-Maskottchen, buendig unter dem Untertitel, zentriert
## Golem_hex.png hat einen opaken weissen Hintergrund (kein Alpha) --
## erst Weiss transparent stanzen, dann auf bg_col flatten.
golem_h <- 280
golem <- image_read("img/Golem_hex.png") %>%
  image_transparent(color = "white", fuzz = 5) %>%
  image_resize(paste0("x", golem_h)) %>%
  image_background(bg_col, flatten = TRUE)
golem_w <- image_info(golem)$width
golem_x <- (canvas_w - golem_w) / 2
golem_y <- subtitle_y + subtitle_h
canvas <- image_composite(canvas, golem,
                           offset = paste0("+", golem_x, "+", golem_y))

## Zentrale Grafik
plot_img <- image_read(plot_path) %>%
  image_background(bg_col, flatten = TRUE)
plot_scaled <- image_resize(plot_img, paste0(canvas_w - 400, "x"))
plot_y <- golem_y + golem_h + 60
canvas <- image_composite(canvas, plot_scaled, offset = paste0("+200+", plot_y))

## Autor
author_img <- image_blank(canvas_w, 300, color = bg_col) %>%
  image_annotate("Sebastian Sauer", size = 130, font = font_fam, weight = 700,
                  gravity = "center", color = "black") %>%
  image_background(bg_col, flatten = TRUE)
canvas <- image_composite(canvas, author_img, offset = paste0("+0+", canvas_h - 400))

image_write(canvas, path = file.path(out_dir, "start-bayes-cover.001.png"), format = "png", density = "300x300")

cat("Cover geschrieben nach:", file.path(out_dir, "start-bayes-cover.001.png"), "\n")
