# Exportiert Daten + Posterior von m_kung_gewicht_c (Kap. 0900) als JSON
# für das HyperFrames-Video "post-mu-regression".
# Aufruf (aus dem Repo-Root): Rscript videos/post-mu-regression/scripts/export-data.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(rstanarm)
  library(jsonlite)
})

kung_erwachsen <- read_csv("data/Howell1a.csv", show_col_types = FALSE) |>
  filter(age > 18) |>
  mutate(weight_c = weight - mean(weight))

# gleiches Modell wie im Kapitel (Chunk "m3")
m_kung_gewicht_c <-
  stan_glm(height ~ weight_c,
           prior = normal(0, 10),
           prior_intercept = normal(178, 20),
           prior_aux = exponential(0.1),
           refresh = 0, seed = 42,
           data = kung_erwachsen)

post <- as_tibble(m_kung_gewicht_c)
a <- post$`(Intercept)`
b <- post$weight_c
w_mean <- mean(kung_erwachsen$weight)

mu_at <- function(w) a + b * (w - w_mean)

# Dichte von mu bei Gewicht w: 61 Punkte auf m ± 4 sd (KDE der Draws)
dens_at <- function(w, n = 61) {
  mu <- mu_at(w)
  m <- mean(mu); s <- sd(mu)
  grid <- seq(m - 4 * s, m + 4 * s, length.out = n)
  d <- density(mu, from = min(grid), to = max(grid), n = n)
  list(w = w, m = m, s = s,
       lo = unname(quantile(mu, .025)), hi = unname(quantile(mu, .975)),
       y = round(grid, 3), d = signif(d$y, 4))
}

w_grid <- seq(32, 63, by = 0.5)
band <- map(w_grid, \(w) {
  mu <- mu_at(w)
  list(w = w, m = round(mean(mu), 3),
       lo = round(unname(quantile(mu, .025)), 3),
       hi = round(unname(quantile(mu, .975)), 3),
       s = round(sd(mu), 4))
})

out <- list(
  n = nrow(kung_erwachsen),
  w_mean = w_mean,
  a = mean(a), b = mean(b),
  points = kung_erwachsen |> transmute(x = round(weight, 2), y = round(height, 2)),
  band = band,
  sweep = map(w_grid, dens_at),
  focus = map(c(40, 45, 50, 55), dens_at)
)

write_json(out, "videos/post-mu-regression/data.json", auto_unbox = TRUE, digits = NA)

cat("n =", out$n, "\n")
walk(out$focus, \(f) cat(sprintf("w=%d  m=%.2f  sd=%.3f\n", f$w, f$m, f$s)))
cat("range weight:", range(kung_erwachsen$weight), " height:", range(kung_erwachsen$height), "\n")

# zusätzlich als JS-Datei, damit die Komposition ohne fetch() auskommt
writeLines(paste0("window.DATA = ", readLines("videos/post-mu-regression/data.json", warn = FALSE), ";"),
           "videos/post-mu-regression/data.js")
