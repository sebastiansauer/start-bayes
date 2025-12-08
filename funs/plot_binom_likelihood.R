# Funktion: Likelihood-Funktion plotten
plot_binom_likelihood <- function(x, n, p_step = 0.1) {
  # Erzeuge p-Werte
  p <- seq(0, 1, by = p_step)

  # Likelihood berechnen
  likelihood <- dbinom(x, size = n, prob = p)

  # Dataframe
  df <- data.frame(p, likelihood)

  # Plot
  ggplot(df, aes(x = p, y = likelihood)) +
    geom_line(color = "grey80") +
    geom_point(aes(x = p, y = likelihood), color = blue, size = 2) +
    labs(
      title = paste("Likelihood-Funktion für Binomialverteilung"),
      subtitle = paste(x, "Treffer bei", n, "Versuchen"),
      x = "p",
      y = "L(p)"
    ) +
    theme_minimal(base_size = 14)
}

# Beispielaufruf: 6 Erfolge bei 9 Versuchen
# plot_binom_likelihood(6, 9, p_step = 0.01)
