
# Binomialverteilung im Tufte-Stil:
# schmale Balken, Wert direkt über dem Balken, keine y-Achse, kein Gitter,
# weiße waagerechte Linien durch die Balken (Tufte) statt y-Achse.
binomial_plot <- function(n, p){
  d <- tibble::tibble(
    Treffer = 0:n,
    Wahrscheinlichkeit = dbinom(0:n, n, p)
  )

  ggplot(d, aes(x = Treffer, y = Wahrscheinlichkeit)) +
    geom_col(fill = mittelgrau, width = 0.5) +
    # weiße waagerechte Linien über den Balken (ersetzen die y-Achse)
    geom_hline(yintercept = setdiff(pretty(c(0, max(d$Wahrscheinlichkeit)), n = 4), 0),
               color = "white", linewidth = 0.6) +
    # Wert direkt über dem Balken
    geom_text(aes(label = sprintf("%.2f", Wahrscheinlichkeit)),
              vjust = -0.5, size = 3.5, color = "#1A1A1A") +
    scale_x_continuous(breaks = 0:n) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Anzahl Treffer (k)", y = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}
