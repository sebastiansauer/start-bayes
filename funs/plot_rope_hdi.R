# Rebuilds Kruschke (2018), Fig. 1 (ROPE/HDI decision rules) as native ggplot2/patchwork
# instead of the scanned img/Kruschke-2018-Fig1.png.

rope_hdi_panel <- function(tag, decision, mu, sigma, hdi_lo, hdi_hi,
                            rope_lo, rope_hi, null_value,
                            xlim = c(-4, 4)) {

  x <- seq(xlim[1], xlim[2], length.out = 400)
  d <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma))
  y_max <- max(d$y)

  ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_vline(xintercept = c(rope_lo, rope_hi),
                         color = "#D55E00", linewidth = 0.9) +
    ggplot2::geom_area(fill = "#56B4E9", alpha = 0.6) +
    ggplot2::geom_line(color = "#56B4E9", linewidth = 0.8) +
    ggplot2::annotate("segment",
                       x = hdi_lo, xend = hdi_hi,
                       y = -0.04 * y_max, yend = -0.04 * y_max,
                       linewidth = 3, color = "black") +
    ggplot2::annotate("segment",
                       x = null_value, xend = null_value,
                       y = 0, yend = -0.08 * y_max,
                       linewidth = 0.4, color = "#4D4D4D") +
    ggplot2::annotate("text", x = null_value, y = -0.16 * y_max,
                       label = "Nullwert", size = 3, color = "#4D4D4D") +
    ggplot2::annotate("text", x = mean(c(rope_lo, rope_hi)), y = y_max * 1.05,
                       label = "ROPE", size = 3.2, color = "#D55E00",
                       fontface = "bold") +
    ggplot2::annotate("text", x = mean(c(hdi_lo, hdi_hi)), y = 0.5 * y_max,
                       label = "HDI", size = 3.2, color = "black") +
    ggplot2::labs(title = paste0(tag, ")  ", decision),
                  x = "Parameterwert", y = NULL) +
    ggplot2::scale_y_continuous(limits = c(-0.2 * y_max, y_max * 1.15)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "italic"),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )
}

rope_hdi_grid <- function() {

  panels <- list(
    a = list(tag = "a", decision = "Entscheidung: Nullwert verwerfen",
             mu = 1.6, sigma = 0.5, hdi_lo = 0.7, hdi_hi = 2.5,
             rope_lo = -0.5, rope_hi = 0.1, null_value = -0.2),
    b = list(tag = "b", decision = "Entscheidung: Nullwert akzeptieren",
             mu = 0, sigma = 0.35, hdi_lo = -0.7, hdi_hi = 0.7,
             rope_lo = -1, rope_hi = 1, null_value = 0),
    c = list(tag = "c", decision = "Entscheidung: Nullwert akzeptieren",
             mu = 0.15, sigma = 0.12, hdi_lo = -0.1, hdi_hi = 0.4,
             rope_lo = -1, rope_hi = 1, null_value = 0),
    d = list(tag = "d", decision = "Entscheidung: unentschieden",
             mu = 1, sigma = 0.6, hdi_lo = 0.1, hdi_hi = 1.9,
             rope_lo = -1, rope_hi = 0.4, null_value = -0.5),
    e = list(tag = "e", decision = "Entscheidung: unentschieden",
             mu = 0.8, sigma = 0.45, hdi_lo = 0, hdi_hi = 1.6,
             rope_lo = -0.6, rope_hi = 0.6, null_value = 0),
    f = list(tag = "f", decision = "Entscheidung: unentschieden",
             mu = 0, sigma = 1.1, hdi_lo = -1.8, hdi_hi = 1.8,
             rope_lo = -1.2, rope_hi = 1.2, null_value = 0)
  )

  plots <- lapply(panels, function(p) do.call(rope_hdi_panel, p))

  patchwork::wrap_plots(plots, ncol = 2) +
    patchwork::plot_annotation(
      title = "ROPE-/HDI-Entscheidungsregel",
      caption = "In Anlehnung an Kruschke (2018), Abbildung 1, S. 272"
    )
}
