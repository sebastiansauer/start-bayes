plot_ask_for_quantile <- function(
  data = bayesbox_g100,
  samples = bayesbox_g100_samples,
  p = 0.5,
  x_label = "Wasseranteil",
  y_label = "Wahrscheinlichkeitsdichte",
  area_fill_color = blue
) {
  title <- paste0(
    "Welcher Wasseranteil q wird mit ",
    p * 100,
    " Wahrscheinlichkeit nicht übertroffen?"
  )

  q <- quantile(samples$p_grid, prob = p)
  q_10_and_90 <- quantile(samples$p_grid, prob = c(.1, .9))

  # bayesbox_g100_samples |>
  #   ggplot(aes(x = p_grid)) +
  #   geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
  #   geom_vline(xintercept = q_80, color = "blue", linetype = "dashed")

  p_quantile <-
    data %>%
    ggplot(aes(x = p_grid, y = post)) +
    geom_line(color = "grey40") +
    labs(
      title = "Welcher Wasseranteil q wird mit 50% Wahrscheinlichkeit nicht übertroffen?",
      x = "Wasseranteil",
      y = "Wahrscheinlichkeitsdichte"
    ) +
    geom_area(
      data = samples %>%
        filter(p_grid <= q),
      fill = blue
    ) +
    geom_point() +
    scale_x_continuous(
      breaks = c(0, as.numeric(q), 1),
      labels = c("0", "q=0.64", "1")
    ) +
    geom_vline(
      xintercept = as.numeric(q),
      color = "grey40",
      linetype = "dashed"
    ) +
    annotate("label", x = as.numeric(q), y = .01, label = "p=0.5", color = blue)

  print(p_quantile)
}









