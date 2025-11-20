plot_ask_for_probability <- function(
  data = bayesbox_g100,
  samples = bayesbox_g100_samples,
  q = 0.5
) {
  title <- paste0(
    "Was ist die Wahrscheinlichkeit eines Wasseranteils von höchstens ",
    q * 100,
    "%?"
  )

  pi <- as.numeric(q)

  F <- ecdf(samples$p_grid)
  q2 <- F(as.numeric(pi)) |> round(2)

  p_probability <-
    data |>
    ggplot(aes(x = p_grid, y = post)) +
    geom_line(color = "grey40") +
    labs(
      title = title,
      x = "Wasseranteil",
      y = "Wahrscheinlichkeitsdichte"
    ) +
    geom_area(
      data = data %>%
        filter(p_grid <= q),
      fill = blue
    ) +
    geom_point() +
    scale_x_continuous(breaks = c(0, 1 / 2, 1)) +
    geom_vline(xintercept = 1 / 2, color = "grey40", linetype = "dashed") +
    annotate(
      "label",
      x = 1 / 2,
      y = .01,
      label = paste0("p= ", q2),
      color = blue
    )

  print(p_probability)
}

# p2 <-
#   d_k100 %>%
#   ggplot(aes(x = p_grid, y = post)) +
#   geom_line(color = "grey20") +
#   labs(title="80%-Quantil (q80)",
#        caption = paste0("q80: ", round(q_80, 2)),
#        x = "Parameterwerte",
#        y = "Wahrscheinlichkeitsdichte") +
#   geom_area(data = d_k100 %>%
#               filter(p_grid < q_80),
#             fill = "grey80",
#             color = "grey20") +
#   geom_vline(xintercept = q_80, color = "blue") +
#   annotate("label", x = q_80, y = .02, label = "q", color = "blue") +
#   scale_x_continuous(breaks = .78) +
#   theme(axis.text.x = element_text(colour = "blue"))

# p2

# p1 + p2
