library(tidyverse)
data("titanic_train", package = "titanic")


prob_surv_grouped <-
  titanic_train |>
  group_by(Pclass) |>
  summarise(prop_surv = mean(Survived))

prop_surv <- 0.38

prob_surv_ungrouped <-
  titanic_train |>
  #group_by(Pclass) |>
  summarise(prop_surv = mean(Survived))

titanic2 <-
  titanic_train %>%
  select(Pclass, Survived, Embarked, Age) %>%
  mutate(Survived = factor(Survived)) |>
  filter(Embarked %in% c("C", "Q", "S")) %>%
  mutate(Survived = factor(Survived)) |>
  mutate(
    Age_prime_num = isprime(Age),
    Age_prime = case_when(
      Age_prime_num == 0 ~ "prime",
      TRUE ~ "non-prime"
    ),
    Age_prime = factor(Age_prime)
  ) %>%
  mutate(Survived = factor(Survived))

# Gestapelte Anteils-Balken im Tufte-Stil (s. Skill ggplot-diagramme):
# Kategorie und Anteil direkt im Balken, keine Legende, keine y-Achse,
# kein Gitter, weisse Trennlinie. Okabe-Ito: Orange (gestorben) / Blau (ueberlebt).
plot_titanic_anteil <- function(var, xlab = NULL, x_labels = waiver()) {
  d <-
    titanic2 |>
    mutate({{ var }} := factor({{ var }})) |>
    count({{ var }}, Survived) |>
    group_by({{ var }}) |>
    mutate(
      anteil = n / sum(n),
      status = if_else(Survived == "1", "überlebt", "gestorben"),
      label = sprintf("%s %.2f", status, anteil),
      textfarbe = if_else(Survived == "1", "#FFFFFF", "#1A1A1A")
    ) |>
    ungroup()

  ggplot(d, aes(x = {{ var }}, y = anteil, fill = status)) +
    geom_col(position = "fill", color = "white", linewidth = 1.2, width = 0.85) +
    geom_text(
      aes(label = label, color = textfarbe),
      position = position_stack(vjust = 0.5),
      fontface = "bold",
      size = 3.6
    ) +
    geom_hline(yintercept = prop_surv, linetype = "dashed", color = "#4D4D4D") +
    annotate(
      "label",
      x = Inf,
      y = prop_surv,
      hjust = 1,
      label = sprintf("Pr(Ü) = %.2f", prop_surv),
      size = 4
    ) +
    scale_fill_manual(values = c("gestorben" = "#E69F00", "überlebt" = "#0072B2")) +
    scale_color_identity() +
    scale_x_discrete(labels = x_labels) +
    labs(y = "Anteil Überlebende", x = xlab) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plottitanic1 <- plot_titanic_anteil(Pclass, xlab = "Passagierklasse")

plottitanic2 <- plot_titanic_anteil(Embarked, xlab = "Einschiffungshafen")

plottitanic3 <- plot_titanic_anteil(
  Age_prime,
  xlab = "Geburtsdatum ist Primzahl",
  x_labels = c("Nicht Prim", "Prim")
)
