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

plottitanic1 <-
  titanic2 |>
  ggplot(aes(x = Pclass)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_fill_okabeito() +
  labs(y = "Anteil Überlebende", x = "Passagierklasse") +
  geom_hline(yintercept = prop_surv, linetype = "dashed", color = "black") +
  annotate(
    "label",
    x = Inf,
    y = 0.38,
    hjust = 1,
    label = "Pr(Ü) = 0.38",
    size = 4
  )


plottitanic2 <-
  titanic2 |>
  ggplot(aes(x = Embarked)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_fill_okabeito() +
  geom_hline(yintercept = prop_surv, linetype = "dashed", color = "black") +
  annotate(
    "label",
    x = Inf,
    y = 0.38,
    hjust = 1,
    label = "Pr(Ü) = 0.38",
    size = 4
  )

plottitanic3 <-
  titanic2 |>
  ggplot(aes(x = Age_prime)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_x_discrete(labels = c("Nicht Prim", "Prim")) +
  scale_fill_okabeito() +
  labs(y = "Anteil Überlebende", x = "Geburtsdatum ist Primzahl") +
  geom_hline(yintercept = prop_surv, linetype = "dashed", color = "black") +
  annotate(
    "label",
    x = Inf,
    y = 0.38,
    hjust = 1,
    label = "Pr(Ü) = 0.38",
    size = 4
  )
