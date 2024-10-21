library(tidyverse)
library(easystats)
#library(lubridate)
library(gmp)  # function "isprime"
library(titanic)
library(knitr)
library(DT)
library(kableExtra)

theme_set(theme_minimal())
#scale_color_okabeito()
scale_colour_discrete <- function(...) 
  scale_color_okabeito()

plot16a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#009E73FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  theme_minimal()


plot_pr_b <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "grey", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .5, y = .75, label = "Pr(B) = 50%") +
  theme_minimal()

plot_pr_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "grey", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .75, y = .5, label = "Pr(A) = 50%") +
  theme_minimal()


plot_pr_ab <-
ggplot(data.frame(A = c(0, 1),
                  B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#009E73FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#E69F00FF", alpha = .7, fill = NA, linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "A,B", color = "#E69F00FF") +
  theme_minimal()





plot_pr_b_geg_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#009E73FF", alpha = .3, fill = NA, linewidth = 2) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "NA") +
  # annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
  #          color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .7, fill = "#56B4E9FF", linewidth = 2) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#E69F00FF", alpha = .3, fill = "#E69F00FF", linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "B|A", color = "#E69F00FF")



## flowchart LR
##  M[Sie werfen die Münze] --> T["T (Treffer) 🥳"]
##   M --> N["N (Niete) 📚"]

n <- 1e3

set.seed(42)
wuerfel_oft <- 
  sample(x = 1:6, size = n, replace = TRUE) 


wuerfel_tab <-
  tibble(
    id = 1:n,
    x = wuerfel_oft,
    ist_6 = ifelse(x == 6, 1, 0),
    ist_6_cumsum = cumsum(ist_6) / id
  )


wuerfel_tab %>% 
  slice_head(n = 1e3) %>% 
  ggplot() +
  aes(x = id, y = ist_6_cumsum) +
  geom_hline(yintercept = 1/6, color = "grey80", size = 3) +
  geom_line() +
  labs(x = "Nummer des Würfelwurfs",
       y = "Kummulierte Häufigkeit einer Sechs") +
  annotate("label", x = 1000, y = 1/6, label = "0.17")

A <- c(1, 2)
B <- c(2, 3)

union(A, B)

A <- c(2, 4, 6)
B <- c(5, 6)
intersect(A, B)

A <- c(4, 5, 6)
B <- c(2, 4, 6)

setdiff(A, B)

setdiff(B, A)

d <- tribble(
  ~ ., ~ S1_B, ~ S1_NB, ~ Summe,
  "S2_B", 85, 9, 94,
  "S2_NB", 5, 1, 6,
  "Summe", 90, 10, 100
)

kable(d)

plot_pr_a

plot_pr_b

plot_pr_b_geg_a

plot_pr_ab

plot_pr_b
plot_pr_a
plot_pr_b_geg_a

d <- 
  tibble::tribble(
      ~id, ~kalt, ~Regen,
      "1", 0L, 0L,
      "2", 0L, 1L,
      "3", 1L, 0L,
      "4", 1L, 1L,
  "SUMME", 2L, 2L
  )

d_kalt_regen <-
   tibble::tribble(
      ~id, ~kalt, ~Regen,
      "1", "nein", "nein",
      "2", "nein", "ja",
      "3", "ja", "nein",
      "4", "ja", "ja")

d_kalt_regen |>  
  datatable(filter = "top", options = list(pageLength = 4))

knitr::kable(d)

d <- tribble(
  ~ ., ~ L, ~ NL, ~ Summe,
  "B", 80, 1, 81,
  "NB", 5, 14, 19,
  "Summe", 85, 15, 100
)

kable(d) %>%
  kable_styling() %>%
  column_spec(1, bold = TRUE)  # Makes the first column bold


data("titanic_train")

titanic2 <- 
titanic_train %>%
  select(Pclass, Survived, Embarked, Age) %>%
  mutate(Survived = factor(Survived)) |> 
  filter(Embarked %in% c("C", "Q", "S")) %>%
  mutate(Survived = factor(Survived)) |> 
  mutate(Age_prime_num = isprime(Age),
         Age_prime = case_when(
           Age_prime_num == 0 ~ "prime",
           TRUE ~ "non-prime"
         ),
         Age_prime = factor(Age_prime)) %>%
  mutate(Survived = factor(Survived))

plottitanic1 <-
  titanic2 |> 
  ggplot(aes(x = Pclass)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom")  +
  scale_fill_okabeito()


plottitanic2 <-
  titanic2 |> 
  ggplot(aes(x = Embarked)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_fill_okabeito()

plottitanic3 <-
  titanic2 |> 
  ggplot(aes(x = Age_prime)) +
  geom_bar(aes(fill = Survived), position = "fill") +
  theme(legend.position = "bottom") +
  scale_x_discrete(breaks = c(0, 2),
                   labels = c("Nicht Prim", "Prim")) +
  scale_fill_okabeito()

# plottitanic3


titanic2 |> 
  count(Survived, Age_prime) |> 
  group_by(Survived) |> 
  mutate(prop = n/sum(n)) |> 
  gt::gt() |> 
  gt::fmt_number(columns = prop, decimals = 2)

## titanic2 |>
##   select(Survived, Age_prime) |>
##   table() |>
##   prop.table(margin = 1) |>
##   round(2) |>
##   as.data.frame() |>
##   kable()

plottitanic1
plottitanic3


# source: https://ourworldindata.org/covid-vaccinations
# access date: 2021-09-24
# licence: https://ourworldindata.org/covid-vaccinations#licence



dfile <- "data/owid-covid-data.csv"



d <- read_csv(dfile)

d2<-
  d %>%
  filter(iso_code %in% c("DEU", "USA")) %>%
  mutate(date = as_date(date)) %>%
  rename(Land = iso_code) %>%
  select(date,
         Land,
         #total_deaths,
         #new_deaths,
         people_fully_vaccinated_per_hundred,
         total_deaths_per_million,
         #new_vaccinations,
         total_vaccinations) %>%
  filter(date == "2021-09-23") %>%
  group_by(Land)


# d2 %>%
#   ungroup() %>%
#   count(people_fully_vaccinated_per_hundred)

plot_covid1 <-
  d2 %>%
  ggplot(aes(x = Land,
             y = people_fully_vaccinated_per_hundred)) +
  geom_col() +
  labs(title = "Anteil komplett geimpfter Personen",
       subtitle = "2021-09-23")




plot_covid2 <-
  d2 %>%
  ggplot(aes(x = Land,
             y = total_deaths_per_million)) +
  geom_col()+
  labs(title = "Corona-Tote pro Million",
       subtitle = "2021-09-23")


plot_covid1
plot_covid2



tibble::tribble(
  ~Ereignis,               ~Pr,
       "0K", "1/2 * 1/2 = 1/4",
       "1K", "1/4 + 1/4 = 1/2",
       "2K", "1/2 * 1/2 = 1/4"
  ) %>% kable()

Pr_K1K2 <- 1/2 * 1/2
Pr_K1K2

tibble::tribble(
  ~Ereignis,                     ~Pr,
       "0K", "1/2 * 1/2 * 1/2 = 1/8",
       "1K", "1/8 + 1/8 + 1/8 = 3/8",
       "2K",          "3 * 1/8 = 3/8",
       "3K", "1/2 * 1/2 * 1/2 = 1/8"
  ) 

## flowchart LR
##   A[Start] -->|4/5|B[1. Zug: R]
##   A -->|1/5|C[1. Zug: B]
##   B -->|3/4|D[2. Zug: R]
##   B -->|1/4|E[2. Zug: B]
##   D --- H[Fazit: RR: 4/5*3/4 = 12/20]
##   E --- I[Fazit: RB: 4/5*1/4 = 4/20]
##   C -->|4/4|F[2. Zug: R]
##   C -->|0/4|G[2. Zug: B]
##   F --- J[Fazit: BR: 1/5*4/4 = 4/20]
##   G --- K[Fazit: BB: 1/5*0/4 = 0/20]

Pfad1 <- 3/20 * 17/19 * 16/18
Pfad2 <- 17/20 * 3/19 * 16/18
Pfad3 <- 17/20 * 16/19 * 3/18

Gesamt_Pr <- Pfad1 + Pfad2 + Pfad3
Gesamt_Pr

## flowchart LR
##   A[Start] -->|0.6|B[A1]
##   A -->|0.1|C[A2]
##   A -->|0.3|D[A3]
##   B -->|0.05|E[B]
##   B -.->|0.95|F[Nicht-B]
##   C -->|0.02|G[B]
##   C -.->|0.98|H[Nicht-B]
##   D -->|0.04|I[B]
##   D -.->|0.96|J[Nicht-B]

W_Ast1 <- 0.6 * 0.05  # Wahrscheinlichkeit für Ast 1
W_Ast2 <- 0.1 * 0.02  # ... Ast 2
W_Ast3 <- 0.3 * 0.04  # ... Ast 3
W_total <- W_Ast1 + W_Ast2 + W_Ast3  # totale W.
W_total

Pr_keine_Bohne <- 17/20 * 16/19 * 15/18
Pr_nicht_keine_Bohne <- round(1 - Pr_keine_Bohne, 2)
