library(tidyverse)
library(gt)
library(patchwork)
library(easystats)

library(tidyverse)
library(ggpubr)  # dataviz

n <- 10
n_success <- 6
n_trials  <- 9

d <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = n),
         prior  = 1) %>% 
  mutate(likelihood = dbinom(n_success, 
                             size = n_trials, 
                             prob = p_grid)) %>% 
  mutate(unstand_post = (likelihood * prior),
         post = unstand_post / sum(unstand_post))

samples <-
  d %>%  # nimmt die Tabelle mit Posteriori-Daten,
  slice_sample(  # Ziehe daraus eine Stichprobe,
    n = 1e4,  # mit insgesamt n=10000 Elementen,
    weight_by = post,  # Gewichte nach Spalte mit Post-Wskt.,
    replace = T)  # Ziehe mit Zurücklegen


p_stripro_vert <-
  tibble(
  Trefferzahl = rbinom(n = 1e4, size = 10, prob = 1/2)
) %>% 
  mutate(signifikant = ifelse(Trefferzahl %in% c(9,10), TRUE, FALSE)) %>% 
  ggplot() +
  aes(x = Trefferzahl, fill = signifikant) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  theme(legend.position = c(0.1, 0.8)) +
  geom_vline(xintercept = 9) +
  labs(title = "Stichprobenverteilung für p=0.5")

# Post-Verteilung:
d_zwielicht <-
  tibble(
    p_grid = seq( from=0 , to=1 , length.out=100),
    prior = 1,  # Priori-Gewichte
    likelihood = dbinom(8, size = 10, prob=p_grid) ,
    unstandardisierte_posterior = likelihood * prior ,
    posterior = unstandardisierte_posterior / sum(unstandardisierte_posterior))

# Stichproben ziehen aus der Posteriori-Verteilung:
samples_zwielicht <- 
  tibble(
    gewinnchance_muenze = sample(
      d_zwielicht$p_grid , 
      prob=d_zwielicht$posterior, 
      size=1e4, 
      replace=TRUE)) %>% 
  mutate(
    id = row_number())



p_samples_zwielicht <- 
samples_zwielicht %>% 
  ggplot() +
  aes(x = gewinnchance_muenze) +
  geom_histogram(fill = "grey60", bins = 20) +
  #geom_vline(xintercept = 0.9) +
  #geom_label(x = 0.8, y= 0, label = "Emp. Ergebnis") +
  labs(title = "Posteriori-Verteilung für p",
       subtitle = "Priori: Gleichverteilung; Daten: 9 von 10 Treffern, binomialverteilt",
       caption = "Das Dreieck zeigt die Wskt. eines Treffers bei einer fairen Münze",
       x = "Gewinnchance der Münze") +
  annotate("point", x = .5, y = 0, size = 5, color = "grey40", shape = 17)

p_samples_zwielicht

## gghistogram(samples_zwielicht,
##             x = "gewinnchance_muenze",
##             title = "Posteriori-Verteilung für p",
##             subtitle = "Priori: Gleichverteilung; Daten: 9 von 10 Treffern, binomialverteilt",
##             xlab = "p (Gewinchance der Münze)",
##             fill = "grey60") +
##   geom_vline(xintercept = 0.5)

samples_zwielicht %>% 
  count(gewinnchance_muenze > 0.45 & gewinnchance_muenze < 0.55) %>% 
  mutate(prop = n/sum(n))

samples_zwielicht %>% 
  count(gewinnchance_muenze > .7) %>% 
  mutate(prop = n / sum(n))

L <- dbinom(0:2, size = 2, prob = 0.5)
L

set.seed(42)  # Zufallszahlen festlegen
rbinom(n = 1, size = 2, prob = .7)  # 0 Treffer (Wasser)

## rbinom(n = Wie oft soll der Versuch wiederholt werden?,
##        size = Wie viele Globuswürfe pro Versuch (Stichprobengröße),
##        prob = Wie hoch ist die Wahrscheinlichkeit für Wasser (bzw. für einen Treffer))

rbinom(n = 10, size = 2, prob = 0.7)

draws <- 
  tibble(
    draws = rbinom(1e6, size = 2, prob = .7))

draws %>% 
  count(draws) %>% 
  mutate(prop = n / sum(n))

dbinom(0:2, size = 2, prob = .7)

## n_draws <- 1e6
## 
## draws_df <-
##   tibble(draws = rbinom(n_draws, size = 9, prob = .7))
## 
## draws_df %>% gghistogram(x = "draws")

n_draws <- 1e6
draws <- tibble(draws = rbinom(n_draws, 
                               size = 9, 
                               prob = .7))

# the histogram
draws %>% 
  ggplot(aes(x = draws)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("Anzahl Wasser (W) pro Versuch",
                     breaks = seq(from = 0, to = 9, by = 1)) +
  scale_y_continuous("Häufigkeit",
                     labels = scales::scientific) +
  coord_cartesian(xlim = c(0, 9)) +
  theme(panel.grid = element_blank()) +
  labs(title = "Stichprobenverteilung für n=9 und p=.7 (binomial verteilt)")

## samples %>%
##   select(p_grid) %>%
##   slice_head(n = 10)

samples %>% 
  select(p_grid) %>% 
  slice_head(n = 10) |> 
  print_md()

ppv <- 
  rbinom(1e4, 
         size = 9, 
         prob = samples$p_grid) %>% 
  as_tibble()

head(ppv)

ppv_plot2 <-
  ppv %>% 
  ggplot() +
  aes(x = value) +
  geom_bar() +
  scale_x_continuous(
    breaks = 0:9)

ppv_plot2

if (knitr:::is_html_output()) {
  knitr::include_graphics("img/post-pred-ppv-anim.gif")
}

n_draws <- 1e5

simulate_binom <- function(probability) {
  set.seed(3)
  rbinom(n_draws, size = 9, prob = probability) 
}

d_small <-
  tibble(probability = seq(from = .1, to = .9, by = .1)) %>% 
  mutate(draws = purrr::map(probability, simulate_binom)) %>% 
  unnest(draws) %>% 
  mutate(label = str_c("p = ", probability))

ppv2_plot <- 
d_small %>%
  ggplot(aes(x = draws)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92") +
  scale_x_continuous("Wasser", breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(subtitle = "Stichprobenverteilungen") +
  coord_cartesian(xlim = c(0, 9)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ label, ncol = 9) 
