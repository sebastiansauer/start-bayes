library(tidyverse)
#library(gt)
#library(patchwork)
library(easystats)
library(gt)
library(ggpubr)
library(plotly)

theme_set(theme_modern())

## library(tidyverse)
## library(easystats)
## library(ggpubr)  # optional

n <- 11
n_success <- 6          
n_trials  <- 9
p_grid <- seq(from = 0, to = 1, length.out = n)         # <1>
L <- dbinom(n_success, size = n_trials, prob = p_grid)  # <2>

d <-                                                    # <3>
  tibble(p_grid = p_grid,prior  = 1) %>% 
  mutate(likelihood = L) %>% 
  mutate(unstand_post = (likelihood * prior),
         post = unstand_post / sum(unstand_post))

## library(ggpubr)
## ggline(d, x = "p_grid", y = "post")

#

p_d <-
d %>% 
  ggplot() +
  aes(x = p_grid, y = post) +
  geom_point(alpha = .5, size = 3) +
  geom_line() +
  scale_y_continuous("Posterior-Wahrscheinlichkeit", 
                     breaks = NULL) +
  theme_minimal()  

  
ggplotly(p_d)

#

p_d <-
d %>% 
  ggplot() +
  aes(x = p_grid, y = post) +
  geom_point(alpha = .5, size = 3) +
  geom_line() +
  scale_y_continuous("Posterior-Wahrscheinlichkeit", 
                     breaks = NULL) +
  theme_minimal()  

d %>% 
  mutate_all(round, 2) %>% 
  gt::gt()

## source("https://raw.githubusercontent.com/sebastiansauer/prada/master/R/NAME_bayesbox.R")
## bayesbox(hyps = p_grid, priors = 1, liks = L)

library(prada)  # <1>
bayesbox(hyps = p_grid, priors = 1, liks = L)  # <3>

samples <-
  d %>%  # nimmt die Tabelle mit Posteriori-Daten,
  slice_sample(  # Ziehe daraus eine Stichprobe,
    n = 1e4,  # mit insgesamt n=10000 Zeilen,
    weight_by = post,  # Gewichte nach Post-Wskt.,
    replace = T)  %>%  # Ziehe mit Zurücklegen
  select(p_grid)

samples %>% 
  slice_head(n = 5) %>% 
  gt() %>% 
  fmt_number(columns = c(1),
             decimals = 3) 

samples$p_grid[1:100] %>% round(2)

## samples |>
##   count(p_grid) |>
##   ggbarplot(x = "p_grid", y = "n")

samples |> 
  count(p_grid) |> 
  ggbarplot(x = "p_grid", y = "n") 

k <- 100   # <1>
n_success <- 6  # <2>
n_trials  <- 9  # <3>

d_k100 <-
  tibble(p_grid = seq(from = 0, 
                      to = 1, 
                      length.out = k),  # <4>
         prior  = 1) %>%   # <5>
  mutate(likelihood = dbinom(n_success,   # <6>
                             size = n_trials, 
                             prob = p_grid)) %>% 
  mutate(unstand_post = (likelihood * prior),  # <7>
         post = unstand_post / sum(unstand_post))

samples_k100 <-
  d_k100 %>%  # nimmt die Tabelle mit Posteriori-Daten,
  slice_sample(  # Ziehe daraus eine Stichprobe,
    n = 1000,  # mit insgesamt n=1000 Elementen,
    weight_by = post,  # Gewichte nach Spalte mit Post-Wskt.,
    replace = T)  # Ziehe mit Zurücklegen


p0 <- 
  d_k100 %>% 
  ggplot(aes(x = p_grid, y= post)) +
  geom_line() +
  geom_point() +
  labs(title = "Post-Verteilung \nexakt") +
  theme_minimal()

p1 <-
  samples_k100 %>% 
  mutate(sample_number = 1:n()) %>% 
  ggplot(aes(x = sample_number, y = p_grid)) +
  geom_point(alpha = 1/10) +
  scale_y_continuous("Anteil Wasser  (p)", 
                     limits = c(0, 1)) +
  labs(x = "Nummer der Stichprobe",
       title = "Stichproben \naus der Post")  +
  theme_minimal()

p2 <-
samples_k100 %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  scale_x_continuous("Anteil Wasser (p)", 
                     limits = c(0, 1)) +
  labs(y = "Wahrscheinlichkeitsdichte",
       title = "Stichproben \naus der Post") +
  theme_minimal()


plots(p0, p1, p2)


d_k100_samples <- 
  d_k100 %>% 
  slice_sample(n = 1e6, weight_by = post, replace = T) 


mode_df <- bayestestR::map_estimate(d_k100_samples$p_grid) 
mode_rounded <- mode_df$MAP_Estimate %>% round(2)
mavg <- mean(d_k100_samples$p_grid) %>% round(2)
md <- median(d_k100_samples$p_grid) %>% round(2)

p_100_samples <-
  d_k100_samples %>%   
ggplot(aes(x = p_grid)) +
  geom_density(fill = "grey40") +
  scale_x_continuous("Anteil Wasser (p)", limits = c(0, 1)) +
  labs(y = "",
       caption = paste0("Modus: ", mode_rounded, "; MW: ", mavg, "; Md: ", md)) +
  geom_vline(xintercept = c(mode_rounded), color = "#E69F00FF")  +
  geom_vline(xintercept = c(mavg), color = "#56B4E9FF")  +
  geom_vline(xintercept = c(md), color = "#009E73FF")  +
  theme_minimal() +
  annotate("label", x = mode_rounded, y = 0, label = glue::glue("Modus: {mode_rounded}"), fill = "#E69F00FF") +
  annotate("label", x = mavg, y = 1, label = glue::glue("MW: {mavg}"), fill = "#56B4E9FF") +
  annotate("label", x = md, y = 2, label = glue::glue("Median: {md}"), fill = "#009E73FF")

p_100_samples

## samples %>%
##   count(p_grid < .5)

samples %>%
  count(p_grid < .5) 

d %>%
  filter(p_grid < .5) %>%
  summarise(sum = sum(post))

samples %>% 
  count(p_grid > .5 & p_grid < .75)

samples %>% 
  count(p_grid > .5 & p_grid < .75) %>% 
  summarise(Anteil = n / 1e4,
            Prozent = 100 * n / 1e4)  # In Prozent

samples %>% 
  filter(p_grid > .5 & p_grid < .75) %>% 
  summarise(sum     =       n() / 1e4,
            anteil = 100 * n() / 1e4)  # In Prozent

## samples %>%
##   count(p_grid >= .9 & p_grid <= 1) %>%
##   summarise(prop = 100 * n() / 1e4)  # prop wie "proportion", Anteil

samples %>% 
  count(p_grid >= .9 & p_grid <= 1) %>% 
  summarise(prop = 100 * n() / 1e4)  # prop wie "proportion", Anteil

samples %>% count(p_grid <= .5)

map_estimate(samples$p_grid)  

samples %>% 
  summarise(mean(p_grid),
            median(p_grid))

samples %>% 
  summarise(quantil90 = quantile(p_grid, p = .9))

samples %>% 
  summarise(
    quant_05 = quantile(p_grid, 0.05),
    quant_95 = quantile(p_grid, 0.95))


# speed_gender_height <- read.csv("https://raw.githubusercontent.com/rpruim/OpenIntro/master/data/speed_gender_height.csv")
data("speed_gender_height", package = "openintro")  # <1>

height_summary <- 
  speed_gender_height %>% 
  mutate(height_cm = height*2.54) %>%  # <2>
  select(height_inch = height, height_cm) %>% 
  drop_na() %>%   # <3>
  pivot_longer(everything(), names_to = "Einheit", values_to = "Messwert") %>%   # <4>
  group_by(Einheit) %>%   # <5>
  summarise(q25 = quantile(Messwert, prob = .25),  # <6>
            q50 = quantile(Messwert, prob = .5),
            q75 = quantile(Messwert, prob = .75))

height_summary

p1 <- speed_gender_height %>% 
  ggplot() +
  aes(x = 1, y = height) +
  geom_boxplot() +
  labs(x = "",
       y = "Größe in Inch",
       title = "Die Box zeigt das 25%-, 50%- und 75%-Quantil") +
  theme_minimal()

height_summary_long <- 
  speed_gender_height %>% 
  select(height) %>% 
  drop_na() %>% 
  summarise(q25 = quantile(height, prob = .25),
            q50 = quantile(height, prob = .5),
            q75 = quantile(height, prob = .75)) %>% 
  pivot_longer(everything(),
               names_to = "q",
               values_to = "height") 

p2 <- 
  speed_gender_height %>% 
  ggplot() +
  aes(x = height) +
  geom_histogram() +
  geom_vline(data = height_summary_long,
             aes(xintercept = height)) +
  geom_text(data = height_summary_long,
             aes(x = height+1,
                 y = 0,
                 label = paste0(q, ": ", height)),
             angle = 90,
            hjust = 0,
            color = "white"
             ) +
  labs(title = "Die vertikalen Striche zeigen die Quantile",
       y = "Häufigkeit")  +
  theme_minimal()

plots(p1, p2)

samples |> 
  summarise(quant99 = quantile(p_grid, p = .99))

samples |> 
  summarise(wahrscheinlich_kleinste = 
              quantile(p_grid, p = .01))

samples |> 
  summarise(wahrscheinlich_kleinste = 
              quantile(p_grid, p = .1))

samples %>% 
  arrange(p_grid) %>%   # sortiere
  slice_head(n = 9000) %>%  # nur die ersten 90%
  summarise(p90 = max(p_grid))

samples %>% 
  summarise(q90 = quantile(p_grid, .9))

q_80 <- quantile(samples$p_grid, prob = .8)
q_10_and_90 <- quantile(samples$p_grid, prob = c(.1, .9))

p1 <-
  d_k100 %>% 
  ggplot(aes(x = p_grid, y = post)) +
  geom_line() +
  labs(title="Untere 80%",
       caption = paste0("q80: ", round(q_80, 2))) +
  geom_area(data = d_k100 %>% 
              filter(p_grid < q_80))   +
  theme_minimal() +
  annotate("label", x = q_80, y = 0, label = q_80)

# lower right panel
p2 <-
  d_k100 %>% 
  ggplot(aes(x = p_grid, y = post)) +
  geom_line() +
  geom_area(data = d_k100 %>% 
              filter(p_grid > q_10_and_90[1] & p_grid < q_10_and_90[2])) +
  labs(title = "Mittlere 80%",
       caption = paste0("q10: ", round(q_10_and_90[1], 2), 
                        "; q90: ",
                        round(q_10_and_90[2]), 2))  +
  theme_minimal() +
  annotate("label", label = round(q_10_and_90[1], 2),
           x = round(q_10_and_90[1], 2), y = 0) +
  annotate("label", label = round(q_10_and_90[2], 2),
           x = round(q_10_and_90[2], 2), y = 0)  

p1
p2

d_anim <-
  samples_k100 |> 
  select(p_grid, post) |>
  mutate(decile = cut(post, 
                      breaks = quantile(samples_k100$p_grid, probs = seq(0, 1, 0.1)), 
                      include.lowest = TRUE),
         decile2 = cut(p_grid, 
                      breaks = quantile(samples_k100$p_grid, probs = seq(0, 1, 0.1)), 
                      include.lowest = TRUE, labels = 1:10)) |> 
  group_by(decile2) |> 
  mutate(post_max = max(p_grid))

d_anim2 <-
  d_anim |> 
  ungroup() |> 
  select(p_grid)

p_quantiles  <- 
d_anim |> 
  ggplot(aes(x = p_grid)) +
  geom_density(data = d_anim2) + 
  geom_vline(aes(xintercept = post_max)) +
  geom_label(aes(label = decile2, x = post_max), y = 0)


## library(gganimate)
## p_quantiles_anim <-
##   p_quantiles +
##   transition_states(decile2, transition_length = 2, state_length = 1) +
##   ggtitle("Dezil: {closest_state}") +
##   enter_fade() +
##   exit_fade()
## 
## anim_save("img/p_quantiles_anim.gif", p_quantiles_anim, renderer = gifski_renderer())

d_33 <- 
  tibble(p_grid = seq(0,1, by =.01),
         prior = 1) %>% 
  mutate(likelihood = dbinom(3, size = 3, prob = p_grid)) %>% 
  mutate(unstand_post = likelihood * prior) %>% 
  mutate(post_33  = unstand_post / sum(unstand_post)) 

samples_33 <- 
  d_33 %>% 
    slice_sample(n = 1e4, 
                 weight_by = post_33, 
                 replace = T)

samples_33 %>% 
  select(-post_33) %>% 
  head() %>% 
  gt() %>% 
  fmt_number(columns = c(1,3,4), decimals = 2)

qi_50_low <- eti(samples_33$p_grid, ci = .5)$CI_low
qi_50_up <- eti(samples_33$p_grid, ci = .5)$CI_high
p1 <-
  d_33 %>% 
  ggplot(aes(x = p_grid, y = post_33)) +
  # check out our sweet `qi()` indexing
  geom_area(data = . %>% 
              filter(p_grid > qi_50_low &  
                    p_grid < qi_50_up),
            fill = "grey75") +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  theme_minimal()




hdi_50_low <- bayestestR::hdi(samples_33$p_grid, 
                                  ci = .5)$CI_low
hdi_50_up <- bayestestR::hdi(samples_33$p_grid, 
                                  ci = .5)$CI_high

p2 <-
  d_33 %>% 
  ggplot(aes(x = p_grid, y = post_33)) +
  geom_area(data = . %>% 
              filter(p_grid > hdi_50_low & 
                     p_grid < hdi_50_up),
            fill = "grey75") +
  geom_line()  +
  theme_minimal()


plots(p1, 
      p2,
      n_rows = 1,
      title= c("PI", "HDI"))

## samples_33 %>%
##   select(p_grid) %>%
##   eti(ci = .5)  # Paket `easystats`

samples_33 %>% 
  select(p_grid) %>% 
  eti(ci = .5)  # Paket `easystats`

point_estimates <-
  bind_rows(samples_33 %>% tidybayes::mean_qi(p_grid),
            samples_33 %>% tidybayes::median_qi(p_grid),
            samples_33 %>% tidybayes::mode_qi(p_grid)) %>% 
  select(p_grid, .point) %>% 
  # these last two columns will help us annotate  
  mutate(x = p_grid + c(-.03, .03, -.03),
         y = c(.005, .012, .02))

d_33 %>% 
  ggplot(aes(x = p_grid)) +
  geom_area(aes(y = post_33),
            fill = "grey75") +
  geom_vline(xintercept = point_estimates$p_grid) +
  geom_text(data = point_estimates,
            aes(x = x, y = y, label = .point),
            angle = 90) +
  labs(x = "Anteil Wasser (p)",
       y = "Wahrscheinlichkeitsdichte") +
  theme(panel.grid = element_blank()) +
  theme_minimal()

## samples %>%
##   select(p_grid) %>%
##   hdi(ci = .5)  # aus dem Paket `{easystats}`

samples %>% 
  select(p_grid) %>% 
  bayestestR::hdi(ci = .5)  # aus dem Paket `{easystats}`

samples %>% 
  summarise(
    mean   = mean(p_grid),
    median = median(p_grid))  

samples %>% 
  summarise(
    p_sd   = sd(p_grid),
    p_iqr = IQR(p_grid),
    p_mad = mad(p_grid))  # Mean Absolute Deviation, Mittlerer Absolutfehler
