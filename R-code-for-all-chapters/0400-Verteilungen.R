library(tidyverse)

library(gt)
library(patchwork)
library(faux)
library(openintro)
library(easystats)
library(ggraph)
library(knitr)

source("funs/uniformplot.R")
source("funs/binomial_plot.R")

theme_set(theme_minimal())
#scale_color_okabeito()
scale_colour_discrete <- function(...) 
  scale_color_okabeito()

my_tree <- tidygraph::create_tree(31, 2, mode = "out")

my_tree %>%
  mutate(lab = 1:31) %>%
  mutate(muenze = case_when(
    lab == 1 ~ 0,
    lab <= 3 ~ 1,
    lab <= 7 ~ 2,
    lab <= 15 ~ 3,
    TRUE ~ 4
  )) %>% 
  mutate(muenze = factor(muenze)) %>% 
  ggraph(circular = FALSE) +
  geom_edge_link() +
   geom_node_label(mapping = aes(label = lab, fill = muenze)) +
  coord_flip() +
  scale_y_reverse() +
  # scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple", "4" = "orange"),
  #                   name = "Muenze") +
  theme_void() +
  theme(text = element_text(size = 12))

d <- tibble::tribble(
   ~i, ~Elementarereignis, ~`Pr(EE)`, ~Trefferzahl, ~`Pr(Trefferzahl)`,
  "1",             "NNNN",    "1/16",          "0",             "1/16",
  "2",             "NNNT",    "1/16",          "1",              "1/4",
  "3",             "NNTN",    "1/16",          "1",              "1/4",
  "4",             "NTNN",    "1/16",          "1",              "1/4",
  "5",             "TNNN",    "1/16",          "1",              "1/4",
  "6",             "NNTT",    "1/16",          "2",                "…",
  "…",                "…",       "…",          "…",                "…"
  )


kable(d, booktabs = TRUE)

## flowchart LR
##   subgraph A[Ereignisse<br> im Ereignisraum]
##     Kopf
##     Zahl
##   end
##   subgraph B[Realisationen der <br>Zufallsvariable]
##     null[0]
##     eins[1]
##   end
##   subgraph C[Wahrscheinlichkeit]
##     half[50%]
##   end
## 
##   Kopf --> null
##   Zahl --> eins
##   null --> half
##   eins --> half

ggplot(data.frame(x=c(0:4), y = 0), aes(x,y)) +
  geom_point(size = 5, alpha = .7) +
  scale_y_continuous(limits = c(-1,1), breaks = NULL) +
  scale_x_continuous(breaks = 0:4, labels = 0:4) +
  theme_minimal() +
  labs(y = "", x = "")


d <- tibble::tribble(
         ~Wahrscheinlichkeitstheorie,                      ~Desktiptive.Statistik,
                   "Zufallsvariable",                                   "Merkmal",
                "Wahrscheinlichkeit",               "relative Häufigkeit, Anteil",
       "Wahrscheinlichkeitsfunktion",   "einfache relative Häufigkeitsverteilung",
               "Verteilungsfunktion", "kumulierte relative Häufigkeitsverteilung",
                    "Erwartungswert",                                "Mittelwert",
                           "Varianz",                                   "Varianz"
       )
gt::gt(d)

dice_outcomes <- expand.grid(Die1 = 1:6, Die2 = 1:6)

# Calculate the sum of the two dice for each outcome
dice_outcomes$Sum <- dice_outcomes$Die1 + dice_outcomes$Die2

# Calculate the probability of each sum using the table function
sum_counts <- table(dice_outcomes$Sum)
total_outcomes <- sum(sum_counts)
probabilities <- sum_counts / total_outcomes

twodice <- tibble(
  Augensumme = 2:12,
  p = probabilities) |> 
  mutate(p_cum = cumsum(p))

p_twodice <- 
  ggplot(twodice, aes(x = Augensumme, y = p)) + 
  geom_col() +
  geom_label(aes(y = p, label = round(p, 2), nudge_y = .1)) +
  scale_x_continuous(breaks = 1:12)

num_trials <- 1000  # You can change this to the desired number of trials

# Simulate repeated throws of two dice
results <- replicate(num_trials, {
  die1 <- sample(1:6, 1, replace = TRUE)  # Simulate the first die
  die2 <- sample(1:6, 1, replace = TRUE)  # Simulate the second die
  c(Die1 = die1, Die2 = die2)  # Return the results as a vector
}) |> 
  t() |> 
  as_tibble() |> 
  mutate(Augensumme  = Die1 + Die2)

# Display

results_count <-
  results |> 
  count(Augensumme) |> 
  mutate(prop = n/num_trials) |> 
  mutate(n_cum = cumsum(n),
         prop_cum = cumsum(prop))

p_sim2dice <-
  ggplot(results_count) +
  aes(x = Augensumme, y = n) +
  geom_col() +
  geom_label(aes(y = n, label = round(prop, 2))) +
  scale_x_continuous(breaks = 1:12)

p_twodice

p_sim2dice

p1 <- 
  mtcars %>% 
  ggplot(aes(x = cyl)) +
  geom_bar()


p2 <- mtcars %>% 
  ggplot(aes(x = hp)) +
  geom_histogram(bins=10)

plots(p1, p2, n_rows = 1)

data(mtcars)
  mtcars %>% 
    count(cyl)

p_F <- 
  ggplot(twodice, aes(x = Augensumme, y = p_cum)) + 
  geom_col() +
  geom_line() +
  geom_label(aes(label = round(p_cum, 2))) + 
  scale_x_continuous(breaks = 1:12) +
  labs(y = "Verteilungsfunktion F")


y_lab <- "empirische Verteilungsfunktion F emp."

p_F_emp <-
  ggplot(results_count) +
  aes(x = Augensumme, y = prop_cum) +
  geom_col() +
  geom_line() +
  geom_label(aes(y = prop_cum, label = round(prop_cum, 2))) +
  labs(y = y_lab) +
  scale_x_continuous(breaks = 2:12)

p_F

p_F_emp 

ggplot(data.frame(x=0, y = 0), aes(x,y)) +
  #geom_point(size = 5, alpha = .7) +
  scale_y_continuous(limits = c(-1,1), breaks = NULL) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200)) +
  annotate("segment", x = 0, xend = 200, y = 0, yend = 0, color = "red")  +
  theme_minimal() +
  annotate("label", x = 200, y = 0, label = "...") +
  labs(y = "", x = "")

p_bus1 <- 
  uniform_Plot(0, 10) + 
  geom_vline(xintercept = .42, color = "#56B4E9FF", size = 1) +
  annotate("label", x = .42, y = .05, hjust = 0, label = "Pr(X=0.42)=?", color="#56B4E9FF") +
  annotate("point", x = .42, y = 0, size = 5, color = "#56B4E9FF", alpha = .7) +
  annotate("label", x= 5, y = 0.1, label = "f(x) = 1/10", color = "#009E73FF", size = 10)

p_bus1

d <- 
  tibble(x=1:10,
         y= 1:10/10) 

ggplot(d, aes(x,y)) +
  geom_point(alpha = .5) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()

#
#source: https://dk81.github.io/dkmathstats_site/rmath-uniform-plots.html

uniform_Plot(-1, 1)
uniform_Plot(0, 3)

## runif(n = 1, min = 0, max = 10)

set.seed(42)
runif(n = 1, min = 0, max = 10)

## x_simu <- runif(n = 1e5, min = 0, max = 10)

n <- 1e5
set.seed(42)
x_simu <- runif(n = n, min = 0, max = 10)  # gibt Vektor zurück

x_simu_df <-
  tibble(id = 1:n,
         x = x_simu)

## library(ggpubr)
## gghistogram(x_simu_df, x = "x_simu", fill = "grey20")

library(ggpubr)
gghistogram(x_simu_df, x = "x_simu", fill = "grey20")

## x_simu_df %>%
##   count(Schnittmenge = x > 3 & x < 5)

x_simu_df %>% 
  count(Schnittmenge = x > 3 & x < 5)

d <- 
  tibble(id = 1:5,
         event = c("T", "T", "N", "N", "N"))


ggplot(d) +
  aes(x = id) +
  theme_minimal() +
  annotate("rect", xmin = 0, xmax = 6, ymin = 0, ymax = 2, fill = "grey80", alpha = .8) +
  geom_point(size = 10, y = 1, aes(color = event) )+
  geom_text(aes(label = event), y = 1) +
  theme(axis.text = element_blank()) +
  labs(x = "", fill = "")

p_a = (2/5)^2 * (3/5)^2
p_a

p_a_strich = 6 * p_a
p_a_strich

choose(4,2)

# Define the values of n and k
n <- 4  # Total number of items
k <- 2  # Number of items to choose

# Generate all combinations
combinations <- combn(n, k) |> t() |> as_tibble()

choose(n = 25, k = 11)

dbinom(x = 2, size = 4, prob = 2/5)

dbinom(x = 5, size = 7, prob = .95)

p_5 <- dbinom(x = 5, size = 7, prob = .95)
p_6 <- dbinom(x = 6, size = 7, prob = .95)
p_7 <- dbinom(x = 7, size = 7, prob = .95)

p_5
p_6
p_7

p_mind_5 <- p_5 + p_6 + p_7

p_mind_5

p_weniger_als_4 <- 1 - p_mind_5
p_weniger_als_4

font_size <- 6

ps <- dbinom(x = 0:7, size = 7, prob = .95)

ps_df <-
  tibble(Motorenzahl = 0:7,
         Pr = ps,
         Pr_log = log(ps, base = 2))

ps_df |> 
  ggplot(aes(x = Motorenzahl, y = Pr)) +
  geom_col() +
  scale_x_continuous(breaks = 0:7) +
  geom_label(aes(label = round(Pr, 3)), size = font_size)

ps_df |> 
  ggplot(aes(x = Motorenzahl, y = Pr_log)) +
  geom_col() +
  scale_x_continuous(breaks = 0:7) +
  geom_label(aes(label = round(Pr_log, 0)), size = font_size)

pbinom(q = 4, size = 7, prob = .95)

log(.5, base = 2)

log(1/4, base = 2)

log(1/8, base = 2)

dbinom(x = 15, size = 20, prob = .5)

pbinom(q = 15, size = 20, prob = .5)

dbinom(x = 3, size = 3, prob = 1/2)

binomial_plot(3, 1/2)

binomial_plot(9, .7)

sample(x = c(0, 1), size = 1)

sample(x = c(0, 1), size = 10, replace = TRUE)

n <- 1e3

muenze_oft <- 
  sample(x = c(0, 1), size = n, replace = TRUE) 


muenze_oft %>% 
  sum()

muenz_tab <-
  tibble(
    id = 1:n,
    x = muenze_oft,
    x_cumsum = cumsum(x) / id  # gibt Anteil von "Zahl" wieder
  )

head(muenz_tab)

muenz_tab %>% 
  slice_head(n = 1e3) %>% 
  ggplot() +
  aes(x = id, y = x_cumsum) +
  geom_line() +
  theme_minimal()

## source(paste0(here::here(),"/R-Code/img15.R"))

data(speed_gender_height, package = "openintro")

height_summary <- 
  speed_gender_height %>% 
  drop_na(height) %>% 
  mutate(height = height * 2.54) %>% 
  summarise(q25 = quantile(height, prob = .25),
            q50 = quantile(height, prob = .5),
            q75 = quantile(height, prob = .75))

height_summary %>% 
  gt()

speed_gender_height <-
  speed_gender_height %>% 
  mutate(height_cm = height * 2.54)

p1 <- 
  speed_gender_height %>% 
  ggplot() +
  aes(x = 1, y = height_cm) +
  geom_boxplot() +
  labs(x = "",
       y = "Größe in cm",
       title = "Die Box zeigt das 25%-, 50%- und 75%-Quantil")

height_summary_long <- 
  height_summary %>% 
  pivot_longer(everything(),
               names_to = "q",
               values_to = "height_cm") 

p2 <- 
  speed_gender_height %>% 
  ggplot() +
  aes(x = height_cm) +
  geom_histogram() +
  geom_vline(data = height_summary_long,
             aes(xintercept = height_cm)) +
  geom_text(data = height_summary_long,
             aes(x = height_cm+1,
                 y = 0,
                 label = paste0(q, ": ",height_cm)),
             angle = 90,
            hjust = 0,
            color = "white"
             ) +
  labs(title = "Die vertikalen Striche zeigen die Quantile",
       y = "Häufigkeit")  +
  theme_minimal()
 
plots(p1, p2)

source(paste0(here::here(),"/R-Code/img13.R"))

source(paste0(here::here(),"/R-Code/img14.R"))

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "#E69F00FF") +
  labs(y = "Dichte", x = "Merkmal, X") +
  stat_function(fun = dt, n = 101, args = list(df = 1, ncp =0), color = "#56B4E9FF") +
  labs(caption = "orange: Normalverteilung\n blau: randlastige Verteilung (t-Verteilung mit df=1)")

## d <-
##   tibble(
##     x = seq(-3, 3,
##             length.out = 100),
##     y = exp(-x^2)
##   )
## 
## ggline(d, x = "x",y = "y")  # aus {ggpubr}

d <-
  tibble(
    x = seq(-3, 3, 
            length.out = 100),
    y = exp(-x^2)
  )

d %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_line()

rnorm(n = 1, mean = 0, sd = 1)

n <- 1e4
d <- 
  tibble(
    id = 1:n,
    x = rnorm(n = n, mean = 1002, sd = 1.5)
  )

head(d)

d %>% 
  count(x < 1000)

d %>% 
  count(x < 1000) %>% 
  mutate(prop = n/1e4)

d <-
  tibble(
  iq = rnorm(n = 1e4, 
             mean = 100, 
             sd = 15))

probs <- c(0.75,.5,.25,.05,.01)

d_summary <- d %>% 
  summarise(p = probs,
            q = quantile(iq, probs))

d_summary %>% 
  gt() %>% 
  fmt_number(p, decimals = 2) %>% 
  fmt_number(q, decimals = 0)

## d <-
##   tibble(
##     iq = rnorm(1e4,
##                mean = 100,
##                sd = 15)) %>%
##   mutate(iq = round(iq))
## 
## qs <- c(75,100,115,130)
## 
## d %>%
##   count(p_100 = iq < 100) %>%
##   mutate(prop = n / sum(n))

d <-
  tibble(
    iq = rnorm(1e4, 
               mean = 100, 
               sd = 15)) %>% 
  mutate(iq = round(iq))

qs <- c(75,100,115,130)

d %>% 
  count(p_100 = iq < 100) %>% 
  mutate(prop = n / sum(n)) %>% 
  gt() %>% 
  fmt_number(columns = 3)

## d %>%
##   mutate(prop = percent_rank(iq)) %>%
##   filter(iq %in% qs) %>%
##   distinct(iq, .keep_all = TRUE)

  

p1 <- 
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, 100)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "X", y = "Dichte",
       title = "50%-Quantil: 100;\nVerteilungsfunktion von 100:50%") +
  scale_y_continuous(breaks = NULL)

p2 <-
    ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, 125)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "X", y = "Dichte",
       title = "95%-Quantil: 125;\nVerteilungsfunktion von 125:95%") +
  scale_y_continuous(breaks = NULL)


plots(p1, p2)

## qnorm(.50, mean = 100, sd = 15)  # 50%-Quantil
## pnorm(100, mean = 100, sd = 15)  # Verteilungsfunktion für IQ=100

q_p50 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, 100)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = "50%-Quantil: 100") +
  scale_y_continuous(breaks = NULL)

q_inv <- .25
q_p <- qnorm(q_inv, mean = 100, sd= 15)
p_q <- pnorm(q_p, mean = 100, sd= 15)

q_p25 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, q_p)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = paste0(q_inv,"-Quantil: ",round(q_p)),
       caption = "MW-0.68sd") +
  scale_y_continuous(breaks = NULL)

q_inv <- .95
q_p <- qnorm(q_inv, mean = 100, sd= 15)

q_p95 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, q_p)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = paste0(q_inv,"-Quantil: ",round(q_p)),
       caption = "MW+1.64sd") +
  scale_y_continuous(breaks = NULL)

q_inv <- .975
q_p <- qnorm(q_inv, mean = 100, sd= 15)
#pnorm(115, mean= 100, sd = 15)

q_p975 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, q_p)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = paste0(q_inv,"-Quantil: ",round(q_p)),
       caption = "MW+2SD") +
  scale_y_continuous(breaks = NULL)

q_inv <- .84
q_p <- qnorm(q_inv, mean = 100, sd= 15)
#pnorm(115, mean= 100, sd = 15)

q_p84 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, q_p)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = paste0(q_inv,"-Quantil: ",round(q_p)),
       caption = "MW+1sd") +
  scale_y_continuous(breaks = NULL)

q_inv <- .69
q_p <- qnorm(q_inv, mean = 100, sd= 15)  # halbe SD
#pnorm(107.5, mean= 100, sd = 15)

q_p69 <-
  ggplot(NULL, aes(c(60,145))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey", args = list(mean=100,sd = 15), xlim= c(60, q_p)) +
  geom_line(stat = "function", fun = dnorm, args = list(mean=100,sd = 15)) +
  labs(x = "IQ", y = "Dichte",
       title = paste0(q_inv,"-Quantil: ",round(q_p)),
       caption = "MW+0.5sd") +
  scale_y_continuous(breaks = NULL)

(q_p50 + q_p25 + q_p69) / (q_p95 + q_p975 + q_p84)

ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  labs(y = "Dichte", x = "Merkmal, X") +
  theme_modern()

knitr::include_graphics("img/10_Deutsche_Mark_-_detail.png")
