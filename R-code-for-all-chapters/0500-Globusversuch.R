library(tidyverse)
library(ggpubr)  # komfortable Visualisierung

library(patchwork)
library(easystats)
library(ggraph)
library(tidygraph)

source("funs/binomial_plot.R")

theme_set(theme_modern())

## graph LR
## A[Priori-Vert.]-->B[Likelihood]-->C[Post-Vert.]-->A

dbinom(x = 2, size = 3, prob = 1/2)

## flowchart TD
##   A[A - Start] -. 1/2 .-> B[B - 0]
##   A -. 1/2 .-> C[C - 1]
##   B -. 1/2 .-> D[D - 0]
##   B -. 1/2 .-> E[E - 1]
##   C -. 1/2 .-> F[F - 0]
##   C -. 1/2 .-> G[G - 1]
##   D -. 1/2 .-> H[H - 0]
##   D -. 1/2 .-> J[I - 1]
##   E -. 1/2 .-> K[K - 0]
##   E -. 1/2 .-> L[L - 1]
##   F -. 1/2 .-> M[M - 0]
##   F -. 1/2 .-> N[N - 1]
##   G -. 1/2 .-> O[O - 0]
##   G -. 1/2 .-> P[P - 1]

dbinom(x = 6, size = 9, prob = 1/2)

binomial_plot(n = 9, p = 1/2)

my_tree <- tidygraph::create_tree(1023, 2, mode = "out")

my_tree %>%
  mutate(lab = 1:1023) %>% 
  ggraph(circular = TRUE) +
  geom_edge_link() +
  geom_node_label(mapping = aes(label = lab), size = 1) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()


dbinom(x = 9, size = 9, prob = 1/2)

## 
## uniform_Plot <- function(a, b){
##   xvals <- data.frame(x = c(a, b)) #Range for x-values
## 
##   ggplot(data.frame(x = xvals),
##          aes(x = x)) + xlim(c(a, b)) + ylim(0, 1/(b - a)) +
##     stat_function(fun = dunif, args = list(min = a, max = b),
##                   geom = "area",
##                   fill = "green", alpha = 0.35) +
##     stat_function(fun = dunif, args = list(min = a, max = b)) +
##     labs(x = "X", y = "Dichte")  +
##     geom_vline(xintercept = a, linetype = "dashed", colour = "red") +
##     geom_vline(xintercept = b, linetype = "dashed", colour = "red")
## 
## }
## uniform_Plot(0, 1)

## flowchart LR
##   A[Start] -->|0.6|B[A1]
##   A -.->|0.1|C[A2]
##   A -.->|0.3|D[A3]
##   B --->|0.05|E[B]
##   B -.->|0.95|F[Nicht-B]
##   C -.->|0.02|G[B]
##   C -.->|0.98|H[Nicht-B]
##   D -.->|0.04|I[B]
##   D -.->|0.96|J[Nicht-B]

p_Gitter <- seq(from = 0, to = 1, by = 0.1)
p_Gitter

Likelihood <- dbinom(6, size = 9, prob = p_Gitter)
Likelihood

d <-
  tibble(
    # definiere die Hypothesen (das "Gitter"): 
    p_Gitter = p_Gitter,
    # bestimme den Priori-Wert:       
    Priori  = .1) %>%  
    mutate(
      # berechne Likelihood für jeden Gitterwert:
      Likelihood = Likelihood,
      # berechne unstand. Posteriori-Werte:
      unstd_Post = Likelihood * Priori,
      # berechne Evidenz, d.i. die Summe aller unstand. Post-Werte:
      Evidenz = sum(unstd_Post),
      # berechne stand. Posteriori-Werte (summiert zu 1):
      Post = unstd_Post / Evidenz)  

d %>% 
  mutate(id = 1:11) %>% 
  relocate(id, .before = 1) %>% 
  knitr::kable(digits = 2)

## library(ggpubr)
## 
## ggline(d,
##        x = "p_Gitter",
##        y = "Post")

library(ggpubr)

ggline(d,
       x = "p_Gitter",
       y = "Post")
