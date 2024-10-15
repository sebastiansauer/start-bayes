library(tidyverse)
library(rstanarm)
library(easystats)

library(gt)
#library(DT)
library(ggdag)
library(dagitty)  # DAGs zeichnen

theme_set(theme_modern())

## SaratogaHouses_path <- "https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SaratogaHouses.csv"
## 
## d <- read.csv(SaratogaHouses_path)

data("SaratogaHouses", package = "mosaicData")
d <- SaratogaHouses  # kürzerer Name, das ist leichter zu tippen

myf <- function(x) -x+0.75

myf2 <- function(x) -x + 1.25

n <- 1e3

d2 <- tibble(
  x = runif(n),
  y = runif(n),
  status = case_when(
    y > myf(x) & y < myf2(x) ~ TRUE,
    TRUE ~ FALSE
  )
)


p_coll1 <-
  d2 %>% 
  ggplot() +
  aes(x  = x,
      y = y) +
  geom_point() +
 # scale_color_manual(values = c("grey80", "black")) +
  theme_bw() +
  labs(x = "Looks",
       y = "Talent") +
    theme(legend.position = "bottom",
          axis.text = element_blank())

p_coll1

p_coll2 <- 
  d2 %>% 
  ggplot() +
  aes(x  = x,
      y = y,
      color = status) +
  geom_point() +
  scale_color_manual(values = c("grey80", "black")) +
  theme_bw() +
  labs(x = "Looks",
       y = "Talent") +
    theme(legend.position = "bottom",
          axis.text = element_blank())

p_coll2

coll1_dag <-
  dagify(date ~ Looks + Talent)

p_coll_dag1 <- 
coll1_dag %>% 
  ggdag() +
  theme_dag()

p_coll_dag1

p_coll_dag2 <-
  collider_triangle(x = "Looks",
                  y = "Talent",
                  m = "date") %>% 
  ggdag_dseparated(controlling_for = "m",
                   text = TRUE,
                   use_labels = "label") +
  theme_dag()

p_coll_dag2

## ggdag_adjust(coll1_dag, var = "date") +
##   theme_dag()

coll2_dag <- ggdag::dagify(s ~ d + iq,
                      outcome = "s",
                      labels = c("s" = "Studium",
                                 "iq" = "Intelligenz",
                                 "d" = "Fleiss"))

p_coll_dag2 <- ggdag(coll2_dag, use_labels = "label")  + theme_dag_blank()
p_coll_dag2

# coll2_dag <-
#   dagify(eignung ~ fleiss + iq)
# 
# p_coll_dag2 <- 
# coll2_dag %>% 
#   ggdag() +
#   theme_dag()
# 
# p_coll_dag2

set.seed(42)  # Reproduzierbarkeit
N <- 1e03  

d_eignung <-
tibble(
  iq = rnorm(N),  # normalverteilt mit MW=0, sd=1
  fleiss = rnorm(N),
  glueck = rnorm(N, mean = 0, sd = .1),
  eignung = 1/2 * iq + 1/2 * fleiss + glueck,
  # nur wer geeignet ist, studiert (in unserem Modell):
  studium = ifelse(eignung > 0, 1, 0) 
  )

m_eignung <-
  stan_glm(iq ~ fleiss, data = d_eignung %>%  filter(studium == 1), refresh = 0)

hdi(m_eignung)

hdi(m_eignung) |> display()

d_eignung %>% 
  filter(studium == 1) %>% 
  ggplot(aes(x = fleiss, y = iq)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  labs(title = "Negativer Zusammenhang von Fleiss und IQ bei Studentis",
       subtitle = "Macht Fleiss blöd?")

p1 <- d_eignung %>% 
 ggplot(aes(x = fleiss, y = iq)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm")

p2 <- 
d_eignung %>% 
  mutate(studium = factor(studium)) %>% 
  ggplot(aes(x = fleiss, y = iq, color = studium)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  scale_color_okabeito() +
  scale_fill_okabeito()


plots(p1, p2,
      tags = "A",
      title = c("Kein Stratifizierung, keine Scheinkorrelation",
               "Mit Stratifizierung gibt es Scheinkorrelation"))

dag_coords <-
  tibble(name = c("G", "E", "K"),
         x    = c(1, 2, 2),
         y    = c(2, 2, 1))

dagify(E ~ G,
       K ~ E + G,
       coords = dag_coords) %>%
  ggdag() +
  theme_dag()

source("funs/gg_fancy_dag.R")

coll4_dag <-
  dagitty("dag
          {
          G -> E
          E -> K
          G -> K
          U -> E
          U -> K
          }
          ")

dag_coords <-
  tibble(name = c("G", "E", "K", "U"),
         x    = c(1, 2, 2, 2.5),
         y    = c(2, 2, 1, 1.5))

dagify(E ~ G + U,
       K ~ E + G + U,
       coords = dag_coords) %>% 
  gg_fancy_dag(x = 2.5, y = 1.5, circle = "U")

source("funs/gg_simple_dag.R")


dag_coords <-
  tibble(name = c("a", "b", "p"),
         x    = c(0, -1, 1),
         y    = c(1, 0, 0))

dagify(p ~ a + b,
       b ~ a,
       coords = dag_coords) %>%
  gg_simple_dag()

dag_coords <-
  tibble(name = c("a", "b", "p"),
         x    = c(0, -1, 1),
         y    = c(1, 0, 0))

dagify(p ~ a + b,
       coords = dag_coords) %>%
  gg_simple_dag()

p_conf <- confounder_triangle(x = NULL, y = NULL, z = NULL, x_y_associated = FALSE) %>% 
  gg_simple_dag() +
  labs(title = "Die Konfundierung")

p_med <- 
  mediation_triangle(x = NULL, y = NULL, m = NULL, x_y_associated = FALSE) %>% 
  gg_simple_dag() +
  labs(title = "Die Mediation")

p_coll <- collider_triangle(x = NULL, y = NULL, m = NULL, x_y_associated = FALSE) %>% 
  gg_simple_dag() +
  labs(title = "Die Kollision")

dag_desc <- 
  dagitty('
          dag{
          
          m [pos="1.000,0.000"]
          x [exposure,pos="0.000,1.000"]
          y [outcome,pos="2.000,1.000"]
          d [pos="1,1"]

          x -> m
          y -> m
          m -> d
          }')

p_desc <-
  dag_desc %>%
  gg_simple_dag() +
  labs(title ="Der Nachfahre")

plots(p_conf, p_med, p_coll, p_desc, n_rows = 2)

p_med

dag <- 
  dagitty('
          dag{
          
          m [pos="1.000,0.000"]
          x [pos="0.000,1.000"]
          y [pos="2.000,1.000"]
         

          x -> m
          x -> y
          m -> y
          }')

gg_fancy_dag(dag)

p_desc

dag_coords <-
  tibble(name = c("A", "B", "C", "U", "X", "Y"),
         x    = c(2, 2, 3, 1, 1, 3),
         y    = c(4, 2, 3, 3, 1, 1))

dagify(B ~ C + U,
       C ~ A,
       U ~ A,
       X ~ U,
       Y ~ C + X,
       coords = dag_coords) %>%
  gg_fancy_dag(x = 1, y = 3, circle = "U")

dag_coords <-
  tibble(name = c("A", "D", "M", "S", "W"),
         x    = c(1, 3, 2, 1, 3),
         y    = c(1, 1, 2, 3, 3))

dagify(A ~ S,
       D ~ A + M + W,
       M ~ A + S,
       W ~ S,
       coords = dag_coords) %>%
  gg_simple_dag()

dag_6.2 <- 
  dagitty(
    "dag {
    A -> D
    A -> M -> D
    A <- S -> M
    S -> W -> D
    }"
  )

impliedConditionalIndependencies(dag_6.2)
