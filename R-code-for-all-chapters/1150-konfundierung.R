library(dagitty)  # DAGs zeichnen
library(tidyverse)
library(rstanarm)
library(easystats)

library(gt)
#library(DT)
library(ggdag)

theme_set(theme_modern())

## SaratogaHouses_path <- "https://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/SaratogaHouses.csv"
## 
## d <- read.csv(SaratogaHouses_path)

data("SaratogaHouses", package = "mosaicData")
d <- SaratogaHouses  # kürzerer Name, das ist leichter zu tippen

studie_a <-
  tibble::tribble(
     ~ Gruppe,      ~`Mit Medikament`,         ~`Ohne Medikament`,
"Männer",    "81/87 überlebt (93%)", "234/270 überlebt (87%)",
"Frauen",  "192/263 überlebt (73%)",   "55/80 überlebt (69%)",
"Gesamt",  "273/350 überlebt (78%)", "289/350 überlebt (83%)"
  ) 

studie_a %>% 
  gt()



dag_studie_a <-
  dagitty("dag{
          gender -> drug
          drug -> recovery
          gender -> recovery
          }
      ")

coordinates(dag_studie_a) <-
  list(x = c(gender = 0, drug = 0, recovery  = 1),
       y = c(gender = 0, drug = 1, recovery = 0.5))


plot(dag_studie_a)

studie_b <- 
  tibble::tribble(
~ Gruppe,          ~`Ohne Medikament`,          ~`Mit Medikament`,
"geringer Blutdruck",    "81/87 überlebt (93%)", "234/270 überlebt (87%)",
"hoher Blutdruck",  "192/263 überlebt (73%)",   "55/80 überlebt (69%)",
"Gesamt",  "273/350 überlebt (78%)", "289/350 überlebt (83%)"
  )

studie_b %>% 
  gt()

dag_studie_b <-
  dagitty("dag{
          drug -> pressure
          drug -> toxic
          pressure -> recovery
          toxic -> recovery
          }
      ")


coordinates(dag_studie_b) <-
  list(x = c(drug = 0, pressure = 1, toxic = 1, recovery  = 2),
       y = c(drug = 1, pressure = 0, toxic = 2, recovery = 1))


plot(dag_studie_b)

dag_studie_c <-
  dagitty("dag{
         size -> recovery
         size -> treatment
         treatment -> recovery
          }
      ")

coordinates(dag_studie_c) <-
  list(x = c(size = 0, treatment = 0, recovery  = 1),
       y = c(size = 0, treatment = 1, recovery = 0.5))
plot(dag_studie_c)



coordinates(dag_studie_c) <-
  list(x = c(size = 0.5, treatment = 0, recovery  = 1),
       y = c(size = 0, treatment = 1, recovery = 1))
plot(dag_studie_c)

d %>% 
  select(price, livingArea, bedrooms,waterfront) %>% 
  slice_head(n = 5)

d %>% 
  ggplot() +
  aes(x = bedrooms, y = price) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") + 
  theme_minimal()

m1 <- stan_glm(price ~ bedrooms,
               refresh = 0,
               seed = 42,
               data = d)

point_estimate(m1)

point_estimate(m1)

dons_house <- tibble(bedrooms = 2)
estimate_prediction(m1, data = dons_house)

estimate_prediction(m1, data = dons_house) |> display() 

dons_new_house <- tibble(bedrooms = 4)
estimate_prediction(m1, dons_new_house)
predict(m1, newdata = dons_new_house)

estimate_prediction(m1, dons_new_house) |> display()

## estimate_expectation(m1, dons_new_house)

estimate_expectation(m1, dons_new_house) |> display()

m2 <- stan_glm(price ~ bedrooms + livingArea, 
               data = d, 
               seed = 42,
               refresh = 0)

point_estimate(m2, centrality = "median")

point_estimate(m2, centrality = "median") |> display()

predict(m2, newdata = data.frame(bedrooms = 4, livingArea = 1200))

estimate_prediction(m2, data = tibble(bedrooms = 4, livingArea = 1200))

estimate_prediction(m2, data = tibble(bedrooms = 2, livingArea = 1200))

estimate_means(m2, at = "bedrooms", length = 7)

estimate_means(m2, at = "bedrooms", length = 7) %>% 
  ggplot() +
  aes(x = bedrooms, y = Mean) +
  geom_line() +
  geom_point(alpha = .7) 

km1 <- confounder_triangle(x = "bedrooms",
                          y = "price",
                          z = "living area") %>% 
  ggdag_dconnected(text = FALSE, use_labels = "label") +
  theme_dag()

print(km1) 

confounder_triangle(x = "bedrooms",
                          y = "price",
                          z = "living area") %>% 
 ggdag_dconnected(text = FALSE, use_labels = "label", 
                  controlling_for = "z") +
  theme_dag()

source("R-Code/controlling-confounder.R")

p_konf1
p_konf2

km_med <-
  dagitty("dag{
          bedrooms -> price
          area -> price
          area -> bedrooms
          }
      ")

coordinates(km_med) <-
  list(x = c(area = 0, bedrooms = 1, price  = 2),
       y = c(area = 0, bedrooms = -1, price = 0))


plot(km_med)

km3 <- collider_triangle(x = "bedrooms",
                          y = "livingArea",
                          m = "price") %>% 
  ggdag_dconnected(text = FALSE, use_labels = "label") +
  theme_dag()

print(km3)

dons_r <- d %>% 
  summarise(cor(bedrooms, livingArea))

# dag_coords <-
#   tibble(name = c("A", "D", "M", "S", "W"),
#          x    = c(1, 3, 2, 1, 3),
#          y    = c(1, 1, 2, 3, 3))
# 
# dagify(A ~ S,
#        D ~ A + M + W,
#        M ~ A + S,
#        W ~ S,
#        coords = dag_coords) %>%
#   gg_simple_dag()

dag_km1 <-
  dagitty("dag{
         a -> b
         a -> p
         }
         ")


coordinates(dag_km1) <- list(
  x = list(a = 0, b = 1, p = 1),
  y = list(a = 0.5, b= 1, p = 0 )
)

ggdag_equivalent_dags(dag_km1) +
  theme_dag()

knitr::include_graphics("img/correlation.png")

confounder_triangle(x = "Schoki",
                          y = "Nobelpreise",
                          z = "Entwicklungsstand") %>% 
  ggdag_dconnected(text = FALSE, use_labels = "label") +
  theme_dag()

knitr::include_graphics("img/correlation_550.png")
