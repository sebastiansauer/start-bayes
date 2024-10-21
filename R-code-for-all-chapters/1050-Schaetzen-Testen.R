library(rstanarm)   # Bayes-Modelle
library(tidyverse)
library(easystats)

library(icons)
library(gt)
library(ggridges)
library(plotly)
library(patchwork)
library(plotly)
library(dagitty)

theme_set(theme_modern())

## penguins_url <- "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv"
## 
## penguins <- read.csv(penguins_url)

data("penguins", package = "palmerpenguins")

m1 <- stan_glm(
  body_mass_g ~ sex, 
  data = penguins, 
  refresh = 0,  # unterdrückt Ausgabe der Posteriori-Stichproben
  seed = 42  # zur Reproduzierbarkeit
)

m1_post <-
  m1 |> 
  as_tibble()

m1_post |> 
  count(sexmale < 0)

knitr::include_graphics("img/5v5531.jpg")

knitr::include_graphics("img/Kruschke-2018-Fig1.png")

m10.6 <- stan_glm(body_mass_g ~ species, 
                  data = penguins, 
                  refresh = 0,  # unterdrückt Ausgabe der Posteriori-Stichproben
                  seed = 42  # zur Reproduzierbarkeit
                  )

rope(m10.6)

## rope(m10.6) %>% plot()

plot(rope(m10.6)) + scale_fill_okabeito()

parameters(m10.6) %>% 
  plot() +
  geom_rect(aes(xmin = 0-80, xmax = 0+80, ymin = -Inf, ymax = Inf), 
              fill = "blue", alpha = 0.2, color = NA)

rope(m10.6, range = c(-100, 100))
plot(rope(m10.6, range = c(-100, 100))) + scale_fill_okabeito()

## rope(m10.6, range = c(-100,100), ci = .89, ci_method = "ETI")

m10.6_r2 <-
  m10.6 %>% 
  r2_posterior() %>% 
  as_tibble()

hdi(m10.6_r2) %>% 
  plot()
