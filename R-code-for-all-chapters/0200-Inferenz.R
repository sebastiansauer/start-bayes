library(tidyverse)
library(easystats)
library(nycflights13)

theme_set(theme_minimal())
#scale_color_okabeito()
scale_colour_discrete <- function(...) 
  scale_color_okabeito()

## flowchart TD
##   A{Goals} --> B(describe)
##   A --> C(predict)
##   A --> D(explain)
##   B --> E(distribution)
##   B --> F(assocation)
##   B --> G(extrapolation)
##   C --> H(point estimate)
##   C --> I(interval)
##   D --> J(causal inference)
##   D --> K(population)
##   D --> L(latent construct)
## 

#library(kableExtra)
library(tidyverse)
library(knitr)
library(easystats)

x <- tribble(
    ~Kennwert, ~Stichprobe, ~`Grundgesamtheit (Aussprache)`, ~Schätzwert,
   "Mittelwert", "$\\bar{X}$", "$\\mu$ (mü)" ,      "$\\hat{\\mu}$",
  "Streuung",     "$sd$",      "$\\sigma$ (sigma)", "$\\hat{\\sigma}$",
  "Anteil", "$p$", "$\\pi$ (pi)", "$\\hat{\\pi}$",
  "Korrelation", "$r$", "$\\rho$ (rho)", "$\\hat{\\rho}$" ,
  "Regression", "$b$", "$\\beta$ (beta)", "$\\hat{\\beta}$"

)

knitr::kable(x, escape = FALSE, booktabs = TRUE, format = "simple") 

## flowchart LR
## X --> Y
## 
## 
## X1 --> Y2
## X2 --> Y2

data(mtcars)

mtcars$am <- factor(mtcars$am)

ggplot(mtcars) +
  aes(x = hp, y = mpg) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mtcars) +
  aes(x = hp, y = mpg, color = am) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_color_okabeito() +
  theme(legend.position = c(0.9, .90))

## flowchart LR
## X1 -->|Werte von Steigung und Achsenabschnitt?|B
## X2 -. Genauigkeit des Modells .-> B
## 

set.seed(42)
stipro1 <- sample_n(flights, size = 100)

set.seed(3141)
stipro2 <- sample_n(flights, size = 100)

set.seed(2718)
stipro3 <- sample_n(flights, size = 100)

## library(nycflights13)
## data(flights)
## 
## stipro1 <- sample_n(flights, size = 100)
## stipro2 <- sample_n(flights, size = 100)
## stipro3 <- sample_n(flights, size = 100)

ggplot(stipro1, aes(x = air_time, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(stipro2, aes(x = air_time, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(stipro3, aes(x = air_time, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = "lm")

set.seed(42)
d1 <- tibble(
  x = rnorm(50),
  y1 = x + rnorm(50, mean = 0, sd = .5),
)

lm1 <-  lm(y1 ~ x, data = d1)

d1 <-
  d1 %>% 
  mutate(pred = predict(lm1)) %>% 
  mutate(above_pred = ifelse(y1 > pred, "above", "below"),
         e = resid(lm1),
         e2 = e * 3,
         e3 = ifelse(e > 0, + 2, -2),
         y2 = pred + e2,
         y3 = pred + e3)

ggplot(d1, aes(x, y1)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(limits = c(-4, 4))


ggplot(d1, aes(x, y2)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(limits = c(-4, 4))

ggplot(d1, aes(x, y3)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(limits = c(-4, 4))


lm1_glm <- lm(mpg ~ hp, data = mtcars)

mtcars <-
  mtcars %>%
  mutate(pred = 30 - hp*0.07)


pred_interval <-
  tibble(
    hp = seq(min(mtcars$hp), max(mtcars$hp), by = 1),
    mpg = predict(lm1_glm, newdata = data.frame(hp)),
    lwr = mpg - 2*3,
    upr = mpg + 2*3
  )

plot1 <-
  ggplot(mtcars,
       aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("point", x = 200,
           y = predict(lm1_glm, newdata = data.frame(hp = 200)),
           color = "red",
           alpha = .5,
           size = 5)


lm1_glm <- lm(mpg ~ hp, data = mtcars)

mtcars <- 
  mtcars %>% 
  mutate(pred = 30 - hp*0.07)


pred_interval <-
  tibble(
    hp = seq(min(mtcars$hp), max(mtcars$hp), by = 1),
    mpg = predict(lm1_glm, newdata = data.frame(hp)),
    lwr = mpg - 2*3,
    upr = mpg + 2*3
  )


plot2 <-
  ggplot(mtcars,
       aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("point", x = 200,
           y = predict(lm1_glm, newdata = data.frame(hp = 200)),
           color = "red",
           alpha = .5,
           size = 5)


# plots(plot1, plot2, n_rows = 1,
#       title = c("Eine Punktschätzung mittels einer Regressionsanalyse \nohne (links) bzw. mit Ungewissheitsintervall (rechts, in grau)"))


lm1 <- lm(mpg ~ hp, data = mtcars)

p_ungewiss1 <- 
  mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,40)) +
  annotate("point", x = 200, y = predict(lm1, newdata = data.frame(hp=200)),
           color = "red", size = 5, alpha = .5)



lm1_pred <- predict(lm1, interval = "prediction")

mtcars <-
  mtcars %>% 
  mutate(upr = predict(lm1, interval = "prediction")[, 3],
         lwr = predict(lm1, interval = "prediction")[, 2])

# pred_interval2 <-
#   predict(lm1_glm,
#           newdata = data.frame(hp = pred_interval$hp),
#           interval = "prediction") %>%
#   as_tibble() %>%
#   rename(mpg = fit) %>% 
#   mutate(hp = pred_interval$hp)



p_ungewiss2 <- ggplot(mtcars) +
  aes(x = hp, y = mpg) +
  geom_point()+
  # geom_line(aes(x = hp, y = upr), linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = hp), fill = "#56B4E9FF", alpha = .3) +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("point", x = 200,
           y = predict(lm1, newdata = data.frame(hp = 200)),
           color = "#E69F00FF",
           alpha = .5,
           size = 5) +
  annotate("errorbar", x = 200, ymin = 8.5, ymax = 24.5, color = "#009E73FF", size = 2) +
  scale_y_continuous(limits = c(0,40))


p_ungewiss1 
p_ungewiss2
