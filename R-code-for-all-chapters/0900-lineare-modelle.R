library(tidyverse)
library(easystats)
library(rstanarm)  # Bayes-Golem
library(ggpubr)  # Datenvisualisierung

library("latex2exp")
library("patchwork")
library("gt")

theme_set(theme_modern())

Kung_path <- "https://raw.githubusercontent.com/sebastiansauer/Lehre/main/data/Howell1a.csv"  # <1>

d <- read.csv(Kung_path)   # <2>

d2 <- d %>% filter(age > 18)  # <3>

d2 %>% 
  ggplot(
       aes(x = weight, y = height)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm")

ggscatter(d2,
          x = "weight", y = "height",
          add = "reg.line")

rm(d)
rm(d2)

Kung_path <- "data/Howell1a.csv"  
d <- read_csv(Kung_path)  

d2 <- d %>% filter(age > 18)

describe_distribution(d2)

describe_distribution(d2) |> display()

library(DataExplorer)

d2 %>% plot_missing()

d2 %>% plot_histogram()

d2 %>% plot_bar()

d2 %>% plot_correlation()

d3 <- 
  d2 %>% 
  mutate(weight_c = as.numeric(center(weight)))

## d3 <-
##   d2 %>%
##   mutate(weight_c = weight - mean(weight))

d3 %>% 
  slice_head(n=3) %>% 
  gt() %>% 
  fmt_number(columns = everything(), decimals = 0)

d3 %>% 
  select(weight, weight_c) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free") + 
  theme_minimal()

# Posteriori-Vert. berechnen:
m43a <-
  stan_glm(
    height ~ weight_c,  # Regressionsformel
    prior = normal(5, 3),  # Regressionsgewicht (beta 1)
    prior_intercept = normal(178, 20),  # mu
    prior_aux = exponential(0.1),  # sigma
    refresh = 0,  # zeig mir keine Details
    seed = 42,  # lege die Zufallszahlen fest für Reproduzierbarkeit
    data = d3)

m43a %>%  
  as_tibble() %>% 
  mutate(id = 1:nrow(.), .before = 1) %>% 
  head(n=3) %>% 
  gt() %>% 
  fmt_number(columns = 2:4, decimals = 1)

parameters(m43a)

parameters(m43a) %>% 
  display()

plot(p_direction(m43a))

m43a %>% 
  as_tibble() %>% 
  head()

d3 %>% 
  ggplot() +
  aes(x = weight_c, y = height) +
  geom_point() +
  geom_abline(
    slope = 0.9,  # Median beta 1
    intercept = 154,  # Median beta 0
    color = "blue")

m43_expect <- estimate_expectation(m43a)   # aus {easystats}
plot(m43_expect)

## m43a %>%
##   parameters() %>%
##   display()

m43a_post <- 
  m43a %>% 
  as_tibble()

m43a_post %>% 
  head()

m43a_post %>% 
  ggplot(aes(x = weight_c)) +
  geom_density(fill = "orange")

## m43a_post %>%
##   summarise(map_b1 = map_estimate(weight_c))

m43a_post %>% 
  ggplot(aes(x = sigma)) +
  geom_density(fill = "orange")

m43a_hdi <- hdi(m43a_post)  # analog mit eti(m43a)

plot(m43a_hdi)

d3 %>% 
  ggplot(aes(x = weight_c, 
             y = height)) +
  geom_point() +
  geom_abline(
    data = m43a_post %>% 
      slice_head(n = 10),
    aes(slope = weight_c,
        intercept = `(Intercept)`),
    alpha = .3)

d3 %>% 
  ggplot(aes(x = weight_c, 
             y = height)) +
  geom_point() +
  geom_abline(
    data = m43a_post %>% 
      slice_head(n = 100),
     aes(slope = weight_c,
        intercept = `(Intercept)`),
    alpha = .1)

d3 %>% 
  ggplot(aes(x = weight_c, 
             y = height)) +
  geom_point() +
  geom_abline(
    data = m43a_post %>% 
      slice_head(n = 1e3),
     aes(slope = weight_c,
        intercept = `(Intercept)`),
    alpha = .01)

estimate_expectation(m43a, seed = 42) %>% plot()

m43a_post %>% 
  summarise(
    q_50 = quantile(`(Intercept)`, prob = .5),
    q_90 = quantile(`(Intercept)`, prob = .9),
    q_05 = quantile(`(Intercept)`, prob = .95))

m43a %>% 
  eti(ci = .5)

m43a_post %>% 
  count(gross = `(Intercept)` >= 155) %>% 
  mutate(prop = n / sum(n))

wskt_gross <- 
  m43a_post %>% 
  count(gross = `(Intercept)` >= 155) %>% 
  mutate(prop = n / sum(n)) %>% 
  pull(prop) %>% 
  `[`(2) %>% 
  round(2)

m43a_post %>% 
  count(klein = (`(Intercept)` <= 154.5)) %>% 
  mutate(prop = n / sum(n))

wskt_klein <- 
  m43a_post %>% 
  count(klein = `(Intercept)` <= 154.5) %>% 
  mutate(prop = n / sum(n)) %>% 
  pull(prop) %>% 
  `[`(2) %>% 
  round(2)

m42 <- read_rds(paste0(here::here(),"/objects/m42.rds"))

m42_post <- as_tibble(m42)
names(m42_post) <- c("mu", "sigma")

plot_post_42 <- 
  m42_post %>% 
  ggplot() +
  aes(x = mu) +
  geom_density(fill = "grey60") +
  labs(x = expression(mu),
       title = TeX("Posteriori-Verteilung für $\\mu$, m42")) +
  scale_y_continuous(breaks = NULL)

lm1 <- lm(height ~ weight, data = d2)

d_pred <-
  tibble(weight = c(40, 45, 50, 55),
         height = predict(lm1, newdata = data.frame(weight)))



plot_condition <- 
  d2 %>% 
  #select(weight, height) %>% 
  #drop_na() %>% 
  ggplot(
       aes(x = weight, y = height)) +
  geom_point(alpha = .7) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = as.factor(weight)), data = d_pred, size = 5, alpha = .5) +
  labs(color = "Gewicht",
       x = "Gewicht (weight)",
       y = "Größe (height)") +
  scale_color_okabeito()


k <- 3
n <- 50
sigma <- sigma(lm1)
ab <- coef(lm1); a <- ab[1]; b <- ab[2]

x <- seq(-k*sigma, k*sigma, length.out = n)
y <- dnorm(x, 0, sigma)/dnorm(0, 0, sigma) * 1

x0 <- 40
y0 <- a+b*x0
path1 <- data.frame(x = y + x0, y = x + y0)
segment1 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)

x0 <- 45
y0 <- a+b*x0
path2 <- data.frame(x = y + x0, y = x + y0)
segment2 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)

x0 <- 50
y0 <- a+b*x0
path3 <- data.frame(x = y + x0, y = x + y0)
segment3 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)


x0 <- 55
y0 <- a+b*x0
path4 <- data.frame(x = y + x0, y = x + y0)
segment4 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)

plot_condition_with_cond_post <- 
plot_condition +
  geom_path(aes(x,y), data = path1, 
            color = okabeito_colors()[1]) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), 
               data = segment1,
               color = okabeito_colors()[1]) +
  
  geom_path(aes(x,y), data = path2, 
            color = okabeito_colors()[2]) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), 
               data = segment2,
               color = okabeito_colors()[2]) +
  
   geom_path(aes(x,y), data = path3, 
            color = okabeito_colors()[3]) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), 
               data = segment3,
               color = okabeito_colors()[3]) +
  
   geom_path(aes(x,y), data = path4, 
            color = okabeito_colors()[4]) +
  geom_segment(aes(x=x,y=y,xend=xend,yend=yend), 
               data = segment4,
               color = okabeito_colors()[4]) 


d2 <-
  d2 %>% 
  mutate(weight_c = weight - mean(weight))

m43 <- 
  stan_glm(height ~ weight_c, 
           prior = normal(0, 10),
           prior_intercept = normal(178, 20),  # mu
           prior_aux = exponential(0.1),  # sigma
           refresh = FALSE,  # bitte nicht so viel Ausgabe drucken
           data = d2)

m43_prior_pred <-
    stan_glm(height ~ weight_c, 
           prior_intercept = normal(178, 20),  # mu
           prior_aux = exponential(0.1),  # sigma
           refresh = FALSE, 
           prior_PD = TRUE,   # THIS LINE MAKES A PRIOR PRED
           data = d2)

stan_glm(height ~ weight_c, 
           prior = normal(0, 10),
           prior_intercept = normal(0, 20),  # mu
           prior_aux = exponential(0.1),  # sigma
           refresh = FALSE,  # bitte nicht so viel Ausgabe drucken
           data = d2)

m43a_prior_pred <-
    stan_glm(height ~ weight_c, 
           prior_intercept = normal(0, 20),  # mu
           prior_aux = exponential(0.1),  # sigma
           refresh = FALSE, 
           prior_PD = TRUE,   # THIS LINE MAKES A PRIOR PRED
           data = d2)

## # print(m43)
## # summary(m43_prior_pred)
## # write_rds(m43, "objects/m43.rds")
## # write_rds(m43a, "objects/m43a.rds")

nd <- tibble(
  weight_c = c(-5, 0, +5, 10)
)

m43_post <- 
  m43 %>% 
  as_tibble()

m43_post <- 
m43_post |> 
  mutate(mu_at_45 = `(Intercept)`,
         mu_at_40 = mu_at_45 - 5 * weight_c,
         mu_at_50 = mu_at_45 + 5 * weight_c,
         mu_at_55 = mu_at_45 + 10 * weight_c)


m43_post_long <-
  m43_post |> 
  select(starts_with("mu_at_")) |> 
  pivot_longer(everything(),
               names_to = "Gewicht",
               values_to = "h")

m43_post_long_summary <-
  m43_post_long |> 
  group_by(Gewicht) |> 
  summarise(
    m = mean(h),
    s = sd(h)) |> 
  mutate(
    ci_low = m - 2*s,
    ci_high = m + 2*s)

p_post_at <-
  m43_post_long %>% 
  ggplot() +
  aes(x = h) +
  geom_density(aes(fill = Gewicht)) +
  facet_wrap(~ Gewicht, nrow = 1, scales = "free") +
  scale_y_continuous(breaks = NULL) +
  labs(
       caption = "Horizontale Balken zeigen MW±2sd",
       x = "Größe",
       y = "Post-Wskt") +
  geom_point(data = m43_post_long_summary,
             aes(x = m,
                 y = 0),
             size = 2, color = "blue", alpha = .5) +
  geom_segment(data = m43_post_long_summary,
               aes(x = m-2*s,
                   xend = m+2*s),
               y = 0,
               yend = 0,
               color = "blue",
               alpha = .5,
               linewidth = 2) +
  scale_x_continuous(breaks = NULL) +
  scale_fill_okabeito()


plots(plot_condition_with_cond_post, p_post_at, 
      n_rows = 2, tags = "A")

mu_at_45 <-
  m43a_post %>% 
  mutate(mu_at_45 = `(Intercept)`)

## mu_at_45 %>%
##   ggplot(aes(x = mu_at_45)) +
##   geom_density()

mu_at_45 %>% 
  ggplot(aes(x = mu_at_45)) +
  geom_density() +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(mu["height | weight = 45"])) +
  scale_x_continuous(limits = c(150, 160))

mu_at_50 <-
  mu_at_45 %>% 
  mutate(mu_at_50 = `(Intercept)` + 5 * weight_c)

head(mu_at_50)

## mu_at_50 %>%
##   ggplot(aes(x = mu_at_50)) +
##   geom_density()

mu_at_50 %>% 
  ggplot(aes(x = mu_at_50)) +
  geom_density() +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(mu["height | weight = 50"])) +
  scale_x_continuous(limits = c(150, 160))

mu_at_50 %>% 
  eti(mu_at_50, ci = .9)

mu_at_45 %>% 
  summarise(q_95 = quantile(mu_at_45, prob = .95))

m43_prior_pred <-
    stan_glm(height ~ weight_c, 
             prior = normal(0, 10),
             prior_intercept = normal(178, 20),  # mu
             prior_aux = exponential(0.1),  # sigma
             refresh = FALSE, 
             prior_PD = TRUE,  # Schalter für Prior-Pred-Verteilung
             data = d3)


m43_prior_pred_draws <- 
  m43_prior_pred %>% 
  as_tibble() %>% 
  rename(a = `(Intercept)`,
         b = weight_c) %>% 
  slice_sample(n = 50)

m43_prior_pred_draws %>% 
  slice_head(n=5) %>% 
  gt() %>% 
  fmt_number(everything(), decimals = 1)

## m43_prior_pred <-
##     stan_glm(height ~ weight_c,
##              prior = normal(0, 10),  # beta
##              prior_intercept = normal(178, 20),  # alpha
##              prior_aux = exponential(0.1),  # sigma
##              refresh = FALSE,
##              prior_PD = TRUE,  # DIESER Schalter macht's
##              data = d3)
## 
## m43_prior_pred_draws <-
##   m43_prior_pred %>%
##   as_tibble() %>%
##   rename(a = `(Intercept)`,
##          b = weight_c) %>%
##   slice_sample(n = 50)

## m43a_prior_pred <-
##     stan_glm(height ~ weight_c,
##              prior = normal(0, 10),  # beta
##              prior_intercept = normal(0, 20),  # alpha
##              prior_aux = exponential(0.1),  # sigma
##              refresh = FALSE,
##              prior_PD = TRUE,  # DIESER Schalter machts
##              data = d3)
## 
## m43a_prior_pred_draws <-
##   m43a_prior_pred %>%
##   as_tibble() %>%
##   rename(a = `(Intercept)`,
##          b = weight_c) %>%
##   slice_sample(n = 50)

## d3 %>% ggplot() +
##   geom_point(aes(x = weight_c, y = height)) +
##   geom_abline(data = m43_prior_pred_draws,
## aes(intercept = a, slope = b), color = "skyblue", size = 0.2) +
##   scale_y_continuous(limits = c(0, 500)) +
##   geom_hline(yintercept = 272, size = .5) +
##   geom_hline(yintercept = 0, linetype = "dashed")



d <-
  tibble(
    x = seq(-30,30,.1),
    y = dnorm(x, mean = 0, sd = 10)
  )

d %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  scale_y_continuous(breaks = NULL) +
  labs(title = "mu=0, s=10")

d <-
  tibble(
    x = seq(-30,30,.1),
    y = dnorm(x, mean = 3, sd = 2)
  )

d %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  scale_y_continuous(breaks = NULL) +
  labs(title = "mu=5, sd = 3")

m43a_prior_pred <-
    stan_glm(
      height ~ weight_c, 
      prior = normal(2, 2),  # Regressionsgewicht
      prior_intercept = normal(178, 20),  # mu
      prior_aux = exponential(0.1),  # sigma
      refresh = FALSE, 
      # Schalter für Prior-Pred-Verteilung:
      prior_PD = TRUE, 
      data = d3)


m43a_prior_pred_draws <- 
  m43a_prior_pred %>% 
  as_tibble() %>% 
  # Spaltennamen kürzen: 
  rename(a = `(Intercept)`) %>%  
  rename(b = weight_c,
         s = sigma)

m43a_prior_pred_draws %>% 
  slice_head(n=5) %>% 
  gt() %>% 
  fmt_number(everything(), decimals = 1)

d3 %>% 
  ggplot(aes(x = weight_c, y = height)) +
  geom_point() +
  geom_abline(data = {m43a_prior_pred_draws %>% slice_head(n=50)},
              aes(slope = b,
                  intercept = a),
              color = "skyblue",
              size = .2,
              alpha = .7) +
  geom_hline(yintercept = 272, size = .5) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_y_continuous(limits = c(0, 500)) 

# Posteriori-Vert. berechnen:
m43a <-
  stan_glm(
    height ~ weight_c,  # Regressionsformel
    prior = normal(5, 3),  # Regressionsgewicht (beta 1)
    prior_intercept = normal(178, 20),  # mu
    prior_aux = exponential(0.1),  # sigma
    refresh = 0,  # zeig mir keine Details
    seed = 42,  # Zufallszahlen festlegen
    data = d3)

## m43a %>%
##   parameters()

m43a %>% 
  parameters() %>% 
  display()

estimate_prediction(m43a) %>% plot()
estimate_relation(m43a) %>% plot()

set.seed(42)
estimate_prediction(m43a, data = tibble(weight_c = 0), seed = 42)

weight_df <- tibble(weight_c = seq(-20,20, by = 5))

mus <- 
  estimate_prediction(m43a, data = weight_df) 

head(mus)

ppv_m43a <- estimate_prediction(
  m43a,
  data = weight_df)

mus 

mus <- 
  mus %>% 
  mutate(height = 154.6 + 0.9*weight_c)

d3 %>% 
  ggplot(aes(x = weight_c, y = height)) +
  geom_point(color = "grey60") +
  geom_abline(slope = coef(m43a)[2], intercept = coef(m43a)[1], color = "blue") +
  geom_errorbar(data = mus,
                aes(ymin = CI_low,
                    ymax = CI_high),
                size = .5,
                width = .5,
                color = "firebrick")

ppv_m43_weight_df <-
  posterior_predict(m43a,
                    newdata = weight_df) %>% 
  as_tibble() %>% 
  pivot_longer(everything(),
               names_to = "weight_condition",
               values_to = "height")

weight_df <-
  weight_df %>% 
  mutate(weight_condition = as.character(c(1:9)))

ppv_m43_weight_df <- 
  ppv_m43_weight_df %>% 
  full_join(weight_df, by = "weight_condition")

d3 %>% 
  ggplot() +
  geom_violin(data = ppv_m43_weight_df,
              aes(x = weight_c, y = height, group = weight_c),
                fill = "grey80",
              width = 1) +
    geom_point(aes(x = weight_c, y = height)) +
  geom_abline(slope = coef(m43a)[2], intercept = coef(m43a)[1], color = "blue")

check_predictions(m43a)  # aus easystatss

ppv_m43a <- posterior_predict(
  m43a,
  newdata = weight_df,
  draws = 100) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "weight_condition",
    values_to = "height")
head(ppv_m43a)

ppv_m43a <-
  ppv_m43a <- posterior_predict(
  m43a,
  newdata = weight_df,
  draws = 100) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "weight_condition",
    values_to = "height")

head(ppv_m43a)

ppv_m43a %>% 
  summarise(
    q_10 = quantile(height, prob = .1),
    height_mean = mean(height),
    q_50 = quantile(height, prob = .5),
    q_90 = quantile(height, prob = .9)
  )

ppv_m43a %>% 
  eti(ci = .5)
