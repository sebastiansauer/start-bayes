library(gt)
library(patchwork)
library(figpatch)
library(ggExtra)
library(tidyverse)
library(easystats)

theme_set(theme_modern())

library(tidyverse)  # Datenjudo
library(rstanarm)  # Bayes-Modelle berechnen
library(easystats)  # Statistik-Komfort
library(DataExplorer)  # Daten verbildlichen
library(ggpubr)  # Daten verbildlichen

Kung_path <-  
  "https://raw.githubusercontent.com/sebastiansauer/Lehre/main/data/Howell1a.csv"  

d <- read.csv(Kung_path) 

head(d)

d2 <- d %>% 
  filter(age >= 18)

nrow(d2)

## describe_distribution(d2)

describe_distribution(d2) %>% 
  gt() %>% 
  fmt_number(columns = c(3:last_col()-1)) %>% 
  fmt_integer(columns = last_col())

plot_density(d2)

p1 <-
  tibble(x = seq(from = 100, to = 250, by = .1)) %>% 
  
  ggplot(aes(x = x, y = dnorm(x, mean = 178, sd = 20))) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 100, to = 250, by = 75)) +
  labs(title = "mu ~ dnorm(178, 20)",
       y = "") +
  scale_y_continuous(breaks = NULL)

p2 <-
  tibble(x = seq(0, 50, by = .01)) %>%
  ggplot(aes(x = x, y = dexp(x, rate = .1))) +
  geom_line() +
  scale_x_continuous() +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("sigma ~ dexp(0.1)")

d <-
  tibble(
    x = seq(0, 5,.1),
    y = dexp(x, rate = 1)
  )


d_qs <-
  tibble(
    prob = c(0.05, .25, .50, .75, .95),
    q = qexp(prob) 
  )

d %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  geom_area(fill = "grey60") +
  geom_vline(data = d_qs,
             aes(xintercept = q)) +
  geom_label(data = d_qs,
             aes(x = q, 
                 label = prob,
                 y = prob)) +
  labs(
       caption = "Vertikale Striche zeigen die Quantile für 5%, 25%, 50%, 75%, 95%",
       y = "Dichte")

gg_exp <- function(r, max_x = 50) {
  med_exp <- round(log(2) / r, 2)
  p <- 
    ggplot(NULL, aes(c(0, max_x))) +
    geom_area(stat = "function", fun = dexp, fill = "grey", args = list(rate = r))  +
    labs(title = paste0("Exp(", r, ")"),
         x = "sigma",
         caption = paste0("Median (Md): ", med_exp)) +
    geom_vline(xintercept = med_exp) +
    geom_label(aes(x = med_exp), y = 0, label = "Md", show.legend = FALSE)
 
}

p_r1 <- gg_exp(r = 1)
 
p_r2 <-  gg_exp(r = 1/2)

p_r1_8 <- gg_exp(r = 1/4)

p_r1_25 <- gg_exp(r = 1/8)


plots(p_r1_25, p_r1_8, p_r2, p_r1)

qexp(p = .5, rate = 1/8)

qexp(p = c(0.025, .975), rate = 1/8)

plots(p1, p2)

if (knitr:::is_html_output()) {
  knitr::include_graphics("img/pretty_good.gif")
}

m41 <- stan_glm(height ~ 1, data = d2, refresh = 0)  # <1>
m41_post <- as_tibble(m41)    # <2>
names(m41_post) <- c("mu", "sigma")  # <3>  

m41_post %>% 
  ggplot() +
  aes(x = mu, y = sigma) %>% 
  geom_hex() +
  scale_fill_viridis_c() 


p_m41_post <- 
  m41_post %>% 
  ggplot() +
  aes(x = mu, y = sigma) +
  geom_point(alpha = .1) 

p10 <- ggExtra::ggMarginal(p_m41_post, type = "density")

p20 <- 
  m41_post %>% 
  ggplot(aes(x = mu)) + 
  geom_histogram()

p10

p20

#
m41_post %>% 
  pivot_longer(mu:sigma) %>% 
  ggplot(aes(x = value)) + 
  geom_density(fill = "grey33") +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ name, scales = "free", 
             labeller = label_parsed,
             ncol = 2)

## library(rstanarm)  # Paket muss gestartet sein.
## 
## # berechnet Post.-Vert.:
## stan_glm(
##   # modelldefinition:
##   AV ~ UV,
##   # Datensatz:
##   data = meine_daten
## )

m41 <- stan_glm(height ~ 1, data = d2, refresh = 0)  # aus Paket rstanarm

parameters(m41)  # aus Paket `easystats`

parameters(m41) %>% display()

ci_low <- parameters(m41)$CI_low
ci_high <- parameters(m41)$CI_high

post_m41 <- as_tibble(m41)
head(post_m41)


names(post_m41) <- 
  c("mu", "sigma")  # den Namen "(Intercept)" durch "mu" ersetzen, ist prägnanter

post_m41 %>% 
  count(mu > 155) %>% 
  mutate(prop = n/sum(n))

names(post_m41) <- 
  c("mu", "sigma")  # den Namen "(Intercept)" durch "mu" ersetzen, ist prägnanter

post_m41 %>% 
  count(mu > 165) %>% 
  mutate(prop = n/sum(n))

post_m41 %>% 
  summarise(q95 = quantile(mu, .95))

post_m41 %>% 
  eti()

## m41 %>%
##   parameters()

m41 %>% 
  parameters() |> 
  display()

m41 %>% 
  parameters() %>% 
  plot(show_intercept = TRUE)

prior_summary(m41)

m41a <- stan_glm(height ~ 1, 
                 data = d2, 
                 chains = 1,  # nur 1 Kette, anstelle von 4 im Default, spart Zeit
                 refresh = 0) 

parameters(m41a)  

## m42 <-
##   stan_glm(height ~ 1,
##            prior_intercept = normal(178, 20),  # mu
##            prior_aux = exponential(0.125),  # sigma
##            refresh = FALSE,  # bitte nicht so viel Ausgabe drucken
##            data = d2)
## parameters(m42)

m42 <- 
  stan_glm(height ~ 1, 
           prior_intercept = normal(178, 20),  # mu
           prior_aux = exponential(0.125),  # sigma
           refresh = FALSE,  # bitte nicht so viel Ausgabe drucken
           seed = 42,  # Zufallszahlen fixieren
           data = d2)
parameters(m42) |> display()

## write_rds(m42, "objects/m42.rds")

m42_tibble <-
  as_tibble(m42)

head(m42_tibble)

m42_tibble |> 
  gghistogram(x = "`(Intercept)`")  # Aus dem Paket "ggpubr"

m42_tibble |> 
  ggplot(aes(x = `(Intercept)`)) +  # Aus dem Paket `ggplot2`
  geom_histogram()

n <- 1e4

sim <- tibble(sample_mu  = 
      rnorm(n, 
            mean = 178, 
            sd   = 20),
    sample_sigma = 
      rexp(n, 
            rate = 0.1)) %>% 
  mutate(height  = 
      rnorm(n, 
            mean = sample_mu, 
            sd   = sample_sigma))

height_sim_sd <- 
  sd(sim$height) %>% round()
height_sim_mean <- 
  mean(sim$height) %>% round()


p3 <- 
  sim %>% 
  ggplot(aes(x = height)) +
  geom_density(fill = "grey33") +
  scale_x_continuous(breaks = c(0, 178-3*height_sim_sd, 178, 178+3*height_sim_sd)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "height ~ dnorm(mu, sigma)",
       caption = "X-Achse zeigt MW±3SD",
       x = "Größe") +
  theme(panel.grid = element_blank()) 

p3

p3

anteil_großer_kung <- 
sim %>% 
  count( height > 200) %>% 
  mutate(prop = n/sum(n))
anteil_großer_kung

sw_path <- paste0(here::here(),"/img/south_west_black_24dp2.png")
se_path <- paste0(here::here(),"/img/south_east_black_24dp2.png")


sw <- fig(sw_path)
se <- fig(se_path)



(p1 + p2) / (se + sw) / (plot_spacer() + p3 + plot_spacer()) 


set.seed(4)


# simulate
sim2 <-
  tibble(sample_mu    = rnorm(n, mean = 178, sd = 100),
         sample_sigma = rexp(n, rate = .01)) %>% 
  mutate(height = rnorm(n, mean = sample_mu, sd = sample_sigma))

# compute the values we'll use to break on our x axis
breaks <-
  c(mean(sim2$height) - 3 * sd(sim2$height), 0, mean(sim2$height), mean(sim2$height) + 3 * sd(sim2$height)) %>% 
    round(digits = 0)

# this is just for aesthetics
text <-
  tibble(height = 272 - 25,
         y      = .0013,
         label  = "größter Mann",
         angle  = 90)

# plot
p4 <-
  sim2 %>% 
  ggplot(aes(x = height)) +
  geom_density(fill = "black") +
  geom_vline(xintercept = 0, color = "grey92") +
  geom_vline(xintercept = 272, color = "grey92", linetype = 3) +
  geom_text(data = text,
            aes(y = y, label = label, angle = angle),
            color = "grey92") +
  scale_x_continuous(breaks = breaks, 
                     limits = c(-400, 700)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "height ~ dnorm(mu, sigma)\nmu ~ dnorm(178, 100)\nsigma ~ E(0.01)",
       x = "Größe",
       caption = "X-Achse zeigt MW±3SD") +
  theme(panel.grid = element_blank()) 

p4

d <- 
  tibble(x = seq(0,75, by =.01),
         y = dexp(x, rate = .01))

d %>% 
  ggplot(aes(x,y)) +
  geom_line()

## sim2 %>%
##   count(height < 0) %>%
##   mutate(prop = n()/n)
