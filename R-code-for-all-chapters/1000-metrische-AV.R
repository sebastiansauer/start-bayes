library(rstanarm)   # Bayes-Modelle
library(tidyverse)
library(easystats)

#library(icons)
library(gt)
library(ggridges)
library(plotly)
library(patchwork)
library(plotly)
library(dagitty)

theme_set(theme_modern())


data("kidiq", package = "rstanarm")

## penguins_url <- "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv"
## 
## penguins <- read.csv(penguins_url)

data("penguins", package = "palmerpenguins")

mein_modell <- "dag{
mom_hs -> kid_score
u -> kid_score
}"

plot(graphLayout(mein_modell))

data("kidiq")  # Paket rstanarm
m10.1 <- stan_glm(
  kid_score ~ mom_hs, 
  seed = 42,
  data = kidiq)

display(parameters(m10.1))

## estimate_expectation(m10.1) %>% plot()

ggplot(kidiq) +
  aes(x = mom_hs, y = kid_score) +
  geom_jitter(width = 0.1, alpha = .5) +
  geom_abline(slope = coef(m10.1)[2],
              intercept = coef(m10.1)[1])  +
  scale_x_continuous(breaks = c(0, 1))

## kidiq %>%
##   group_by(mom_hs) %>%
##   summarise(kid_score_avg =
##               mean(kid_score))

kidiq %>% 
  group_by(mom_hs) %>% 
  summarise(kid_score_avg = 
              mean(kid_score)) |> 
  display()

rope(m10.1)

rope(m10.1) %>% plot()
parameters(m10.1) %>% 
  plot() +
  geom_rect(aes(xmin = 0-2, xmax = 0+2, ymin = -Inf, ymax = Inf), 
              fill = "blue", alpha = 0.2, color = NA)

m10.1_post <-
  m10.1 %>% 
  as_tibble() 

names(m10.1_post) <- c("Achsenabschnitt", "momhs", "sigma")  # schönere Namen

m10.1_post %>% 
 # rename(momhs = mom_hs) %>% 
  slice_sample(n=5) %>% 
  gt() %>% 
  fmt_number(1:3, decimals = 1) %>% 
  tab_header("Stichprobe aus der Post-Verteilung")

pi_mom_hs <-
  m10.1_post %>% 
  summarise(pi_95 = quantile(momhs, c(.025, .975)))

pi_mom_hs

plot(eti(m10.1))

## library(remotes)  # dieses Paket können Sie mit `install.packages("remotes") installieren
## install_github("sebastiansauer/prada")

library(prada)

is_in_tolerance(asis = 77.56,  # Ihr Wert
                tobe = 77,   # Referenzwert
                tol_rel = .05,   # relative Toleranz
                tol_abs = .05 * sd(kidiq$kid_score)  # absolute Toleranz
                )

mein_modell <- "dag{
mom_hs -> kid_score
u -> kid_score
mom_iq -> kid_score
}"

plot(graphLayout(mein_modell))

data("kidiq")  # Paket rstanarm, alternativ über CSV einlesen
describe_distribution(kidiq)

describe_distribution(kidiq) %>% 
  display()

m10.2 <-
  stan_glm(kid_score ~ mom_iq, data = kidiq, seed = 42)

m10.2 %>% 
  parameters()

m10.2 %>% 
  parameters() %>% 
  display()

kidiq %>% 
  ggplot(aes(x = mom_iq, y = kid_score)) +
  geom_point(alpha = .7) +
  geom_abline(slope = coef(m10.2)[2],
              intercept = coef(m10.2)[1],
              color = "blue")

plot(estimate_expectation(m10.2))

m10.3 <- 
  stan_glm(
    kid_score ~ mom_iq + mom_hs, 
    refresh = 0,
    seed = 42,
    data = kidiq)

parameters(m10.3) %>% 
  display()

coef(m10.3)

coef(m10.3)[3]

kidiq2 <-
  kidiq %>% 
  mutate(mom_hs = as.factor(mom_hs))

m10.3a <- 
  stan_glm(
    kid_score ~ mom_iq + mom_hs, 
    refresh = 0,
    seed = 42,
    data = kidiq2)

plot(estimate_expectation(m10.3a))

m10.4 <- 
  stan_glm(kid_score ~ mom_iq + mom_hs + mom_hs:mom_iq, 
           seed = 42,
           data = kidiq, 
           refresh = 0)

parameters(m10.4) %>% 
  display()

kidiq %>% 
  mutate(mom_hs = factor(mom_hs)) %>%  
  ggplot(aes(x = mom_iq, y = kid_score, color = mom_hs)) +
  geom_point(alpha = .7) +
  geom_abline(slope = coef(m10.4)[2],
              intercept = coef(m10.4)[1],
              linewidth = 1,
              color =  "#56B4E9E6" ) +
  geom_abline(slope = coef(m10.4)[2]+coef(m10.4)[4],
              intercept = coef(m10.4)[1] + coef(m10.4)[3],
              linewidth = 2,
              color = "#009E73E6") +
  scale_color_manual(values = c( "#56B4E9E6" , "#009E73E6")) +
  scale_x_continuous(limits = c(0, 140))

mein_modell <- "dag{
u -> kid_score
mom_iq -> kid_score
momiqmom_hs -> kid_score
}"

plot(graphLayout(mein_modell))

kidiq <-
  kidiq %>% 
  mutate(mom_iq_c = mom_iq - mean(mom_iq))

m10.5 <- stan_glm(kid_score ~ mom_hs + mom_iq_c + mom_hs:mom_iq_c, 
                  data = kidiq, 
                  seed = 42,
                  refresh = 0)
coef(m10.5)

parameters(m10.5) %>% 
  select(1,2) %>% 
  display()

kidiq %>% 
  mutate(mom_hs = factor(mom_hs)) %>%  
  ggplot(aes(x = mom_iq_c, y = kid_score, color = mom_hs)) +
  geom_point(alpha = .7) +
  geom_abline(slope = coef(m10.5)[3],
              intercept = coef(m10.5)[1],
              size = 1,
              color = "#56B4E9E6") +
  geom_abline(slope = coef(m10.5)[3]+coef(m10.5)[3]*coef(m10.5)[4],
              intercept = coef(m10.5)[1] + coef(m10.5)[2],
              size = 2,
              color = "#009E73E6") +
  scale_color_manual(values = c("#56B4E9E6", "#009E73E6")) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 8),
        legend.title = element_text(size = 6)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40")

new <- tibble(mom_hs = 0, mom_iq = mean(kidiq$mom_iq))
pred_new <- predict(m10.4, newdata = new)
mean(pred_new)

new <- tibble(mom_hs = 0, mom_iq_c = 0)
pred_new <- predict(m10.5, newdata = new)
mean(pred_new)

parameters(m10.5) %>% 
  display()

parameters(m10.5, ci_method = "hdi") %>% 
  display()

mein_modell <- "dag{
g -> y
u -> y
}"

plot(graphLayout(mein_modell))

penguins %>% 
  select(body_mass_g, species) %>% 
  group_by(species) %>% 
  describe_distribution(range = FALSE, iqr = FALSE)

penguins %>% 
  select(body_mass_g, species) %>% 
  group_by(species) %>% 
  describe_distribution(range = FALSE, iqr = FALSE)

library(ggpubr)
ggviolin(penguins,
          x = "species",
         fill = "species",  # Füllfarbe nach `species`
          y = "body_mass_g") +
  scale_fill_okabeito()  #  Farbpalette nach Okabe & Ito

options(mc.cores = parallel::detectCores())  # Turbo einschalten

m10.6 <- stan_glm(body_mass_g ~ species, 
                  data = penguins, 
                  refresh = 0,  # unterdrückt Ausgabe der Posteriori-Stichproben
                  seed = 42  # zur Reproduzierbarkeit
                  )

m10.6 %>% parameters()

m10.6 %>% 
  parameters() %>% 
  display()

plot(hdi(m10.6)) + scale_fill_okabeito()

prior_summary(m10.6)

m10.6b <- stan_glm(
  body_mass_g ~ species, 
  data = penguins, 
  refresh = 0,
  seed = 42,
  prior = normal(location = c(0, 0),  # betas, Mittelwert
                 scale = c(500, 500)),  # betas, Streuung
  prior_intercept = normal(3000, 500),  # Achsenabschnitt, Mittelwert und Streuung
  prior_aux = exponential(0.001)
)
coef(m10.6b)

m10.6c <- stan_glm(
  body_mass_g ~ species, 
  data = penguins, 
  refresh = 0,
  seed = 42,
  prior = normal(location = c(0, 0),  # betas, Mittelwert
                 scale = c(2.5, 2.5),  # betas, Streuung
                 autoscale = TRUE),  # in z-Einheiten
  prior_intercept = normal(4200, 2.5,   # Achsenabschnitt, Mittelwert und Streuung
                           autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE)
)
coef(m10.6c)

sigma(m10.6b)

penguins <- penguins %>% 
  mutate(species = factor(species))

levels(penguins$species)

library(forcats)
penguins <- penguins %>% 
  mutate(species = factor(species),
    species = fct_relevel(species, "Gentoo"))

levels(penguins$species)

m10.6a <- stan_glm(body_mass_g ~ species, data = penguins, refresh = 0)
hdi(m10.6a)

m10.6a <- stan_glm(body_mass_g ~ species, data = penguins, refresh = 0)
hdi(m10.6a) |> display()

## kidiq %>%
##   correlation()

kidiq %>% 
  correlation() %>% 
  display()

## kidiq %>%
##   correlation() %>%
##   summary()

kidiq %>% 
  correlation() %>% 
  summary() |> 
  display()

kidiq %>% 
  correlation() %>% 
  summary() %>% 
  plot()

m10.7 <- stan_glm(kid_score ~ mom_iq, data = kidiq, refresh = 0)
m10.8 <- stan_glm(kid_score ~ mom_age, data = kidiq, refresh = 0)

coef(m10.7)

coef(m10.8)

p1 <- 
  kidiq %>% 
  ggplot(aes(x = mom_iq, y = kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(m10.7)[1],
              slope = coef(m10.7)[2],
              color = "blue") 

p2 <- 
kidiq %>% 
  ggplot(aes(x = mom_age, y = kid_score)) +
  geom_point() +
  geom_abline(intercept = coef(m10.8)[1],
              slope = coef(m10.8)[2],
              color = "blue")

plots(p1, p2,
      title = c("m10.7: Die univariate Regression mit dem Alter der Mutter als Prädiktor",
        "m10.8: Die univariate Regression mit dem IQ der Mutter als Prädiktor"))

m10.9 <- stan_glm(kid_score ~ mom_iq + mom_age, 
                  data = kidiq, 
                  refresh = 0)
coef(m10.9)

coef(m10.9)

lm1_coef <- coef(m10.9)
x1_seq <- seq(min(kidiq$mom_iq), max(kidiq$mom_iq), length.out = 25)
x2_seq <- seq(min(kidiq$mom_age), max(kidiq$mom_age), length.out = 25)

z <- t(outer(x1_seq, x2_seq, 
              function(x,y) lm1_coef[1]+lm1_coef[2]*x+lm1_coef[3]*y))

if (knitr:::is_html_output()) {
plot_ly(width = 800, height = 500,
  x=~x1_seq, y=~x2_seq, z=~z,type="surface") %>%
  add_trace(data=kidiq, 
            x=~mom_iq, y=~mom_age, z=~kid_score, 
            mode="markers", 
            type="scatter3d",
            marker = list(color="#00998a", 
                          opacity=0.7, 
                          size = 1,
                          symbol=105)) %>% 
  layout(scene = list(
    aspectmode = "manual", 
    aspectratio = list(x=1, y=1, z=1),
    xaxis = list(title = "mom_iq"),
    yaxis = list(title = "mom_age"),
    zaxis = list(title = "kid_score")))
}

if (knitr:::is_latex_output()) {
  knitr::include_graphics("img/m109-plotly.jpg")
}

kidiq <-
  kidiq %>%
  mutate(pred_m10.9 = predict(m10.9))

grid1 <- expand_grid(mom_iq = x1_seq, mom_age =  x2_seq) %>% 
  mutate(pred_m10.9 = predict(m10.9, newdata = data.frame(mom_iq,
                                                              mom_age)))

ggplot(aes(x = mom_iq, y = mom_age), data = grid1) +
  geom_raster(aes(fill = pred_m10.9)) +
 # geom_point(aes(color = kid_score_pred)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_point(data = kidiq,  alpha = .3, size = .7)

ggplot(data.frame(x=NA, y=NA)) +
  theme(panel.background = element_rect(fill = 'lightblue'))

kidiq2 <- 
  kidiq %>% 
  mutate(mom_iq_z = ((mom_iq - mean(mom_iq)) / sd(mom_iq)))  %>% 
  select(mom_iq, mom_iq_z) %>% 
  head()

kidiq_z <- 
  standardize(kidiq, append = TRUE)  # z-transformiert alle numerischen Werte

standardize(kidiq, append = TRUE) |> 
  head() |> 
  display()

## kidiq %>%
##   standardize(select = c("mom_iq", "mom_age", "kid_score"))

## kidiq %>%
##   mutate(mom_iq_z2 = scale(mom_iq),
##          mom_age_z2 = scale(mom_age),
##          kid_score_z2 = scale(kid_score))

kidiq %>% 
  mutate(mom_iq_z2 = scale(mom_iq),
         mom_age_z2 = scale(mom_age),
         kid_score_z2 = scale(kid_score)) %>% 
  head()

m10.10 <- stan_glm(kid_score_z ~ mom_iq_z + mom_age_z, 
                   data = kidiq_z, 
                   refresh = 0)
coef(m10.10)

## parameters(m10.10)

parameters(m10.10) %>% display()

plot(eti(m10.10)) + scale_fill_okabeito()

r2(m10.10)

interpret_r2(0.2)  # aus `easystats`

prior_summary(m10.10)  # aus rstanarm

m10.11 <- 
  stan_glm(kid_score_z ~ mom_iq_z , data = kidiq_z, refresh = 0)
coef(m10.11)

kidiq_z %>% 
  select(kid_score, mom_iq, kid_score_z, mom_iq_z) %>% 
  correlation() |> 
  display()

kidiq <-
  kidiq %>% 
  mutate(m10.10_pred = predict(m10.10),  # vorhergesagten Werte
         m10.10_resid = resid(m10.10))  # Residuen

kidiq %>% 
  ggplot(aes(x = m10.10_pred, y = m10.10_resid)) +
  geom_hline(color="white", yintercept = 0, size = 2) +
  geom_hline(color = "grey40", 
             yintercept = c(-1,1), 
             size = 1, 
             linetype = "dashed") +
  geom_point(alpha = .7) +
  geom_smooth()

pp_check(m10.10)

kidiq3 <- 
  kidiq %>% 
  standardize(append = TRUE) %>% 
  sample_n(size = 300)

#| results: "hide"
m10.10a <- stan_glm(mom_age_z ~ mom_iq_z, data = kidiq3, refresh = 0, chains = 1)
m10.10b <- stan_glm(mom_iq_z ~ mom_age_z, data = kidiq3, refresh = 0, chains = 1)

kidiq3 <-
  kidiq3 %>% 
  mutate(mom_age_resid = resid(m10.10a)) %>% 
  mutate(mom_iq_resid = resid(m10.10b))


m10.10c <- stan_glm(kid_score_z ~ mom_age_resid, data = kidiq3, refresh = 0, chains = 1)
m10.10d <- stan_glm(kid_score_z ~ mom_iq_resid, data = kidiq3, refresh = 0, chains = 1)


kidiq3 <-
  kidiq3 %>% 
  mutate(m10.10c_resid = resid(m10.10c)) %>% 
  mutate(m10.10d_resid = resid(m10.10d))

m10.10a_plot <-
  kidiq3 %>% 
  ggplot() +
  aes(x = mom_iq_z, y = mom_age_z) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m10.10a)[2],
              intercept = coef(m10.10a)[1],
              color = "blue") +
  geom_segment(aes(y = predict(m10.10a),
                   x = mom_iq_z,
                   xend = mom_iq_z,
                   yend = mom_age_z),
               color = "skyblue3",
               alpha = .1) +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2))


m10.10b_plot <-
  kidiq3 %>% 
  ggplot() +
  aes(y = mom_iq_z, x = mom_age_z) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m10.10b)[2],
              intercept = coef(m10.10b)[1],
              color = "blue") +
  geom_segment(aes(y = predict(m10.10b),
                   x = mom_age_z,
                   xend = mom_age_z,
                   yend = mom_iq_z),
               color = "skyblue3",
               alpha = .1)  +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2))


m10.10c_plot <-
  kidiq3 %>%
  ggplot() +
  aes(x = mom_age_resid, y = kid_score_z) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m10.10c)[2],
              intercept = coef(m10.10c)[1],
              color = "blue")   +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2))



m10.10d_plot <-
  kidiq3 %>% 
  ggplot() +
  aes(x = mom_iq_resid, y = kid_score_z) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m10.10d)[2],
              intercept = coef(m10.10d)[1],
              color = "blue")  +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(-2, 2))


plots(m10.10a_plot, m10.10b_plot, m10.10c_plot, m10.10d_plot, 
      n_rows = 2, tags = "A",
      caption = "Die vertikalen Balken zeigen die Residuen.",
      guides = "collect")

mtcars2 <-
  mtcars %>% 
  standardize()

m12a <- stan_glm(disp ~ wt, data = mtcars2, refresh = 0, chains = 1)
m12b <- stan_glm(wt ~ disp, data = mtcars2, refresh = 0, chains = 1)

mtcars2 <-
  mtcars %>% 
  mutate(m12a_resid = resid(m12a)) %>% 
  mutate(m12b_resid = resid(m12b))


m12c <- stan_glm(mpg ~ m12a_resid, data = mtcars2, refresh = 0, chains = 1)
m12d <- stan_glm(mpg ~ m12b_resid, data = mtcars2, refresh = 0, chains = 1)


mtcars2 <-
  mtcars2 %>% 
  mutate(m12c_resid = resid(m12c)) %>% 
  mutate(m12d_resid = resid(m12d))

m12a_plot <-
  mtcars2 %>% 
  ggplot() +
  aes(x = wt, y = disp) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m12a)[2],
              intercept = coef(m12a)[1],
              color = "blue") +
  geom_segment(aes(y = predict(m12a),
                   x = wt,
                   xend = wt,
                   yend = disp),
               color = "skyblue3")



m12b_plot <- 
mtcars2 %>% 
  ggplot() +
  aes(y = wt, x = disp) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m12b)[2],
              intercept = coef(m12b)[1],
              color = "blue") +
  geom_segment(aes(y = predict(m12b),
                   x = disp,
                   xend = disp,
                   yend = wt),
               color = "skyblue3")

m12c_plot <-
  mtcars2 %>%
  ggplot() +
  aes(x = m12a_resid, y = mpg) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m12c)[2],
              intercept = coef(m12c)[1],
              color = "blue")  +
  scale_x_continuous(limits = c(-1,1))



m12d_plot <-
  mtcars2 %>% 
  ggplot() +
  aes(x = m12b_resid, y = mpg) +
  geom_point(alpha = .5) +
  geom_abline(slope = coef(m12d)[2],
              intercept = coef(m12d)[1],
              color = "blue")   +
  scale_x_continuous(limits = c(-1,1))


## plots(m12a_plot, m12b_plot, m12c_plot, m12d_plot, n_rows = 2, tags = "A",
##       guides = "collect")

stan_glm(mpg ~ hp + cyl, data = mtcars, refresh = 0) %>% coef()

lm(mpg ~ hp + cyl, data = mtcars) %>% coef()

data(mtcars)
mtcars2 <-
  mtcars %>% 
  standardize(append = TRUE)

m13 <-
  stan_glm(am ~ mpg_z, 
           data = mtcars2, 
           refresh = 0)
coef(m13)

mtcars2 %>% 
  ggplot(aes(x = mpg_z, y = am)) +
  geom_hline(yintercept = 0.5, color = "white", size = 2) +
  geom_point() +
  geom_abline(intercept = coef(m13)[1],
              slope = coef(m13)[2],
              color = "blue") 

neg_am <- predict(m13, newdata = tibble(mpg_z = -1.3))

if (knitr:::is_html_output()) {
  knitr::include_graphics("https://media.giphy.com/media/XIqCQx02E1U9W/giphy.gif")
}
