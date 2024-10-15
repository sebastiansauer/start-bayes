library(tidyverse)
library(easystats)
library(rstanarm)

library(dagitty)

theme_set(theme_modern())

m1 <- stan_glm(mpg ~ hp, data = mtcars, refresh = 0)

post_verteilung <- m1 %>% 
  as_tibble()
head(post_verteilung)

ppv <- posterior_predict(m1) %>% as_tibble()

ppv %>% 
  pivot_longer(everything()) %>% 
  slice_head(n=5)

funcShaded_low <- function(x, lower_bound) {
    y = dnorm(x, mean = 0, sd = 1)
    y[x < lower_bound] <- NA
    return(y)
}
# source: https://stackoverflow.com/questions/48753007/using-stat-function-to-draw-partially-shaded-normal-curve-in-ggplot2

funcShaded_up <- function(x, upper_bound) {
    y = dnorm(x, mean = 0, sd = 1)
    y[x > upper_bound] <- NA
    return(y)
}

ggplot(tibble(x = -3:3)) +
  aes(x) + 
  stat_function(fun = dnorm, n = 100) +
  stat_function(fun = funcShaded_up, args = list(upper_bound = 0),
                geom = "area", fill = "grey", alpha = .5) +
  theme(axis.text.x = element_text(color = "red", size = 20)) +
  scale_x_continuous(breaks = 0)

ggplot(tibble(x = -3:3)) +
  aes(x) + 
  stat_function(fun = dnorm, n = 100) +
  stat_function(fun = funcShaded_up, args = list(upper_bound = 0),
                geom = "area", fill = "red", alpha = .5) +
  scale_x_continuous(breaks = 0)

m2 <- stan_glm(mpg ~ hp*vs, data = mtcars)  # mit Interaktionseffekt

## parameters(m2)

parameters(m2) %>% display()

plot(parameters(m2))

m3 <- stan_glm(mpg ~ hp, data = mtcars)

plot(hdi(m3)) + scale_fill_okabeito()

dag2_raw <-
  'dag {
A [pos="-2.200,-1.520"]
B [pos="1.400,-1.460"]
D [outcome,pos="1.400,1.621"]
E [exposure,pos="-2.200,1.597"]
Z [pos="-0.300,-0.082"]
A -> E
A -> Z
B -> D
B -> Z
E -> D
Z -> D
Z -> E
}
'

dag2_raw %>% dagitty() %>% plot()

dag3_raw <-
  'dag {
1 [pos="-5.600,2.115"]
10 [pos="4.955,-4.447"]
11 [pos="7.152,-1.901"]
12 [pos="7.424,1.256"]
13 [pos="5.971,3.498"]
14 [pos="4.225,5.936"]
15 [pos="1.634,7.468"]
2 [pos="-7.120,-0.603"]
3 [pos="-7.138,-3.731"]
4 [pos="-5.395,-6.348"]
5 [pos="-2.468,-6.074"]
6 [pos="-2.179,-2.682"]
7 [pos="-1.049,-0.238"]
8 [pos="1.043,0.916"]
9 [pos="2.654,-2.614"]
D [outcome,pos="-1.510,6.789"]
E [exposure,pos="-3.728,5.092"]
1 -> E
10 -> 9
11 -> 10
12 -> 11
13 -> 12
14 -> 13
15 -> 14
15 -> D
2 -> 1
3 -> 2
4 -> 3
5 -> 4
6 -> 5
7 -> 6
7 -> E
8 -> 7
8 -> D
9 -> 8
E -> D
}
'

dag3_raw %>% dagitty() %>% plot()

dad_van_kampen_raw <-
  'dag {
AFF [pos="0.262,0.477"]
AIS [pos="0.123,0.736"]
ALN [pos="0.438,0.506"]
APA [pos="0.376,0.147"]
CDR [pos="0.628,0.332"]
DET [pos="0.920,0.561"]
EGC [outcome,pos="0.916,1.016"]
FTW [pos="0.667,0.639"]
HOS [pos="0.886,0.727"]
PER [pos="0.920,0.382"]
SAN [pos="0.031,0.371"]
SUS [exposure,pos="0.295,1.003"]
AFF -> ALN
AFF -> APA
AFF -> CDR
AIS -> AFF
AIS -> EGC
AIS -> SUS
ALN -> APA
ALN -> DET
ALN -> FTW
ALN -> PER
ALN -> SUS
CDR -> DET
EGC -> HOS
FTW -> DET
FTW -> EGC
PER -> DET
SAN -> AFF
SAN -> AIS
SAN -> ALN
SAN -> APA
SAN -> CDR
SUS -> EGC
SUS -> FTW
SUS -> HOS
}
'

dad_van_kampen_raw %>% dagitty() %>% plot()
