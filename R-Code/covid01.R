# source: https://ourworldindata.org/covid-vaccinations
# access date: 2021-09-24
# licence: https://ourworldindata.org/covid-vaccinations#licence



dfile <- "data/owid-covid-data.csv"

library(lubridate)

d <- read_csv(dfile, show_col_types = FALSE)

d2<-
  d %>%
  filter(iso_code %in% c("DEU", "USA")) %>%
  mutate(date = as_date(date)) %>%
  rename(Land = iso_code) %>%
  select(date,
         Land,
         #total_deaths,
         #new_deaths,
         people_fully_vaccinated_per_hundred,
         total_deaths_per_million,
         #new_vaccinations,
         total_vaccinations) %>%
  filter(date == "2021-09-23") %>%
  group_by(Land)


# d2 %>%
#   ungroup() %>%
#   count(people_fully_vaccinated_per_hundred)

source("funs/binomial_plot.R")
d2 <- ungroup(d2)

plot_covid1 <-
  tufte_bars(d2, "Land", "people_fully_vaccinated_per_hundred",
             sprintf("%.1f", d2$people_fully_vaccinated_per_hundred),
             x_lab = "Land: Anteil vollständig Geimpfter (je 100 Einwohner)")


plot_covid2 <-
  tufte_bars(d2, "Land", "total_deaths_per_million",
             sprintf("%.0f", d2$total_deaths_per_million),
             x_lab = "Land: Corona-Tote (je Million Einwohner)")


