# source: https://ourworldindata.org/covid-vaccinations
# access date: 2021-09-24
# licence: https://ourworldindata.org/covid-vaccinations#licence

dfile <- "data/owid-covid-data.csv"


corona_d <- read_csv(dfile)

corona_d_summary <-
  corona_d |>
  filter(iso_code %in% c("DEU", "USA")) %>%
  mutate(date = as_date(date)) %>%
  rename(Land = iso_code) %>%
  summarise(
    people_fully_vaccinated_per_hundred = mean(
      people_fully_vaccinated_per_hundred,
      na.rm = TRUE
    ),
    total_deaths_per_million = mean(total_deaths_per_million, na.rm = TRUE),
    total_vaccinations = mean(total_vaccinations, na.rm = TRUE)
  )

corona_d2 <-
  corona_d %>%
  filter(iso_code %in% c("DEU", "USA")) %>%
  mutate(date = as_date(date)) %>%
  rename(Land = iso_code) %>%
  select(
    date,
    Land,
    #total_deaths,
    #new_deaths,
    people_fully_vaccinated_per_hundred,
    total_deaths_per_million,
    #new_vaccinations,
    total_vaccinations
  ) %>%
  filter(date == "2021-09-23") %>%
  group_by(Land)


plot_covid1 <-
  corona_d2 %>%
  ggplot(aes(x = Land, y = people_fully_vaccinated_per_hundred)) +
  geom_col() +
  labs(y = "Anteil komplett geimpfter Personen", caption = "2021-09-23") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black") +
  annotate("label", x = Inf, y = 51.8, hjust = 1, label = "Pr(I) = 51.8%")


plot_covid2 <-
  corona_d2 %>%
  ggplot(aes(x = Land, y = total_deaths_per_million)) +
  geom_col() +
  labs(y = "Corona-Tote pro Million", caption = "Stichtag 2021-09-23") +
  geom_hline(yintercept = 1313, linetype = "dashed", color = "black") +
  annotate("label", x = Inf, y = 1313, hjust = 1, label = "Pr(T) = 0.001313")


plot_covid1
plot_covid2
