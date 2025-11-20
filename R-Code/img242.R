d <-
  tibble(
    p_grid = seq(from = 0, to = 1, length.out = 21), # define grid
    prior = 1
  ) %>% # define prior
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>% # compute likelihood at each value in grid
  mutate(unstd_posterior = likelihood * prior) %>% # compute product of likelihood and prior
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) # standardize the posterior, so it sums to 1


p1 <-
  d %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(
    subtitle = "20 Parameterwerte",
    x = "pi",
    y = ""
  ) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(
    breaks = d$p_grid, # show tick marks at all grid points
    labels = function(x) ifelse(x %in% c(0, 0.5, 1), x, "") # label only 0, .5, 1
  ) +
  scale_y_continuous(breaks = NULL, labels = NULL)

p2 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 10), prior = 1) %>%
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10 Parameterwerte", x = "pi", y = "") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(
    breaks = d$p_grid, # show tick marks at all grid points
    labels = function(x) ifelse(x %in% c(0, 0.5, 1), x, "") # label only 0, .5, 1
  )  +
  scale_y_continuous(breaks = NULL, labels = NULL)
  


p3 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 5), prior = 1) %>%
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>%

  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "5 Parameterwerte", x = "pi", y = "") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(
    breaks = d$p_grid, # show tick marks at all grid points
    labels = function(x) ifelse(x %in% c(0, 0.5, 1), x, "") # label only 0, .5, 1
  )  +
  scale_y_continuous(breaks = NULL, labels = NULL)


p4 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 1e5), prior = 1) %>%
  mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%
  mutate(unstd_posterior = likelihood * prior) %>%
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>%

  ggplot(aes(x = p_grid, y = posterior)) +
  #  geom_point() +
  geom_line() +
  labs(subtitle = "Unendlich viele Parameterwerte", x = "pi", y = "") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(
    breaks = d$p_grid, # show tick marks at all grid points
    labels = function(x) ifelse(x %in% c(0, 0.5, 1), x, "") # label only 0, .5, 1
  )  +
  scale_y_continuous(breaks = NULL, labels = NULL)


plot242 <- p4 + p2 + p3 + p1 + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Posterior-Verteilung bei unterschiedlicher Gitterfeinheit",
    subtitle = "6 Erfolge bei 9 Versuchen, uniforme Prior-Verteilung"
  )
