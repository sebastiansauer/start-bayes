q_80 <- quantile(samples$p_grid, prob = .8)
q_10_and_90 <- quantile(samples$p_grid, prob = c(.1, .9))

p1 <-
  d_k100 %>% 
  ggplot(aes(x = p_grid, y = post)) +
  geom_line() +
  labs(title="80% Wahrscheinlichkeitsmasse (p80)",
       x = "Parameterwerte",
       y = "Wahrscheinlichkeitsdichte") +
  geom_area(data = d_k100 %>% 
              filter(p_grid < q_80),
            fill = "blue") +
  annotate("label", x = .65, y = .01, label = "p80", color = "blue")

p1


p2 <-
  d_k100 %>% 
  ggplot(aes(x = p_grid, y = post)) +
  geom_line(color = "grey20") +
  labs(title="80%-Quantil (q80)",
       caption = paste0("q80: ", round(q_80, 2)),
       x = "Parameterwerte",
       y = "Wahrscheinlichkeitsdichte") +
  geom_area(data = d_k100 %>% 
              filter(p_grid < q_80),
            fill = "grey80",
            color = "grey20") +
  geom_vline(xintercept = q_80, color = "blue") +
  annotate("label", x = q_80, y = .02, label = "q", color = "blue") +
  scale_x_continuous(breaks = .78) +
  theme(axis.text.x = element_text(colour = "blue"))

p2


p1 + p2
