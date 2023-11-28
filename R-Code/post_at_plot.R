
# ei jedem Prädiktorwert eine Post-Verteilung für $\mu$

# PPV:
ppv_m43 <- 
  posterior_predict(m43, , newdata = nd) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), 
               names_to = "weight_condition",
               values_to = "h") %>% 
  mutate(weight =
           case_when(weight_condition == 1 ~ 40,
                     weight_condition == 2 ~ 45,
                     weight_condition == 3 ~ 50,
                     weight_condition == 4 ~ 55
           )) %>% 
  mutate(weight = as.factor(weight))



ppv_m43_summary <-
  ppv_m43 %>% 
  group_by(weight) %>% 
  summarise(m = mean(h),
            s = sd(h))


post_at_plot <-
  ppv_m43 %>% 
  ggplot() +
  aes(x = h) +
  geom_density(aes(fill = weight)) +
  facet_wrap(~ weight, nrow = 1, scales = "free") +
  scale_y_continuous(breaks = NULL) +
  labs(
    caption = "Horizontale Balken zeigen MW±2sd",
    x = "Größe",
    y = "Post-Wskt") +
  geom_point(data = ppv_m43_summary,
             aes(x = m,
                 y = 0),
             size = 2, color = "blue", alpha = .5) +
  geom_segment(data = ppv_m43_summary,
               aes(x = m-2*s,
                   xend = m+2*s),
               y = 0,
               yend = 0,
               color = "blue",
               alpha = .5,
               linewidth = 2) +
  scale_x_continuous(breaks = c(140, 160)) +
  scale_fill_okabeito()
