
plot16a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = green, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = green) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .5, fill = himmelblau) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  achsentext_gross() +
  labs(x = "", y = "")


plot_pr_b <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = dunkelgrau, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = green) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .5, y = .75, label = "Pr(B) = 50%") +
  achsentext_gross() +
  labs(x = "", y = "")

plot_pr_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = dunkelgrau, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .5, fill = himmelblau) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .75, y = .5, label = "Pr(A) = 50%") +
  achsentext_gross() +
  labs(x = "", y = "")


plot_pr_ab <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = green, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = green) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .5, fill = himmelblau) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = orange, alpha = .7, fill = NA, linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "A,B", color = textfarbe(orange)) +
  achsentext_gross() +
  labs(x = "", y = "")






plot_pr_b_geg_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = green, alpha = .3, fill = NA, linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = NA) +
  # annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
  #          color = himmelblau, alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = himmelblau, alpha = .5, fill = himmelblau, linewidth = 2) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = orange, alpha = .7, fill = orange, linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "B|A", color = textfarbe(orange))  +
  achsentext_gross() +
  labs(x = "", y = "")

