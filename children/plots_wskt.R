
plot16a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#009E73FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  theme_minimal()  +
  theme(axis.text = element_text(size = 18)) +
  labs(x = "", y = "")


plot_pr_b <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "grey", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .5, y = .75, label = "Pr(B) = 50%") +
  theme_minimal() +
  theme(axis.text = element_text(size = 18)) +
  labs(x = "", y = "")

plot_pr_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "grey", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .75, y = .5, label = "Pr(A) = 50%") +
  theme_minimal()  +
  theme(axis.text = element_text(size = 18)) +
  labs(x = "", y = "")


plot_pr_ab <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#009E73FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#009E73FF") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF") +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#E69F00FF", alpha = .7, fill = NA, linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "A,B", color = "#E69F00FF") +
  theme_minimal()  +
  theme(axis.text = element_text(size = 18)) +
  labs(x = "", y = "")






plot_pr_b_geg_a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#009E73FF", alpha = .3, fill = NA, linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "NA") +
  # annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
  #          color = "#56B4E9FF", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#56B4E9FF", alpha = .5, fill = "#56B4E9FF", linewidth = 2) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "#E69F00FF", alpha = .7, fill = "#E69F00FF", linewidth = 2) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "B|A", color = "#E69F00FF")  +
  theme(axis.text = element_text(size = 18)) +
  labs(x = "", y = "")

