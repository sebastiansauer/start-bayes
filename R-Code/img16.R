# "A", "¬A"

plot16a <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#E69F00", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#E69F00") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .5, fill = "#0072B2") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B"))


plot16a1 <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#E69F00", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#E69F00") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .5, y = .75, label = "Pr(B) = 50%")

plot16a2 <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .5, fill = "#0072B2") +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate("label", x = .75, y = .5, label = "Pr(A) = 50%")


plot16b <-
ggplot(data.frame(A = c(0, 1),
                  B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#E69F00", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#E69F00") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .5, fill = "#0072B2") +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "green", alpha = .7, fill = NA) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "A,B")





plot16c <-
  ggplot(data.frame(A = c(0, 1),
                    B = c(0, 1))) +
  aes(x = A, y = B) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = .5,
           color = "#E69F00", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0.5, ymax = 1,
           color = NA, alpha = .5, fill = "#E69F00") +
  annotate("rect", xmin = 0, xmax =0.5, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .7, fill = NA) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 1,
           color = "#0072B2", alpha = .5, fill = "#0072B2") +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1,
           color = "green", alpha = .7, fill = NA) +
  scale_x_continuous(breaks = c(0.25, 0.75), labels = c("¬A", "A")) +
  scale_y_continuous(breaks = c(0.25, 0.75), labels = c("¬B", "B")) +
  annotate(geom = "label", x = .75, y = 0.75, label = "A,B")


plot16 <- plot16a1 + plot16a2 + plot16c

print(plot16)
