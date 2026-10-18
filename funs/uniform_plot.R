uniform_plot <- function(a, b){
  xvals <- data.frame(x = c(a, b)) #Range for x-values
  
  ggplot(data.frame(x = xvals), 
         aes(x = x)) + xlim(c(a, b)) + ylim(0, 1/(b - a)) +
    stat_function(fun = dunif, args = list(min = a, max = b),
                  geom = "area",
                  fill = "#999999", alpha = 0.35) +
    stat_function(fun = dunif, args = list(min = a, max = b), color = "#0072B2") +
    labs(x = "X", y = "Dichte")  +
    geom_vline(xintercept = a, linetype = "dashed", colour = "#4D4D4D") +
    geom_vline(xintercept = b, linetype = "dashed", colour = "#4D4D4D")
  
}
