uniform_Plot <- function(a, b){
  xvals <- data.frame(x = c(a, b)) #Range for x-values
  
  ggplot(data.frame(x = xvals), 
         aes(x = x)) + xlim(c(a, b)) + ylim(0, 1/(b - a)) +
    stat_function(fun = dunif, args = list(min = a, max = b), 
                  geom = "area", 
                  fill = "#999999FF", alpha = 0.75) + 
    stat_function(fun = dunif, args = list(min = a, max = b), color = "#009E73FF") +
    labs(x = "X", y = "Dichte")  +
    geom_vline(xintercept = a, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = b, linetype = "dashed", colour = "grey")
  
}
