uniform_Plot <- function(a, b){
  xvals <- data.frame(x = c(a, b)) #Range for x-values
  
  ggplot(data.frame(x = xvals), 
         aes(x = x)) + xlim(c(a, b)) + ylim(0, 1/(b - a)) +
    stat_function(fun = dunif, args = list(min = a, max = b), 
                  geom = "area", 
                  fill = mittelgrau, alpha = 0.75) + 
    stat_function(fun = dunif, args = list(min = a, max = b)) +
    labs(x = "Wert (x)", y = "Wahrscheinlichkeitsdichte (d)")  +
    referenzlinie(xintercept = c(a, b))
  
}
