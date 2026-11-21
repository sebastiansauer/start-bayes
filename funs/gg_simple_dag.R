# source: https://bookdown.org/content/4857/the-haunted-dag-the-causal-terror.html#confronting-confounding


gg_simple_dag <- function(d) {
  
  d %>% 
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(color = blue, alpha = 1/2, size = 6.5) +
    geom_dag_text(color = fast_schwarz, size = 4.5) +
    geom_dag_edges() + 
    theme_dag()
}
