generate_pairs <- function(var, data) {
  data %>% 
  select(contains(var), diagnosis) %>% 
  ggpairs(aes(color = diagnosis, alpha = .5), upper = list(continuous = wrap("cor", family="sans", size = 2.5))) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          strip.text.x = element_text(size = 6),
          strip.text.y = element_text(size = 6))
}

generate_scatter_pairs <- function(var, data) {
  
  X = data %>% select(contains(var))
  
  featurePlot(x = X, y = data$diagnosis, plot = "pairs", auto.key = list(columns = 2), pch=".")  
}
