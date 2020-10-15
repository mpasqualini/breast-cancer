generate_pairs <- function(var, data) {
  data %>% 
  select(contains(var)) %>% 
  ggpairs(upper = list(continuous = wrap("cor", family="sans")))
}