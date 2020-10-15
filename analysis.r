library(tidyverse)
library(GGally)

source("src/functions.r")

# Looking at data ----

data_raw <- read.csv("data/data.csv")

str(data_raw)

head(data_raw)

# Pre-processing data -----

# Removing the ID column and the X column
data_processed <- data_raw %>% select(-c(id, X))

str(data_processed)

# Changing diagnosis to factor 

data_processed <- data_processed %>% mutate(diagnosis = as.factor(diagnosis))

# Summary 
summary(data_processed)

data_processed %>% 
 count(diagnosis) %>% mutate(perc = (n/sum(n))*100)

pairs <- 
  data_processed %>% 
  select(contains("mean")) %>% 
  ggpairs(upper = list(continuous = wrap("cor", family="sans")))

var_type <- c("mean", "se", "worst")
            
pairs <- pmap(list(var_type), generate_pairs, data_processed)
