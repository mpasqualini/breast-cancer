library(tidyverse)
library(janitor)
library(GGally)
library(caret)
library(printr)
library(corrplot)
library(factoextra)
library(pROC)
library(yardstick)

source("src/functions.r")

set.seed(889)

# Looking at data ----

data_raw <- read.csv("data/data.csv")

#str(data_raw)

#head(data_raw)

# Pre-processing data -----

# Removing the ID column and the X column
data_processed <- data_raw %>% select(-c(id, X))

#str(data_processed)

# Changing diagnosis to factor 

data_processed <- data_processed %>% mutate(diagnosis = factor(ifelse(diagnosis == "B", "Benign", "Malignant")))

# Summary 
#summary(data_processed)

data_processed %>% 
 count(diagnosis) %>% mutate(perc = (n/sum(n))*100)

var_type <- c("mean", "se", "worst")
            
pairs <- pmap(list(var_type), generate_pairs, data_processed)

cor_matrix <- cor(data_processed[,-1])
#corrplot(cor_matrix, method = "square", type = "upper")

# PCA
upper <- cor_matrix
upper[upper.tri(cor_matrix)] <- NA 

eig <- eigen(x = cor_matrix)

pca <- princomp(data_processed[,-1], cor = TRUE, scores = TRUE)

fviz_pca_biplot(pca, col.ind = data_processed$diagnosis, col="black",
                palette = "Dark2", geom = "point", repel=TRUE,
                legend.title="Diagnóstico", addEllipses = TRUE) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Modelling ----

data_scores <- 
  bind_cols(data_processed$diagnosis, as.data.frame(pca$scores[,1:6])) %>% 
  clean_names() %>% 
  rename(diagnosis = x1)

train <- data_scores %>% sample_frac(.7)
test <- data_scores %>% anti_join(train)

logistic_model1 <- glm(diagnosis ~ ., family = binomial, data = train)
summary(logistic_model1)

logistic_model2 <- glm(diagnosis ~ comp_1 + comp_2 + comp_3 + comp_4 + comp_5, 
                       family = binomial, data = train)

summary(logistic_model2)

pred <-
  predict(logistic_model2, test, type = "response") %>% 
  as.data.frame() %>% 
  rename(., "prob" = ".") %>% 
  bind_cols(test) %>% 
  mutate(predicted = as.factor(ifelse(prob > 0.5, "Malignant", "Benign")))


cmat <- conf_mat(table(pred$predicted, pred$diagnosis))
autoplot(cmat, type = "heatmap")

mean(pred$diagnosis != pred$predicted)
