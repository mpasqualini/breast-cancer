library(tidyverse)
library(janitor)
library(GGally)
library(caret)
library(printr)
library(corrplot)
library(factoextra)
library(pROC)

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

data_processed <- data_processed %>% mutate(diagnosis = factor(ifelse(diagnosis == "B", "Benign", "Malignant")))

# Summary 
summary(data_processed)

data_processed %>% 
 count(diagnosis) %>% mutate(perc = (n/sum(n))*100)

var_type <- c("mean", "se", "worst")
            
pairs <- pmap(list(var_type), generate_pairs, data_processed)

cor_matrix <- cor(data_processed[,-1])
corrplot(cor_matrix, method = "square", type = "upper")

# PCA
upper <- cor_matrix
upper[upper.tri(cor_matrix)] <- NA 

eig <- eigen(x = cor_matrix)


plot(eig$values)
abline(h = 1)

eig$vectors[,1:6]

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

log_reg <- glm(diagnosis ~ ., family = binomial, data = train)
p <- predict(logit, test, type = "response") %>% as.data.frame() %>% rename(., "prob" = ".")

class <- cbind(p, test)

class <- class %>% mutate(class = as.factor(ifelse(prob > 0.5, "Malignant", "Benign")))

table(class$diagnosis, class$class)

