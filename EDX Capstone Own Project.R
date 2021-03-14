# Loading the required libraries
library(tidyverse)
library(caret)
library(dplyr)
library(stringr)
library(ggplot2)

#########################
# Downloading of dataset
#########################
colnames <-    c('age', # Age of the patient 
                 'sex', # Sex of the patient 
                 'tb', # Total Bilirubin
                 'db', # Direct Bilirubin 
                 'alkphos', # Alkaline Phosphotase
                 'sgpt', # Alamine Aminotransferase
                 'sgot', # Aspartate Aminotransferase
                 'tp', # Total Protiens
                 'alb', # Albumin
                 'ag', # Ratio	Albumin and Globulin Ratio 
                 'outcome') # Selector field used to split the data into two sets

data <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
                   sep=',',
                   header=FALSE,
                   col.names=colnames)

# Formatting of dataset
data <- subset(data, complete.cases(data))
data <- data %>% 
  mutate(outcome = as.character(outcome)) %>% 
  mutate(outcome = replace(outcome, outcome == '1', 'Disease')) %>%
  mutate(outcome = replace(outcome, outcome == '2', 'Normal')) %>%
  mutate(outcome = as.factor(outcome))

# Quick overview of dataset
head(data)

# Splitting of dataset into train and test sets
set.seed(1)
train_index <- createDataPartition(data$outcome, p=.7, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]

# Exploration of Age variable
train %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 10) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of Sex variable
train %>% 
  ggplot(aes(x = sex)) + 
  geom_bar() + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of tb variable
train %>% 
  ggplot(aes(x = tb)) + 
  geom_histogram(binwidth = 3) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of db variable
train %>% 
  ggplot(aes(x = db)) + 
  geom_histogram(binwidth = 1) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of alkphos variable
train %>% 
  ggplot(aes(x = alkphos)) + 
  geom_histogram(binwidth = 100) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of sgpt variable
train %>% 
  ggplot(aes(x = sgpt)) + 
  geom_histogram(binwidth = 100) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of sgot variable
train %>% 
  ggplot(aes(x = sgot)) + 
  geom_histogram(binwidth = 100) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of tp variable
train %>% 
  ggplot(aes(x = tp)) + 
  geom_histogram(binwidth = 1) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of alb variable
train %>% 
  ggplot(aes(x = alb)) + 
  geom_histogram(binwidth = 0.5) + 
  theme_bw() +
  facet_grid(~ outcome)

# Exploration of ag variable
train %>% 
  ggplot(aes(x = ag)) + 
  geom_histogram(binwidth = 0.3) + 
  theme_bw() +
  facet_grid(~ outcome)

# Checking predictors correlation and final formatting of data
cor(subset(train, select = -c(sex, outcome)))

train <- train %>% subset(select = -c(db, sgpt, alb))
test <- test %>% subset(select = -c(db, sgpt, alb))

# Code to compile the results
results <- data.frame(Model = character(), 
                      Accuracy = double(), 
                      stringsAsFactors = FALSE)

############
# KNN Model
############

# Optimal tuning parameter is first determined before developing the model further
# Model is trained and tested to determine the accuracy
knn = train(outcome ~ ., data = train, method = "knn", preProcess=c('knnImpute'))
knn

pred = predict(knn, newdata = test)
confusionMatrix <- confusionMatrix(pred, test$outcome, prevalence = 0.06)
results[nrow(results) + 1, ] <- c(as.character('K-nearest neighbours (knn)'), 
                                  confusionMatrix$overall['Accuracy'])
confusionMatrix

results %>% knitr::kable()

##########################
# Linear Classifier Model
##########################

# Model is trained and tested to determine the accuracy
lc = train(outcome ~ ., data = train, method = "glmboost")
pred = predict(lc, newdata = test)
confusionMatrix <- confusionMatrix(pred, test$outcome)
results[nrow(results) + 1, ] <- c(as.character('Linear Classifier (glmboost)'), 
                                  confusionMatrix$overall['Accuracy'])
confusionMatrix

results %>% knitr::kable()

##########################
# Linear Regression Model
##########################

# Model is trained and tested to determine the accuracy
lr = train(outcome ~ ., data = train, method = "bayesglm")
pred = predict(lr, newdata = test)
confusionMatrix <- confusionMatrix(pred, test$outcome)
results[nrow(results) + 1, ] <- c(as.character('Logistic Regression (bayesglm)'), 
                                  confusionMatrix$overall['Accuracy'])
confusionMatrix

results %>% knitr::kable()

#######################
# Results Compilation
#######################
results %>% arrange(Accuracy) %>% knitr::kable()
