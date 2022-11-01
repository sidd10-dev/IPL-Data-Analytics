library(tidyverse)
library(randomForest)

train <- read_csv('./models/win predictor after first innings/train.csv')
test <- read_csv('./models/win predictor after first innings/test.csv')

train <- 
  train %>% 
  mutate(winner = as.factor(winner)) %>% 
  select(-c("match_id"))

test <- 
  test %>% 
  mutate(winner = as.factor(winner)) %>% 
  select(-c("match_id"))

#Training
random_forest_model = randomForest(winner ~ .,
                                   data = train,
                                   na.action = na.exclude,
                                   n_tree = 1000)

#Summary
random_forest_model

#Predicting
predictions <- predict(random_forest_model, test)

#Confusion matrix
cm <- table(test$winner, predictions)
cm

#Accuracy
accuracy <- (cm[1] + cm[4])/nrow(test)
accuracy

#Precision
precision <- (cm[1])/(cm[1] + cm[2])
precision