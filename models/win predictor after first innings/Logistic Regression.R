library(tidyverse)
library(ROCR)

train <- read_csv('./models/win predictor after first innings/train.csv')
test <- read_csv('./models/win predictor after first innings/test.csv')

#Training model
logistic_model <- glm(winner ~ batting + bowling + dl + target + head_to_head + 
                        home_ground_advantage + batting_team_points + bowling_team_points +
                        chase_win_perc + chase_win_perc_bat + defense_win_perc_bowl +
                        first_inng_avg,
                      data = train,
                      family = "binomial")

#Summary
summary(logistic_model)

#Predicting results
predictions <- predict(logistic_model, test, type="response")
predictions <- ifelse(predictions >0.5, 1, 0)

#Confusion matrix
cm <- table(test$winner, predictions)

#Accuracy
accuracy <- (cm[1] + cm[4])/nrow(test)
accuracy

#Precision
precision <- (cm[1])/(cm[1] + cm[2])
precision
