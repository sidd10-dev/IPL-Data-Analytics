library(tidyverse)
library(caTools)

df <- read_csv('./Datasets for model building/first_innings.csv')

split <- sample.split(df$match_id, SplitRatio = 0.8)

train <- df[split,]
test <- df[!split,]

train %>% 
  write.csv(., file='./models/win predictor after first innings/train.csv', row.names = F)

test %>% 
  write.csv(., file='./models/win predictor after first innings/test.csv', row.names = F)
