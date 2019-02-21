##################################################
## Project: Soccer Analysis
## Script purpose: Create models for match result predictions
## Input: Match.csv 
## Output: One NaiveBayes Model and one randomforest-model
## Date: 2018-11-11
## Author: Marcus Sjölin
##################################################

# Read in necessary libraries
library(readr)    # Import data 
library(plyr)     # Data cleaning
library(dplyr)    # Data cleaning
library(lubridate) # Date handling

library(caret)    # Machine learning
library(e1071)    # NaiveBayes


# Read in data  

data <-
  read.csv("matches.csv", stringsAsFactors = TRUE) %>%
  filter(Season<2019 & !is.na(HomeStrength) & !is.na(AwayStrength)) %>% 
  select(Result, Win_H, L5Win_H, HomeStrength, 
         H2H, Division,
         Win_A, L5Win_A, AwayStrength)

summary(data)

# Split data into train and test 
inTrain <- createDataPartition(data$Result, p=0.75, list=FALSE)  
train<-data[inTrain,]
test<-data[-inTrain,]

# Include weights with rf
model_weights <- ifelse(as.character(train$Result) == "Draw", (1/table(train$Result)[1]) * (1/3),
                        ifelse(as.character(train$Result) == "Lost", (1/table(train$Result)[2]) * (1/3), (1/table(train$Result)[3]) * (1/3)))

# Use 10 fold cross validation; summaryfunction Logloss as LogLoss is used as metric. Want to penalize error in classification
fit_control <- trainControl( ## 10-fold CV
  method = "cv",
  number = 10,
  summaryFunction=mnLogLoss,
  classProbs=TRUE)

rf.model <-
  train(Result ~ .,
        data=train,
        method="rf", 
        metric="logLoss",
        trControl=fit_control,
        weights=model_weights)

varImp(rf.model, scale=TRUE)

test$rf_matches<-predict(rf.model, test)
test$rf_matches_prob<-predict(rf.model, test, type="prob")

confusionMatrix(test$rf_matches, test$Result)

save(rf.model, file = "rf_soccer.rda")

# Naive Bayes model
nB.model <-naiveBayes(Result ~ ., 
                      data = train)

test$nB.prediction<-predict(nB.model, test)
confusionMatrix(test$nB.prediction, test$Result)

save(nB.model, file = "nb_soccer.rda")
  