##################################################
## Project: Soccer Analysis
## Script purpose: Evaluate models for prediction
## Date: 2018-11-11
## Author: Marcus Sjölin
##################################################


################## SOCCER PREDICTION #######################

# Read in necessary libraries
library(readr)    # Import data 
library(plyr)     # Data cleaning
library(dplyr)    # Data cleaning
library(lubridate) # Date handling
library(caret)    # Machine learning
library(randomForest)

library(e1071) # NaiveBayes

library(rpart) # Decision trees
library(rpart.plot)

##### Read in data  

data <-
  read.csv("/home/marcus/R/soccer_analysis/data/teams.csv", stringsAsFactors = FALSE) %>%
  filter(Season<2019 & !is.na(NrWin_H_A) & !is.na(NrWinL5)) %>% 
  select(Result, Team, H_A, WinPerc, DrawPerc, LostPerc, 
         L5WinPerc, L5DrawPerc, L5LostPerc, 
         H_A_WinPerc, H_A_DrawPerc, H_A_LostPerc) %>%
  mutate_if(is.character, as.factor)

# Only keep rows without NA
# data <- data[complete.cases(data), ]

# Make train and test data set
inTrain <- createDataPartition(data$Result, p=0.75, list=FALSE)  
train<-data[inTrain,]
test<-data[-inTrain,]

# Check data
summary(train)

# Naive Bayes
nB.model <-naiveBayes(Result ~ ., 
                      data = train)

test$nB.prediction<-predict(nB.model, test)

confusionMatrix(test$nB.prediction, test$Res)

save(nB.model, file = "/home/marcus/R/soccer_analysis/models/nb_soccer.rda")

# RandomForest with caret

fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 5,
  classProbs=TRUE)

rf.model <-
  train(Result ~ .,
        data=train,
        method="ranger", 
        trControl=fit_control)


test$rf.prediction<-predict(rf.model, test)
test$rf.prediction_prob<-predict(rf.model, test, type="prob")

confusionMatrix(test$rf.prediction, test$Res)

save(rf.model, file = "/home/marcus/R/soccer_analysis/models/rf_soccer.rda")

# Include weights with rf
model_weights <- ifelse(as.character(train$Result) == "Draw", (1/table(train$Result)[1]) * (1/3),
                        ifelse(as.character(train$Result) == "Lost", (1/table(train$Result)[2]) * (1/3), (1/table(train$Result)[3]) * (1/3)))

fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 3,
  classProbs=TRUE)

rf.weightmodel <-
  train(Result ~ .,
        data=train,
        method="ranger", 
        trControl=fit_control,
        weights=model_weights)


test$rf_w.prediction<-predict(rf.weightmodel, test)
test$rf_w.prediction_prob<-predict(rf.weightmodel, test, type="prob")

confusionMatrix(test$rf_w.prediction, test$Res)

save(rf.weightmodel, file = "/home/marcus/R/soccer_analysis/models/rfweight_soccer.rda")


# Decision Tree
dT <- rpart(Res~H_A + 
              WinPerc+DrawPerc+LostPerc+
              L5WinPerc+L5DrawPerc+L5LostPerc+
              H_A_WinPerc + H_A_DrawPerc + H_A_LostPerc, data = train, method = 'class', minsplit=1, cp=0.001)

rpart.plot(dT, extra=101)




  