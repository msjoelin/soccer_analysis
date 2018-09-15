

##### Read in soccer games to predict 

library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(ggplot2)
library(RCurl)

library(rvest) # Importing

library(tesseract)
library(doMC)


# Load RF model for prediction
load(url("https://raw.githubusercontent.com/msjoelin/stryktipset/master/rf_soccer.rda"))

# READ IN MaSTER DATA
stryktipset_data<-
  read.delim("Input_Betting/stryktipset_data.csv", header=TRUE, sep=",") %>%
  mutate(MatchNr =str_pad(MatchNr,2,pad="0")) %>%
  filter(Date=="2018-09-01")

predictions<-stryktipset_data[,c(4:20)]

# Turn characters into factors 
i<-sapply(predictions, is.character)
predictions[i]<-lapply(predictions[i], as.factor)

# Split into Home and Away
prediction_H<-
  select(predictions, -Away) %>%
  rename(Team=Home)

prediction_A<-
  select(predictions, -Home) %>%
  rename(Team=Away)

rf.predict.H<-
  predict(rf.model, prediction_H, type="prob") %>%
  rename(ProbH_A=A, 
         ProbH_D=D,
         ProbH_H=H)

rf.predict.A<-
  predict(rf.model, prediction_A, type="prob") %>%
  rename(ProbA_A=A, 
         ProbA_D=D,
         ProbA_H=H)

rm(prediction_A)
rm(prediction_H)
rm(i)

# Put together
predictions <- 
  cbind(predictions, rf.predict.H, rf.predict.A) %>%
  mutate(Prob_H=ProbH_H*ProbA_H,
         Prob_D=ProbH_D*ProbA_D,
         Prob_A=ProbH_A*ProbA_A,
         Chance_1=Prob_H/(Prob_H+Prob_D+Prob_A),
         Chance_X=Prob_D/(Prob_H+Prob_D+Prob_A),
         Chance_2=Prob_A/(Prob_H+Prob_D+Prob_A)
  )

stryktipset_data$Model_ProbH<-predictions$Chance_1
stryktipset_data$Model_ProbD<-predictions$Chance_X
stryktipset_data$Model_ProbA<-predictions$Chance_2

stryktipset_data


write.csv(stryktipset_data, "Input_Betting/stryktipset_aktprob.csv")
