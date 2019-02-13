##################################################
## Project: Soccer Analysis
## Script purpose: Update dataset for matches and data
## Date: 2018-11-11
## Author: Marcus Sjölin
##################################################


# Libraries 
library(readr)
library(lubridate) # Date handling
library(plyr)
library(dplyr)
library(tidyr)    

library(e1071) # NaiveBayes
library(caret)

setwd("/home/marcus/R/soccer_analysis/data/")

# Load Model for prediction
load("/home/marcus/R/soccer_analysis/models/nb_soccer.rda")
load("/home/marcus/R/soccer_analysis/models/rfweight_soccer.rda")

# Backup files
file.copy("/home/marcus/R/soccer_analysis/data/predictions.csv", 
          paste0("/home/marcus/R/soccer_analysis/data/backup/predictions", Sys.Date(), ".csv"))


# Get matches from drive
library(googledrive)

team_id <- "1m1c-xgH_1tH_ReUvu__40daHK3hfn0ot_wl3FEqfJg8"
predictions_id <- "1mFyChLxUdx8ZLg9E11tfKRP5ZambXs2e1gAFEvX4nEQ"

drive_download(as_id(predictions_id), path="predictions", type="csv", overwrite=TRUE)

# Read in team data and filter rows that can be predicted
team.pred <- 
  read.csv("teams.csv", stringsAsFactors = FALSE) %>%
  mutate(Date=as.Date(Date)) %>%
  filter(!is.na(NrWinL5)) # If NA, model can tbe used

# get predictions
# nb.pred <- as.data.frame(predict(nB.model, team.pred, "raw"))
# 
# team.pred$Prob_Win <- nb.pred$Win
# team.pred$Prob_Draw <- nb.pred$Draw
# team.pred$Prob_Lost <- nb.pred$Lost


rf.pred <- as.data.frame(predict(rf.weightmodel, team.pred, "prob"))

team.pred$Prob_Win <- rf.pred$Win
team.pred$Prob_Draw <- rf.pred$Draw
team.pred$Prob_Lost <- rf.pred$Lost


# Filter out matches
matches.pred <- 
  filter(team.pred, H_A=="H") %>%
  rename(Home=Team,
         Away=Facing,
         Prob_Win_H=Prob_Win,
         Prob_Draw_H=Prob_Draw, 
         Prob_Lost_H=Prob_Lost) %>%
  select(Date, Home, Away, Prob_Win_H, Prob_Draw_H, Prob_Lost_H) %>%
  left_join(team.pred[,c("Date", "Team", "Prob_Win", "Prob_Draw", "Prob_Lost")], by=c("Away"="Team", "Date"="Date")) %>%
  rename(Prob_Win_A=Prob_Win,
         Prob_Draw_A=Prob_Draw, 
         Prob_Lost_A=Prob_Lost) %>%
  mutate(Prob_H=Prob_Win_H+Prob_Lost_A,
         Prob_D=Prob_Draw_H+Prob_Draw_A,
         Prob_A=Prob_Lost_H+Prob_Win_A,
         Prediction=ifelse(Prob_H>Prob_D & Prob_H>Prob_A, "H",
                           ifelse(Prob_D>Prob_H & Prob_D>Prob_A, "D",
                                  ifelse(Prob_A>Prob_H & Prob_A>Prob_D, "A", NA))),
         Date=as.character(Date), 
         Precaution_perc=ifelse(Prob_H>Prob_D & Prob_H<Prob_A, Prob_H,
                                ifelse(Prob_D>Prob_H & Prob_D<Prob_A, Prob_D, Prob_A))
         ) 


matches <- 
  read.csv("matches.csv", stringsAsFactors = FALSE) %>%
  select(Date, Home, Away, Result, Season, Division, Odds_Home, Odds_Draw, Odds_Away)
  

matches.pred <- 
  left_join(matches.pred, matches, by=c("Date", "Home", "Away")) %>%
  mutate(Predict_Success=(Prediction==Result),
         Odds_Outcome=ifelse(Prediction=="H", Odds_Home, 
                             ifelse(Prediction=="A", Odds_Away, Odds_Draw)))

# Write new to file
write.csv(matches.pred, "predictions.csv", row.names = FALSE)

x<- drive_update(file=as_id(predictions_id),
                 media="predictions.csv")

