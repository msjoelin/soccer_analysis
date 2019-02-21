##################################################
## Project: Soccer Analysis
## Script purpose: Update dataset for matches and data
## Output: Two csv-files:
##  * matches.csv:  one row per match, including predictions
##  * teams.csv:    one row per team and match
## Date: 2019-02-01
## Author: Marcus Sjölin
##################################################

# Libraries 
library(readr) # Read and write data
library(lubridate) # Date handling

# Clening data
library(plyr)
library(dplyr)
library(tidyr)    
library(RcppRoll) #Efficient rolling

# Import data from web
library(htmlTable)
library(rvest) 

# Predictions
library(caret)    
library(e1071) # NaiveBayes

# Upload and download from googledrive
library(googledrive)

# Backup files (comment out if no need for backup ) 
file.copy("matches.csv", 
          paste0("backup/matches", Sys.Date(), ".csv"), overwrite = TRUE)

file.copy("teams.csv", 
          paste0("backup/teams", Sys.Date(), ".csv"), overwrite = TRUE)

# Set IDs for googledrive data
team_id <- "1m1c-xgH_1tH_ReUvu__40daHK3hfn0ot_wl3FEqfJg8"
match_id <- "11J38BIxUw2_MiVxaHnikz4bEIMo9I6y1ufMxH7vlnzI"

# Read in previous data (before current season) 
matches_previous<-read.csv("matches_bf_2019.csv", stringsAsFactors = FALSE)

################ UPDATE CURRENT MATCH DATA ######################### 

url_data <- read.csv("url_data.csv", stringsAsFactors = FALSE)
nbr_url <- nrow(url_data)

# Initialize dataframe
matches_current <- data.frame(Date=as.Date(character()), 
                 Home=character(),
                 Away=character(),
                 Result=character(),
                 Score_Home=numeric(),
                 Score_Away=numeric(),
                 Odds_Home=numeric(),
                 Odds_Draw=numeric(),
                 Odds_Away=numeric(),
                 Division=character(),
                 Season=numeric()
                 , stringsAsFactors = FALSE)

# Loop over all urls for current season 
for (i in 1:nbr_url) {
  
  # URLs
  url_result <- url_data$url_result[i]
  url_fixture <- url_data$url_fixture[i]
  season <- url_data$season[i]
  league <- url_data$league[i]
  
  #Reading the HTML code from the website and save to data frame
  result <- 
    read_html(url_result) %>% 
    html_table(fill=TRUE, header=TRUE) %>% 
    as.data.frame() 
  
  result <- result[,c(1:9)]
  
  colnames(result) <- c("Date", "Round", "Status", "Home", "Result", "Away", "Odds_Home", "Odds_Draw", "Odds_Away")
  
  result <- 
    result %>%
    filter(Date!="") %>%
    separate(Result, c("Score_Home", "Score_Away"), sep=":") %>%
    mutate(
      Score_Home = as.numeric(Score_Home), 
      Score_Away = as.numeric(Score_Away),
      Result=ifelse(Score_Home>Score_Away, "H", 
                    ifelse(Score_Away>Score_Home, "A", "D"))) %>%
    select(Date, Home, Away, Result, Score_Home, Score_Away, Odds_Home, Odds_Draw, Odds_Away) 
  
  
  # Reading the HTML code and create dataframe
  fixtures <- 
    read_html(url_fixture) %>%
    html_table(fill=TRUE, header=TRUE) %>% 
    as.data.frame() 
  
  fixtures <- fixtures[,c(1, 4, 6)]
  
  colnames(fixtures) <- c("Date", "Home", "Away")
  
  fixtures <- 
    fixtures %>%
    filter(Date!="") 
  
  # Combine matches and fixtures
  matches <- 
    bind_rows(fixtures, result) %>%
    mutate(
      Date=paste0(Date, " 1960"),
      Date=as.Date(Date, "%d %b %Y"),
      Date_yr=ifelse(month(Date)>=7, season-1, season),
      Date=as.Date(paste(Date_yr, month(Date), day(Date), sep='-')),
      Season=2019,
      Division=league) %>%
    select(-Date_yr)
  
  matches_current <- bind_rows(matches_current, matches)
  
  rm(matches)
  rm(fixtures)
  rm(result)
  
}

# Import mapping table and replace soccervista names with old ones

name_mapping <- read.csv("teamname_mapping.csv", stringsAsFactors = FALSE)

home_new <- 
  select(matches_current, Home) %>%
  left_join(name_mapping, by=c("Home"="soccervista"))

away_new <- 
  select(matches_current, Away) %>%
  left_join(name_mapping, by=c("Away"="soccervista"))

matches_current$Home <- home_new$historic
matches_current$Away <- away_new$historic

rm(home_new)
rm(away_new)

# Combine current and previous data
matches <- 
  rbind(matches_current, matches_previous) %>%
  mutate(Coming5=ifelse(Date-today()<=5 & is.na(Result), 1, 0),
         IsPlayed=ifelse(is.na(Result), 0, 1)) %>%
  arrange(desc(Coming5), desc(IsPlayed), desc(Date), Division) 
         
################### UPDATE TEAM DATA #################

# Extract results for home team
team_home<-
  matches %>% 
  mutate(Team=Home,
         Facing=Away,
         H_A="H",
         Result=ifelse(Result=="H", "Win", 
                    ifelse(Result=="D", "Draw", 
                           ifelse(Result=="A", "Lost", NA)))) %>%
  rename(Odds=Odds_Home, 
         OddsDraw=Odds_Draw,
         OddsOpp=Odds_Away) %>%
  select(-Home, -Away)

# Extract results for away team
team_away<-
  matches %>% 
  mutate(Team=Away,
         Facing=Home,
         H_A="A",
         Result=ifelse(Result=="A", "Win", 
                    ifelse(Result=="D", "Draw", 
                           ifelse(Result=="H", "Lost", NA)))) %>%
  rename(Odds=Odds_Away, 
         OddsDraw=Odds_Draw,
         OddsOpp=Odds_Home) %>%
  select(-Home, -Away)

# Put together home and away
teams<-
  rbind(team_home, team_away)  %>%
  mutate(Win=ifelse(Result=="Win", 1, 0),
         Draw=ifelse(Result=="Draw", 1, 0),
         Lost=ifelse(Result=="Lost", 1, 0),
         Result=as.factor(Result), 
         Season=as.factor(Season),
         Division=as.factor(Division),
         Odds_Intervall=cut(Odds, breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 10, 100)),
         Country=ifelse(Division=="Allsvenskan", "SE", 
                        ifelse(Division=="E0" | Division=="E1", "EN", "DE"))) %>% 
  arrange(Team, Season, Date)

# Create variables for form before each match
teams <- 
  teams %>%
  group_by(Team, Season) %>% # For each Team / Season: Total number of Win/Lost/Draws, and ration
  mutate(NrWin=lag(cumsum(Win)),  
         NrDraw=lag(cumsum(Draw)),
         NrLost=lag(cumsum(Lost)),
         WinPerc=NrWin/(NrWin+NrDraw+NrLost), 
         DrawPerc=NrDraw/(NrWin+NrDraw+NrLost), 
         LostPerc=NrLost/(NrWin+NrDraw+NrLost),
         Matchday=seq(n())) %>%
  ungroup() %>%
  group_by(Team) %>% # For each Team: Form last 5 matches
  mutate(NrWinL5 = lag(roll_sum(Win, 5, align="right", fill=NA)),
         NrDrawL5=lag(roll_sum(Draw, 5, align="right", fill=NA)),
         NrLostL5=lag(roll_sum(Lost, 5, align="right", fill=NA)),
         L5WinPerc=NrWinL5/(NrWinL5+NrDrawL5+NrLostL5), 
         L5DrawPerc=NrDrawL5/(NrWinL5+NrDrawL5+NrLostL5), 
         L5LostPerc=NrLostL5/(NrWinL5+NrDrawL5+NrLostL5)) %>%
  ungroup() %>%
  group_by(Team, Season, H_A) %>% # For each Team / Season / Home or Away: Total number of Win/Lost/Draws, and ratio
  mutate(NrWin_H_A=lag(cumsum(Win)), 
         NrDraw_H_A=lag(cumsum(Draw)),
         NrLost_H_A=lag(cumsum(Lost)),
         H_A_WinPerc=NrWin_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A), 
         H_A_DrawPerc=NrDraw_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A), 
         H_A_LostPerc=NrLost_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A)) %>%
  ungroup() %>%
  group_by(Team, Facing) %>% # Head to head ratio
  mutate(H2H=lag(cumsum(Win))/(lag(cumsum(Win))+lag(cumsum(Lost)))) %>%
  ungroup() 

# Replace NA in NrWin/Draw/Lost-Cols with zeroes 
teams[,c("WinPerc", "DrawPerc", "LostPerc")][is.na(teams[,c("WinPerc", "DrawPerc", "LostPerc")])] <- 0
teams[,c("L5WinPerc", "L5DrawPerc", "L5LostPerc")][is.na(teams[,c("L5WinPerc", "L5DrawPerc", "L5LostPerc")])] <- 0
teams[,c("H_A_WinPerc", "H_A_DrawPerc", "H_A_LostPerc")][is.na(teams[,c("H_A_WinPerc", "H_A_DrawPerc", "H_A_LostPerc")])] <- 0

# If head-to-head is NA (teams have not played eachother yet), set a value to 0.5 (meaning same ratio win as lose)
teams[,c("H2H")][is.na(teams[,c("H2H")])] <- 0.5


########## ADD DATA FROM TEAM TO EACH MATCH FOR HOME AND AWAY #################
matches <- 
  left_join(matches, teams[,c("Date", "Team", "WinPerc", "L5WinPerc", "H_A_WinPerc", "H2H")], by=c("Home"="Team", "Date"="Date")) %>%
  rename(Win_H = WinPerc, 
         L5Win_H=L5WinPerc,
         HomeStrength=H_A_WinPerc) %>%
  left_join(teams[,c("Date", "Team","WinPerc", "L5WinPerc", "H_A_WinPerc")], by=c("Away"="Team", "Date"="Date")) %>%
  rename(Win_A=WinPerc, 
         L5Win_A=L5WinPerc,
         AwayStrength=H_A_WinPerc)

########## PREDICT RESULT USING ML-MODELS  ##########################

load("rf_soccer.rda")
load("nb_soccer.rda")

# Load in  predictions
matches$rf_pred <- predict(rf.model, matches)
matches$nB_pred <- predict(nB.model, matches)

# Load in nprobabilities
matches$rf_pred_prob_H <- predict(rf.model, matches, type="prob")$H
matches$rf_pred_prob_D <- predict(rf.model, matches, type="prob")$D
matches$rf_pred_prob_A <- predict(rf.model, matches, type="prob")$A
matches$nB_pred_prob_H <- as.data.frame(predict(nB.model, matches, type="raw"))$H
matches$nB_pred_prob_D <- as.data.frame(predict(nB.model, matches, type="raw"))$D
matches$nB_pred_prob_A <- as.data.frame(predict(nB.model, matches, type="raw"))$A

# Add some variables
matches <- 
  matches %>% 
  mutate(Predict_Success_rf=ifelse(rf_pred==Result, 1, 0),
         Predict_Success_nB=ifelse(nB_pred==Result, 1, 0),
         Prob_pred_rf = ifelse(rf_pred == "H", rf_pred_prob_H, 
                            ifelse(rf_pred=="D", rf_pred_prob_D, rf_pred_prob_A)),
         Prob_pred_nB = ifelse(nB_pred == "H", nB_pred_prob_H, 
                               ifelse(nB_pred=="D", nB_pred_prob_D, nB_pred_prob_A)),
         Odds_Outcome=ifelse(Result=="H", Odds_Home, 
                           ifelse(Result=="A", Odds_Away, Odds_Draw)),
       DateWeek=isoweek(Date))

# Write files to drive
write.csv(matches, "matches.csv", row.names=FALSE)
write.csv(teams, "teams.csv", row.names=FALSE)

# Update matches
x<- drive_update(file=as_id(match_id),
                 media="matches.csv")

# Upload teams and matches to googledrive
x<- drive_update(file=as_id(team_id),
                 media="teams.csv")

