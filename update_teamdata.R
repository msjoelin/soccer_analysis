
library(stringr)
library(reshape2)
library(plyr)

library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(RCurl)
library(lubridate)


# Read in data
matches_data <- read.csv("Data/matches_data.csv", stringsAsFactors = FALSE)
stryktipset <- read.csv("Data/stryktipset.csv", stringsAsFactors = FALSE)
input_stryktipset <- read.csv("Data/input_stryktipset.csv", stringsAsFactors = FALSE)

# Load RF model for prediction
load(url("https://raw.githubusercontent.com/msjoelin/stryktipset/master/rf_soccer.rda"))

# Set input date stryktipset
input_dt<-input_stryktipset$Date[1]

head(matches_data)

# Start evaluation
team_home<-
  matches_data %>% 
  mutate(Team=Home,
         Res=ifelse(Res=="H", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="A", "Lost", NA)))) %>%
  rename(Odds=AvgH, 
         OddsDraw=AvgD,
         OddsOpp=AvgA) %>%
  select(-Home, -Away)


team_away<-
  matches_data %>% 
  mutate(Team=Away,
         Res=ifelse(Res=="A", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="H", "Lost", NA)))) %>%
  rename(Odds=AvgA, 
         OddsDraw=AvgD,
         OddsOpp=AvgH) %>%
  select(-Home, -Away)

team_data<-
  rbind(team_home, team_away)  %>%
  arrange(Team, Season, Date)  %>%
  mutate(Res_1_0=ifelse(Res=="Win", 1, 0), 
         Res=as.factor(Res), 
         Season=as.factor(Season),
         Div=as.factor(Div),
         Date=as.Date(Date),
         Odds_Intervall=cut(Odds, breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 10, 100)),
         Country=ifelse(Div=="Allsvenskan", "SE", 
                        ifelse(Div=="E0" | Div=="E1", "EN", "DE")))


write.csv(team_data, "team_data.csv", row.names=FALSE)

rm(team_away)
rm(team_home)

# Update last 5 matches 

team_L5<-
  filter(team_data, Date>="2018-04-01" & !is.na(Res)) %>%
  group_by(Team) %>%
  top_n(5, Date) %>%
  group_by(Team, Res) %>%
  summarize(n=n()) %>%
  spread(Res, n) %>%
  replace_na(list(Draw=0, Lost=0, Win=0))

team_currentseason <-
  filter(team_data, Season==2018 & !is.na(Res)) %>%
  group_by(Team, Res) %>%
  summarize(n=n()) %>%
  spread(Res, n) %>%
  replace_na(list(Draw=0, Lost=0, Win=0))

# Add info about last matches
new_stryktipset <- input_stryktipset %>%
  left_join(team_currentseason, by=c("Home"="Team")) %>%
  rename(NrWinH=Win, 
         NrDrawH=Draw, 
         NrLostH=Lost) %>%
  left_join(team_currentseason, by=c("Away"="Team")) %>%
  rename(NrWinA=Win,
         NrDrawA=Draw,
         NrLostA=Lost) %>%
  left_join(team_L5, by=c("Home"="Team")) %>%
  rename(NrWinL5H=Win, 
         NrDrawL5H=Draw, 
         NrLostL5H=Lost) %>%
  left_join(team_L5, by=c("Away"="Team")) %>%
  rename(NrWinL5A=Win,
         NrDrawL5A=Draw,
         NrLostL5A=Lost)




# Split into Home and Away

prediction_H<-
  select(new_stryktipset, -Away) %>%
  rename(Team=Home)

prediction_A<-
  select(new_stryktipset, -Home) %>%
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

#Put together
new_stryktipset <- 
  cbind(new_stryktipset, rf.predict.H, rf.predict.A) %>%
  mutate(Prob_H=ProbH_H*ProbA_H,
         Prob_D=ProbH_D*ProbA_D,
         Prob_A=ProbH_A*ProbA_A,
         Model_ProbH=Prob_H/(Prob_H+Prob_D+Prob_A),
         Model_ProbD=Prob_D/(Prob_H+Prob_D+Prob_A),
         Model_ProbA=Prob_A/(Prob_H+Prob_D+Prob_A)
  ) %>%
  select(Season, Date, MatchNr, Home, Away, AvgH, AvgD, AvgA, NrWinH, NrDrawH, NrLostH, NrWinA, NrDrawA, NrLostA, 
         NrWinL5H, NrDrawL5H, NrLostL5H, NrWinL5A, NrDrawL5A, NrLostL5A, Model_ProbH, Model_ProbD, Model_ProbA, 
         Bet_Model, Bet_Lul, Bet_Eric, Bet_Maki, Result)

stryktipset <-
  filter(stryktipset, Date!=input_dt) %>%
  rbind(new_stryktipset)

write.csv(stryktipset, "Data/stryktipset.csv", row.names=FALSE)
