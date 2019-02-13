
####  UPDATE MATCH AND TEAM DATA ######

# Get new match data from internet, update dataset and thereafter update dataset for teams

library(readr)
library(lubridate) # Date handling
library(plyr)
library(tidyr)    
library(dplyr)

library(ggplot2)

library(RcppRoll) #Efficient rolling


# Links to data online
PL1819<-"http://www.football-data.co.uk/mmz4281/1819/E0.csv"
PL1718<-"http://www.football-data.co.uk/mmz4281/1718/E0.csv"
PL1617<-"http://www.football-data.co.uk/mmz4281/1617/E0.csv"


PL1819<-
  read.csv(PL1819, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTHG, FTAG, FTR, BbAvH, BbAvD, BbAvA) %>%
  mutate(Season=2019)

PL1718<-
  read.csv(PL1718, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTHG, FTAG, FTR, BbAvH, BbAvD, BbAvA) %>%
  mutate(Season=2018)

PL1617<-
  read.csv(PL1617, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTHG, FTAG, FTR, BbAvH, BbAvD, BbAvA) %>%
  mutate(Season=2017)

PL <- 
  rbind(PL1819, PL1718, PL1617) %>%
  mutate(Date=as.character(as.Date(Date, "%d/%m/%y"))) %>%
  rename(Home=HomeTeam, 
          Away=AwayTeam,
          Res=FTR, 
          AvgH=BbAvH,
          AvgD=BbAvD,
          AvgA=BbAvA)
         
         

################### UPDATE TEAM DATA #################
         
 # Extract results for home team
PL_home<-
  PL %>% 
  mutate(Team=Home,
         Facing=Away,
         Goals=FTHG,
         Goals_Opp=FTAG,
         H_A="H",
         Res=ifelse(Res=="H", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="A", "Lost", NA)))) %>%
  rename(Odds=AvgH, 
         OddsDraw=AvgD,
         OddsOpp=AvgA) %>%
  select(-Home, -Away, - FTHG, -FTAG)
         

# Extract results for away team
PL_away<-
  PL %>% 
  mutate(Team=Away,
         Facing=Home,
         Goals=FTAG,
         Goals_Opp=FTHG,
         H_A="A",
         Res=ifelse(Res=="A", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="H", "Lost", NA)))) %>%
  rename(Odds=AvgA, 
         OddsDraw=AvgD,
         OddsOpp=AvgH) %>%
  select(-Home, -Away, - FTHG, -FTAG)



# Put together home and away
PL_teams <-
  rbind(PL_home, PL_away)  %>%
  mutate(Win=ifelse(Res=="Win", 1, 0),
         Draw=ifelse(Res=="Draw", 1, 0),
         Lost=ifelse(Res=="Lost", 1, 0),
         Res=as.factor(Res), 
         Season=as.factor(Season),
         Div=as.factor(Div),
         Date=as.Date(Date),
         Odds_Intervall=cut(Odds, breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 10, 100))
         ) %>% 
  arrange(Team, desc(Date))
  
rm(PL_away, PL_home, PL1617, PL1718, PL1819)


get_avgscore <- function(home) {
  
  filter(PL_teams, Team == home & Season==2019)  %>%
  group_by(Team) %>%
  summarize(AvgScored=mean(Goals), AvgConcede=mean(Goals_Opp))
  
}

home_avg<- get_avgscore("Huddersfield")
away_avg<- get_avgscore("Liverpool")

home_score <- 0.5*(home_avg$AvgScored+away_avg$AvgConcede)
away_score <- 0.5*(away_avg$AvgScored+home_avg$AvgConcede)


avg_scores<-t(as.matrix(c(home_score, away_score)))


reps<-1000

simulation.score <- 
  as.data.frame(sapply(avg_scores, function(x) replicate(reps, rpois(1, x)))) %>%
  rename(Goals_Home=V1, 
         Goals_Away=V2) %>%
  mutate(Pred_Score=paste(Goals_Home, Goals_Away, sep="-"))

group_by(simulation.score, Pred_Score) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(freq = n / sum(n))


  teams_rolling <- teams %>%
  group_by(Team, Season) %>%
  mutate(NrWin=lag(cumsum(Win)),  # Lag value it is used as input for coming matches
         NrDraw=lag(cumsum(Draw)),
         NrLost=lag(cumsum(Lost)),
         NrWinPerc=NrWin/(NrWin+NrDraw+NrLost), 
         NrDrawPerc=NrDraw/(NrWin+NrDraw+NrLost), 
         NrLostPerc=NrLost/(NrWin+NrDraw+NrLost)) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(NrWinL5 = lag(roll_sum(Win, 5, align="right", fill=NA)),
         NrDrawL5=lag(roll_sum(Draw, 5, align="right", fill=NA)),
         NrLostL5=lag(roll_sum(Lost, 5, align="right", fill=NA))) %>%
  ungroup() 
