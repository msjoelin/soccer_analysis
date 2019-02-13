
####  UPDATE MATCH AND TEAM DATA ######

# Get new match data from internet, update dataset and thereafter update dataset for teams

library(readr)
library(lubridate) # Date handling
library(plyr)
library(tidyr)    
library(dplyr)

library(RcppRoll) #Efficient rolling


# Links to data online
Link_PL<-"http://www.football-data.co.uk/mmz4281/1819/E0.csv"
Link_CH <- "http://www.football-data.co.uk/mmz4281/1819/E1.csv"
Link_DE <- "http://www.football-data.co.uk/mmz4281/1819/D1.csv"
Link_SE <- "http://www.football-data.co.uk/new/SWE.csv"

# Read in match and team data
matches<-read.csv("/home/marcus/R/stryktipset/Data/matches.csv", stringsAsFactors = FALSE)
teams<-read.csv("/home/marcus/R/stryktipset/Data/teams.csv", stringsAsFactors = FALSE)

# Create backup-files
write.csv(matches, paste0("/home/marcus/R/stryktipset/Data/backup/matches", Sys.Date(), ".csv"), row.names=FALSE)
write.csv(teams, paste0("/home/marcus/R/stryktipset/Data/backup/teams", Sys.Date(), ".csv"), row.names=FALSE)

############### UPDATE MATCH DATA #######################

# Read in current data from internet and put it together
PL_current<-
  read.csv(Link_PL, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTR, BbAvH, BbAvD, BbAvA)

CH_current<-
  read.csv(Link_CH, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTR, BbAvH, BbAvD, BbAvA)

DE_current <- 
  read.csv(Link_DE, stringsAsFactors = FALSE) %>%
  select(Date, Div, HomeTeam, AwayTeam, FTR, BbAvH, BbAvD, BbAvA)

matches_update <-
  rbind(PL_current, CH_current, DE_current) %>%
  rename(Home=HomeTeam, 
         Away=AwayTeam, 
         Res=FTR, 
         AvgH=BbAvH,
         AvgD=BbAvD,
         AvgA=BbAvA) %>%
  mutate(Date=as.character(as.Date(Date, "%d/%m/%y")),
         Season=2018)

# Prepare data SWeden and add to matches_update
SE_current <- 
  read.csv(Link_SE, stringsAsFactors = FALSE) %>%
  select(Date, League, Home, Away, Res, AvgH, AvgD, AvgA, Season) %>%
  rename(Div=League) %>%
  filter(Season==2018) %>% # Only keep 2018
  mutate(Date=as.character(as.Date(Date, "%d/%m/%y")),
         Div="Allsvenskan")

matches_update <- rbind(matches_update, SE_current)

# Update complete data
matches<-
  filter(matches, Season!=2018) %>%
  rbind(matches_update) %>%
  arrange(Div, Date)


# Write to file
write.csv(matches, "/home/marcus/R/stryktipset/Data/matches.csv", row.names=FALSE)



################### UPDATE TEAM DATA #################

# Extract results for home team
team_home<-
  matches %>% 
  mutate(Team=Home,
         Facing=Away,
         H_A="H",
         Res=ifelse(Res=="H", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="A", "Lost", NA)))) %>%
  rename(Odds=AvgH, 
         OddsDraw=AvgD,
         OddsOpp=AvgA) %>%
  select(-Home, -Away)


# Extract results for away team
team_away<-
  matches %>% 
  mutate(Team=Away,
         Facing=Home,
         H_A="A",
         Res=ifelse(Res=="A", "Win", 
                    ifelse(Res=="D", "Draw", 
                           ifelse(Res=="H", "Lost", NA)))) %>%
  rename(Odds=AvgA, 
         OddsDraw=AvgD,
         OddsOpp=AvgH) %>%
  select(-Home, -Away)

# Put together home and away
teams<-
  rbind(team_home, team_away)  %>%
  mutate(Win=ifelse(Res=="Win", 1, 0),
         Draw=ifelse(Res=="Draw", 1, 0),
         Lost=ifelse(Res=="Lost", 1, 0),
         Res=as.factor(Res), 
         Season=as.factor(Season),
         Div=as.factor(Div),
         Date=as.Date(Date),
         Odds_Intervall=cut(Odds, breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 10, 100)),
         Country=ifelse(Div=="Allsvenskan", "SE", 
                        ifelse(Div=="E0" | Div=="E1", "EN", "DE"))) %>% 
  arrange(Team, Season, Date)


teams <- teams %>%
  group_by(Team, Season) %>%
  mutate(NrWin=lag(cumsum(Win)),  # Lag value it is used as input for coming matches
         NrDraw=lag(cumsum(Draw)),
         NrLost=lag(cumsum(Lost)),
         WinPerc=NrWin/(NrWin+NrDraw+NrLost), 
         DrawPerc=NrDraw/(NrWin+NrDraw+NrLost), 
         LostPerc=NrLost/(NrWin+NrDraw+NrLost),
         Matchday=seq(n())) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(NrWinL5 = lag(roll_sum(Win, 5, align="right", fill=NA)),
         NrDrawL5=lag(roll_sum(Draw, 5, align="right", fill=NA)),
         NrLostL5=lag(roll_sum(Lost, 5, align="right", fill=NA)),
         L5WinPerc=NrWinL5/(NrWinL5+NrDrawL5+NrLostL5), 
         L5DrawPerc=NrDrawL5/(NrWinL5+NrDrawL5+NrLostL5), 
         L5LostPerc=NrLostL5/(NrWinL5+NrDrawL5+NrLostL5)) %>%
  ungroup() %>%
  group_by(Team, Season, H_A) %>%
  mutate(NrWin_H_A=lag(cumsum(Win)),  # Lag value it is used as input for coming matches
         NrDraw_H_A=lag(cumsum(Draw)),
         NrLost_H_A=lag(cumsum(Lost)),
         H_A_WinPerc=NrWin_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A), 
         H_A_DrawPerc=NrDraw_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A), 
         H_A_LostPerc=NrLost_H_A/(NrWin_H_A+NrDraw_H_A+NrLost_H_A)) %>%
  ungroup() 

# Replace NA in NrWin/Draw/Lost-Cols with zeroes 
teams[,c("WinPerc", "DrawPerc", "LostPerc")][is.na(teams[,c("WinPerc", "DrawPerc", "LostPerc")])] <- 0
teams[,c("L5WinPerc", "L5DrawPerc", "L5LostPerc")][is.na(teams[,c("L5WinPerc", "L5DrawPerc", "L5LostPerc")])] <- 0
teams[,c("H_A_WinPerc", "H_A_DrawPerc", "H_A_LostPerc")][is.na(teams[,c("H_A_WinPerc", "H_A_DrawPerc", "H_A_LostPerc")])] <- 0


teams<-
  arrange(teams, Team, Date)

write.csv(teams, "/home/marcus/R/stryktipset/Data/teams.csv", row.names=FALSE)





         