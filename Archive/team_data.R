
library(stringr)
library(reshape2)
library(plyr)

library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(RCurl)
library(lubridate)


library(rvest) # Importing


##### Read in data  

# Sweden
sweden_data<-
  read.csv("Data/SWE.csv", sep=",") %>%
  select(Date, Home, Away, Res, AvgH, AvgD, AvgA) %>%
  mutate(Country="SWE", 
         Div="Allsvenskan")

#England
PL_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="PL", full.names = TRUE ) # Files starting with PL
CH_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="CH", full.names=TRUE )
GER_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="Bundesliga", full.names=TRUE ) # Files starting with Bundesliga

all_files<-c(PL_files, CH_files, GER_files)

all_files_list<-llply(all_files, read_csv)
tot_df<-llply(all_files_list, subset, select=c("Date","Div", "HomeTeam", "AwayTeam", "FTR", "BbAvH", "BbAvD", "BbAvA"))

tot_data<-
  ldply(tot_df, data.frame) %>%
  mutate(Country="EN/DE") %>%
  rename(Home=HomeTeam, 
         Away=AwayTeam, 
         Res=FTR, 
         AvgH=BbAvH, 
         AvgD=BbAvD, 
         AvgA=BbAvA)

rm(CH_files)
rm(PL_files)
rm(GER_files)
rm(all_files)
rm(all_files_list)
rm(tot_df)

# Put datasets together

soccer_data<-
  rbind(tot_data, sweden_data) %>%
  filter(!is.na(Date)) %>%
  mutate(Date=as.Date(Date, "%d/%m/%y"),
         YearWeek=as.numeric(paste(year(Date), week(Date),sep="")),
         Season=ifelse(Country=="SWE", year(Date), 
                       ifelse(month(Date)>7, year(Date), year(Date)-1)),
         Res=as.character(Res))

rm(sweden_data)
rm(tot_data)


write.csv(soccer_data, "matches_data.csv", row.names=FALSE)

# Start evaluation
team_home<-
  soccer_data %>% 
  mutate(Team=Home,
         Res=ifelse(Res=="H", "Win", 
                    ifelse(Res=="D", "Draw", "Lost"))) %>%
  rename(Odds=AvgH, 
         OddsDraw=AvgD,
         OddsOpp=AvgA) %>%
  select(-Home, -Away)


team_away<-
  soccer_data %>% 
  mutate(Team=Away,
         Res=ifelse(Res=="A", "Win", 
                    ifelse(Res=="D", "Draw", "Lost"))) %>%
  rename(Odds=AvgA, 
         OddsDraw=AvgD,
         OddsOpp=AvgH) %>%
  select(-Home, -Away)

team<-
  rbind(team_home, team_away)  %>%
  arrange(Team, Season, Date)  %>%
  mutate(Res_1_0=ifelse(Res=="Win", 1, 0),
         Country=as.factor(Country), 
         Res=as.factor(Res), 
         Season=as.factor(Season),
         Div=as.factor(Div),
         Odds_Intervall=cut(Odds, breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 10, 100)),
         Country=ifelse(Div=="Allsvenskan", "SE", 
                        ifelse(Div=="E0" | Div=="E1", "EN", "DE")))

write.csv(team, "team_data.csv")