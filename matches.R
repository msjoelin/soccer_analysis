
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)

# Read in matchdata 
matches<-read.csv("Data/matches_data.csv", stringsAsFactors = FALSE)

# Read in mapping
mapping_teams<-read.csv("Data/mapping_teams.csv", stringsAsFactors = FALSE)

# Read in current matches
current_season<-read.csv("Data/current_season.csv", stringsAsFactors = FALSE)

# Set max date contained in dataset
max_matches<-max(matches$Date)

# Create mapping to division
mapping_div<-
  filter(matches, Season==2018) %>%
  select(Home, Div) %>%
  unique()


# Join into mapping

current_season <- 
  left_join(current_season, mapping_teams, by=c("name"="Fotball_Org")) %>%
  rename(Home=Team_data) %>%
  left_join(mapping_teams, by=c("name.1"="Fotball_Org")) %>%
  rename(Away=Team_data)

head(current_season)

matches_current <- 
  current_season %>%  
  mutate(Res=ifelse(winner=="HOME_TEAM", "H", 
                    ifelse(winner=="AWAY_TEAM", "A", "D")),
         Date=as.character(as.Date(utcDate)),
         Season=2018,
         AvgH=NA, 
         AvgD=NA, 
         AvgA=NA) %>%
  filter(Date>max_matches) %>%
  left_join(mapping_div) %>%
  select(Date, Div, Home, Away, Res, AvgH, AvgD, AvgA, Season)

head(matches_current)
head(matches)


# Make rbind 
matches_data<-
  rbind(matches, matches_current)

write.csv(matches_data, "Data/matches_data.csv", row.names=FALSE)

         