##################################################
## Project: Soccer Analysis
## Script purpose: Scrape data from soccervista for current season
## Date: 2019-02-09
## Author: Marcus Sjölin
##################################################


# Libraries 
library(readr)
library(lubridate) # Date handling
library(plyr)
library(tidyr)    
library(dplyr)

# Import data from web
library(htmlTable)
library(rvest) 

url_data <- read.csv("data/url_data.csv", stringsAsFactors = FALSE)
nbr_url <- nrow(url_data)

# Initialize dataframe
df <- data.frame(Date=as.Date(character()), 
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
      Division=league,
      Season=2019) %>%
    select(-Date_yr)
  
  df <- bind_rows(df, matches)
  
  rm(matches)
  rm(fixtures)
  rm(result)
    
}

write.csv(df, "data/current_season.csv", row.names = FALSE)

