##################################################
## Project: Soccer Analysis
## Script purpose: Update  stryktipsdata
## Date: 2018-12-14
## Author: Marcus Sjölin
##################################################

# Libraries 
library(readr)
library(lubridate) # Date handling
library(plyr)
library(tidyr)    
library(dplyr)

setwd("/home/marcus/R/soccer_analysis/data/")


# Backup files
file.copy("/home/marcus/R/soccer_analysis/data/stryktipset.csv", 
          paste0("/home/marcus/R/soccer_analysis/data/backup/stryktipset", Sys.Date(), ".csv"))


# Get matches from drive
library(googledrive)

stryktipset_id <- "1YjTv9gAGdFLdCvvQGmhBVJWw78qVYoDIpNfE15yZ0kQ"

drive_download(as_id(stryktipset_id), path="stryktipset", type="csv", overwrite=TRUE)


# Write new to file
write.csv(matches.pred, "predictions.csv", row.names = FALSE)

x<- drive_update(file=as_id(predictions_id),
                 media="predictions.csv")