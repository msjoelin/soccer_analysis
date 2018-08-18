
################## SOCCER PREDICTION #######################

# Read in necessary libraries

library(readr)    # Import data 
library(ggplot2)  # Visualization
library(plyr)     # Data cleaning
library(dplyr)    # Data cleaning
library(lubridate) # Date handling
library(RcppRoll) #Efficient rolling

library(caret)    # Machine learning
library(randomForest)

library(e1071) # NaiveBayes


##### Read in data  

# Sweden
sweden_data<-
  read.csv("Data/SWE.csv", sep=",") %>%
  select(Date, Home, Away, Res, AvgH, AvgD, AvgA) %>%
  mutate(Country="SWE")


temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)
#England
PL_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="PL", full.names = TRUE ) # Files starting with PL
CH_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="CH", full.names=TRUE )
GER_files<-list.files(path="/home/marcus/R/stryktipset/Data", pattern="Bundesliga", full.names=TRUE ) # Files starting with Bundesliga

all_files<-c(PL_files, CH_files, GER_files)

all_files_list<-llply(all_files, read_csv)
tot_df<-llply(all_files_list, subset, select=c("Date","HomeTeam", "AwayTeam", "FTR", "BbAvH", "BbAvD", "BbAvA"))

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

# Start evaluation
team_home<-
  soccer_data %>% 
  mutate(Team=Home,
         Win=ifelse(Res=="H", 1, 0),
         Draw=ifelse(Res=="D", 1,0),
         Lost=ifelse(Res=="A", 1,0)) %>%
  select(-Home, -Away)


team_away<-
  soccer_data %>% 
  mutate(Team=Away,
         Win=ifelse(Res=="A", 1, 0),
         Draw=ifelse(Res=="D", 1,0),
         Lost=ifelse(Res=="H", 1,0)) %>%
  select(-Home, -Away)
  
team<-
  rbind(team_home, team_away)  %>%
  arrange(Team, Season, Date) %>%
  group_by(Team, Season) %>%
  mutate(NrWin=lag(cumsum(Win)),  # Lag value it is used as input for coming matches
         NrDraw=lag(cumsum(Draw)),
         NrLost=lag(cumsum(Lost))) %>%
  ungroup() %>%
  group_by(Team) %>%
  mutate(NrWinL5 = lag(roll_sum(Win, 5, align="right", fill=NA)),
         NrDrawL5=lag(roll_sum(Draw, 5, align="right", fill=NA)),
         NrLostL5=lag(roll_sum(Lost, 5, align="right", fill=NA))) %>%
  ungroup() 

# Replace NA in NrWin/Draw/Lost-Cols with zeroes 
team[,c("NrWin", "NrDraw", "NrLost")][is.na(team[,c("NrWin", "NrDraw", "NrLost")])] <- 0

rm(team_away)
rm(team_home)

head(team)


# Join in data to soccer_data
soccer_data2<-
  soccer_data %>%
  left_join(team[,c("Season", "Date", "Team", "NrWin", "NrDraw", "NrLost", "NrWinL5", "NrDrawL5", "NrLostL5")], 
            by=c("Season", "Date", "Home"="Team")) %>%
  rename(NrWinH=NrWin, 
         NrDrawH=NrDraw,
         NrLostH=NrLost,
         NrWinL5H=NrWinL5, 
         NrDrawL5H=NrDrawL5,
         NrLostL5H=NrLostL5) %>%
  left_join(team[,c("Season", "Date", "Team", "NrWin", "NrDraw", "NrLost", "NrWinL5", "NrDrawL5", "NrLostL5")], 
            by=c("Season", "Date", "Away"="Team")) %>%
  rename(NrWinA=NrWin, 
         NrDrawA=NrDraw,
         NrLostA=NrLost,
         NrWinL5A=NrWinL5, 
         NrDrawL5A=NrDrawL5,
         NrLostL5A=NrLostL5) %>%
  filter(as.character(Res)!="") %>%
  mutate(Res=as.factor(as.character(Res)),
         OutcomeOdds=ifelse(as.character(Res)=="H", AvgH, 
                            ifelse(as.character(Res)=="D", AvgD, AvgA)))
  
summary(soccer_data2)
rm(soccer_data)

# Extract Home Teams

soccer_data_H<-
  select(soccer_data2, -Away) %>%
  mutate(HomeAway="H") %>%
  rename(Team=Home)

soccer_data_A<-
  select(soccer_data2, -Home) %>% 
  mutate(HomeAway="A") %>%
  rename(Team=Away)

soccer_data_3<-
  rbind(soccer_data_H, soccer_data_A) %>%
  filter(!is.na(NrWinL5H) & !is.na(NrWinL5A)) 


rm(soccer_data_A)
rm(soccer_data_H)
rm(soccer_data2)

i<-sapply(soccer_data_3, is.character)
soccer_data_3[i]<-lapply(soccer_data_3[i], as.factor)

rm(i)

# Make train and test data set
inTrain <- createDataPartition(soccer_data_3$Res, p=0.75, list=FALSE)  
train<-soccer_data_3[inTrain,]
test<-soccer_data_3[-inTrain,]

summary(train)

# Set up rf model
rf.model<-train(Res~Team+
                     AvgH+AvgD+AvgA+
                     NrWinH+NrDrawH+NrLostH+
                     NrWinA+NrDrawA+NrLostA+
                     NrWinL5H+NrDrawL5H+NrLostL5H+
                     NrWinL5A+NrDrawL5A+NrLostL5A,
                   tuneLength=1, 
                   data=train, method="rf",
                   trControl=trainControl(method="cv", number=5, verboseIter = TRUE, classProbs=TRUE))

test$rf.prediction<-predict(rf.model, test)

# Vector machine model

svm.model<-train(Res~Team+
                  AvgH+AvgD+AvgA+
                  NrWinH+NrDrawH+NrLostH+
                  NrWinA+NrDrawA+NrLostA+
                  NrWinL5H+NrDrawL5H+NrLostL5H+
                  NrWinL5A+NrDrawL5A+NrLostL5A,
                tuneLength=1, 
                data=train, method="svmLinear", 
                trControl=trainControl(method="cv", number=5, savePredictions  = TRUE, classProbs=TRUE))

test$svm.prediction<-predict(svm.model, test)

# Naive Bayes

nB.model <-naiveBayes(Res ~ Team+
                        AvgH+AvgD+AvgA+
                        NrWinH+NrDrawH+NrLostH+
                        NrWinA+NrDrawA+NrLostA+
                        NrWinL5H+NrDrawL5H+NrLostL5H+
                        NrWinL5A+NrDrawL5A+NrLostL5A, 
                      data = train)


test$nB.prediction<-predict(nB.model, test)

confusionMatrix(test$nB.prediction, test$Res)


save(rf.model, file = "rf_soccer.rda")

test<-mutate()

select(test_SWE, Home, Away, Res, prediction, OutcomeOdds)


filter(test, prediction==Res) %>% 
  ggplot(aes(x=Res, y=OutcomeOdds))+
  geom_boxplot()

  
  