

##### Read in soccer games to predict 

library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(ggplot2)

library(shiny)
library(shinydashboard)

library(rvest) # Importing

library(tesseract)
library(doMC)

# Load RF model for prediction
load("~/R/stryktipset/rf_soccer.rda")

# READ IN MaSTER DATA
stryktipset_data<-
  read.delim("https://raw.githubusercontent.com/msjoelin/stryktipset/master/Input_Betting/stryktipset_data_akt.csv", header=TRUE, sep=",") %>%
  mutate(MatchNr =str_pad(MatchNr,2,pad="0"))


predictions<-
  select(stryktipset_data, -Bet_Lul, -Bet_Eric, -Bet_Maki, -Res)

# Turn characters into factors 
i<-sapply(predictions, is.character)
predictions[i]<-lapply(predictions[i], as.factor)

# Split into Home and Away
prediction_H<-
  select(predictions, -Away) %>%
  rename(Team=Home)

prediction_A<-
  select(predictions, -Home) %>%
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
rm(i)

# Put together
predictions <- 
  cbind(predictions, rf.predict.H, rf.predict.A) %>%
  mutate(Prob_H=ProbH_H*ProbA_H,
         Prob_D=ProbH_D*ProbA_D,
         Prob_A=ProbH_A*ProbA_A,
         Chance_1=Prob_H/(Prob_H+Prob_D+Prob_A),
         Chance_X=Prob_D/(Prob_H+Prob_D+Prob_A),
         Chance_2=Prob_A/(Prob_H+Prob_D+Prob_A)
  )

head(predictions)


##### 

# Add first bet: Highest of the columns Prob_H, Prob_D and Prob_A
# predictions[, "firstbet"] <- 
#   apply(predictions[, c("Prob_H", "Prob_D", "Prob_A")], 1, max)

# Add second bet: Second highest of the columns Prob_H, Prob_D and Prob_A
# predictions[, "secondbet"] <- 
#   apply(predictions[, c("Prob_H", "Prob_D", "Prob_A")], 1, function(i) sort(i)[ dim(predictions[, c("Prob_H", "Prob_D", "Prob_A")])[2]-1])


# Set precaution limit outgoing
allprobs<-c(predictions$Prob_H, predictions$Prob_D, predictions$Prob_A)

nrPrecautions<-5

precaution_limit<-min(tail(sort(c(predictions$Chance_1, predictions$Chance_X, predictions$Chance_2)), nrPrecautions+13)) # Add 13 as this is base sign

predictions<-
  mutate(predictions,
         Match=paste(MatchNr, Home, Away, sep=" "),
         sign1=ifelse(Chance_1>=precaution_limit, "1", ""),
         signX=ifelse(Chance_X>=precaution_limit, "X", ""),
         sign2=ifelse(Chance_2>=precaution_limit, "2", ""),
         betting=paste(sign1, signX, sign2, sep=""))

# Visualize probabilities
predictions_melted<-
  melt(predictions[,c("Match", "Chance_1", "Chance_X", "Chance_2")]) 

predictions_melted$PlaceBet<-ifelse(predictions_melted$value>=precaution_limit, "Yes", "No")

predictions_melted$Match<-as.factor(predictions_melted$Match)

old.lvl<-levels(predictions_melted$Match)
predictions_melted$Match<-factor(predictions_melted$Match, levels=c(sort(old.lvl, decreasing=T)))


         


# All available omgångar
dates_list<-as.character(unique(stryktipset_data$Date))

ui <- fluidPage(
  
  title = "Stryktipset",
  
  fluidRow(
    column(2,
    selectInput("date_choosen", "Omgang", dates_list, selected=NULL, multiple=FALSE)
    ),
    column(10,
           plotOutput("plot_with_percent")
    )
  ),
  fluidRow(
    column(1,
           selectInput("name_txt", "Name", c("Lul", "Eric", "Maki"), selected=NULL, multiple=FALSE)),
    column(2,
           fileInput("file1", "Choose PNG/JPG",
                     multiple = FALSE,
                     accept = c("png/jpg"))),
    column(1, 
           actionButton('save_inputs', 'Upload'))
  ),
  
  fluidRow(
    column(3, 
           tableOutput("input_tbl")),
    column(4, 
           tableOutput("betting_upd")),
    column(5,
           plotOutput("plot_live"))
    
  )
)

    

server <- function(input, output) {
 
  observeEvent(input$save_inputs, {
    # Define inputs to save
      inFile<-input$file1
    
       if (is.null(inFile))
      return(NULL)
    
    
    input_betting <- 
      tesseract::ocr_data(inFile$datapath) %>%
      filter(confidence>75) %>%
      mutate(Name=input$name_txt) %>%
      select(Name, word) %>%
      rename(Betting=word)
  
    
    if (input$name_txt=="Lul" & nrow(input_betting==13))
      stryktipset_data$Bet_Lul<-input_betting$Betting
    
    if (input$name_txt=="Eric" & nrow(input_betting==13)) 
      stryktipset_data$Bet_Eric<-input_betting$Betting
     
    if (input$name_txt=="Maki" & nrow(input_betting==13))
      stryktipset_data$Bet_Maki<-input_betting$Betting
    
    write_csv(stryktipset_data, "Input_Betting/stryktipset_data_akt.csv")
    
    
    
  }) 
  
  updated_data<-eventReactive(input$save_inputs,
    {
    
    stryktipset_updated<-read_csv("Input_Betting/stryktipset_data_akt.csv") %>%
      mutate(MatchNr =str_pad(MatchNr,2,pad="0"))
      
      # Read in live result
      page_txt<-
        read_html('https://www.svt.se/svttext/web/pages/551.html') %>%
        html_nodes("pre") %>%
        html_text() %>%
        strsplit("\n") %>%
        as.data.frame()
      
      page_txt[,1]<-as.character(page_txt[,1])
      
      colnames(page_txt)<-c("Data")
      
      liveresult <- page_txt %>%
        filter(grepl('1.|2.|3.|4.|5.|6.|7.|8.|9.|10.|11.|12.|13.', Data) & grepl('-', Data)) %>%
        mutate(
          Data=gsub("^\\s+|\\s+$", "", Data),
          Sign=substr(Data, nchar(Data), nchar(Data)))
      
      
      stryktipset_updated$Sign<-liveresult$Sign
      stryktipset_updated$Match<-predictions$Match
      stryktipset_updated$Bet_Predict <- predictions$betting
      
        stryktipset_updated %>%
        mutate(Right_Lul=ifelse(Sign %in% Bet_Lul, 1, 0),
               Right_Eric=ifelse(Sign %in% Bet_Eric, 1, 0),
               Right_Maki=ifelse(Sign %in% Bet_Maki, 1, 0)
        )
    
  }, ignoreNULL=FALSE)
  
  output$plot_with_percent <- renderPlot( {
    
    # Print percentages and proposed betting
    ggplot(predictions_melted, aes(variable, Match))+
      geom_tile(aes(fill=PlaceBet), colour="white")+
      scale_fill_manual(values=c("Yes"="blue", "No"="grey"))+
      geom_text(aes(label = round(value*100,0), fontface="bold"), color="white", size=5)+
      theme(text = element_text(size=16))+
      ggtitle("Calculated probabilities")
  }
  )
  
  output$input_tbl <- renderTable({
    
    req(input$file1)
    
    input_betting<-
      tesseract::ocr_data(input$file1$datapath) %>%
      filter(confidence>75) %>%
      mutate(Name=input$name_txt) %>%
      select(Name, word) %>%
      rename(Betting=word) 
    
    input_betting
    
  })
  
  output$betting_upd <- renderTable({
    
    updated_data() %>%
      select(Match, Bet_Lul, Bet_Eric, Bet_Maki, Sign) %>%
      rename(Result=Sign)
  })

  
}


shinyApp(ui, server)

