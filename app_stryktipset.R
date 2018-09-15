

##### Read in soccer games to predict 

library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(ggplot2)
library(RCurl)

library(shiny)
library(shinydashboard)

library(htmlTable)
library(rvest) # Importing

# READ IN MaSTER DATA
stryktipset_data<-
  read.delim("https://raw.githubusercontent.com/msjoelin/stryktipset/master/Input_Betting/stryktipset_data.csv", header=TRUE, sep=",") %>%
  mutate(MatchNr =str_pad(MatchNr,2,pad="0")) %>%
  filter(Date=="2018-09-01")

# Read in data for teams 
team<-
  read.delim("https://raw.githubusercontent.com/msjoelin/stryktipset/master/team_data.csv", header=TRUE, sep=",") 

# Set precaution limit outgoing
all_probs<-sort(c(stryktipset_data$Model_ProbH, stryktipset_data$Model_ProbD, stryktipset_data$Model_ProbA))

ui <- navbarPage("Stryktipset", 
                 
                 tabPanel("Aktuell", 
                          sidebarLayout(
                            sidebarPanel(
                              actionButton('update', 'Update'),
                              width=2
                            ),
                            mainPanel(
                              fluidRow(
                                column(2, 
                                       sliderInput("nrprecaution", "Garderingar", 
                                                   min=1, max=10, 
                                                   value=5, step=1)),
                                column(10, 
                                       plotOutput("plot_with_percent")
                                )
                              ),
                              br(),
                              br(),
                              br(),
                              fluidRow(
                                column(7, 
                                       plotOutput("betting_upd")),
                                column(5,
                                       plotOutput("plot_live"))
                              )
                              )
                            )
                 ),
                 tabPanel("Team analysis", 
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("country", "Country", unique(team$Country), 
                                           selected = c("EN")),
                              checkboxGroupInput("division", "Division", unique(team$Div),
                                                 selected=unique(team$Div)),
                              checkboxInput('all_no_teams', 'All / No Teams', 
                                            value=TRUE),
                              checkboxGroupInput("teamlist", "Teams", unique(team$Team), 
                                                 selected = c("Arsenal", "Liverpool", "Ipswich"))
                            ),
                            mainPanel(
                              tabsetPanel(type="tabs", 
                                          tabPanel("Result_vs_Odds",
                                                   sliderInput(inputId="sliderodds", label="Odds interval",
                                                   min = 1, max = 10, step=0.1,
                                                   value = c(1, 5)),
                                                   plotOutput("odds_result")),
                                          tabPanel("WinProb ~ Odds",
                                                   plotOutput("odds_winprob"))
                                          )
                            )
                          )
                 )
                 )

server <- function(input, output, session) {
  
  liveresult<-eventReactive(input$update,
                              {
                                
                                # Read in live result
                                page_txt<-
                                  read_html('https://www.svt.se/svttext/web/pages/551.html') %>%
                                  html_nodes("pre") %>%
                                  html_text() %>%
                                  strsplit("\n") %>%
                                  as.data.frame()
                                
                                page_txt[,1]<-as.character(page_txt[,1])
                                
                                colnames(page_txt)<-c("Matches")
                                
                                page_txt %>%
                                  filter(grepl('1.|2.|3.|4.|5.|6.|7.|8.|9.|10.|11.|12.|13.', Matches) & grepl('-', Matches)) %>%
                                  mutate(
                                    Matches=gsub("^\\s+|\\s+$", "", Matches),
                                    Result=substr(Matches, nchar(Matches), nchar(Matches))) %>%
                                  cbind(stryktipset_data[,c("MatchNr", "Bet_Lul", "Bet_Eric", "Bet_Maki")]) %>%
                                    mutate(Matches=paste(MatchNr, substr(Matches, 4, nchar(Matches)-1))) %>%
                                  select(-MatchNr) %>%
                                  melt(id=c("Matches", "Result")) %>%
                                  mutate(correct=ifelse(str_detect(as.character(value), Result), "Y", "N"),
                                         correct_num=ifelse(str_detect(as.character(value), Result), 1, 0))
                                
                              }, ignoreNULL=FALSE)
  
  min_slider<-reactive({
    min(input$sliderodds)
  })
  
  max_slider<-reactive({
    max(input$sliderodds)
  })
  
  teamlist_filt <- reactive({
    
    unique(team$Team[which(team$Country == input$country)])
  })
  
  teams_selected<-reactive({
    filter(team, Team %in% input$teamlist)                           
  })
  
  precautions <- reactive({
    input$nrprecaution
  })
  
  
  observe({
    # -------------------- 
    country_division<-filter(team, 
                             Div %in% input$division & Country==input$country)
    
    x <- unique(country_division$Team)
    
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "teamlist",
                             label = "Teams",
                             choices = x,
                             selected = if (input$all_no_teams) x
                             )
    
  })
  
  
  
  
  output$plot_with_percent <- renderPlot( {
    
    # Print percentages and proposed betting
   
    
    nrPrecautions<-precautions()
    
    precaution_limit<-min(tail(all_probs, 
                               nrPrecautions+13)) # Add 13 as this is base sign
    
    model_plot<-
      mutate(stryktipset_data,
             Match=paste(MatchNr, Home, Away, sep=" "),
             sign1=ifelse(Model_ProbH>=precaution_limit, "1", ""),
             signX=ifelse(Model_ProbD>=precaution_limit, "X", ""),
             sign2=ifelse(Model_ProbA>=precaution_limit, "2", ""),
             betting=paste(sign1, signX, sign2, sep=""))
    
    # Visualize probabilities
    model_plot_melted<-
      melt(model_plot[,c("Match", "Model_ProbH", "Model_ProbD", "Model_ProbA")]) 
    
    model_plot_melted$PlaceBet<-ifelse(model_plot_melted$value>=precaution_limit, "Yes", "No")
    
    model_plot_melted$Match<-as.factor(model_plot_melted$Match)
    
    old.lvl<-levels(model_plot_melted$Match)
    model_plot_melted$Match<-factor(model_plot_melted$Match, levels=c(sort(old.lvl, decreasing=T)))
    
  
    model_plot_melted %>%
    ggplot(aes(variable, Match))+
      geom_tile(aes(fill=PlaceBet), colour="white")+
      scale_fill_manual(values=c("Yes"="blue", "No"="grey"))+
      geom_text(aes(label = round(value*100,0), fontface="bold"), color="white", size=5)+
      theme(text = element_text(size=18),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      ggtitle("Calculated probabilities")
  }
  )
  
  output$betting_upd <- renderPlot({
    
    liveresult<-liveresult()
    # Change factor level
    liveresult$Matches<-as.factor(liveresult$Matches)
    old.lvl<-levels(liveresult$Matches)
    liveresult$Matches<-factor(liveresult$Matches, levels=c(sort(old.lvl, decreasing=T)))
    
    # Plot data 
    liveresult %>%
      ggplot(aes(variable, Matches))+
      geom_tile(aes(fill=correct), colour="white")+
      scale_fill_manual(values=c("Y"="green", "N"="red"))+
      geom_text(aes(label = as.character(value), fontface="bold"), color="black", size=5)+
      theme(text = element_text(size=18),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      ggtitle("Live result")       
    
  })
  
  output$plot_live <- renderPlot({
    
    group_by(liveresult(), variable) %>%
      summarize(nrcorrect=sum(correct_num, na.rm=TRUE)) %>%
      mutate(statuscorrect=as.character(ifelse(nrcorrect<10, 0, 
                                               nrcorrect))) %>%
      
      ggplot(aes(x=variable, y=nrcorrect, label=nrcorrect, color=statuscorrect))+
      geom_point(size=8) +
      geom_text(vjust=-1, size=8)+
      geom_segment(aes(x=variable, 
                       xend=variable, 
                       y=0, 
                       yend=nrcorrect), size=4)+
      scale_y_continuous(limits = c(0, 13),
                         breaks=seq(1,13,1),
                         minor_breaks = seq(1,13,1), 
                         name="Antal ratt")+
      scale_x_discrete(name="")+
      theme(text = element_text(size=18),
            legend.position="none")+
      scale_color_manual(values=c("0"="red", "10"="springgreen3", "11"="green2", "12"="green4", "13"="gold4"))+
      geom_hline(yintercept =10, size=2)+
      coord_flip()
    
  })
  
  
  output$odds_result <- renderPlot({
    
    teams_selected() %>%
      ggplot(aes(x=Team, y=Odds, color=Res)) +
      geom_point(size=4) +
      scale_color_manual(values=c("Draw"="blue", "Win"="green", "Lost"="red"))+
      coord_flip(ylim=c(min_slider(),max_slider()))+
      theme(text = element_text(size=18))
    
  }, height = 700)  
  
  
  output$odds_winprob <- renderPlot({
    
    teams_selected() %>%
      group_by(Team, Odds_Intervall) %>%
      summarize(WinProb=mean(Res_1_0), 
                n=n()) %>%
      filter(n>10) %>%
      ggplot(aes(x=Odds_Intervall, y=WinProb, label=Team)) +
      geom_text(size=4) +
      theme(text = element_text(size=18))
    
  }, height = 700) 
  
}


shinyApp(ui, server)

