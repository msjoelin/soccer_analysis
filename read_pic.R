

# Read in 


library(tesseract)
library(doMC)


text <- tesseract::ocr_data("testpic.png")

text


inFile<-input$file1

if (is.null(inFile))
  return(NULL)


input_betting <- 
  tesseract::ocr_data("testpic.png") %>%
  filter(confidence>75) %>%
  mutate(Name="Lul") %>%
  select(Name, word) %>%
  rename(Betting=word) 

betting_hist<-rbind(betting_hist, input_betting)



if (input$name_txt=="Lul") {
  stryktipset_data$Bet_Lul<-input_betting$Betting
}
else if (input$name_txt=="Eric") {
  stryktipset_data$Bet_Eric<-input_betting$Betting
} else if (input$name_txt=="Maki") {
  stryktipset_data$Bet_Maki<-input_betting$Betting
}
