
library(mailR)
library(htmlTable)
library(condformat)
library(knitr)


library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(RCurl)

library(rvest) # Importing

mail_rec<-c("marcussjolin89@gmail.com", "eric.wilhelmsson@gmail.com", "jacob.jb.brandon@gmail.com")
# mail_rec<-c("marcussjolin89@gmail.com")


matches<-read.delim("/home/marcus/R/stryktipset/Input_Betting/stryktipset_akt.csv", header=TRUE, sep=",") %>%
  mutate(MatchNr =str_pad(MatchNr,2,pad="0")) %>%
  select(MatchNr, Home, Away, Bet_Lul, Bet_Eric, Bet_Maki, Sign) %>%
  rename(Sign_old=Sign)


# Read in live result
liveresult<-
  read_html('https://www.svt.se/svttext/web/pages/551.html') %>%
  html_nodes("pre") %>%
  html_text() %>%
  strsplit("\n") %>%
  as.data.frame()

liveresult[,1]<-as.character(liveresult[,1])

colnames(liveresult)<-c("Stryktipset")

liveresult <- liveresult %>%
  filter(grepl('1.|2.|3.|4.|5.|6.|7.|8.|9.|10.|11.|12.|13.', Stryktipset) & grepl('-', Stryktipset)) %>%
  mutate(
    Stryktipset=gsub("^\\s+|\\s+$", "", Stryktipset),
    Sign=substr(Stryktipset, nchar(Stryktipset), nchar(Stryktipset)))


matches <- matches %>% 
  mutate(Sign=liveresult$Sign,
        Update=ifelse(Sign==Sign_old, "", "PLING!"), 
        Lul_Correct=ifelse(str_detect(as.character(Bet_Lul), Sign), 1, 0),
        Eric_Correct=ifelse(str_detect(as.character(Bet_Eric), Sign), 1, 0),
        Maki_Correct=ifelse(str_detect(as.character(Bet_Maki), Sign), 1, 0)
        )


matches_formatted<-
  matches %>%
  condformat() %>%
  rule_fill_discrete(Bet_Lul,
                     expression = (Lul_Correct == 1),
                     colours = c("TRUE" = "#9ACD32")) %>%
  rule_fill_discrete(Bet_Eric,
                     expression = (Eric_Correct == 1),
                     colours = c("TRUE" = "#9ACD32")) %>%
  rule_fill_discrete(Bet_Maki,
                     expression = (Maki_Correct == 1),
                     colours = c("TRUE" = "#9ACD32")) %>%
  show_columns(c("Home", "Away", "Bet_Lul", "Bet_Eric", "Bet_Maki", "Sign", "Update")) %>%
  condformat2html()


nrcorrect <- data.frame("Name"=c("Lul", "Eric", "Maki"), 
                        "NrCorrect "=c(sum(matches$Lul_Correct, na.rm=TRUE),
                                       sum(matches$Eric_Correct, na.rm=TRUE), 
                                       sum(matches$Maki_Correct, na.rm=TRUE)))

if (length(which(matches$Update!=""))>0) {

html_body_form <- paste0("<p>  LIVERESULT   </p>", 
                         matches_formatted,
                         "<hr>  </hr>",
                         htmlTable(nrcorrect, rnames=FALSE),
                         "<hr>  </hr>",
                         cat('"<p> ++++ <a href="https://msjolin.shinyapps.io/stryktipset/">here</a> ++++ </p>"'),
                         "<p>  Powered by MKx Analytics   </p>")
                         

send.mail(from = "marcussjolin89@gmail.com",
          to = mail_rec,
          subject = "+++ PLING! +++ ",
          body = html_body_form,
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "marcussjolin89", passwd = "wtjhgtzgdzxkygms", ssl = TRUE),
          authenticate = TRUE,
          html = TRUE,
          inline=TRUE,
          send = TRUE)

}

# Save old sign

matches<-read.delim("/home/marcus/R/stryktipset/Input_Betting/stryktipset_akt.csv", header=TRUE, sep=",")

matches$Sign<-liveresult$Sign

write.csv(matches, "/home/marcus/R/stryktipset/Input_Betting/stryktipset_akt.csv", row.names = FALSE)



