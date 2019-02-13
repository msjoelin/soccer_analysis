
library(mailR)
library(htmlTable)
library(condformat)
library(knitr)

library(ggplot2)
library(lubridate)
library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)    # Import data 
library(RCurl)

library(rvest) # Importing

old<-theme_set(theme_light())

# mail_rec<-c("marcussjolin89@gmail.com", "eric.wilhelmsson@gmail.com", "jacob.jb.brandon@gmail.com")
mail_rec<-c("marcussjolin89@gmail.com")

# Read in data
stryktipset_live<-read.csv("/home/marcus/R/stryktipset/Data/stryktipset_live.csv", stringsAsFactors = FALSE) %>%
  mutate(MatchNr =str_pad(MatchNr,2,pad="0")) %>%
  rename(Sign_old=Sign)

nrcorrect_data <- 
  read.csv("/home/marcus/R/stryktipset/Data/nrcorrect_data.csv", stringsAsFactors = FALSE) %>%
  mutate(Time=as.POSIXct(Time))

################## Read in live result ########################
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


################## Add liveresult to data ######################

stryktipset_live <- stryktipset_live %>% 
  mutate(Sign=liveresult$Sign,
        Update=ifelse(Sign==Sign_old, "", "PLING!"), 
        Lul_Correct=ifelse(str_detect(as.character(Bet_Lul), Sign), 1, 0),
        Eric_Correct=ifelse(str_detect(as.character(Bet_Eric), Sign), 1, 0),
        Maki_Correct=ifelse(str_detect(as.character(Bet_Maki), Sign), 1, 0)
        )

# Create formatted table for email
matches_formatted<-
  stryktipset_live %>%
  rename(old=Sign_old) %>%
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
  show_columns(c("Home", "Away", "Bet_Lul", "Bet_Eric", "Bet_Maki", "Sign", "old", "Update")) %>%
  condformat2html()

############## Count corrent nr of games

nrcorrect <- 
  data.frame("Name"=c("Lul", "Eric", "Maki"), 
             "NrCorrect"=c(sum(stryktipset_live$Lul_Correct, na.rm=TRUE),
                           sum(stryktipset_live$Eric_Correct, na.rm=TRUE), 
                           sum(stryktipset_live$Maki_Correct, na.rm=TRUE))) %>%
  mutate(Time=Sys.time(),
         Name=as.character(Name))


#############  Send Email if there has been a update ############

if (length(which(stryktipset_live$Update!=""))>0) {

  # 
  # Add to total dataset
  nrcorrect_data <- 
    rbind(nrcorrect_data, nrcorrect)
  
  # Make plot
  nrcorrect_data %>%
    filter(day(Time)==day(Sys.Date())) %>% 
    filter(hour(Time)>=16) %>% # Only todays update 
    mutate(NrCorrect=ifelse(Name=="Lul", NrCorrect+0.05, 
                            ifelse(Name=="Maki", NrCorrect-0.05, NrCorrect)),
           Time=format.Date(Time, "%H:%M")) %>%
    ggplot(aes(x=as.factor(Time), y=NrCorrect, group=Name, color=Name)) + 
    geom_line(size=1)+
    geom_hline(yintercept = 10, linetype="dotdash")+
    geom_point(size=2)+ 
    scale_y_continuous(limits=c(4,13), 
                       breaks=seq(4,13,1),
                       minor_breaks = seq(4,13,1),
                       name="") + 
    theme(legend.title=element_blank(),
          axis.text.x = element_text(size=10),
          text = element_text(size=14))+
    labs(x = "Timestamp")+
    ggtitle("NrCorrect development")
  
  ggsave('/home/marcus/R/stryktipset/nrcorrect.png', width = 8, height = 5, dpi = 100)
  
  
  # Create html form
html_body_form <- paste0("<p>  LIVERESULT   </p>", 
                         matches_formatted,
                         "<hr>  </hr>",
                         htmlTable(nrcorrect, rnames=FALSE),
                         '<img src="/home/marcus/R/stryktipset/nrcorrect.png">',
                         "<hr>  </hr>",
                         "<p>  Powered by   </p>",
                         '<img src="/home/marcus/R/stryktipset/MKX.png">')
                         
# Send email
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

############# WRITE NEW DATA TO FILE ####################

stryktipset_live<-
  select(stryktipset_live, Date, MatchNr, Home, Away, Bet_Lul, Bet_Eric, Bet_Maki, Sign) %>%
  mutate(MatchNr=as.numeric(MatchNr))

# Write to file
write.csv(stryktipset_live, "/home/marcus/R/stryktipset/Data/stryktipset_live.csv", row.names = FALSE)
write.csv(nrcorrect_data, "/home/marcus/R/stryktipset/Data/nrcorrect_data.csv", row.names = FALSE)




