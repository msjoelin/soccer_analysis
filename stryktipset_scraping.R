


url <- 'https://www.svt.se/svttext/web/pages/551.html'

page <- read_html(url) 

page_txt<-
page %>%
  html_nodes("pre") %>%
  html_text() %>%
  strsplit("\n") %>%
  as.data.frame()

page_txt[,1]<-as.character(page_txt[,1])

colnames(page_txt)<-c("Data")

page_txt_2 <- page_txt %>%
  filter(grepl('1.|2.|3.|4.|5.|6.|7.|8.|9.|10.|11.|12.|13.', Data) & grepl('-', Data)) %>%
  mutate(
    Data=gsub("^\\s+|\\s+$", "", Data),
    Sign=substr(Data, nchar(Data), nchar(Data)))

page_txt_2



