library(xml2)
library(rvest)
library(tidyverse)

link_amv <- 'https://www.amv-wuerzburg.de/semesterprogramm'
page_amv <- read_html(link_amv)

events_amv <- html_nodes(page_amv,'#comp-irfhuo3u .font_9') %>%
  html_text() 
events_amv

# get the date 
regex_date <- "[A-z]{2}\\s{1,2}[0-9]{2}[\\./][0-9]{2}[\\./]"
date_amv <- str_extract_all(events_amv,regex_date) %>%
  as.character()%>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub("\U00A0"," ",.) 
date_amv

# get the time
regex_time <- "\\b([A-z]{2}\\s)?[0-9]{2}[\\:/][0-9]{2}"
time_amv <- str_extract_all(events_amv,regex_time) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
time_amv

# get the events
regex_new_events <- "-\\s\\D{5,50}"
new_events_amv <- str_extract_all(events_amv,regex_new_events) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub("\U00A0","",.) %>%
  gsub("- ","",.) %>%
  gsub("c[\\(/]","",.) 
new_events_amv

ort_amw <- rep("AMW", length(new_events_amv))
veranstalter_amw <- rep("AMW", length(new_events_amv))
# complete list of events
final_event <- data.frame(
  Datum = date_amv,
  Uhrzeit = time_amv,
  Veranstaltung = new_events_amv,
  Ort = ort_amw,
  Veranstalter = veranstalter_amw
)

final_event %>%
  filter(!is.na(Veranstaltung)) -> event_clean

write.csv(event_clean,'amv_wuerzburg.csv',row.names = FALSE)
