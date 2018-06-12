library(xml2)
library(rvest)
library(tidyverse)
library(RSelenium)

##############################
# AMW
##############################
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
  gsub("\U00A0"," ",.) %>%
  gsub("[A-z]{2}\\s{1,2}","",.)%>%
  str_extract_all("\\d{2}[\\./]\\d{2}") %>%
  as.character()
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
preis_amw <- rep(NA,length(new_events_amv))
kurz_beschreibung <- rep(NA,length(new_events_amv))

# complete list of events
final_event <- data.frame(
  Datum = date_amv,
  Uhrzeit = time_amv,
  Veranstaltung = new_events_amv,
  Beschreibung=kurz_beschreibung,
  Preis=preis_amw,
  Ort = ort_amw,
  Veranstalter = veranstalter_amw
)

final_event %>%
  filter(!is.na(Veranstaltung)) -> event_clean

##############################
# Cinemaxx
##############################
cinemaxx_link <- 'https://www.cinemaxx.de/event'
page_cinemaxx <- read_html(cinemaxx_link)
# get the events and the date
events_cinemaxx <-html_nodes(page_cinemaxx,'.underline--alt') %>%
  html_text() %>%
  gsub("\r\n","",.) %>%
  str_trim()

regex_cinemaxx_date <- '[0-9]{1,2}[\\./]\\s[A-z]{3,9}'
cinemaxx_date <- str_extract_all(events_cinemaxx,regex_cinemaxx_date) %>%
  as.character()

for (i in cinemaxx_date){
  cinemaxx_date = gsub(" Januar","01",cinemaxx_date)
  cinemaxx_date = gsub(" Februar","02",cinemaxx_date)
  cinemaxx_date = gsub(" März", "03", cinemaxx_date)
  cinemaxx_date = gsub(" April", "04", cinemaxx_date)
  cinemaxx_date = gsub(" Mai", "05", cinemaxx_date)
  cinemaxx_date = gsub(" Juni", "06", cinemaxx_date)
  cinemaxx_date = gsub(" Juli", "07", cinemaxx_date)
  cinemaxx_date = gsub(" August", "08", cinemaxx_date)
  cinemaxx_date = gsub(" September", "09", cinemaxx_date)
  cinemaxx_date = gsub(" Oktober", "10", cinemaxx_date)
  cinemaxx_date = gsub(" November", "11", cinemaxx_date)
  cinemaxx_date = gsub(" Dezember", "12", cinemaxx_date)
}

cinemaxx_event <- gsub('[0-9]{1,2}[\\./]\\s[A-z]{3,9}\\s',"",events_cinemaxx) 
cinemaxx_event

# get the links of events
main_url_cinemaxx <- 'https://www.cinemaxx.de'
events_detail <- html_nodes(page_cinemaxx,'.ml-movie-boxes__layer__link') %>%
  html_attr('href') %>%
  paste(main_url_cinemaxx,.) %>%
  gsub(" ","",.)
events_detail

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate('https://www.cinemaxx.de/event')
Sys.sleep(3)
remDr$maxWindowSize()
# shut down the cookie message
remDr$findElement(using = 'css selector', "#offershub > div.cookie-msg.active > div > span > svg > path")$clickElement()

# choose the city
remDr$findElement(using = 'css selector', "li:nth-child(29) a")$clickElement()

# try to get the detail of the event
zeit_list <- c()
kurz_infor_list <- c()
for (event_link in events_detail){
  remDr$navigate(event_link)
  # get the HTML-Site
  detail_pg <- read_html(remDr$getPageSource()[[1]])
  zeit <- html_nodes(detail_pg,'.default') %>%
    html_text()
  zeit_list <- append(zeit_list,zeit)
  kurz_infor_cinemaxx <- html_nodes(detail_pg,'.expand-small .offer__intro') %>%
    html_text()
  kurz_infor_list <- append(kurz_infor_list,kurz_infor_cinemaxx)
  Sys.sleep(2)
}

remDr$close()
rm(rD)
gc()

preis_cinemaxx <- rep(NA,length(cinemaxx_date))
veranstalter_cinemaxx <- rep("Cinemaxx",length(cinemaxx_date))
ort_cinemaxx <- rep("Cinemaxx",length(cinemaxx_date))

complete_events_list <- data.frame(
  Datum = cinemaxx_date,
  Uhrzeit = zeit_list,
  Veranstaltung = cinemaxx_event,
  Beschreibung=kurz_infor_list,
  Preis=preis_cinemaxx,
  Ort = ort_cinemaxx,
  Veranstalter = veranstalter_cinemaxx
)

##############################
# merging all calendars
##############################
end_eventcalender <- rbind(event_clean, complete_events_list)

write.csv(end_eventcalender,"Event Calender_Qiuyan.csv",row.names = FALSE)
