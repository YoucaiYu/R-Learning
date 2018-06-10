library(xml2)
library(rvest)
library(tidyverse)
library(RSelenium)

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

cinemaxx_event <- gsub('[0-9]{1,2}[\\./]\\s[A-z]{3,9}\\s',"",events_cinemaxx) 
cinemaxx_event

# get the links of events
main_url_cinemaxx <- 'https://www.cinemaxx.de'
events_detail <- html_nodes(page_cinemaxx,'.ml-movie-boxes__layer__link') %>%
  html_attr('href') %>%
  paste(main_url_cinemaxx,.) %>%
  gsub(" ","",.)

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate('https://www.cinemaxx.de/event')
Sys.sleep(5)
remDr$maxWindowSize()
# shut down the cookie message
remDr$findElement(using = 'css selector', "#offershub > div.cookie-msg.active > div > span > svg > path")$clickElement()
# scroll to the end of the page
# remDr$sendKeysToElement(list(key = "end"))
# choose the city
remDr$findElement(using = 'css selector', "li:nth-child(29) a")$clickElement()

# try to get the detail of the event
zeit_list <- c()
for (event_link in events_detail){
  remDr$navigate(event_link)
  # get the HTML-Site
  detail_pg <- read_html(remDr$getPageSource()[[1]])
  zeit <- html_nodes(detail_pg,'.default') %>%
    html_text()
  zeit_list <- append(zeit_list,zeit)
  Sys.sleep(2)
}
zeit_list

remDr$close()
rm(rD)
gc()

veranstalter_cinemaxx <- rep("Cinemaxx",length(cinemaxx_date))
ort_cinemaxx <- rep("Cinemaxx",length(cinemaxx_date))

complete_events_list <- data.frame(
  Datum = cinemaxx_date,
  Uhrzeit = zeit_list,
  Event = cinemaxx_event,
  Ort = ort_cinemaxx,
  Veranstalter = veranstalter_cinemaxx
)

write.csv(complete_events_list,'event liste of cinemaxx.csv',row.names = FALSE)
