library(xml2)
library(rvest)
library(tidyverse)

# Jugendkulturhaus Cairo
url_cairo <- 'https://cairo.wue.de/home'
page_cairo <- read_html(url_cairo)

veran_cairo_html <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .titlefr')
veran_cairo <- html_text(veran_cairo_html)
head(veran_cairo)

time_cairo_html <- html_nodes(page_cairo,'time')
time_cairo <- html_text(time_cairo_html)
head(time_cairo)

# get the event link
main_url_cairo <- 'https://cairo.wue.de'

detail_link_relative <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .front-inner') %>%
  html_attr('href')
detail_link <- paste(main_url_cairo, detail_link_relative) %>%
  gsub(" ","",.)
str(detail_link)

# Use RSelenium to get all detail
library(RSelenium)

rD <- rsDriver()
remDr <- rD[["client"]]

uhrzeit_list <- c()
for (event_url in detail_link){
  remDr$navigate(event_url)
  event_pg <- read_html(remDr$getPageSource()[[1]])
  
  event_pg %>%
    html_node('.col-sm-3') -> node_data
  
  node_data %>%
    html_node('.field--name-field-beginn .field--item') %>%
    html_text() -> uhrzeit_cairo_event
  
  uhrzeit_list <- append(uhrzeit_list,uhrzeit_cairo_event)
  Sys.sleep(1)
}
str(uhrzeit_list)

veranstalter_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))
ort_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))

veranstaltungen_cairo <- data.frame(
  Datum=time_cairo,
  Uhrzeit=uhrzeit_list,
  Veranstaltung=veran_cairo,
  Ort = ort_cairo,
  Veranstalter = veranstalter_cairo
)

remDr$close()
rm(rD)
gc()

write.csv(veranstaltungen_cairo,"Event Cairo.csv", row.names = FALSE)

# Theater Ensemble
url_theater <- 'http://theater-ensemble.net/spielplan/'
page_theater <- read_html(url_theater)

veran_theater_html <- html_nodes(page_theater,'.title a')
veran_theater <- html_text(veran_theater_html)
veran_theater

date_theater <- html_nodes(page_theater,'span:nth-child(1)') %>%
  html_text()
data_theater <- date_theater[-1]
data_theater

time_theater <- html_nodes(page_theater,'.time') %>%
  html_text()
time_theater

ort_theater <- rep("Theater Ensemble",length(veran_theater))

veranstalter_theater <- rep("Theater Ensemble",length(veran_theater))

veranstaltungen_theater <- data.frame(Datum=data_theater,
                                      Uhrzeit=time_theater,
                                      Veranstaltung=veran_theater,
                                      Ort=ort_theater,
                                      Veranstalter=veranstalter_theater)
write.csv(veranstaltungen_theater,"Theater Ensemble.csv",row.names = FALSE)
