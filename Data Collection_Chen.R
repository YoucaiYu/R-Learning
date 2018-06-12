library(xml2)
library(rvest)
library(tidyverse)
library(RSelenium)

##############################
# Bockshorn
##############################
url_bock <- 'http://www.bockshorn.de/spielplan.htm'
page_bock <- read_html(url_bock)

# INFO link
info_links <- html_nodes(page_bock,'.aspevpreis') %>%
  html_attr('href')
main_link_bock <- 'http://www.bockshorn.de/'
info_links <- paste(main_link_bock,info_links) %>%
  gsub(" ","",.)
info_links

# use RSelenium to get all title
rD <- rsDriver()
remDr <- rD[["client"]]

title_list_bock <- c()
for (infor_link in info_links){
  remDr$navigate(infor_link)
  event_page <- read_html(remDr$getPageSource()[[1]])
  event_page %>%
    html_nodes('h1') %>%
    html_text() -> event_title
  
  title_list_bock <- append(title_list_bock,event_title)
}

# shut down the browser
remDr$close()
rm(rD)
gc()

table_bock <- html_table(page_bock, fill=TRUE)
table_bock

reg_date <- "[A-z]{2}[\\,/]\\s[0-9]{2}[\\./][0-9]{2}[\\./]20[1-2]{1}[0-9]{1}"
datum_bock <- str_extract_all(table_bock[[3]]$X1,reg_date) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) 
datum_bock <- datum_bock[!is.na(datum_bock)]

# format the date
for (i in datum_bock) {
  datum_bock=gsub("Mo, ","",datum_bock)
  datum_bock=gsub("Di, ","",datum_bock)
  datum_bock=gsub("Mi, ","",datum_bock)
  datum_bock=gsub("Do, ","",datum_bock)
  datum_bock=gsub("Fr, ","",datum_bock)
  datum_bock=gsub("Sa, ","",datum_bock)
  datum_bock=gsub("So, ","",datum_bock)
}

reg_uhrzeit <- "[0-9]{2}[\\:/][0-9]{2}\\sUhr"
uhrzeit_bock <- str_extract_all(table_bock[[3]]$X2,reg_uhrzeit) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
uhrzeit_bock <- uhrzeit_bock[!is.na(uhrzeit_bock)]

reg_ort <- "\\bOrt[\\:/]\\s[A-z]+"
ort_bock <- str_extract_all(table_bock[[3]]$X1,reg_ort) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
ort_bock <- ort_bock[!is.na(ort_bock)]
ort_bock <- gsub("Ort: ","",ort_bock)
ort_bock

veranstalter_bock <- rep("Bockshorn",length(uhrzeit_bock))

reg_preis <- '[0-9]{1,2}[\\,/][0-9]{1,2}.+'
preis_bock <- str_extract_all(table_bock[[3]]$X4,reg_preis) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
preis_bock <- preis_bock[!is.na(preis_bock)]
preis_bock

kurz_infor_bock <- rep(NA,length(uhrzeit_bock))

veranstaltungen_bock <- data.frame(Datum=datum_bock,
                                   Uhrzeit=uhrzeit_bock,
                                   Veranstaltung=title_list_bock,
                                   Beschreibung=kurz_infor_bock,
                                   Preis=preis_bock,
                                   Ort=ort_bock,
                                   Veranstalter=veranstalter_bock,
                                   stringsAsFactors = F)
##############################
# Jugendkulturhaus Cairo
##############################
url_cairo <- 'https://cairo.wue.de/home'
page_cairo <- read_html(url_cairo)

veran_cairo_html <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .titlefr')
veran_cairo <- html_text(veran_cairo_html)
head(veran_cairo)

time_cairo_html <- html_nodes(page_cairo,'time')
time_cairo <- html_text(time_cairo_html)
for (i in time_cairo) {
  time_cairo=gsub("Mo/","",time_cairo)
  time_cairo=gsub("Di/","",time_cairo)
  time_cairo=gsub("Mi/","",time_cairo)
  time_cairo=gsub("Do/","",time_cairo)
  time_cairo=gsub("Fr/","",time_cairo)
  time_cairo=gsub("Sa/","",time_cairo)
  time_cairo=gsub("So/","",time_cairo)
  time_cairo=gsub("/",".",time_cairo)
}

time_cairo <- paste(time_cairo,".2018") %>%
  gsub(" ","",.)

# get the event link
main_url_cairo <- 'https://cairo.wue.de'

detail_link_relative <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .front-inner') %>%
  html_attr('href')
detail_link <- paste(main_url_cairo, detail_link_relative) %>%
  gsub(" ","",.)
detail_link

# Use RSelenium to get all detail
rD <- rsDriver()
remDr <- rD[["client"]]

uhrzeit_list <- c()
preis_list <- c()
beschreibung_list <- c()

for (event_url in detail_link){
  remDr$navigate(event_url)
  event_pg <- read_html(remDr$getPageSource()[[1]])
  
  event_pg %>%
    html_node('.col-sm-3') -> node_data
  
  event_pg %>%
    html_node('.field--name-field-abendkasse .field--item') %>%
    html_text() -> preis_1
  
  event_pg %>%
    html_node('body > div.main-container.container.js-quickedit-main-content > div > section > div.region.region-content > article > div > div:nth-child(1) > div.col-sm-3 > div.field.field--name-field-info.field--type-string.field--label-inline > div.field--item') %>%
    html_text() -> preis_2
  
  preis_puff <- paste(preis_1,preis_2) %>%
    gsub("NA","",.)
  preis_list <- append(preis_list,preis_puff)
  
  event_pg %>%
    html_node('p:nth-child(1)') %>%
    html_text() -> beschreibung_cairo 
  beschreibung_list <- append(beschreibung_list, beschreibung_cairo)
  
  node_data %>%
    html_node('.field--name-field-beginn .field--item') %>%
    html_text() -> uhrzeit_cairo_event
  
  uhrzeit_list <- append(uhrzeit_list,uhrzeit_cairo_event)
  Sys.sleep(1)
}

# shut down the browser
remDr$close()
rm(rD)
gc()

preis_list %>%
  str_trim()

veranstalter_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))
ort_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))

veranstaltungen_cairo <- data.frame(
  Datum=time_cairo,
  Uhrzeit=uhrzeit_list,
  Veranstaltung=veran_cairo,
  Beschreibung=beschreibung_list,
  Preis=preis_list,
  Ort = ort_cairo,
  Veranstalter = veranstalter_cairo
)

##############################
# Theater Ensemble
##############################
url_theater <- 'http://theater-ensemble.net/spielplan/'
page_theater <- read_html(url_theater)

veran_theater_html <- html_nodes(page_theater,'.title a')
veran_theater <- html_text(veran_theater_html)
veran_theater

date_theater <- html_nodes(page_theater,'span:nth-child(1)') %>%
  html_text()
date_theater <- date_theater[-1]

for (datum_theater in date_theater){
  date_theater = gsub(", 18",".2018",date_theater)
  date_theater = gsub(" Jan","01",date_theater)
  date_theater = gsub(" Feb","02",date_theater)
  date_theater = gsub(" Mär","03",date_theater)
  date_theater = gsub(" Apr","04",date_theater)
  date_theater = gsub(" Mai","05",date_theater)
  date_theater = gsub(" Jun","06",date_theater)
  date_theater = gsub(" Jul","07",date_theater)
  date_theater = gsub(" Aug","08",date_theater)
  date_theater = gsub(" Sep","09",date_theater)
  date_theater = gsub(" Okt","10",date_theater)
  date_theater = gsub(" Nov","11",date_theater)
  date_theater = gsub(" Dez","12",date_theater)
}

date_theater

time_theater <- html_nodes(page_theater,'.time') %>%
  html_text()
time_theater

preis_theater <- html_nodes(page_theater,'.meta') %>%
  html_text() %>%
  str_trim()

regex_preis_theater <- '\\d{1,2}.+'
preis_theater <- str_extract_all(preis_theater,regex_preis_theater) %>%
  as.character()
preis_theater

ort_theater <- rep("Theater Ensemble",length(veran_theater))

veranstalter_theater <- rep("Theater Ensemble",length(veran_theater))
kurz_infor_theater <- rep(NA,length(veran_theater))

veranstaltungen_theater <- data.frame(Datum=date_theater,
                                      Uhrzeit=time_theater,
                                      Veranstaltung=veran_theater,
                                      Beschreibung=kurz_infor_theater,
                                      Preis=preis_theater,
                                      Ort=ort_theater,
                                      Veranstalter=veranstalter_theater)

##############################
# Uni Klinikum
##############################

klinik_link <- 'https://www.ukw.de/patienten-besucher/veranstaltungskalender/'

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(klinik_link)

# Smarter: Click button until all sites are loaded
xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "jscroll-next", " " ))]'

run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'xpath', value = xpath)$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

# Get Nodes
site %>%
  html_nodes(".ui-helper-reset") -> node_data

# Get Values
title_selector <- ".title"
from_date_selector <- ".date1"
time_selector <- ".time"
teaser_selector <- ".teaser"

node_data %>%
  html_node(title_selector) %>%
  html_text() -> title
node_data %>%
  html_node(from_date_selector) %>%
  html_text() %>%
  as.character() -> from_date
node_data %>%
  html_node(time_selector) %>%
  html_text() %>%
  as.character() -> time
node_data %>%
  html_node(teaser_selector) %>%
  html_text() %>% 
  as.character() -> kurz_infor_klinik

ort_klinik <- rep(NA,length(title))
veranstalter <- rep("Universitätsklinikum Würzburg - Zentrum für Operative Medizin (ZOM)",length(title)) %>%
  as.character()

preis_klinik <- rep(NA,length(title)) %>%
  as.character()
# Merge
df_klinik <- data.frame(
  Datum = from_date, 
  Uhrzeit = time,
  Veranstaltung = title,
  Beschreibung=kurz_infor_klinik,
  Preis=preis_klinik,
  Ort=ort_klinik,
  Veranstalter=veranstalter)

# Clean up
df_klinik %>%
  filter(!is.na(Veranstaltung)) -> df_klinik_clean


# Shut down selenium
remDr$close()
rm(rD)
gc()

##############################
# merging all calendars
##############################
final_eventcalender <- rbind(veranstaltungen_bock, veranstaltungen_cairo,veranstaltungen_theater,df_klinik_clean)

write.csv(final_eventcalender,"Event Calender_Chen.csv",row.names = FALSE)
