library(xml2)
library(rvest)
library(tidyverse)
library(RSelenium)

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

node_data %>%
  html_node(title_selector) %>%
  html_text() -> title
node_data %>%
  html_node(from_date_selector) %>%
  html_text() -> from_date
node_data %>%
  html_node(time_selector) %>%
  html_text() -> time

ort_klinik <- rep(NA,length(title))
veranstalter <- rep("Universitätsklinikum Würzburg - Zentrum für Operative Medizin (ZOM)",length(title))
# Merge
df_klinik <- data.frame(
                 Datum = from_date, 
                 Uhrzeit = time,
                 Veranstaltung = title,
                 Ort=ort_klinik,
                 Veransalter=veranstalter)

# Clean up
df_klinik %>%
  filter(!is.na(Veranstaltung)) -> df_klinik_clean


# Shut down selenium
remDr$close()
rm(rD)
gc()

write.csv(df_klinik_clean,file="Uni_klinik.csv",row.names = FALSE)
