library(xml2)
library(rvest)
library(tidyverse)

# Bockshorn
url_bock <- 'http://www.bockshorn.de/spielplan.htm'
page_bock <- read_html(url_bock)

veran_bock <- html_nodes(page_bock,'#splist b') %>%
  html_text()
veran_bock

table_bock <- html_table(page_bock, fill=TRUE)
table_bock

reg_date <- "[A-z]{2}[\\,/]\\s[0-9]{2}[\\./][0-9]{2}[\\./]20[1-2]{1}[0-9]{1}"
datum_bock <- str_extract_all(table_bock[[3]]$X1,reg_date) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) 
datum_bock <- datum_bock[!is.na(datum_bock)]

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

veranstaltungen_bock <- data.frame(Datum=datum_bock,
           Uhrzeit=uhrzeit_bock,
           Veranstaltung=veran_bock,
           Ort=ort_bock,
           Veranstalter=veranstalter_bock,
           stringsAsFactors = F)

write.csv(veranstaltungen_bock,'veranstaltungen_bock_Chen.csv',row.names = FALSE)
