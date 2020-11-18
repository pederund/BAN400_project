library(XML)
library(RCurl)
library(tidyverse)

avinor_url <- "https://flydata.avinor.no/XmlFeed.asp?airport=OSL&TimeFrom=24&TimeTo=24"
data_url <- getURL(avinor_url)
data <- xmlParse(data_url)

status_url <- "https://flydata.avinor.no/flightStatuses.asp?"
statuscode_url <- getURL(status_url)
status_codes <- xmlParse(statuscode_url)

df_status <- XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status"))) %>% 
  mutate(uniqueID = lag(uniqueID)) %>% 
  filter(!is.na(uniqueID))

flight_df <- 
  cbind(xmlToDataFrame(getNodeSet(data, "//flight")),
        XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))

full_df <- flight_df %>% 
  full_join(df_status)
