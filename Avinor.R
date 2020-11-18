library(XML)
library(RCurl)
library(tidyverse)

avinor_url <- "https://flydata.avinor.no/XmlFeed.asp?airport=OSL&TimeFrom=24&TimeTo=24"
data_url <- getURL(avinor_url)
data <- xmlParse(data_url)

status_url <- getURL("https://flydata.avinor.no/flightStatuses.asp?")
status_codes <- xmlParse(status_url)

airports_url <- getURL("https://flydata.avinor.no/airportNames.asp?")
airport_names <- xmlParse(airports_url)

airlines_url <- getURL("https://flydata.avinor.no/airlineNames.asp")
airline_names <- xmlParse(airlines_url)

df_status <- XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status"))) %>% 
  mutate(uniqueID = lag(uniqueID)) %>% 
  filter(!is.na(uniqueID))

flight_df <- 
  cbind(xmlToDataFrame(getNodeSet(data, "//flight")),
        XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))

status_codes_df <- 
  XML:::xmlAttrsToDataFrame(getNodeSet(status_codes, "//flightStatus"))

full_df <- flight_df %>% 
  full_join(df_status) %>% 
  select(-status)

full_df <- full_df %>% 
  full_join(status_codes_df)

airport_df <- 
  XML:::xmlAttrsToDataFrame(getNodeSet(airport_names, "//airportName"))

airlines_df <- 
  XML:::xmlAttrsToDataFrame(getNodeSet(airline_names, "//airlineName"))
