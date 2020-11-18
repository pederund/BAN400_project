library(XML)
library(RCurl)
library(tidyverse)

avinor_url <- "https://flydata.avinor.no/XmlFeed.asp?airport=OSL&TimeFrom=24&TimeTo=24"
data_url <- getURL(avinor_url)
data <- xmlParse(data_url)

status_url <- "https://flydata.avinor.no/flightStatuses.asp?"
statuscode_url <- getURL(status_url)
status_codes <- xmlParse(statuscode_url)

#df <- xmlToDataFrame(getNodeSet(data, "//flight"))

df_status <- XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status")))
#df_testing <- XML:::xmlAttrsToDataFrame(getNodeSet(data, "//status"))

flight_df <- 
  cbind(xmlToDataFrame(getNodeSet(data, "//flight")),
        XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))
