library(XML)
library(RCurl)

avinor_url <- "https://flydata.avinor.no/XmlFeed.asp?airport=OSL"
data_url <- getURL(avinor_url)
data <- xmlParse(data_url)
