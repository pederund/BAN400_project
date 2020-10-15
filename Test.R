#client ID frost: a60e470c-5176-4f3f-bbcb-20e4585efd24
#client secret: 80a446c9-55f5-44cb-8483-2898596217d3

library(jsonlite)
library(tidyverse)
library(PxWebApiData)

client_id = "a60e470c-5176-4f3f-bbcb-20e4585efd24"

endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
sources <- 'SN18700'
elements <- 'best_estimate_mean(air_temperature P3M)'
referenceTime <- '2002-01-01/2020-12-01'
# Build the URL to Frost
url <- paste0(
  endpoint, "?",
  "sources=", sources,
  "&referencetime=", referenceTime,
  "&elements=", elements
)
# Issue an HTTP GET request and extract JSON data
xs <- try(fromJSON(URLencode(url),flatten=T))

# Check if the request worked, print out any errors
if (class(xs) != 'try-error') {
  df <- unnest(xs$data)
  print("Data retrieved from frost.met.no!")
} else {
  print("Error: the data retrieval was not successful!")
}

ssb_data <- ApiData("https://data.ssb.no/api/v0/no/table/04475/", ContentsCode = "ForbrukAlkoliter",
                    Tid = TRUE, Alkohol = "Alkohol totalt")
alkoholkonsum <- ssb_data$dataset

ssb_data1 <- ApiData("https://data.ssb.no/api/v0/no/table/08460/", 
                     ContentsCode = c("Grensehandel (mill. kr)", "Antall dagsturer (1 000)"),
                     Tid = TRUE)
grensehandel <- ssb_data1$dataset
