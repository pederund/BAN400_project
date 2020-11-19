avinor_airports <- c("OSL", "BGO", "KRS", "VDB", "KSU", "MOL", "HOV", "AES", "ANX",
                     "BOO", "BNN", "EVE", "LKN", "MQN", "MJF", "RET", "SSJ", "SKN",
                     "SVJ", "VRY", "HAU", "SVG", "LYR", "OSY", "RRS", "RVK", "TRD",
                     "ALF", "BVG", "BJF", "HFT", "HAA", "HVG", "KKN", "LKL", "MEH",
                     "SOJ", "TOS", "VDS", "VAW", "FRO", "FDE", "SDN", "SOG")

base_url <- "https://flydata.avinor.no/XmlFeed.asp?airport="
list_urls <- paste0(base_url, avinor_airports, "&TimeFrom=24&TimeTo=24")

test_func <- function(data, airport){
  flight_df <- 
    cbind(xmlToDataFrame(getNodeSet(data, "//flight")),
          XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight"))) 
  flight_df <- flight_df %>% 
   rbind(flight_df)
  return(flight_df)
}


for (i in 1:length(avinor_airports)){
  airport = avinor_airports[i]
  link = list_urls[i]
  data_url <- getURL(link)
  data <- xmlParse(data_url)
  lapply(data, test_func)
}

