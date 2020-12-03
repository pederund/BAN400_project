library(XML)
library(xml2)
library(RCurl)
library(tidyverse)
library(lubridate)
library(chron)
library(stringr)
library(tcltk2)

tes_func <- function(status_url, airports_url, airlines_url){
  status_codes <- xmlParse(status_url)
  airport_names <- read_xml(airports_url, encoding = "ISO-8859-1")
  airline_names <- read_xml(airlines_url, encoding = "ISO-8859-1")
  
  airport_kids <- xml_children(airport_names)
  airline_kids <- xml_children(airline_names)
  list_dfs <- list()
  
  list_dfs$status_codes_df <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(status_codes, "//flightStatus"))
  
  list_dfs$airport_df <- 
    data.frame(
      code = xml_attr(airport_kids, "code"),
      airport_name = xml_attr(airport_kids, "name")
    )
  #XML:::xmlAttrsToDataFrame(getNodeSet(airport_names, "//airportName")) %>% 
  #rename(airport_name = name)
  
  list_dfs$airlines_df <- 
    data.frame(
      code = xml_attr(airline_kids, "code"),
      airline_name = xml_attr(airline_kids, "name")
    )
  #XML:::xmlAttrsToDataFrame(getNodeSet(airline_names, "//airlineName")) %>% 
  #rename(airline_name = name)
  
  return(list_dfs)
}

new_function <- function(data_url, origin){
  data <- xmlParse(data_url)
  
  #test_df <- xmlToDataFrame(getNodeSet(data, "//flight"))
  #if(nrow(test_df) == 0){
  #}
  #else{
  df_status <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status"))) %>% 
    mutate(uniqueID = lag(uniqueID)) %>% 
    filter(!is.na(uniqueID))
  
  flight_df <- 
    bind_cols(xmlToDataFrame(getNodeSet(data, "//flight")),
              XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))
  
  full_df <- flight_df %>% 
    full_join(df_status) %>% 
    mutate(origin = origin) %>% 
    filter(!is.na(uniqueID))
  
  #}
}


avinor_airports <- c("OSL", "BGO", "KRS", "BDU", "KSU", "MOL", "HOV", "AES", "ANX",
                     "BOO", "BNN", "EVE", "LKN", "MQN", "MJF", "RET", "SSJ", "SKN",
                     "SVJ", "VRY", "HAU", "SVG", "LYR", "OSY", "RRS", "RVK", "TRD",
                     "ALF", "BVG", "BJF", "HFT", "HAA", "HVG", "KKN", "LKL", "MEH",
                     "SOJ", "TOS", "VDS", "VAW", "FRO", "FDE", "SDN", "SOG")

status_url <- getURL("https://flydata.avinor.no/flightStatuses.asp?", .encoding = "ISO-8859-1")
airports_url <- getURL("https://flydata.avinor.no/airportNames.asp?", .encoding = "ISO-8859-1")
airlines_url <- getURL("https://flydata.avinor.no/airlineNames.asp", .encoding = "ISO-8859-1")

test_dfs <- tes_func(status_url, airports_url, airlines_url)
status_codes_df <- test_dfs$status_codes_df
airport_df <- test_dfs$airport_df
airlines_df <- test_dfs$airlines_df

avinor_df <- data.frame(avinor_airports) %>% 
  rename(code = avinor_airports) %>% 
  left_join(airport_df) %>% 
  mutate(airport_name = str_replace(airport_name, pattern = "Ã¸", replacement = "ø"),
         airport_name = str_replace(airport_name, pattern = "Ã¦", replacement = "æ"))

avinor_base_url <- "https://flydata.avinor.no/XmlFeed.asp?airport="
avinor_urls <- paste0(avinor_base_url, avinor_airports, "&TimeFrom=24&TimeTo=24")

run_function <- function(){
  
  #data_url <- map(.x = avinor_urls, ~getURL(., .encoding = "ISO-8859-1"))
  # linja under henter inn url'ene kjappere, pga async = TRUE, da laster 
  # getURL funksjonen inn de 44 urlene samtidig
  data_url <- getURL(avinor_urls, async = TRUE, .encoding = "ISO-8859-1")
  
  final_df <- data_url %>% 
    # creating the dataframe
    # map2dfr lar oss loope over 2 inputs samtidig. Her looper vi altså over både
    # data_url og avinor_airports.
    map2_dfr(.x = ., .y = avinor_airports ,~new_function(.x, .y)) %>% 
    
    ## joining in dataframes containing airline & airport names
    left_join(airlines_df, by = c("airline" = "code")) %>% 
    left_join(airport_df, by = c("airport" = "code")) %>% 
    full_join(status_codes_df) %>% 
    
    ## housekeeping ##
    select(-status) %>%
    mutate(schedule_time = ymd_hms(schedule_time, tz = ("UTC")),
           schedule_time = force_tz(with_tz(schedule_time, tz = "CET")),
           time = ymd_hms(time, tz = "UTC"),
           time = force_tz(with_tz(time, tz = "CET"))) %>% 
    separate(schedule_time,
             into = c("scheduled_date", "scheduled_time"),
             sep = " ") %>% 
    separate(time,
             into = c("updated_date", "updated_time"),
             sep = " ") %>% 
    mutate(scheduled_time = chron::times(scheduled_time),
           scheduled_date = ymd(scheduled_date),
           updated_time = chron::times(updated_time),
           updated_date = ymd(updated_date)) %>% 
    rename(airline_code = airline,
           airport_code = airport,
           status_code = code,
           status_text_EN = statusTextEn,
           status_text_NO = statusTextNo) %>% 
    mutate(airport_name = str_replace(airport_name, pattern = "Ã¸", replacement = "ø"),
           airport_name = str_replace(airport_name, pattern = "Ã¦", replacement = "æ")) %>%
    
    ## mutations done to format what is displayed in shiny app ##
    mutate(
      belt_html = if_else(
        is.na(belt), belt, paste0("<b>","Belt ", belt, "</b>")),
      gate_html = if_else(
        is.na(gate), gate, paste0("<b>","Gate ", gate, "</b>")
      ),
      scheduled_time_html = paste0("<b>", str_sub(scheduled_time,1,5), "</b>"),
      updated_time_html = paste0(str_sub(updated_time,1,5)),
      flight_id_html = paste0("<b>",flight_id,"</b>")) %>% 
    unite(updated_timestatus, c(status_text_EN, updated_time_html),
          sep = " ", remove = FALSE, na.rm = TRUE) %>%
    unite(updated_flightstatus, c(scheduled_time_html, updated_timestatus),
          sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
    mutate(updated_flightstatus = 
             str_replace(updated_flightstatus,"NA", "")) %>% 
    unite(flight_html, c(flight_id_html, airline_name),
          sep = "<br>", remove = FALSE, na.rm = TRUE)
  
  # assigning final_df to the global environment
  .GlobalEnv$final_df <- final_df
}


run_function()

#Linja under kjÃ¸rer funksjonen for Ã¥ oppdatere hvert tredje minutt, helt til vi
#sier at den skal stoppe.
#Det gjÃ¸res ved: tclTaskDelete("testing")

#tclTaskSchedule(180000, run_function(), id = "testing", redo = TRUE)