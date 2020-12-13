library(XML)
library(xml2)
library(RCurl)
library(tidyverse)
library(lubridate)
library(chron)
library(stringr)
library(docstring)

# Getting the URL's for airlines, airports and status codes from Avinor
status_url <- "https://flydata.avinor.no/flightStatuses.asp?"
airports_url <- "https://flydata.avinor.no/airportNames.asp?"
airlines_url <- "https://flydata.avinor.no/airlineNames.asp"

# Function that will return three dataframes for the data retrieved above
get_flight_metadata <- function(status_url, airports_url, airlines_url){
  #' Get flight metadata
  #' 
  #' Converting xml data to list of R dataframes. 
  #' 
  #' 
  #' @param status_url URL for the status codes
  #' @param airports_url URL for the airport names
  #' @param airlines_url URL for the airline names
  #' 
  #' @examples 
  #' status_url <- "https://flydata.avinor.no/flightStatuses.asp?"
  #' airports_url <- "https://flydata.avinor.no/airportNames.asp?"
  #' airlines_url <- "https://flydata.avinor.no/airlineNames.asp"
  #' metadata_dfs <- get_flight_metadata(status_url, airports_url, airlines_url)

  # Parsing the XML-data
  status_codes <- read_xml(status_url)
  airport_names <- read_xml(airports_url)
  airline_names <- read_xml(airlines_url)
  
  # Getting the nodeset of the XML-data
  status_kids <- xml_children(status_codes)
  airport_kids <- xml_children(airport_names)
  airline_kids <- xml_children(airline_names)
  
  # Creating a list that will hold the three different df's 
  list_dfs <- list()
  
  # Creating a dataframe of the attributes from the XML-data, using the nodeset
  list_dfs$status_codes_df <- 
    data.frame(
      code = xml_attr(status_kids, "code"),
      statusTextEn = xml_attr(status_kids, "statusTextEn"),
      statusTextNo = xml_attr(status_kids, "statusTextNo")
    )
  
  list_dfs$airport_df <- 
    data.frame(
      code = xml_attr(airport_kids, "code"),
      airport_name = xml_attr(airport_kids, "name")
    )
  
  list_dfs$airlines_df <- 
    data.frame(
      code = xml_attr(airline_kids, "code"),
      airline_name = xml_attr(airline_kids, "name")
    )
  
  return(list_dfs)
}

metadata_dfs <- get_flight_metadata(status_url, airports_url, airlines_url)

status_codes_df <- metadata_dfs$status_codes_df
airport_df <- metadata_dfs$airport_df
airlines_df <- metadata_dfs$airlines_df


# Vector containing all the IATA-codes of Avinor's airports
avinor_airports <- c("OSL", "BGO", "KRS", "BDU", "KSU", "MOL", "HOV", "AES", "ANX",
                     "BOO", "BNN", "EVE", "LKN", "MQN", "MJF", "RET", "SSJ", "SKN",
                     "SVJ", "VRY", "HAU", "SVG", "LYR", "OSY", "RRS", "RVK", "TRD",
                     "ALF", "BVG", "BJF", "HFT", "HAA", "HVG", "KKN", "LKL", "MEH",
                     "SOJ", "TOS", "VDS", "VAW", "FRO", "FDE", "SDN", "SOG")

# Creating a dataframe consisting of only Avinor's airports
avinor_df <- data.frame(avinor_airports) %>% 
  rename(code = avinor_airports) %>% 
  # Joining in the airport names
  left_join(airport_df)


get_flightdata <- function(xml_data, origin){
  #' Get Avinor flight data
  #'
  #' Retrieves flight data for an airport from the Avinor "flydata" service, 
  #' and turns the xml-data into a dataframe object.
  #' 
  #' @details Remember to run the URL to the xml-data through getURL first
  #' 
  #' @param xml_data The xml-data for the flight data
  #' @param origin IATA code for the airport the flight data is from
  #' 
  #' @usage get_flightdata(xml_data, origin)
  #' 
  #' @examples 
  #' osl_xml <- getURL("https://flydata.avinor.no/XmlFeed.asp?airport=OSL&TimeFrom=24&TimeTo=24")
  #' get_flightdata(osl_xml, "OSL")
  
  # Parsing the XML-data 
  data <- xmlParse(xml_data)
  
  # Creating a dataframe to test if the given airport has any data, because if not
  # the script will break
  test_df <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(data, ("//flight")))
  
  #If there is data for the given airport, do the following
  if(nrow(test_df != 0)) {
    # Getting the status code and status time for each unique flight-id
    # This is done due to the "status" column from the XML-data containing two values.
    # It was not able to be read in correctly, so it will be joined in with the flight data later
    df_status <- 
      XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status"))) %>% 
      # Due to the unique id and status code not appearing on the same row,
      # the unique id is lagged (pushed a row down)
      mutate(uniqueID = lag(uniqueID)) %>% 
      filter(!is.na(uniqueID))
    
    # Getting all the data for each flight
    flight_df <- 
      bind_cols(xmlToDataFrame(getNodeSet(data, "//flight")),
                XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))
    
    # Joining the flight data and status codes and times toghether
    full_df <- flight_df %>% 
      full_join(df_status) %>% 
      mutate(origin = origin) %>% 
      filter(!is.na(uniqueID))
    
    return(full_df)
  } else { 
    #Do nothing if there is no data
  }
  

}

# Getting the base URL for each airport
avinor_base_url <- "https://flydata.avinor.no/XmlFeed.asp?airport="

# Creating a vector of the full url for all 44 airports. The base url is pasted 
# together with the airport IATA-code and the rest of the URL
avinor_urls <- paste0(avinor_base_url, avinor_airports, "&TimeFrom=24&TimeTo=24")

create_final_df <- function(){

  # Reading in all 44 URLs with the XML-data for each airport simultaneously 
  data_url <- getURL(avinor_urls, async = TRUE, .encoding = "ISO-8859-1")
  
  # Creating the final dataframe
  
  final_df <-  
    # Iterating through the objects data_url and avinor_airports, calling the 
    # get_flightdata function 44 times. The data_url is the first argument to the function,
    # and avinor_airports the second argument.
    map2_dfr(.x = data_url, .y = avinor_airports ,~get_flightdata(.x, .y)) %>% 
    
    ## Joining in dataframes containing airline- & airport names and also text for 
    ## each status code.
    left_join(airlines_df, by = c("airline" = "code")) %>% 
    left_join(airport_df, by = c("airport" = "code")) %>% 
    left_join(status_codes_df) %>% 
    
    ## Housekeeping ##
    select(-status) %>%
    
    # All times are given in timezone UTC, thus we have to convert it to CET
    mutate(schedule_time = ymd_hms(schedule_time, tz = ("UTC")),
           schedule_time = force_tz(with_tz(schedule_time, tz = "CET")),
           time = ymd_hms(time, tz = "UTC"),
           time = force_tz(with_tz(time, tz = "CET"))) %>%
    
    # Separating the schedule_time and time columns into date and time columns
    separate(schedule_time,
             into = c("scheduled_date", "scheduled_time"),
             sep = " ") %>% 
    separate(time,
             into = c("updated_date", "updated_time"),
             sep = " ") %>% 
    
    # After the separation, the column are now character. Therefore we convert it
    # back to date and time format.
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
    
    # Mutations done to format what is displayed in shiny app 
    # Adding bold text, line breaks etc. in html-code
    mutate(
      belt_html = if_else(
        is.na(belt), belt, paste0("<b>","Belt ", belt, "</b>")),
      
      gate_html = if_else(
        is.na(gate), gate, paste0("<b>","Gate ", gate, "</b>")),
      
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
  
  return(final_df)
  
}

final_df <- create_final_df()
