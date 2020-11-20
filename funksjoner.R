tes_func <- function(status_url, airports_url, airlines_url){
  status_codes <- xmlParse(status_url)
  airport_names <- xmlParse(airports_url)
  airline_names <- xmlParse(airlines_url)
  
  list_dfs <- list()
  
  list_dfs$status_codes_df <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(status_codes, "//flightStatus"))
  
  list_dfs$airport_df <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(airport_names, "//airportName")) %>% 
    rename(airport_name = name)
  
  list_dfs$airlines_df <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(airline_names, "//airlineName")) %>% 
    rename(airline_name = name)
  
  return(list_dfs)
}

new_function <- function(data_url, origin){
  data <- xmlParse(data_url)
  
  df_status <- 
    XML:::xmlAttrsToDataFrame(getNodeSet(data, c("//flight","//status"))) %>% 
    mutate(uniqueID = lag(uniqueID)) %>% 
    filter(!is.na(uniqueID))
  
  flight_df <- 
    cbind(xmlToDataFrame(getNodeSet(data, "//flight")),
          XML:::xmlAttrsToDataFrame(getNodeSet(data, "//flight")))
  full_df <- flight_df %>% 
    full_join(df_status) %>% 
    select(-status)
  
  full_df <- full_df %>% 
    full_join(status_codes_df)
  
  full_df <- full_df %>% 
    #Rydder opp i tidskolonnene
    mutate(schedule_time = ymd_hms(schedule_time,
                                   tz = ("UTC")),
           schedule_time = force_tz(with_tz(schedule_time,
                                            tz = "CET")),
           time = ymd_hms(time,
                          tz = "UTC"),
           time = force_tz(with_tz(time,
                                   tz = "CET"))) %>% 
    #Skiller dato og tid fra hverandre
    separate(schedule_time,
             into = c("scheduled_date", "scheduled_time"),
             sep = " ") %>% 
    separate(time,
             into = c("updated_date", "updated_time"),
             sep = " ") %>% 
    #Etter kolonnene ble separert, er de nå character. Konverterer derfor tilbake
    #til tid og datoformat
    mutate(scheduled_time = times(scheduled_time),
           updated_time = times(updated_time),
           scheduled_date = ymd(scheduled_date),
           updated_date = ymd(updated_date)) %>% 
    #Renamer kolonner
    rename(airline_code = airline,
           airport_code = airport,
           status_code = code,
           status_text_EN = statusTextEn,
           status_text_NO = statusTextNo) %>% 
    #Joiner inn airline navn og airport navn
    left_join(airlines_df, by = c("airline_code" = "code")) %>% 
    left_join(airport_df, by = c("airport_code" = "code")) %>% 
    #Relocater noen kolonner for å gjøre dataframen mer leselig
    relocate(airline_name, .after = airline_code) %>% 
    relocate(airport_name, .after = airport_code) %>% 
    relocate(c(status_text_NO, status_text_EN), .after = status_code) %>% 
    mutate(origin = origin) %>% 
    filter(!is.na(uniqueID))
}

final_update <- function(){
  if(nrow(final_df) == 0)
    final_df <<- full_df
  else
    final_df <<- bind_rows(final_df, full_df) 
}

if (!exists("final_df")){
  final_df <- data.frame()
}

avinor_airports <- c("OSL", "BGO", "KRS", "VDB", "KSU", "MOL", "HOV", "AES", "ANX",
                     "BOO", "BNN", "EVE", "LKN", "MQN", "MJF", "RET", "SSJ", "SKN",
                     "SVJ", "VRY", "HAU", "SVG", "LYR", "OSY", "RRS", "RVK", "TRD",
                     "ALF", "BVG", "BJF", "HFT", "HAA", "HVG", "KKN", "LKL", "MEH",
                     "SOJ", "TOS", "VDS", "VAW", "FRO", "FDE", "SDN", "SOG")

status_url <- getURL("https://flydata.avinor.no/flightStatuses.asp?")
airports_url <- getURL("https://flydata.avinor.no/airportNames.asp?")
airlines_url <- getURL("https://flydata.avinor.no/airlineNames.asp")

test_dfs <- tes_func(status_url, airports_url, airlines_url)
status_codes_df <- test_dfs$status_codes_df
airport_df <- test_dfs$airport_df
airlines_df <- test_dfs$airlines_df

avinor_df <- data.frame(avinor_airports) %>% 
  rename(code = avinor_airports) %>% 
  left_join(airport_df)

avinor_base_url <- "https://flydata.avinor.no/XmlFeed.asp?airport="
avinor_urls <- paste0(avinor_base_url, avinor_airports)
origin <- stringr::str_sub(avinor_urls, -3, -1)
data_url <- map(avinor_urls, getURL)

for (i in length(avinor_airports)){
  full_df <- new_function(data_url[[i]], origin[i])
  final_update()
}


