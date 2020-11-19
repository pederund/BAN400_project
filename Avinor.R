library(XML)
library(RCurl)
library(tidyverse)
library(lubridate)
library(chron)

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

airport_df <- 
  XML:::xmlAttrsToDataFrame(getNodeSet(airport_names, "//airportName")) %>% 
  rename(airport_name = name)

airlines_df <- 
  XML:::xmlAttrsToDataFrame(getNodeSet(airline_names, "//airlineName")) %>% 
  rename(airline_name = name)

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
  relocate(airline_name, .after = airline_code) %>% 
  relocate(airport_name, .after = airport_code) %>% 
  relocate(c(status_text_NO, status_text_EN), .after = status_code)

avinor_airports <- c("OSL", "BGO", "KRS", "VDB", "KSU", "MOL", "HOV", "AES", "ANX",
                     "BOO", "BNN", "EVE", "LKN", "MQN", "MJF", "RET", "SSJ", "SKN",
                     "SVJ", "VRY", "HAU", "SVG", "LYR", "OSY", "RRS", "RVK", "TRD",
                     "ALF", "BVG", "BJF", "HFT", "HAA", "HVG", "KKN", "LKL", "MEH",
                     "SOJ", "TOS", "VDS", "VAW", "FRO", "FDE", "SDN", "SOG")
avinor_df <- data.frame(avinor_airports) %>% 
  rename(code = avinor_airports) %>% 
  left_join(airport_df)
