# R interface to Avinor flight data

## Data
[Flydata fra Avinor/Flightdata from Avinor](https://avinor.no)

## Purpose
In this project we have collected real time flight data and created a shiny app that visualizes departures and arrivals for airports owned by Avinor. We have separated the process into two scripts. The first, “flight_data.R”, collects the data from Avinor’s own service called "Flydata", while the second one, "avinor_shiny.R" creates the app. The goal has been to replicate the homepage of Avinor (https://avinor.no/flyplass/oslo/flyinformasjon/avganger/).
The final product should look something like this:

#### Arrivals at Oslo airport
![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/shiny_arrival_example.PNG)

#### Departures from Oslo airport
![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/shiny_departure_example.PNG)

## Flight Data
#### Intallation:
This script depends on some packages. To run the script, please make sure to install the following packages:

    install.packages("XML")
    install.packages("xml2")
    install.packages("RCurl")
    install.packages("lubridate")
    install.packages("chron")
    install.packages("tidyverse")
    install.packages("docstringr")
    install.packages("stringr")

#### Collecting data
Avinor asks for all projects built on their database to be cached and not let the end user go directly to their servers. This is therefore the main goal of the source script “flight_data.R”. Data is retrieved from Avinor in XML and needs to be cleansed and converted to an R data frame and then cached. This can be challenging to automate as the XML trees will have lots of variation in both length and structure.

The structure of the source script is to first get all “static” data into an R data frame, such as airline names, airport names etc. Subsequently, the real-time data for each individual airport is fetched and then merged with the metadata. Finally, the complete R data frame is mutated with appropriate formatting for the Shiny application.



## Shiny

#### Installation
When creating the app some packages are necessary for running the app and when designing the layout. Please make sure to have these installed prior to running the code:

    install.packages("shiny")
    install.packages("shinyjs")
    install.packages("shinythemes")
    install.packages("shinyalert")
    install.packages("DT")

#### User interface
The user is allowed to select airports directly and intuitively from a drop-down menu. Further, the user is allowed to filter flights based on whether a flight is domestic or international, or both at the same time - which is the default. The user is also allowed to filter flights based on date. Due to only having access to data which is +- 24 hours from any given time, the user can only see flights for the current, previous and the next day.
 
The data presented to the user will be automatically updated every three minutes while the app is running, the user will be prompted with a informational alert when this happens. The data is updated every three minutes, this is due to that according to Avinor, the data should be updated in this interval to make sure that the end user has the correct and updated data. Updating the data more frequent than every three minutes should not be necessary.
