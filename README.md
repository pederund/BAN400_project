# R interface to Avinor flight data

## Data
[Flydata fra Avinor/Flightdata from Avinor](https://avinor.no)

## Purpose
In this project we have collected real time flight data and created a shiny app that visualizes departures and arrivals for airports owned by Avinor. We have separated the process into two scripts. The first, “flight_data.R”, collects the data from Avinor’s own service called "Flydata", while the second one, "avinor_shiny.R" creates the app. The goal has been to replicate the homepage of Avinor (https://avinor.no/flyplass/oslo/flyinformasjon/avganger/).
The final product should look something like this:


![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/Shiny%20app%2C%20Avinor%20table.png)


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
Avinor asks for all projects built on their database to be cached and not let the end-user go directly to their servers. This is therefore the main goal of the source script “flight_data.R”. Data is retrieved from Avinor in XML and needs to be cleansed and converted to an R data frame and then cached. This can be challenging to automate as the XML trees will have lots of variation in both length and structure.

The structure of the source script is to first get all “static” data into an R data frame, such as airline names, airport names etc. Subsequently, the real-time data for each individual airport is fetched and then merged with the metadata. Finally, the complete R data frame is mutated with appropriate formatting for the Shiny application.



## Shiny

#### Installation
When creating the app some packages are necessary for running the app and when designing the layout. Please make sure to have these installed prior to running the code:

    install.packages("shiny")
    install.packages("shinyjs")
    install.packages("shinythemes")
    install.packages("shinyalert")
    install.packages("DT")

#### Input
The Shiny app-function has two inputs. The user interface (ui) and the presented data (server). The script creates this ui first:

###### User interface
In this data frame we construct a fluid page. The layout theme is selected as "yeti", this provides the colors and styling needed to replicate Avinor's own homepage. 

"shinyjs"?????? 

As the data is collected every third minute, the app needs to be refreshed with the same time interval. Therefore there have been added an alert, by using the shinyalert function, to give the user a warning when this is happening. The input for the alert is added in “server”, as we only initiate the alert here. 

Furter, the code provides some helpful information for the user of the app, filtering opportunities in the side panel and a main panel with the data table. On top of the main table the “tabsetPanel” produces a tabset, so the user can choose if he or she wants to see departures or arrivals. The syntax “actionButton” produces a button that allows the user to choose if (s)he wants to see only the flights from one hour back in time and further into the future, or all the past flights. 







