# R interface to Avinor flight data

In this project we have collected real time flight data and created a shiny app that visualizes departures and arrivals for airports owned by Avinor. We have separated the process into two scripts. The first, “flight_data.R”, collects the data from Avinor’s own service called "Flydata", while the second one, "avinor_shiny.R" creates the app. The goal has been to replicate the homepage of Avinor (https://avinor.no/flyplass/oslo/flyinformasjon/avganger/).
The final product should look something like this:


![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/Shiny%20app%2C%20Avinor%20table.png)


## Flight Data
#### Intallation:
This script depends on some packages. As the data from Avinor are in XML format, we use some packages that allow us to read this data. To run the script one therefor need to install:

    install.packages("XML")
    install.packages("xml2")
    install.packages("RCurl")

Further the script uses these packages to format and read time and date:

    install.packages("lubridate")
    install.packages("chron")

For formating data and structuring the script, functions provided by tidyverse have been used:

    install.packages("tidyverse")

The script produces docstrings, descriptions to the functions created, and for this there are need for these:

    install.packages("docstringr")
    install.packages("stringr")

#### Collecting data


#### Formating


##### Final_df


## Shiny

Now as the data is collected and sorted, it is time to create the Shiny app.

#### Installation
When creating the app some packages are necessary for running the app and when designing the layout:

    install.packages("shiny")
    install.packages("shinyjs")
    install.packages("shinythemes")
    install.packages("shinyalert")
    install.packages("stringr")
    install.packages("DT")

#### Input
The Shiny app-function has two inputs. The user interface (ui) and the presented data (server). THe script creates this ui first:

###### User interface
In this data frame we construct a fluid page. The layout theme is selected as "yeti", this provides the colors and styling needed to replicate Avinor's own homepage. 

"shinyjs"?????? 

As the data is collected every third minute, the app needs to be refreshed with the same time interval. Therefore there have been added an alert, by using the shinyalert function, to give the user a warning when this is happening. The input for the alert is added in “server”, as we only initiate the alert here. 

Furter, the code provides some helpful information for the user of the app, filtering opportunities in the side panel and a main panel with the data table. On top of the main table the “tabsetPanel” produces a tabset, so the user can choose if he or she wants to see departures or arrivals. The syntax “actionButton” produces a button that allows the user to choose if (s)he wants to see only the flights from one hour back in time and further into the future, or all the past flights. 







