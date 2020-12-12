# R interface to Avinor flight data

In this project we have collected real time flight data and created a shiny app that visualizes departures and arrivals for Norwegian airports. We have separated the process into two scripts. The first, “flight data”, collects the data from Avinor’s own homepage, while the second one creates the app. The goal has been to replicate the homepage of Avinor (https://avinor.no/flyplass/oslo/flyinformasjon/avganger/).
\\ The final product should look something like this:


![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/Shiny%20app%2C%20Avinor%20table.png)


## Flight Data
#### Intallation:
This script depends on some packeges. As the data from Avinor are in XML format, we use some packeges that gives us the opportunity to reed this data. To run the script one therefor need to install:

    install.packeged("XML")
    install.packeged("xml2")
    install.packeged("RCurl")

Further the script uses these packeges to format and read time and date:

    install.packeged("lubridate")
    install.packeged("chron")

For formating data and structuring the script, functions provided by tidyverse have been used: 

    install.packeged("tidyverse")

The script produces docstrings, descriptions to the functions created, and for this there are need for these:

    install.packeged("docstringr")
    install.packeged("stringr")

#### Collecting data


#### Formating


##### Final_df


## Shiny

Now as the data is collected and sorted, it is time to create the Shiny app.

#### Installation
When creating the app some packeges are necessary for running the app and when designing the layout: 

    install.packeged("shiny")
    install.packeged("shinyjs")
    install.packeged("shinythemes")
    install.packeged("shinyalert")
    install.packeged("stringr")
    install.packeged("DT")

#### Input
The Shiny app-function has to inputs. The user interface (ui) and the presented data (server). THe script creates this ui first:

###### User interface
In this data frame we construes a fluid page. The layout theme is selected as "yeti", this provides the colors and styling needed to replicate Avinor's 
own homepage. 
"shinyjs"??????
As the data is collected every third minute, the app needs to be refreshed with the same interval. Therefor there have been added an alert, by using the shinyalert function, 
to






