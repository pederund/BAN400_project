# R interface to Avinor flight data

In this project we have collected real time flight data and created a shiny app that visualizes departures and arrivals for Norwegian airports. 
We have separated the process into to scripts. The first, “flight data”, collect the data from Avinor’s own homepage, while the second one creates the app. 
The final product should look something like this:

![Image of Shiny App](https://github.com/pederund/BAN400_project/blob/main/Shiny%20app%2C%20Avinor%20table.png)


## Flight Data
### Intallation:
This script depends on some packeges. As the data from Avinor are in XML format, we use some packeges that gives us the opportunity to reed this data. To run the script one therefor need to install:

    install.packeged("XML")
    install.packeged("xml2")
    install.packeged("RCurl")

Further the script uses these packeges to format and read time and date:

    install.packeged("lubridate")
    install.packeged("chron")

For formating data and structuring the script, functions provided by tidyverse have been used: 

    install.packeged("tidyverse")



    install.packeged("stringr")

The script produces docstrings, descriptions to the functions created, and for this there are need for this:

    install.packeged("docstringr")


## Shiny




