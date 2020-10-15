library(shiny)
library(jsonlite)
library(tidyverse)
library(PxWebApiData)

ssb_alkohol <- ApiData("https://data.ssb.no/api/v0/no/table/04475/", ContentsCode = "ForbrukAlkoliter",
                    Tid = TRUE, Alkohol = "Alkohol totalt")
alkohol_konsum <- ssb_data$dataset

# Define UI ----
ui <-fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
