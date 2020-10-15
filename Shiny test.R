library(shiny)
library(jsonlite)
library(tidyverse)
library(PxWebApiData)

ssb_alkohol <- ApiData("https://data.ssb.no/api/v0/no/table/04475/", ContentsCode = "ForbrukAlkoliter",
                    Tid = TRUE, Alkohol = "Alkohol totalt")
alkohol_konsum <- ssb_data$dataset

# Define UI ----
ui <- shinyUI(fluidPage(  
  titlePanel("Alkoholkonsum per kvartal i Norge"),  
  sidebarLayout(  
    sidebarPanel(
      p("Select year"),
      sliderInput("yeartime",
                  label="Year",
                  min=2002,
                  max=2020,
                  value=2002)
      )),
    mainPanel(
      plotOutput("alko_plot")  
    )  
  )  
)
# Define server logic ----
server <- function(input, output) {
  output$alko_plot <- renderPlot({
    alkohol_konsum %>% 
      ggplot(aes(x = Tid, y = value)) +
      geom_bar(stat = "identity")})
}

# Run the app ----
shinyApp(ui = ui, server = server)
