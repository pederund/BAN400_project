
library(tidyverse)
library(PxWebApiData)
library(shiny)

# API request from SSB
ssb_data <- ApiData("https://data.ssb.no/api/v0/no/table/04475/", 
                    ContentsCode = "ForbrukAlkoliter",
                    Tid = TRUE, 
                    Alkohol = "Alkohol totalt")

# Storing, selecting and renaming the alcohol consumption data frame
alk_konsum <- 
  ssb_data$dataset %>% 
  select(ContentsCode, Tid, value) %>% 
  rename(
    variable = ContentsCode,
    year = Tid,
    value = value
  )

# Splitting up the years and quarters
alk_konsum$year <- str_split(alk_konsum$year, "K")

for(i in 1:length(alk_konsum$year)){
  
  alk_konsum$q[i] <- alk_konsum$year[[i]][2]
  alk_konsum$year[i] <- alk_konsum$year[[i]][1]
  
}

# format year and quarter as numeric and relocate columns
alk_konsum$year <- unlist(alk_konsum$year)
alk_konsum$year <- as.numeric(alk_konsum$year)
alk_konsum$q <- as.numeric(alk_konsum$q)
alk_konsum <- relocate(alk_konsum, q, .after = year)



#### BUILDING APP ####
ui <- fluidPage(
  titlePanel('Norwegian alcohol consumption'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Plot Norwegian alcohol consumption 
               by year and quarter.'),
      
      selectInput(inputId = 'quart',
                  label = 'Select quarter',
                  choices = c(1:4),
                  selected = 1),
      
      sliderInput(inputId = 'years',
                  label = 'Select range of years:',
                  min = 2002, max = 2020, value = c(2002, 2020),
                  sep = "")),
    
    mainPanel(plotOutput(outputId = 'hist'))))

server <- function(input, output) {
  
  output$hist <- renderPlot({
    alk_konsum %>%
      filter(q == input$quart) %>%
      filter(year %in% c(input$years[1]:input$years[2])) %>%
      ggplot(aes(x = year, y = value)) +
      geom_point(col = 'red', size = 2) +
      geom_line(col = 'blue') +
      theme_classic() +
      labs(
        title = paste('Alcohol consumption, ', 'Q -', input$quart),
        subtitle = paste('Year: ', input$years[1], '-', input$years[2]),
        y = 'Alcohol liters in thousands',
        x = 'Year'
      )
  })
  
}

shinyApp(ui = ui, server = server)

#
