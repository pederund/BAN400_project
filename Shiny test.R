
library(tidyverse)
library(PxWebApiData)
library(shiny)
library(stargazer)


#### COLLECTING SSB DATA ####

# API request from SSB - table 04475 alcohol consumption Norway
ssb_04475 <- 
  ApiData(04475, 
          ContentsCode = "ForbrukAlkoliter",
          Tid = TRUE,
          Alkohol = "Alkohol totalt")

# extracting data (total alcohol liters in thousands) and creating data frame
alc_liters <-
  ssb_04475$dataset %>% 
  select(Tid, value) %>% 
  transmute(
    year = str_split(Tid, "K"),
    quart = as.integer(map(year, `[`, 2)), # subset position 2 of every obs.
    year = as.integer(map(year, `[`, 1)),
    k_liters = value
  )


# API request from SSB - table 08460 grensehandel
ssb_08460 <- 
  ApiData(08460,
          ContentsCode = "Grenseh",
          Tid = TRUE)

# extracting data (total spent in MNOK) and creating data frame
shopping <-
  ssb_08460$dataset %>% 
  select(Tid, value) %>%
  transmute(
    year = str_split(Tid, "K"),
    quart = as.integer(map(year, `[`, 2)), 
    year = as.integer(map(year, `[`, 1)),
    m_nok = value
  )

# API request from SSB - table 10594 number of unemployed people
ssb_10594 <-
  ApiData(10594,
          ContentsCode = "Registrerte1",
          Region = "Hele landet",
          Kjonn = "Begge kjønn",
          Tid = TRUE)


# creating function to convert month to quarter

month_to_quarter <- function(x){
  
  if(x %in% c(1:3)){
    return(1)
  } else if(x %in% c(4:6)){
    return(2)
  } else if(x %in% c(7:9)){
    return(3)
  } else if(x %in% c(10:12)){
    return(4)
  }
  
}

# extracting data (monthly unemployment rates) and creating data frame

#Ser at fra 2016 og opp får vi bare tall for ett kvartal. Er det pga at dataene kommer slik
#eller har vi kodet noe feil?
unemployment <-
  ssb_10594$dataset %>% 
  select(Tid, value) %>% 
  transmute(
    year = str_split(Tid, "M"),
    month = as.integer(map(year, `[`, 2)), 
    year = as.integer(map(year, `[`, 1)),
    quart = as.integer(map(month, month_to_quarter)),
    unemployed = value
  ) %>% 
  group_by(year, quart) %>% 
  transmute(
    year = year,
    quart = quart,
    q_unemployed = sum(unemployed)
  ) %>% 
  ungroup() %>% 
  unique()

#API request from SSB - table 09660, divorces per year
ssb_09660 <- 
  ApiData(09660,
          ContentsCode = "Skillsmisser",
          Tid = TRUE)

divorces <- #Verdiene er kun per år, ikke per kvartal som ellers. Må kanskje finne en løsning
  ssb_09660$dataset %>% 
  select(Tid, value) %>% 
  filter(value != 0)

#API request from SSB - table 12439, absence from work due to illness
ssb_12439 <- 
  ApiData(12439,
          ContentsCode = "SykefravProsent",
          Tid = TRUE,
          Sykefraver2 = "Alt",
          Kjonn = "0")

absence_illness <- 
  ssb_12439$dataset %>% 
  select(Tid, value) %>%
  transmute(
    year = str_split(Tid, "K"),
    quart = as.integer(map(year, `[`, 2)), 
    year = as.integer(map(year, `[`, 1)),
    perc_absence = value)


#### TESTING REGRESSION ####


# merging the three data frames above
reg_df <-
  alc_liters %>% 
  filter(!year %in% c(2002:2003)) %>% 
  inner_join(shopping, by = c("year", "quart")) %>% 
  inner_join(unemployment, by = c("year", "quart")) %>% 
  inner_join(absence_illness, by = c("year", "quart"))


# checking for correlation
cor_mat <- cor(reg_df)

# running a simple regression
result <- lm(k_liters ~ m_nok + q_unemployed + perc_absence, data = reg_df)
stargazer(result, type = "text")



#### BUILDING TEST APP ####

# defining user interface and input variables
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

# defining what to output
server <- function(input, output) {
  
  output$hist <- renderPlot({
    alc_liters %>%
      filter(quart == input$quart) %>%
      filter(year %in% c(input$years[1]:input$years[2])) %>%
      ggplot(aes(x = year, y = k_liters)) +
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

# run app
shinyApp(ui = ui, server = server)
