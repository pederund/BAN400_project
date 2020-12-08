library(shiny)
library(shinyjs)
library(stringr)
library(DT)

# defining user interface and input variables
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel('Flight info position 1'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Flight info position 2'),
      
      selectInput(inputId = 'airport',
                  label = 'Select airport',
                  choices = avinor_df %>% select(airport_name) %>% arrange(airport_name),
                  selected = avinor_df$airport_name[1]),
    
      radioButtons(inputId = 'arrdep',
                   label = "Arrival / Departure",
                   choices = c("Arrival", "Departure"),
                   selected = "Arrival"),
    
      checkboxGroupInput(inputId = "domint",
                         label = "Domestic / International",
                         choices = c("Domestic", "International"),
                         selected = c("Domestic", "International")),
      
      dateInput(inputId = "date",
                label = "Select date",
                min = Sys.Date()-1,
                max = Sys.Date()+1,
                weekstart = 1,
                language = "nb",
                format = "dd M. yyyy"),
      width = 3),
    
    mainPanel(actionButton("viewButton", "Show earlier flights",
                           style = "color: white; background-color: #84216b;
                                    position:relative;left:350px"),
              actionButton("resetviewButton", "Hide earlier flights",
                           style = "color: white; background-color: #84216b;
                                    position:relative;left:350px"),
              tags$style(HTML("
                              thead {background-color:#f0f1f3;}
                              table.dataTable tbody tr {background-color:white !important}"
                              )),
              dataTableOutput(outputId = 'df'),
              width = 9)))

# defining what to output
server <- function(input, output) {
  
  hide("resetviewButton")
  
  #setting the number of rows shown and only showing the table, filtering-box 
  #and also keep pagination control. Also removing the option to order columns.
  #As well as changing the text in some instances.
  options(DT.options = list(pageLength = 30, 
                            dom = "ftp",
                            ordering = FALSE,
                            language = list(zeroRecords = "No flights to display",
                                            search = "Search (city, flight number or airline):")))
  
  #Creating a reactive object, which will be updated every 3 minutes with data from
  #Avinor through running the function to get the data at a set interval
  final_dfReactive <-  reactive({
    invalidateLater(30000)
    run_function()
  })
  
  #default table output
  output$df <- renderDataTable({
    if(input$arrdep == "Arrival"){
      final_dfReactive() %>%
        filter(origin == as.character(
          avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
        filter(arr_dep == "A") %>% 
        filter(dom_int %in%
                 if(length(input$domint) == 2 | length(input$domint) == 0) {
                   c("D", "S", "I")
                 } else if(input$domint == "International") {
                   c("S", "I")
                 } else {
                   "D"
                 }) %>%
        filter(scheduled_date == input$date) %>%
        filter(
          if(input$date == Sys.Date()){
            scheduled_time >= chron::times(str_sub(Sys.time(), 12))
          }
          else{
            scheduled_time >= chron::times(00:00:00)
          }) %>%
        select(updated_flightstatus ,airport_name, flight_html ,belt_html) %>% 
        rename("Updated status" = updated_flightstatus,
               "From airport" = airport_name,
               "Belt" = belt_html,
               "Flight" = flight_html)
    }
    else{
      final_dfReactive() %>%
        filter(origin == as.character(
          avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
        filter(arr_dep == "D") %>% 
        filter(dom_int %in%
                 if(length(input$domint) == 2 | length(input$domint) == 0) {
                   c("D", "S", "I")
                 } else if(input$domint == "International") {
                   c("S", "I")
                 } else {
                   "D"
                 }) %>%
        filter(scheduled_date == input$date) %>%
        filter(
          if(input$date == Sys.Date()){
            scheduled_time >= chron::times(stringr::str_sub(Sys.time(), 12))
          }
          else{
            scheduled_time >= chron::times(00:00:00)
          }) %>%
        select(updated_flightstatus, airport_name ,flight_html ,gate_html) %>%
        rename("Updated status" = updated_flightstatus,
               "To airport" = airport_name,
               "Gate" = gate_html,
               "Flight" = flight_html)
    }
    
  }, escape = FALSE, rownames = FALSE) 
  
#### If the show earlier flights button is clicked ####
  observeEvent(
    input$viewButton,{
      output$df <- renderDataTable({
        if(input$arrdep == "Arrival"){
          final_dfReactive() %>%
            filter(origin == as.character(
              avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
            filter(arr_dep == "A") %>% 
            filter(dom_int %in%
                     if(length(input$domint) == 2 | length(input$domint) == 0) {
                       c("D", "S", "I")
                     } else if(input$domint == "International") {
                       c("S", "I")
                     } else {
                       "D"
                     }) %>%
            filter(scheduled_date == input$date) %>%
            select(updated_flightstatus ,airport_name, flight_html ,belt_html) %>% 
            rename("Updated status" = updated_flightstatus,
                   "From airport" = airport_name,
                   "Belt" = belt_html,
                   "Flight" = flight_html)
        }
        else{
          final_dfReactive() %>%
            filter(origin == as.character(
              avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
            filter(arr_dep == "D") %>% 
            filter(dom_int %in%
                     if(length(input$domint) == 2 | length(input$domint) == 0) {
                       c("D", "S", "I")
                     } else if(input$domint == "International") {
                       c("S", "I")
                     } else {
                       "D"
                     }) %>%
            filter(scheduled_date == input$date) %>%
            select(updated_flightstatus, airport_name ,flight_html ,gate_html) %>%
            rename("Updated status" = updated_flightstatus,
                   "To airport" = airport_name,
                   "Gate" = gate_html,
                   "Flight" = flight_html)
        }
        
      }, escape = FALSE, rownames = FALSE)
      hide("viewButton")
      show("resetviewButton")
    })
  observeEvent(
    input$resetviewButton,{
      output$df <- renderDataTable({
        if(input$arrdep == "Arrival"){
          final_dfReactive() %>%
            filter(origin == as.character(
              avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
            filter(arr_dep == "A") %>% 
            filter(dom_int %in%
                     if(length(input$domint) == 2 | length(input$domint) == 0) {
                       c("D", "S", "I")
                     } else if(input$domint == "International") {
                       c("S", "I")
                     } else {
                       "D"
                     }) %>%
            filter(scheduled_date == input$date) %>%
            filter(
              if(input$date == Sys.Date()){
                scheduled_time >= chron::times(str_sub(Sys.time(), 12))
              }
              else{
                scheduled_time >= chron::times(00:00:00)
              }) %>%
            select(updated_flightstatus ,airport_name, flight_html ,belt_html) %>% 
            rename("Updated status" = updated_flightstatus,
                   "From airport" = airport_name,
                   "Belt" = belt_html,
                   "Flight" = flight_html)
        }
        else{
          final_dfReactive() %>%
            filter(origin == as.character(
              avinor_df[(avinor_df$airport_name == input$airport), ][1])) %>% 
            filter(arr_dep == "D") %>% 
            filter(dom_int %in%
                     if(length(input$domint) == 2 | length(input$domint) == 0) {
                       c("D", "S", "I")
                     } else if(input$domint == "International") {
                       c("S", "I")
                     } else {
                       "D"
                     }) %>%
            filter(scheduled_date == input$date) %>% 
            filter(
              if(input$date == Sys.Date()){
                scheduled_time >= chron::times(stringr::str_sub(Sys.time(), 12))
              }
              else{
                scheduled_time >= chron::times(00:00:00)
              }) %>%
            select(updated_flightstatus, airport_name ,flight_html ,gate_html) %>%
            rename("Updated status" = updated_flightstatus,
                   "To airport" = airport_name,
                   "Gate" = gate_html,
                   "Flight" = flight_html)
        }
        
      }, escape = FALSE, rownames = FALSE)
      hide("resetviewButton")
      show("viewButton")
    })
}

# run app
shinyApp(ui = ui, server = server)
