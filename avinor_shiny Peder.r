library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyalert)
library(stringr)
library(DT)

first_run <- 1

# defining user interface and input variables
ui <- fluidPage(
  theme = shinytheme("yeti"),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  
  headerPanel(
    h1('Avinor flight data', align = "center")
    ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText('Data will be automatically updated every three minutes.'),
      
      selectInput(inputId = 'airport',
                  label = 'Select airport',
                  choices = avinor_df %>% select(airport_name) %>% arrange(airport_name),
                  selected = avinor_df$airport_name[1]),
      
      
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
      
      tags$a(href = "https://www.avinor.no", "Data from Avinor"),
      
      width = 3),
    
    mainPanel(actionButton("viewButton", "Show earlier flights",
                           style = "color: white; background-color: #84216b;
                                    position:relative;left:350px"),
              
              actionButton("resetviewButton", "Hide earlier flights",
                           style = "color: white; background-color: #84216b;
                                    position:relative;left:350px"),
              br(),
              br(),
              
              tabsetPanel(
                id = 'dataset',
                tabPanel("Arrival",dataTableOutput("table1")),
                tabPanel("Departure", dataTableOutput("table2"))),
              
              br(),
              
              dataTableOutput(outputId = 'df', width = "100%"),
              width = 9)),
  
  tags$style(HTML("
  thead {background-color:#f0f1f3;}
  tbody tr {background-color:white !important}
  .dataTables_wrapper {background-color: #f0f1f3;}
  .tabbable > .nav > li[class=active]  > a {background-color:#f0f1f3;   color:black}
  .tabbable > .nav > li > a {color:black; width: 35vw !important;} 
  "
  ))
  
)


# defining what to output
server <- function(input, output) {
  
  hide("resetviewButton")
  
  #setting the number of rows shown and only showing the table, filtering-box 
  #and also keep pagination control. Also removing the option to order columns.
  #As well as changing the text in some instances.
  options(DT.options = list(pageLength = 30, 
                            dom = "ftp",
                            ordering = FALSE,
                            searchHighlight = TRUE,
                            language = list(zeroRecords = "No flights to display",
                                            search = "Search (city, flight number or airline):")))
  
  #Creating a reactive object, which will be updated every 3 minutes with data from
  #Avinor through running the function to get the data at a set interval
  final_dfReactive <-  reactive({
    invalidateLater(180000)
    if (first_run < 2){
      first_run <<- first_run + 1
      return(final_df)
    } else{
      shinyalert(
        title = "Updating data", text = "This may take a few seconds", type = "info",
        closeOnClickOutside = TRUE, animation = TRUE, timer = 5000,
        showConfirmButton = FALSE
      )
      create_final_df()
    }
  })
  
  #default table output
  output$table1 <- renderDataTable({
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
          scheduled_time >= chron::times(str_sub(Sys.time()-1*60*60, 12))
        }
        else{
          scheduled_time >= chron::times(00:00:00)
        }) %>%
      select(updated_flightstatus ,airport_name, flight_html ,belt_html) %>% 
      rename("Updated status" = updated_flightstatus,
             "From airport" = airport_name,
             "Belt" = belt_html,
             "Flight" = flight_html)
  }, escape = FALSE, rownames = FALSE)
  
  output$table2 <- renderDataTable({
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
          scheduled_time >= chron::times(stringr::str_sub(Sys.time()-1*60*60, 12))
        }
        else{
          scheduled_time >= chron::times(00:00:00)
        }) %>%
      select(updated_flightstatus, airport_name ,flight_html ,gate_html) %>%
      rename("Updated status" = updated_flightstatus,
             "To airport" = airport_name,
             "Gate" = gate_html,
             "Flight" = flight_html)
    
  }, escape = FALSE, rownames = FALSE)
  
  #### If the show earlier flights button is clicked ####
  observeEvent(
    input$viewButton,{
      output$table1 <- renderDataTable({
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
      }, escape = FALSE, rownames = FALSE)
      
      output$table2 <- renderDataTable({
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
        
      }, escape = FALSE, rownames = FALSE)
      hide("viewButton")
      show("resetviewButton")
    })
  
  observeEvent(
    input$resetviewButton,{
      output$table1 <- renderDataTable({
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
              scheduled_time >= chron::times(str_sub(Sys.time()-1*60*60, 12))
            }
            else{
              scheduled_time >= chron::times(00:00:00)
            }) %>%
          select(updated_flightstatus ,airport_name, flight_html ,belt_html) %>% 
          rename("Updated status" = updated_flightstatus,
                 "From airport" = airport_name,
                 "Belt" = belt_html,
                 "Flight" = flight_html)
      }, escape = FALSE, rownames = FALSE)
      
      output$table2 <- renderDataTable({
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
              scheduled_time >= chron::times(stringr::str_sub(Sys.time()-1*60*60, 12))
            }
            else{
              scheduled_time >= chron::times(00:00:00)
            }) %>%
          select(updated_flightstatus, airport_name ,flight_html ,gate_html) %>%
          rename("Updated status" = updated_flightstatus,
                 "To airport" = airport_name,
                 "Gate" = gate_html,
                 "Flight" = flight_html)
        
      }, escape = FALSE, rownames = FALSE)
      hide("resetviewButton")
      show("viewButton")
    })  
  
}

# run app
shinyApp(ui = ui, server = server)
