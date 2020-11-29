library(shiny)
library(shinyjs)

# defining user interface and input variables
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel('Flight info position 1'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Flight info position 2'),
      
      selectInput(inputId = 'airport',
                  label = 'Select airport',
                  choices = avinor_df$airport_name,
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
      
      actionButton("refreshButton", "Refresh"),
      p("Data can only be refreshed every 3 minutes"),
      
      width = 3),
    
    mainPanel(actionButton("viewButton", "Show earlier flights"),
              dataTableOutput(outputId = 'df'),
              width = 9)))

# defining what to output
server <- function(input, output) {

  output$df <- renderDataTable({
    if(input$arrdep == "Arrival"){
      final_df %>%
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
        mutate(scheduled_time1 = 
                 paste0("<b>", stringr::str_sub(scheduled_time,1,5), "</b>")) %>% 
        unite(
          updated_timestatus, c(status_text_EN, updated_time),
          sep = " ", remove = FALSE, na.rm = TRUE) %>%
        unite(updated_flightstatus, c(scheduled_time1, updated_timestatus),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
        mutate(flight_id = paste0("<b>",flight_id,"</b>" )) %>% 
        unite(
          Flight, c(flight_id, airline_name),
          sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
        mutate(
          belt = if_else(
            is.na(belt), belt, paste0("<b>","Belt ", belt, "</b>"))) %>% 
        filter(
          if(input$date == Sys.Date()){
            scheduled_time >= times(stringr::str_sub(Sys.time(), 12))
          }
          else{
            scheduled_time >= times(00:00:00)
          }) %>%
        select(
          updated_flightstatus ,airport_name, Flight ,belt) %>% 
        rename("Updated status" = updated_flightstatus,
               "From airport" = airport_name,
               "Belt" = belt)
    }
    else{
      final_df %>%
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
        unite(
          updated_timestatus, c(status_text_EN, updated_time),
          sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
        unite(
          Flight, c(flight_id, airline_name),
          sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
        select(
          scheduled_time, updated_timestatus, airport_name ,Flight ,gate) %>% 
        mutate(gate = if_else(
          is.na(gate), gate, paste0("Gate ", gate)
        )) %>%
        filter(
          if(input$date == Sys.Date()){
            scheduled_time >= times(stringr::str_sub(Sys.time(), 12))
          }
          else{
            scheduled_time >= times(00:00:00)
          }
        ) %>%
        rename("Scheduled time" = scheduled_time,
               "Updated status" = updated_timestatus,
               "To airport" = airport_name,
               "Gate" = gate)
    }
    
  }, escape = FALSE)
  
####If the refreshbutton is clicked####  
  observeEvent(
    input$refreshButton,{
      #run the function to update final_df
      run_function() 
      #and refresh the output in the shiny app
      output$df <- renderDataTable({
        if(input$arrdep == "Arrival"){
          final_df %>%
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
            unite(
              updated_timestatus, c(status_text_EN, updated_time),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            unite(
              Flight, c(flight_id, airline_name),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            select(
              scheduled_time, updated_timestatus ,airport_name, Flight ,belt) %>% 
            mutate(belt = if_else(
              is.na(belt), belt, paste0("Belt ", belt)
            )) %>% 
            filter(
              if(input$date == Sys.Date()){
                scheduled_time >= times(stringr::str_sub(Sys.time(), 12))
              }
              else{
                scheduled_time >= times(00:00:00)
              }) %>%
            rename("Scheduled time" = scheduled_time,
                   "Updated status" = updated_timestatus,
                   "From airport" = airport_name,
                   "Belt" = belt)
        }
        else{
          final_df %>%
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
            unite(
              updated_timestatus, c(status_text_EN, updated_time),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            unite(
              Flight, c(flight_id, airline_name),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            select(
              scheduled_time, updated_timestatus, airport_name ,Flight ,gate) %>% 
            mutate(gate = if_else(
              is.na(gate), gate, paste0("Gate ", gate)
            )) %>%
            filter(
              if(input$date == Sys.Date()){
                scheduled_time >= times(stringr::str_sub(Sys.time(), 12))
              }
              else{
                scheduled_time >= times(00:00:00)
              }
            ) %>%
            rename("Scheduled time" = scheduled_time,
                   "Updated status" = updated_timestatus,
                   "To airport" = airport_name,
                   "Gate" = gate)
        }
        
      }, escape = FALSE)
      shinyjs::delay(180000,enable("refreshButton"))
      shinyjs::disable("refreshButton")
      })
  
#### If the show earlier flights button is clicked ####
  observeEvent(
    input$viewButton,{
      output$df <- renderDataTable({
        if(input$arrdep == "Arrival"){
          final_df %>%
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
            unite(
              updated_timestatus, c(status_text_EN, updated_time),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            unite(
              Flight, c(flight_id, airline_name),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            select(
              scheduled_time, updated_timestatus ,airport_name, Flight ,belt) %>% 
            mutate(belt = if_else(
              is.na(belt), belt, paste0("Belt ", belt)
            )) %>% 
            rename("Scheduled time" = scheduled_time,
                   "Updated status" = updated_timestatus,
                   "From airport" = airport_name,
                   "Belt" = belt)
        }
        else{
          final_df %>%
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
            unite(
              updated_timestatus, c(status_text_EN, updated_time),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            unite(
              Flight, c(flight_id, airline_name),
              sep = "<br>", remove = FALSE, na.rm = TRUE) %>% 
            select(
              scheduled_time, updated_timestatus, airport_name ,Flight ,gate) %>% 
            mutate(gate = if_else(
              is.na(gate), gate, paste0("Gate ", gate)
            ))%>%
            rename("Scheduled time" = scheduled_time,
                   "Updated status" = updated_timestatus,
                   "To airport" = airport_name,
                   "Gate" = gate)
        }
        
      }, escape = FALSE)
    })
  
}


# run app
shinyApp(ui = ui, server = server)
