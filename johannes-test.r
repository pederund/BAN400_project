library(shiny)
library(DT)
library(imputeTS)


final_df1 <- final_df %>% 
  select(airline_name, flight_id, scheduled_date, scheduled_time, airport_name,
         check_in, gate, belt, delayed, statusTextEn, origin) %>% 
  rename(Airline = airline_name,
         "Flight ID" = flight_id,
         "Scheduled date" = scheduled_date,
         "Scheduled time" = scheduled_time,
         Airport = airport_name) %>% 
  dplyr::mutate(delayed = replace_na(delayed, "No"))#%>% 
#  transform("Scheduled date" = as.Date("Scheduled date", 
#                                      format = "%y-%m-%d"))
#            "Scheduled time" = as.POSIXct("Scheduled time", 
#                                          format = "%H-%M-%S")
          

departure_df <- final_df1 %>% 
  drop_na(check_in) %>% 
  select(-(belt), -(origin))
  
arrived_df <- final_df1 %>% 
  drop_na(belt) %>% 
  select(-(check_in), -(gate))


ui1 <- fluidPage(
  title = "Avinor flight traffic",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Departure"',
        helpText("Click the column header to sort a column."),
        dateInput("date1", "Schedule time")
      ),
      conditionalPanel(
        'input.dataset === "Arrival"',
        helpText("Bø"),
        dateInput("date1", "Schedule time")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Departure", DT::dataTableOutput("table1")),
        tabPanel("Arrival", DT::dataTableOutput("table2"))
      )
    )
  )
)
  
  
server1 <- function(input, output){
  
  departure = departure_df[sample(nrow(departure_df), 100), ]
  
  output$table1 <- DT::renderDataTable({
    DT::datatable(departure_df)
  })
    
    output$table2 <- DT::renderDataTable({
    DT::datatable(arrived_df)
  })
}
  
#server1 <- function(input, output){
#  output$tbl = renderDT(
#    final_df1, options = list(lengthChange = FALSE))
#  }
  

shinyApp(ui = ui1, server = server1)
