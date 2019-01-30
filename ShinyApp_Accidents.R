# Loading the required libraries
library(shiny)
library(tidyverse)
library(leaflet)

#getwd()

# Loading the dataset
df <- read.csv(file = "Vehicle_Accident_Data.csv")

# Structure of the dataset
str(df)

# Separating the column Location column into 2 columns names Latitude and Longitude
df <- tidyr::separate(
  data = df,
  col = Location,
  into = c("Latitude", "Longitude"),
  sep = ",",
  remove = FALSE
)

# There are parenthesis in the Latitude and Longitude values. Replacing them by blank.
df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")

# Checking the data types of all the columns
sapply(df, class)

# The 2 new columns are "character" type. So, converting them to numeric datatype
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

sapply(df, class)

# Checking if there are any missing values in the relevant columns - No missing values
any(is.na(df[, c("Location", "Fatality", "Hit.And.Run", "Crash.Date.Time")]))

# But seems the missing "Location" values are represented by (0, 0) which is an invalid location in this case
nrow(df[df$Location == "(0, 0)",])

# There are 217 observations with "Location" as (0, 0) - Dropping those observations
df <- df[df$Location != "(0, 0)",]

nrow(df[df$Location == "(0, 0)",])

# "Crash.Date.Time" datatype is 'Factor'. Let's convert it to 'Date' type
df$Crash.Date.Time <-
  as.Date(df$Crash.Date.Time, format = "%m/%d/%Y")


# Define UI for application that shows locations of vehicle accidents on a Map and a Data table below
ui <- fluidPage(
  # Application title
  titlePanel("Locations of Vehicle Accidents"),
  
  # Sidebar with 2 drop-downs and a date-picker
  sidebarLayout(
    sidebarPanel(
      # Select choices for filter "Fatal Accidents"
      selectInput(
        inputId = "Fatal",
        label = "Fatal Accidents:",
        choices = c(
          "Yes"          = TRUE,
          "No"           = FALSE,
          "All"          = "All"
        ),
        selected = "All"
      ),
      # Select choices for filter "Hit and Run"
      selectInput(
        inputId = "HitAndRun",
        label = "Hit and Run",
        choices = c(
          "Yes"          = TRUE,
          "No"           = FALSE,
          "All"          = "All"
        ),
        selected = "All"
      ),
      # Date-picker to filter by date of the crash - Default selected date is today's date
      dateInput(inputId = "Date",
                label = "Accident Date"),
      width = 2
    ),
    
    # Outputs
    mainPanel(
      # Output pane for the map
      leafletOutput("mymap", height = 600, width = 1500),
      br(),
      br(),
      # Output pane for data table below the map
      DT::dataTableOutput(outputId = "accidentstable")
    )
  )
)

# Define server function required to create the Map and Data Table
server <- function(input, output) {
  data <- reactive({
    x <- df
  })
  
  # Creating Map output and customizing it
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(
      data = df %>%
        filter(Fatality == input$Fatal |
                 input$Fatal == 'All') %>%
        filter(Hit.And.Run == input$HitAndRun |
                 input$HitAndRun == 'All')  %>%
        filter(Crash.Date.Time == input$Date |
                 input$Date == Sys.Date())
    ) %>%
      addTiles() %>%
      addMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        
        
        popup = ~ paste(# popup is showing wrong data for the markers. Based on some online blogs, it seems there is a bug in leaflet package
          "Rep",
          df$Report.Number,
          "<br>",
          "Street",
          df$Street.Name,
          "<br>",
          "Date:",
          df$Crash.Date.Time,
          "<br>",
          "Fatality:",
          df$Fatality,
          "<br>",
          "Hit & Run:",
          df$Hit.And.Run
        )
      )
    m
  })
  
  # Create data table to display below the map
  output$accidentstable <- DT::renderDataTable( expr =
    df %>%
      filter(Fatality == input$Fatal |
               input$Fatal == 'All') %>%
      filter(Hit.And.Run == input$HitAndRun |
               input$HitAndRun == 'All') %>%
      filter(Crash.Date.Time == input$Date |
               input$Date == Sys.Date()), caption = 'Data Table of Vehicle Accidents'
  )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
