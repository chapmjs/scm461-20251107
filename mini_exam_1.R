# Mini Exam 1: Dashboard Template
# Complete the TODOs to finish the app

library(shiny)
library(shinydashboard)
library(tidyverse)

# Load data (you write the actual loading code)
part_data <- read_csv("data/part_data.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Supply Chain Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Part Selection",
        selectInput("part_number", "Select Part:",
                   choices = unique(part_data$part_number)),
        width = 4
      ),
      box(
        title = "Part Information",
        textOutput("part_description"),
        textOutput("part_cost"),
        # TODO: Add more part info outputs
        width = 8
      )
    ),
    fluidRow(
      box(
        title = "Forecast Display",
        tableOutput("forecast_table"),
        # TODO: Add forecast graph
        width = 12
      )
    ),
    fluidRow(
      box(
        title = "Supply Shortage Warning",
        # TODO: Add shortage warning display
        width = 12
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to get selected part data
  selected_part <- reactive({
    part_data %>%
      filter(part_number == input$part_number)
  })
  
  # Output: Part description
  output$part_description <- renderText({
    # TODO: Extract and display part description
  })
  
  # TODO: Add more outputs
  
}

shinyApp(ui, server)
