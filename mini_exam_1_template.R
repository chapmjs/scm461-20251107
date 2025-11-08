#==============================================================================
# Mini Exam 1: Supply Chain Dashboard
# Student Name: [Your Name]
# Date: [Date]
#
# INSTRUCTIONS:
# 1. Complete all sections marked with TODO
# 2. Test thoroughly with different part numbers
# 3. Ensure all calculations are accurate
# 4. Make it professional and user-friendly
#==============================================================================

# Load required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(lubridate)

# Source the function library (provided separately)
source("scm_functions.R")

#==============================================================================
# DATA LOADING
#==============================================================================

# Load all necessary data files
# TODO: Update file paths as needed
part_data <- read_csv("data/part_data.csv")
demand_data <- read_csv("data/demand_data.csv")
supply_data <- read_csv("data/supply_data.csv")

#==============================================================================
# UI DEFINITION
#==============================================================================

ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "Supply Chain Dashboard"),
  
  # Sidebar (can be empty for this exam)
  dashboardSidebar(
    disable = TRUE  # No sidebar needed for single-page dashboard
  ),
  
  # Main body
  dashboardBody(
    
    # Custom CSS for professional appearance
    tags$head(
      tags$style(HTML("
        .info-box { min-height: 90px; }
        .title-box { 
          background-color: #3c8dbc; 
          color: white; 
          padding: 15px; 
          margin-bottom: 20px;
          border-radius: 3px;
        }
        .shortage-warning { 
          background-color: #dd4b39; 
          color: white; 
          padding: 10px; 
          margin: 10px 0;
          border-radius: 3px;
        }
      "))
    ),
    
    # Title section with part number and date
    fluidRow(
      column(12,
        div(class = "title-box",
          h3(textOutput("dashboard_title")),
          h4(textOutput("current_date"))
        )
      )
    ),
    
    # Part selection and key metrics
    fluidRow(
      # Part selection
      box(
        title = "Part Selection",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        selectInput("part_number", 
                   "Select Part Number:",
                   choices = sort(unique(part_data$part_number)),
                   selected = part_data$part_number[1]),
        actionButton("retrieve_data", 
                    "Retrieve Data", 
                    class = "btn-primary",
                    style = "width: 100%; margin-top: 10px;")
      ),
      
      # Key part information
      box(
        title = "Part Information",
        status = "info",
        solidHeader = TRUE,
        width = 8,
        fluidRow(
          valueBoxOutput("part_description", width = 12),
          valueBoxOutput("unit_cost", width = 4),
          valueBoxOutput("on_hand_qty", width = 4),
          valueBoxOutput("lead_time", width = 4)
        )
      )
    ),
    
    # Forecast and supply data
    fluidRow(
      # 12-month forecast
      box(
        title = "12-Month Forecast",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        # TODO: Add tabs for table view and graph view
        DT::dataTableOutput("forecast_table")
      )
    ),
    
    # Supply shortage warnings
    fluidRow(
      box(
        title = "Supply Shortage Warnings",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        
        # Instructions for students
        helpText("This section should flag any projected shortages for the next 6 months."),
        helpText("Compare total supply (inventory + scheduled receipts + capacity) against selected forecast."),
        
        # TODO: Implement shortage warning system
        uiOutput("shortage_warnings")
      )
    ),
    
    # Data update section
    fluidRow(
      box(
        title = "Update Data",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        helpText("Modify values in the green cells above, then click Save/Update to write back to source data."),
        
        actionButton("save_data", 
                    "Save/Update Data", 
                    class = "btn-success",
                    icon = icon("save"))
      )
    )
  )
)

#==============================================================================
# SERVER LOGIC
#==============================================================================

server <- function(input, output, session) {
  
  # Reactive value to store current date (first of month)
  current_date <- reactive({
    floor_date(Sys.Date(), "month")
  })
  
  # Reactive expression for selected part data
  selected_part_data <- reactive({
    # TODO: Filter part_data for selected part
    # Hint: Use input$part_number
    
    part_data %>%
      filter(part_number == input$part_number)
  })
  
  # Reactive expression for demand data
  selected_demand_data <- reactive({
    # TODO: Filter and process demand data for selected part
    # Should return 12 months of forecast
    
    demand_data %>%
      filter(part_number == input$part_number) %>%
      # Add logic to get next 12 months from current_date()
      mutate(month_date = current_date() %m+% months(0:11))
  })
  
  # Reactive expression for supply data
  selected_supply_data <- reactive({
    # TODO: Filter and process supply data for selected part
    
    supply_data %>%
      filter(part_number == input$part_number)
  })
  
  #============================================================================
  # OUTPUTS - Dashboard Title and Date
  #============================================================================
  
  output$dashboard_title <- renderText({
    # TODO: Format as "PART# (DESCRIPTION)"
    # Example: "E1111-11111 (HAPPY MEAL)"
    
    part_info <- selected_part_data()
    paste0(part_info$part_number, " (", toupper(part_info$description), ")")
  })
  
  output$current_date <- renderText({
    # TODO: Format current date nicely
    format(current_date(), "%B %Y")
  })
  
  #============================================================================
  # OUTPUTS - Part Information Value Boxes
  #============================================================================
  
  output$part_description <- renderValueBox({
    part_info <- selected_part_data()
    
    valueBox(
      value = part_info$description,
      subtitle = "Part Description",
      icon = icon("info-circle"),
      color = "light-blue"
    )
  })
  
  output$unit_cost <- renderValueBox({
    # TODO: Get unit cost from selected_part_data()
    # Format as currency
    
    part_info <- selected_part_data()
    
    valueBox(
      value = paste0("$", format(part_info$unit_cost, nsmall = 2)),
      subtitle = "Unit Cost",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$on_hand_qty <- renderValueBox({
    # TODO: Get on-hand quantity from selected_part_data()
    
    part_info <- selected_part_data()
    
    valueBox(
      value = format(part_info$on_hand, big.mark = ","),
      subtitle = "On Hand Quantity",
      icon = icon("boxes"),
      color = "yellow"
    )
  })
  
  output$lead_time <- renderValueBox({
    # TODO: Get lead time from selected_part_data()
    
    part_info <- selected_part_data()
    
    valueBox(
      value = paste(part_info$lead_time, "weeks"),
      subtitle = "Lead Time",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  #============================================================================
  # OUTPUTS - Forecast Table
  #============================================================================
  
  output$forecast_table <- DT::renderDataTable({
    # TODO: Create formatted table with 12 months of forecast
    # Columns should include: Month, High Forecast, Expected Forecast, Low Forecast
    
    demand <- selected_demand_data()
    
    DT::datatable(
      demand,
      options = list(
        pageLength = 12,
        dom = 't',  # Just show table, no search box
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  #============================================================================
  # OUTPUTS - Shortage Warnings
  #============================================================================
  
  output$shortage_warnings <- renderUI({
    # TODO: Implement shortage detection logic
    # Compare supply (inventory + scheduled receipts + capacity) vs. demand
    # Flag shortages by month
    
    # Placeholder logic - students should improve this
    supply <- selected_supply_data()
    demand <- selected_demand_data()
    
    # Calculate shortages (simplified example)
    # TODO: Implement proper supply vs. demand comparison
    
    # If shortages exist, create warning boxes
    # If no shortages, show success message
    
    div(
      class = "alert alert-success",
      icon("check-circle"),
      "No supply shortages projected for the next 6 months."
      # TODO: Replace with actual shortage detection
    )
  })
  
  #============================================================================
  # EVENT HANDLERS
  #============================================================================
  
  # Retrieve Data button
  observeEvent(input$retrieve_data, {
    # TODO: Refresh all displays
    # This should reload data for the selected part
    
    showNotification("Data retrieved successfully!", 
                    type = "message",
                    duration = 2)
  })
  
  # Save/Update Data button
  observeEvent(input$save_data, {
    # TODO: Write modified data back to CSV files
    # This is the "push data back" functionality
    
    # Example:
    # write_csv(updated_data, "data/part_data.csv")
    
    showNotification("Data saved successfully!", 
                    type = "message",
                    duration = 3)
  })
}

#==============================================================================
# RUN THE APP
#==============================================================================

shinyApp(ui = ui, server = server)
