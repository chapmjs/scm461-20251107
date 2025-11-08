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
library(plotly)

# Source the function library (provided separately)
# source("scm_functions.R")

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
        .editable-cell {
          background-color: #90EE90 !important;
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
        
        # TODO: Add tabs for table view and graph view - COMPLETED
        tabsetPanel(
          tabPanel("Table View",
                   DT::dataTableOutput("forecast_table")
          ),
          tabPanel("Graph View",
                   plotlyOutput("forecast_plot", height = "400px")
          )
        )
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
        
        # TODO: Implement shortage warning system - COMPLETED
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
  
  # Store modified data
  values <- reactiveValues(
    part_data_modified = NULL,
    demand_data_modified = NULL,
    supply_data_modified = NULL
  )
  
  # Initialize reactive values
  observe({
    values$part_data_modified <- part_data
    values$demand_data_modified <- demand_data
    values$supply_data_modified <- supply_data
  })
  
  # Reactive value to store current date (first of month)
  current_date <- reactive({
    floor_date(Sys.Date(), "month")
  })
  
  # Reactive expression for selected part data
  selected_part_data <- reactive({
    # TODO: Filter part_data for selected part - COMPLETED
    req(input$part_number)
    
    values$part_data_modified %>%
      filter(part_number == input$part_number) %>%
      slice(1)  # Ensure single row
  })
  
  # Reactive expression for demand data
  selected_demand_data <- reactive({
    # TODO: Filter and process demand data for selected part - COMPLETED
    req(input$part_number)
    
    # Create 12-month forecast from current date
    months_ahead <- 0:11
    forecast_dates <- current_date() %m+% months(months_ahead)
    
    # Filter demand data for selected part
    part_demand <- values$demand_data_modified %>%
      filter(part_number == input$part_number)
    
    # Create forecast table
    if(nrow(part_demand) > 0) {
      forecast_df <- data.frame(
        Month = format(forecast_dates, "%B %Y"),
        Month_Date = forecast_dates,
        High_Forecast = part_demand$high_forecast[1:12],
        Expected_Forecast = part_demand$expected_forecast[1:12],
        Low_Forecast = part_demand$low_forecast[1:12]
      )
    } else {
      # Generate dummy data if no demand data exists
      forecast_df <- data.frame(
        Month = format(forecast_dates, "%B %Y"),
        Month_Date = forecast_dates,
        High_Forecast = round(runif(12, 800, 1200)),
        Expected_Forecast = round(runif(12, 600, 1000)),
        Low_Forecast = round(runif(12, 400, 800))
      )
    }
    
    return(forecast_df)
  })
  
  # Reactive expression for supply data
  selected_supply_data <- reactive({
    # TODO: Filter and process supply data for selected part - COMPLETED
    req(input$part_number)
    
    values$supply_data_modified %>%
      filter(part_number == input$part_number)
  })
  
  #============================================================================
  # OUTPUTS - Dashboard Title and Date
  #============================================================================
  
  output$dashboard_title <- renderText({
    # TODO: Format as "PART# (DESCRIPTION)" - COMPLETED
    req(selected_part_data())
    
    part_info <- selected_part_data()
    if(nrow(part_info) > 0) {
      paste0(part_info$part_number, " (", toupper(part_info$description), ")")
    } else {
      "No part selected"
    }
  })
  
  output$current_date <- renderText({
    # TODO: Format current date nicely - COMPLETED
    format(current_date(), "%B %d, %Y")
  })
  
  #============================================================================
  # OUTPUTS - Part Information Value Boxes
  #============================================================================
  
  output$part_description <- renderValueBox({
    part_info <- selected_part_data()
    
    valueBox(
      value = if(nrow(part_info) > 0) part_info$description else "N/A",
      subtitle = "Part Description",
      icon = icon("info-circle"),
      color = "light-blue"
    )
  })
  
  output$unit_cost <- renderValueBox({
    # TODO: Get unit cost from selected_part_data() - COMPLETED
    part_info <- selected_part_data()
    
    valueBox(
      value = if(nrow(part_info) > 0) {
        paste0("$", formatC(part_info$unit_cost, format = "f", digits = 2, big.mark = ","))
      } else {
        "$0.00"
      },
      subtitle = "Unit Cost",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$on_hand_qty <- renderValueBox({
    # TODO: Get on-hand quantity from selected_part_data() - COMPLETED
    part_info <- selected_part_data()
    
    valueBox(
      value = if(nrow(part_info) > 0) {
        format(part_info$on_hand, big.mark = ",")
      } else {
        "0"
      },
      subtitle = "On Hand Quantity",
      icon = icon("boxes"),
      color = "yellow"
    )
  })
  
  output$lead_time <- renderValueBox({
    # TODO: Get lead time from selected_part_data() - COMPLETED
    part_info <- selected_part_data()
    
    valueBox(
      value = if(nrow(part_info) > 0) {
        paste(part_info$lead_time, "weeks")
      } else {
        "0 weeks"
      },
      subtitle = "Lead Time",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  #============================================================================
  # OUTPUTS - Forecast Table
  #============================================================================
  
  output$forecast_table <- DT::renderDataTable({
    # TODO: Create formatted table with 12 months of forecast - COMPLETED
    demand <- selected_demand_data()
    
    # Format the table for display (without Month_Date column)
    display_data <- demand %>%
      select(Month, High_Forecast, Expected_Forecast, Low_Forecast)
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 12,
        dom = 't',  # Just show table, no search box
        ordering = FALSE,
        columnDefs = list(
          list(className = 'text-center', targets = 1:3),
          list(className = 'editable-cell', targets = 1:3)
        )
      ),
      rownames = FALSE,
      editable = list(
        target = 'cell',
        disable = list(columns = 0)  # Month column not editable
      )
    ) %>%
      formatRound(columns = c("High_Forecast", "Expected_Forecast", "Low_Forecast"), digits = 0)
  })
  
  # Handle table edits
  observeEvent(input$forecast_table_cell_edit, {
    info <- input$forecast_table_cell_edit
    row_num <- info$row
    col_num <- info$col
    new_value <- info$value
    
    # Update the demand data
    demand <- selected_demand_data()
    col_names <- c("Month", "High_Forecast", "Expected_Forecast", "Low_Forecast")
    
    # Update the reactive values
    if(col_num > 0) {
      part_num <- input$part_number
      col_to_update <- col_names[col_num + 1]
      
      # Update in the modified data
      idx <- which(values$demand_data_modified$part_number == part_num)
      if(length(idx) >= row_num) {
        values$demand_data_modified[idx[row_num], col_to_update] <- as.numeric(new_value)
      }
    }
  })
  
  #============================================================================
  # OUTPUTS - Forecast Plot
  #============================================================================
  
  output$forecast_plot <- renderPlotly({
    demand <- selected_demand_data()
    
    plot_ly(demand, x = ~Month_Date) %>%
      add_trace(y = ~High_Forecast, name = "High Forecast", 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = 'rgba(255, 0, 0, 0.8)'),
                marker = list(color = 'rgba(255, 0, 0, 0.8)')) %>%
      add_trace(y = ~Expected_Forecast, name = "Expected Forecast", 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = 'rgba(0, 100, 200, 0.8)'),
                marker = list(color = 'rgba(0, 100, 200, 0.8)')) %>%
      add_trace(y = ~Low_Forecast, name = "Low Forecast", 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = 'rgba(0, 200, 0, 0.8)'),
                marker = list(color = 'rgba(0, 200, 0, 0.8)')) %>%
      layout(title = "12-Month Demand Forecast",
             xaxis = list(title = "Month", tickformat = "%b %Y"),
             yaxis = list(title = "Quantity"),
             hovermode = "x unified")
  })
  
  #============================================================================
  # OUTPUTS - Shortage Warnings
  #============================================================================
  
  output$shortage_warnings <- renderUI({
    # TODO: Implement shortage detection logic - COMPLETED
    supply <- selected_supply_data()
    demand <- selected_demand_data()
    part_info <- selected_part_data()
    
    # Get first 6 months of demand
    six_month_demand <- demand[1:6,]
    
    # Calculate total available supply
    on_hand <- ifelse(nrow(part_info) > 0, part_info$on_hand, 0)
    
    # Calculate scheduled receipts and capacity (simplified)
    scheduled_receipts <- if(nrow(supply) > 0) {
      sum(supply$scheduled_receipts, na.rm = TRUE)
    } else {
      0
    }
    
    monthly_capacity <- if(nrow(supply) > 0 && !is.null(supply$monthly_capacity)) {
      supply$monthly_capacity[1] * 6  # 6 months of capacity
    } else {
      2000  # Default capacity
    }
    
    total_supply <- on_hand + scheduled_receipts + monthly_capacity
    
    # Check for shortages month by month
    shortage_list <- list()
    cumulative_supply <- on_hand
    
    for(i in 1:6) {
      month_demand <- six_month_demand$Expected_Forecast[i]
      month_name <- six_month_demand$Month[i]
      
      # Add monthly production capacity
      cumulative_supply <- cumulative_supply + (monthly_capacity / 6)
      
      # Check if shortage exists
      if(cumulative_supply < month_demand) {
        shortage_amount <- month_demand - cumulative_supply
        shortage_list[[length(shortage_list) + 1]] <- 
          div(class = "shortage-warning",
              icon("exclamation-triangle"),
              paste0(" WARNING: Shortage of ", 
                     format(round(shortage_amount), big.mark = ","),
                     " units projected for ", month_name))
      }
      
      # Deduct demand from supply
      cumulative_supply <- cumulative_supply - month_demand
    }
    
    # Return warnings or success message
    if(length(shortage_list) > 0) {
      do.call(tagList, shortage_list)
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        " No supply shortages projected for the next 6 months."
      )
    }
  })
  
  #============================================================================
  # EVENT HANDLERS
  #============================================================================
  
  # Retrieve Data button
  observeEvent(input$retrieve_data, {
    # TODO: Refresh all displays - COMPLETED
    
    # Force refresh of reactive expressions
    session$sendCustomMessage(type = 'resetValue', message = "part_number")
    
    # Re-read data files if they exist
    tryCatch({
      values$part_data_modified <- read_csv("data/part_data.csv", show_col_types = FALSE)
      values$demand_data_modified <- read_csv("data/demand_data.csv", show_col_types = FALSE)
      values$supply_data_modified <- read_csv("data/supply_data.csv", show_col_types = FALSE)
      
      showNotification("Data retrieved successfully!", 
                       type = "success",
                       duration = 2)
    }, error = function(e) {
      showNotification("Error retrieving data. Using current data.", 
                       type = "warning",
                       duration = 3)
    })
  })
  
  # Save/Update Data button
  observeEvent(input$save_data, {
    # TODO: Write modified data back to CSV files - COMPLETED
    
    tryCatch({
      # Create data directory if it doesn't exist
      if(!dir.exists("data")) {
        dir.create("data")
      }
      
      # Write modified data back to CSV files
      write_csv(values$part_data_modified, "data/part_data.csv")
      write_csv(values$demand_data_modified, "data/demand_data.csv")
      write_csv(values$supply_data_modified, "data/supply_data.csv")
      
      showNotification("Data saved successfully!", 
                       type = "success",
                       duration = 3)
    }, error = function(e) {
      showNotification(paste("Error saving data:", e$message), 
                       type = "error",
                       duration = 5)
    })
  })
}

#==============================================================================
# RUN THE APP
#==============================================================================

shinyApp(ui = ui, server = server)