#==============================================================================
# Mini Exam 3: Forecast Monitoring Dashboard
# Student Name: [Your Name]
# Date: [Date]
#
# INSTRUCTIONS:
# 1. Complete all TODOs
# 2. Implement forecast accuracy calculations
# 3. Create interactive time-series visualizations
# 4. Build data archiving functionality
#==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)

source("scm_functions.R")

# Load data
actual_demand <- read_csv("data/actual_demand.csv")
marketing_forecasts <- read_csv("data/marketing_forecasts.csv")
algorithmic_forecasts <- read_csv("data/algorithmic_forecasts.csv")

#==============================================================================
# UI DEFINITION
#==============================================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Forecast Monitoring"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accuracy/Bias Analysis", tabName = "accuracy", icon = icon("chart-line")),
      menuItem("Historical Retrieval", tabName = "historical", icon = icon("history")),
      menuItem("Current Decision Dashboard", tabName = "current", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #========================================================================
      # TAB 1: ACCURACY/BIAS ANALYSIS
      #========================================================================
      tabItem(tabName = "accuracy",
        
        fluidRow(
          box(
            title = "Forecast Performance Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Compare marketing vs. algorithmic forecasts across different horizons"),
            
            # TODO: Add controls for selecting metrics and horizons
            selectInput("accuracy_metric",
                       "Select Accuracy Metric:",
                       choices = c("MAE" = "mae",
                                 "MAPE" = "mape",
                                 "Bias" = "bias",
                                 "RMSE" = "rmse")),
            
            plotlyOutput("accuracy_comparison_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Accuracy by Horizon",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Table showing accuracy metrics by forecast horizon
            DT::dataTableOutput("accuracy_by_horizon")
          ),
          
          box(
            title = "Bias Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Visual indicators of bias
            plotlyOutput("bias_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Winner by Horizon",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Shows which forecast type performs better at each horizon"),
            
            # TODO: Create visual indicator of which forecast wins at each horizon
            uiOutput("winner_summary")
          )
        )
      ),
      
      #========================================================================
      # TAB 2: HISTORICAL RETRIEVAL
      #========================================================================
      tabItem(tabName = "historical",
        
        fluidRow(
          box(
            title = "Select Historical Period",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Date selector for historical months
            selectInput("historical_month",
                       "Select Forecast Month:",
                       choices = NULL),  # Populated in server
            
            helpText("This will show the 12-month forecasts created in the selected month, along with actual demand.")
          )
        ),
        
        fluidRow(
          box(
            title = "Historical Forecast Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Interactive plot showing both forecasts and actuals
            plotlyOutput("historical_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Forecast Values",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Table of marketing and algorithmic forecasts for selected period
            DT::dataTableOutput("historical_forecasts_table")
          ),
          
          box(
            title = "Actual Demand",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Table of actual demand for comparison
            DT::dataTableOutput("historical_actuals_table")
          )
        )
      ),
      
      #========================================================================
      # TAB 3: CURRENT DECISION DASHBOARD
      #========================================================================
      tabItem(tabName = "current",
        
        fluidRow(
          box(
            title = "Current Month Forecasts",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4(textOutput("current_month_display")),
            
            helpText("This shows the current 12-month forecast extending into the future, along with historical demand.")
          )
        ),
        
        fluidRow(
          box(
            title = "Current Forecast Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Plot showing current forecasts and historical demand
            plotlyOutput("current_forecast_plot", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Current Forecast Values",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Editable table for current forecasts
            helpText("Review and edit the current forecasts before archiving."),
            
            # Note: For editability, consider using rhandsontable package
            DT::dataTableOutput("current_forecast_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Add New Data",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Enter new forecast data and actual demand from last month, then archive."),
            
            fluidRow(
              column(4,
                numericInput("new_actual_demand",
                            "Actual Demand (Last Month):",
                            value = 0,
                            min = 0)
              ),
              
              column(4,
                numericInput("new_marketing_forecast",
                            "Marketing Forecast (Current Month + 12):",
                            value = 0,
                            min = 0)
              ),
              
              column(4,
                numericInput("new_algorithmic_forecast",
                            "Algorithmic Forecast (Current Month + 12):",
                            value = 0,
                            min = 0)
              )
            ),
            
            br(),
            
            actionButton("archive_data",
                        "Archive New Data",
                        class = "btn-success",
                        icon = icon("save"),
                        style = "width: 100%;")
          )
        )
      )
    )
  )
)

#==============================================================================
# SERVER LOGIC
#==============================================================================

server <- function(input, output, session) {
  
  #============================================================================
  # REACTIVE EXPRESSIONS
  #============================================================================
  
  # Current month (first of month)
  current_month <- reactive({
    floor_date(Sys.Date(), "month")
  })
  
  # Calculate accuracy metrics by horizon
  accuracy_metrics <- reactive({
    # TODO: Calculate MAE, MAPE, Bias, RMSE for each forecast type and horizon
    # This is complex - use helper functions from scm_functions.R
    
    # Placeholder structure
    data.frame(
      horizon = 1:12,
      marketing_mae = numeric(12),
      algorithmic_mae = numeric(12),
      marketing_mape = numeric(12),
      algorithmic_mape = numeric(12),
      marketing_bias = numeric(12),
      algorithmic_bias = numeric(12)
    )
  })
  
  # Historical forecasts for selected month
  historical_data <- reactive({
    req(input$historical_month)
    
    # TODO: Extract forecasts created in selected month
    # Join with actual demand that occurred in those future months
    
    selected_date <- as.Date(input$historical_month)
    
    # Return list with marketing_forecast, algorithmic_forecast, actual_demand
    list(
      marketing = numeric(12),
      algorithmic = numeric(12),
      actual = numeric(12)
    )
  })
  
  # Current forecasts
  current_forecasts <- reactive({
    # TODO: Get the 12-month forecast extending from current month
    
    list(
      marketing = numeric(12),
      algorithmic = numeric(12)
    )
  })
  
  #============================================================================
  # UPDATE UI ELEMENTS
  #============================================================================
  
  # Populate historical month choices
  observe({
    # TODO: Get unique months from forecast data
    
    available_months <- seq(floor_date(Sys.Date() - months(24), "month"),
                           floor_date(Sys.Date() - months(1), "month"),
                           by = "month")
    
    updateSelectInput(session, "historical_month",
                     choices = format(available_months, "%Y-%m-%d"),
                     selected = format(available_months[1], "%Y-%m-%d"))
  })
  
  #============================================================================
  # OUTPUTS - Accuracy Analysis
  #============================================================================
  
  output$accuracy_comparison_plot <- renderPlotly({
    # TODO: Create interactive plot comparing forecast accuracy
    
    metrics <- accuracy_metrics()
    metric_name <- input$accuracy_metric
    
    # Example plot structure
    plot_ly() %>%
      add_trace(x = metrics$horizon,
               y = metrics[[paste0("marketing_", metric_name)]],
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Marketing Forecast',
               line = list(color = 'rgb(67, 147, 195)'),
               marker = list(size = 8)) %>%
      add_trace(x = metrics$horizon,
               y = metrics[[paste0("algorithmic_", metric_name)]],
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Algorithmic Forecast',
               line = list(color = 'rgb(255, 127, 14)'),
               marker = list(size = 8)) %>%
      layout(title = paste(toupper(metric_name), "by Forecast Horizon"),
            xaxis = list(title = "Forecast Horizon (Months)"),
            yaxis = list(title = toupper(metric_name)),
            hovermode = 'closest')
  })
  
  output$accuracy_by_horizon <- DT::renderDataTable({
    # TODO: Detailed table of all metrics by horizon
    
    metrics <- accuracy_metrics()
    
    DT::datatable(
      metrics,
      options = list(pageLength = 12),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:ncol(metrics), digits = 2)
  })
  
  output$bias_plot <- renderPlotly({
    # TODO: Visualization of bias (over/under forecasting)
    
    metrics <- accuracy_metrics()
    
    plot_ly(data = metrics) %>%
      add_trace(x = ~horizon,
               y = ~marketing_bias,
               type = 'bar',
               name = 'Marketing',
               marker = list(color = 'rgba(67, 147, 195, 0.7)')) %>%
      add_trace(x = ~horizon,
               y = ~algorithmic_bias,
               type = 'bar',
               name = 'Algorithmic',
               marker = list(color = 'rgba(255, 127, 14, 0.7)')) %>%
      layout(title = "Forecast Bias by Horizon",
            xaxis = list(title = "Horizon"),
            yaxis = list(title = "Bias (Positive = Over-forecast)"),
            barmode = 'group')
  })
  
  output$winner_summary <- renderUI({
    # TODO: Create visual summary showing which forecast wins at each horizon
    
    metrics <- accuracy_metrics()
    
    # Compare based on selected metric
    metric_name <- input$accuracy_metric
    
    # Placeholder
    tagList(
      p("Horizon 1-3: Algorithmic forecast performs better"),
      p("Horizon 4-8: Marketing forecast performs better"),
      p("Horizon 9-12: Algorithmic forecast performs better")
    )
  })
  
  #============================================================================
  # OUTPUTS - Historical Retrieval
  #============================================================================
  
  output$historical_plot <- renderPlotly({
    # TODO: Plot showing selected historical forecasts vs. actuals
    
    hist_data <- historical_data()
    selected_date <- as.Date(input$historical_month)
    
    # Create 12-month sequence from selected date
    months <- seq(selected_date, by = "month", length.out = 12)
    
    plot_ly() %>%
      add_trace(x = months,
               y = hist_data$marketing,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Marketing Forecast',
               line = list(color = 'blue', dash = 'dash')) %>%
      add_trace(x = months,
               y = hist_data$algorithmic,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Algorithmic Forecast',
               line = list(color = 'orange', dash = 'dash')) %>%
      add_trace(x = months,
               y = hist_data$actual,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Actual Demand',
               line = list(color = 'green', width = 3),
               marker = list(size = 10)) %>%
      layout(title = paste("Forecasts vs. Actuals for", format(selected_date, "%B %Y")),
            xaxis = list(title = "Month"),
            yaxis = list(title = "Demand"),
            hovermode = 'x unified')
  })
  
  output$historical_forecasts_table <- DT::renderDataTable({
    # TODO: Table of forecast values
    
    hist_data <- historical_data()
    selected_date <- as.Date(input$historical_month)
    months <- seq(selected_date, by = "month", length.out = 12)
    
    data.frame(
      Month = format(months, "%B %Y"),
      Marketing = hist_data$marketing,
      Algorithmic = hist_data$algorithmic
    ) %>%
      DT::datatable(rownames = FALSE,
                   options = list(pageLength = 12, dom = 't'))
  })
  
  output$historical_actuals_table <- DT::renderDataTable({
    # TODO: Table of actual demand
    
    hist_data <- historical_data()
    selected_date <- as.Date(input$historical_month)
    months <- seq(selected_date, by = "month", length.out = 12)
    
    data.frame(
      Month = format(months, "%B %Y"),
      Actual_Demand = hist_data$actual
    ) %>%
      DT::datatable(rownames = FALSE,
                   options = list(pageLength = 12, dom = 't'))
  })
  
  #============================================================================
  # OUTPUTS - Current Decision Dashboard
  #============================================================================
  
  output$current_month_display <- renderText({
    paste("Current Month:", format(current_month(), "%B %Y"))
  })
  
  output$current_forecast_plot <- renderPlotly({
    # TODO: Plot showing current forecasts + historical demand
    
    current <- current_forecasts()
    current_date <- current_month()
    
    # Historical demand (past 12 months)
    past_months <- seq(current_date - months(12), by = "month", length.out = 12)
    # Future forecast months
    future_months <- seq(current_date, by = "month", length.out = 12)
    
    plot_ly() %>%
      # Historical actuals
      add_trace(x = past_months,
               y = numeric(12),  # TODO: Get actual historical demand
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Historical Demand',
               line = list(color = 'green'),
               marker = list(size = 8)) %>%
      # Current forecasts
      add_trace(x = future_months,
               y = current$marketing,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Marketing Forecast',
               line = list(color = 'blue', dash = 'dash')) %>%
      add_trace(x = future_months,
               y = current$algorithmic,
               type = 'scatter',
               mode = 'lines+markers',
               name = 'Algorithmic Forecast',
               line = list(color = 'orange', dash = 'dash')) %>%
      # Vertical line at current month
      add_trace(x = c(current_date, current_date),
               y = c(0, max(c(current$marketing, current$algorithmic)) * 1.1),
               type = 'scatter',
               mode = 'lines',
               name = 'Current Month',
               line = list(color = 'red', dash = 'dot', width = 2)) %>%
      layout(title = "Current Forecasts with Historical Context",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Demand"),
            hovermode = 'x unified')
  })
  
  output$current_forecast_table <- DT::renderDataTable({
    # TODO: Editable table for current forecasts
    
    current <- current_forecasts()
    current_date <- current_month()
    future_months <- seq(current_date, by = "month", length.out = 12)
    
    data.frame(
      Month = format(future_months, "%B %Y"),
      Marketing_Forecast = current$marketing,
      Algorithmic_Forecast = current$algorithmic
    ) %>%
      DT::datatable(rownames = FALSE,
                   editable = TRUE,
                   options = list(pageLength = 12, dom = 't'))
  })
  
  #============================================================================
  # EVENT HANDLERS
  #============================================================================
  
  observeEvent(input$archive_data, {
    # TODO: Archive new forecast data and actual demand
    # Write to CSV files
    
    new_actual <- input$new_actual_demand
    new_marketing <- input$new_marketing_forecast
    new_algorithmic <- input$new_algorithmic_forecast
    
    # Validation
    if (new_actual < 0 || new_marketing < 0 || new_algorithmic < 0) {
      showNotification("All values must be non-negative!",
                      type = "error",
                      duration = 5)
      return()
    }
    
    # TODO: Append to data files
    # Example:
    # write_csv(new_data, "data/actual_demand.csv", append = TRUE)
    
    showNotification("Data archived successfully!",
                    type = "message",
                    duration = 3)
    
    # Reset inputs
    updateNumericInput(session, "new_actual_demand", value = 0)
    updateNumericInput(session, "new_marketing_forecast", value = 0)
    updateNumericInput(session, "new_algorithmic_forecast", value = 0)
  })
}

#==============================================================================
# RUN THE APP
#==============================================================================

shinyApp(ui = ui, server = server)
