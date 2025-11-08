#==============================================================================
# MEGA EXAM: Integrated Café Supply Chain Management System
# Student Name: [Your Name]
# Date: [Date]
#
# This application integrates forecasting, planning, inventory, and purchasing
# modules into a comprehensive supply chain management system.
#==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)
library(lubridate)

# Source function libraries
source("scm_functions.R")
source("mrp_functions.R")  # Additional MRP-specific functions

# Load all data
menu_items <- read_csv("data/menu_items.csv")  # 5 menu items
ingredients <- read_csv("data/ingredients.csv")  # All ingredients
bom <- read_csv("data/bom.csv")  # Bill of materials
forecast_data <- read_csv("data/forecasts.csv")
production_plans <- read_csv("data/production_plans.csv")
inventory <- read_csv("data/inventory.csv")
scheduled_receipts <- read_csv("data/scheduled_receipts.csv")

#==============================================================================
# UI DEFINITION
#==============================================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Café Supply Chain System"),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Planning", tabName = "planning", icon = icon("calendar-alt")),
      menuItem("Inventory", tabName = "inventory", icon = icon("boxes")),
      menuItem("Purchasing (MRP)", tabName = "mrp", icon = icon("shopping-cart")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .main-header .logo { font-weight: bold; }
        .content-wrapper { background-color: #ecf0f5; }
        .alert-custom { padding: 15px; margin-bottom: 20px; border-radius: 4px; }
        .alert-danger-custom { background-color: #f2dede; border: 1px solid #ebccd1; }
        .alert-success-custom { background-color: #dff0d8; border: 1px solid #d6e9c6; }
      "))
    ),
    
    tabItems(
      
      #========================================================================
      # HOME TAB
      #========================================================================
      tabItem(tabName = "home",
        fluidRow(
          box(
            title = "Welcome to Café Supply Chain Management",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h3("System Overview"),
            p("This integrated supply chain management system helps manage all operational aspects of the café."),
            
            h4("Modules:"),
            tags$ul(
              tags$li(strong("Forecasting:"), "Manage demand forecasts and track accuracy"),
              tags$li(strong("Planning:"), "Create production plans and identify supply constraints"),
              tags$li(strong("Inventory:"), "Optimize safety stock and identify risks"),
              tags$li(strong("Purchasing (MRP):"), "Generate purchase orders using MRP logic")
            ),
            
            h4("Navigation:"),
            p("Use the sidebar menu to access each module. Data flows between modules automatically."),
            
            br(),
            
            actionButton("start_forecasting", 
                        "Get Started with Forecasting",
                        class = "btn-primary btn-lg",
                        icon = icon("arrow-right"))
          )
        ),
        
        fluidRow(
          valueBoxOutput("current_week_box", width = 4),
          valueBoxOutput("data_status_box", width = 4),
          valueBoxOutput("alerts_box", width = 4)
        )
      ),
      
      #========================================================================
      # FORECASTING MODULE
      #========================================================================
      tabItem(tabName = "forecasting",
        
        h2("Forecasting Module"),
        
        # Data entry section
        fluidRow(
          box(
            title = "Enter New Forecast Data",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            helpText("At the end of each week, enter historical demand and 6-week forecast for each menu item."),
            
            fluidRow(
              column(3,
                h4("Menu Item")
              ),
              column(3,
                h4("Historical Demand (Last Week)")
              ),
              column(6,
                h4("6-Week Forecast")
              )
            ),
            
            hr(),
            
            # TODO: Create input fields for each of 5 menu items
            # Burgers
            fluidRow(
              column(3, p(strong("Burgers"))),
              column(3, numericInput("burger_demand", NULL, value = 0, min = 0)),
              column(6, 
                fluidRow(
                  column(2, numericInput("burger_fc_wk1", "Wk1", value = 0, min = 0)),
                  column(2, numericInput("burger_fc_wk2", "Wk2", value = 0, min = 0)),
                  column(2, numericInput("burger_fc_wk3", "Wk3", value = 0, min = 0)),
                  column(2, numericInput("burger_fc_wk4", "Wk4", value = 0, min = 0)),
                  column(2, numericInput("burger_fc_wk5", "Wk5", value = 0, min = 0)),
                  column(2, numericInput("burger_fc_wk6", "Wk6", value = 0, min = 0))
                )
              )
            ),
            
            # TODO: Repeat for Sub Sandwiches, Tacos, Spaghetti, Pizza
            
            br(),
            
            actionButton("archive_forecast",
                        "Archive Forecast Data",
                        class = "btn-success",
                        icon = icon("save"),
                        style = "width: 100%;")
          )
        ),
        
        # Visualization section
        fluidRow(
          box(
            title = "Forecast Visualization",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Interactive graph showing forecasts over time
            plotlyOutput("forecast_viz", height = "400px")
          )
        ),
        
        # Accuracy tracking
        fluidRow(
          box(
            title = "Forecast Accuracy Tracking",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Display MAE, MAPE, Bias by menu item
            DT::dataTableOutput("forecast_accuracy_table")
          )
        )
      ),
      
      #========================================================================
      # PLANNING MODULE
      #========================================================================
      tabItem(tabName = "planning",
        
        h2("Planning Module"),
        
        # Current forecast display
        fluidRow(
          box(
            title = "6-Week Forecast",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Graph showing current 6-week forecast for all items
            plotlyOutput("planning_forecast_graph", height = "300px")
          )
        ),
        
        # Production plan input
        fluidRow(
          box(
            title = "Enter Production Plan",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Enter your planned production for each menu item for the next 6 weeks."),
            
            # TODO: Editable table for production plan
            # Consider using rhandsontable for easy editing
            
            DT::dataTableOutput("production_plan_table"),
            
            br(),
            
            actionButton("save_production_plan",
                        "Save Production Plan",
                        class = "btn-primary",
                        icon = icon("save"))
          )
        ),
        
        # Feasibility analysis
        fluidRow(
          box(
            title = "Supply Feasibility Analysis",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            
            helpText("This section compares your production plan against available supply."),
            
            # TODO: Show supply gaps by ingredient and week
            # Red flags for shortages
            
            uiOutput("supply_shortage_alerts"),
            
            br(),
            
            DT::dataTableOutput("supply_analysis_table")
          )
        )
      ),
      
      #========================================================================
      # INVENTORY MODULE
      #========================================================================
      tabItem(tabName = "inventory",
        
        h2("Inventory Module"),
        
        # Critical ingredient identification
        fluidRow(
          box(
            title = "Critical Ingredient Analysis (ABC)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Pareto chart showing ingredient importance
            plotlyOutput("abc_pareto", height = "400px")
          )
        ),
        
        # Safety stock optimization
        fluidRow(
          box(
            title = "Safety Stock Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Adjust service levels and safety stock for each ingredient."),
            
            # TODO: Table with sliders/inputs for each ingredient
            # Show cost/service tradeoffs
            
            DT::dataTableOutput("safety_stock_table")
          )
        ),
        
        # DOS analysis
        fluidRow(
          box(
            title = "Days of Supply (DOS) Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Chart showing current DOS vs. target
            plotlyOutput("dos_chart")
          ),
          
          box(
            title = "Inventory Risk Flags",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            # TODO: Table of high-risk ingredients
            # Flag: DOS > 60 (excess) or DOS < 15 (shortage risk)
            
            DT::dataTableOutput("risk_flags_table")
          )
        )
      ),
      
      #========================================================================
      # PURCHASING (MRP) MODULE
      #========================================================================
      tabItem(tabName = "mrp",
        
        h2("Purchasing Module (MRP)"),
        
        # Ingredient selection
        fluidRow(
          box(
            title = "MRP Report Generator",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            selectInput("mrp_ingredient",
                       "Select Ingredient:",
                       choices = NULL),  # Populated from ingredients data
            
            actionButton("generate_mrp",
                        "Generate MRP Report",
                        class = "btn-primary",
                        icon = icon("calculator"))
          )
        ),
        
        # MRP report display
        fluidRow(
          box(
            title = "MRP Report",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            helpText("Material Requirements Planning (MRP) report showing week-by-week requirements."),
            
            # TODO: MRP table showing:
            # - Gross Requirements
            # - Scheduled Receipts
            # - On Hand (beginning/ending)
            # - Net Requirements
            # - Planned Order Receipts
            # - Planned Order Releases
            
            DT::dataTableOutput("mrp_report_table")
          )
        ),
        
        # Summary of all purchases needed
        fluidRow(
          box(
            title = "Purchase Order Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Summary table of all ingredients needing orders
            # Show: Ingredient, Week, Qty to Order, Total $
            
            DT::dataTableOutput("purchase_summary_table"),
            
            br(),
            
            downloadButton("download_purchase_orders", 
                          "Download Purchase Orders (CSV)",
                          class = "btn-success")
          )
        )
      ),
      
      #========================================================================
      # HELP TAB
      #========================================================================
      tabItem(tabName = "help",
        
        h2("Help & Documentation"),
        
        fluidRow(
          box(
            title = "How to Use This System",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            h3("Weekly Workflow:"),
            tags$ol(
              tags$li(strong("Forecasting:"), "At end of week, enter historical demand and create 6-week forecast"),
              tags$li(strong("Planning:"), "Review forecast, enter production plan, check for supply issues"),
              tags$li(strong("Inventory:"), "Review safety stock settings, monitor DOS levels"),
              tags$li(strong("Purchasing:"), "Generate MRP reports, create purchase orders")
            ),
            
            h3("Tips:"),
            tags$ul(
              tags$li("Data automatically flows between modules"),
              tags$li("Red flags indicate action needed"),
              tags$li("Save your work frequently"),
              tags$li("Review accuracy metrics weekly to improve forecasting")
            ),
            
            h3("Data Files:"),
            p("All data is stored in CSV files in the 'data/' directory. 
               The system automatically reads and writes to these files.")
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
  # REACTIVE VALUES
  #============================================================================
  
  # Shared reactive values across all modules
  app_data <- reactiveValues(
    current_week = 1,
    forecast_data = NULL,
    production_plan = NULL,
    inventory_data = NULL,
    mrp_results = NULL,
    last_update = Sys.time()
  )
  
  #============================================================================
  # HOME TAB
  #============================================================================
  
  output$current_week_box <- renderValueBox({
    valueBox(
      value = paste("Week", app_data$current_week),
      subtitle = "Current Week",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$data_status_box <- renderValueBox({
    valueBox(
      value = "All Systems Operational",
      subtitle = "Data Status",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$alerts_box <- renderValueBox({
    # TODO: Count of alerts (shortages, stockouts, etc.)
    
    valueBox(
      value = 0,
      subtitle = "Active Alerts",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  # Navigate to forecasting when button clicked
  observeEvent(input$start_forecasting, {
    updateTabItems(session, "sidebar", "forecasting")
  })
  
  #============================================================================
  # FORECASTING MODULE
  #============================================================================
  
  # TODO: Implement forecasting logic
  
  observeEvent(input$archive_forecast, {
    # TODO: Collect all forecast inputs
    # Write to forecast_data.csv
    # Update app_data$forecast_data
    
    showNotification("Forecast data archived successfully!",
                    type = "message",
                    duration = 3)
  })
  
  output$forecast_viz <- renderPlotly({
    # TODO: Interactive time-series plot of forecasts
    
    plot_ly() %>%
      layout(title = "6-Week Forecast by Menu Item")
  })
  
  output$forecast_accuracy_table <- DT::renderDataTable({
    # TODO: Calculate and display accuracy metrics
    
    DT::datatable(data.frame(
      Menu_Item = character(),
      MAE = numeric(),
      MAPE = numeric(),
      Bias = numeric()
    ))
  })
  
  #============================================================================
  # PLANNING MODULE
  #============================================================================
  
  # TODO: Implement planning logic
  
  output$planning_forecast_graph <- renderPlotly({
    # TODO: Bar chart of 6-week forecast
    
    plot_ly() %>%
      layout(title = "Current 6-Week Forecast")
  })
  
  output$production_plan_table <- DT::renderDataTable({
    # TODO: Editable table for production plan
    
    DT::datatable(data.frame(
      Menu_Item = c("Burgers", "Subs", "Tacos", "Spaghetti", "Pizza"),
      Week_1 = numeric(5),
      Week_2 = numeric(5),
      Week_3 = numeric(5),
      Week_4 = numeric(5),
      Week_5 = numeric(5),
      Week_6 = numeric(5)
    ), editable = TRUE)
  })
  
  output$supply_shortage_alerts <- renderUI({
    # TODO: Check for supply shortages
    # Compare production plan * BOM vs. available supply
    
    # If shortages exist, create alert boxes
    div(
      class = "alert alert-success",
      icon("check"),
      " No supply shortages detected"
    )
  })
  
  output$supply_analysis_table <- DT::renderDataTable({
    # TODO: Detailed supply analysis by ingredient and week
    
    DT::datatable(data.frame(
      Ingredient = character(),
      Week = numeric(),
      Required = numeric(),
      Available = numeric(),
      Gap = numeric()
    ))
  })
  
  #============================================================================
  # INVENTORY MODULE
  #============================================================================
  
  # TODO: Implement inventory logic
  
  output$abc_pareto <- renderPlotly({
    # TODO: ABC Pareto chart
    
    plot_ly() %>%
      layout(title = "ABC Analysis - Ingredient Value")
  })
  
  output$safety_stock_table <- DT::renderDataTable({
    # TODO: Safety stock settings table
    
    DT::datatable(data.frame(
      Ingredient = character(),
      Current_Safety_Stock = numeric(),
      Recommended = numeric(),
      Service_Level = numeric()
    ))
  })
  
  output$dos_chart <- renderPlotly({
    # TODO: DOS scatter plot (current vs. target)
    
    plot_ly() %>%
      layout(title = "Days of Supply Analysis")
  })
  
  output$risk_flags_table <- DT::renderDataTable({
    # TODO: High-risk ingredients table
    
    DT::datatable(data.frame(
      Ingredient = character(),
      DOS = numeric(),
      Risk_Flag = character()
    ))
  })
  
  #============================================================================
  # MRP MODULE
  #============================================================================
  
  # Populate ingredient choices
  observe({
    updateSelectInput(session, "mrp_ingredient",
                     choices = unique(ingredients$ingredient_name))
  })
  
  observeEvent(input$generate_mrp, {
    # TODO: Run MRP logic for selected ingredient
    
    showNotification("MRP report generated!",
                    type = "message",
                    duration = 2)
  })
  
  output$mrp_report_table <- DT::renderDataTable({
    # TODO: MRP report with all calculations
    
    DT::datatable(data.frame(
      Week = 1:6,
      Gross_Requirements = numeric(6),
      Scheduled_Receipts = numeric(6),
      On_Hand_Beginning = numeric(6),
      Net_Requirements = numeric(6),
      Planned_Order_Receipts = numeric(6),
      Planned_Order_Releases = numeric(6),
      On_Hand_Ending = numeric(6)
    ))
  })
  
  output$purchase_summary_table <- DT::renderDataTable({
    # TODO: Summary of all purchase orders needed
    
    DT::datatable(data.frame(
      Ingredient = character(),
      Week = numeric(),
      Quantity_to_Order = numeric(),
      Dollar_Amount = numeric()
    ))
  })
  
  # Download handler for purchase orders
  output$download_purchase_orders <- downloadHandler(
    filename = function() {
      paste0("purchase_orders_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # TODO: Write purchase summary to CSV
      write_csv(data.frame(), file)
    }
  )
}

#==============================================================================
# RUN THE APP
#==============================================================================

shinyApp(ui = ui, server = server)
