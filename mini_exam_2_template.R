#==============================================================================
# Mini Exam 2: Inventory Management Dashboard
# Student Name: [Your Name]
# Date: [Date]
#
# INSTRUCTIONS:
# 1. Complete all TODOs
# 2. Implement EOQ and safety stock calculations
# 3. Create purchase recommendations
# 4. Build "Purchases Required" report
#==============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinyWidgets)

# Source function library
source("scm_functions.R")

# Load data
inventory_data <- read_csv("data/inventory_data.csv")
demand_history <- read_csv("data/demand_history.csv")
lead_time_history <- read_csv("data/lead_time_history.csv")

#==============================================================================
# UI DEFINITION
#==============================================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Inventory Management"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Purchases Required", tabName = "purchases", icon = icon("shopping-cart")),
      menuItem("Inventory Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #========================================================================
      # TAB 1: DASHBOARD
      #========================================================================
      tabItem(tabName = "dashboard",
        
        fluidRow(
          box(
            title = "Part Selection & Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                selectInput("part_number_inv", 
                           "Select Part Number:",
                           choices = sort(unique(inventory_data$part_number)))
              ),
              
              column(4,
                # TODO: Add slider for confidence level (50% - 99.9%)
                sliderInput("confidence_level",
                           "Confidence Level (Service Level):",
                           min = 50,
                           max = 99.9,
                           value = 95,
                           step = 0.1,
                           post = "%")
              ),
              
              column(4,
                # TODO: Add checkbox for including supply uncertainty
                checkboxInput("include_supply_uncertainty",
                             "Include Supply Uncertainty",
                             value = FALSE)
              )
            )
          )
        ),
        
        # Key metrics
        fluidRow(
          valueBoxOutput("safety_stock_box", width = 3),
          valueBoxOutput("reorder_point_box", width = 3),
          valueBoxOutput("current_dos_box", width = 3),
          valueBoxOutput("expected_dos_box", width = 3)
        ),
        
        # Purchase recommendation
        fluidRow(
          box(
            title = "Purchase Recommendation",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Show purchase warning conditionally
            uiOutput("purchase_warning"),
            uiOutput("purchase_details")
          )
        ),
        
        # All parts summary
        fluidRow(
          box(
            title = "Total Purchase Requirements",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Show total $ amount needed across all parts
            valueBoxOutput("total_purchase_amount", width = 12)
          )
        )
      ),
      
      #========================================================================
      # TAB 2: PURCHASES REQUIRED
      #========================================================================
      tabItem(tabName = "purchases",
        
        fluidRow(
          box(
            title = "Parts Requiring Purchase Orders",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            helpText("This table shows all parts that need to be ordered now."),
            
            actionButton("generate_purchase_report",
                        "Generate Purchase Report",
                        class = "btn-primary",
                        icon = icon("refresh")),
            
            br(), br(),
            
            # TODO: Create table of parts needing orders
            DT::dataTableOutput("purchases_table")
          )
        )
      ),
      
      #========================================================================
      # TAB 3: INVENTORY ANALYSIS
      #========================================================================
      tabItem(tabName = "analysis",
        
        fluidRow(
          box(
            title = "Inventory Risk Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            # TODO: Add visualizations
            # - ABC Pareto chart
            # - DOS by part
            # - High risk parts
            
            plotOutput("pareto_chart")
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
  
  # Selected part data
  selected_inventory <- reactive({
    inventory_data %>%
      filter(part_number == input$part_number_inv)
  })
  
  # Demand statistics for selected part
  demand_stats <- reactive({
    # TODO: Calculate mean and sd of historical demand
    
    history <- demand_history %>%
      filter(part_number == input$part_number_inv)
    
    list(
      mean = mean(history$demand, na.rm = TRUE),
      sd = sd(history$demand, na.rm = TRUE)
    )
  })
  
  # Lead time statistics
  lead_time_stats <- reactive({
    # TODO: Calculate mean and sd of lead time
    # Only if supply uncertainty is included
    
    if (input$include_supply_uncertainty) {
      history <- lead_time_history %>%
        filter(part_number == input$part_number_inv)
      
      list(
        mean = mean(history$lead_time_days, na.rm = TRUE),
        sd = sd(history$lead_time_days, na.rm = TRUE)
      )
    } else {
      list(
        mean = selected_inventory()$lead_time_days,
        sd = 0
      )
    }
  })
  
  # Safety stock calculation
  safety_stock_calc <- reactive({
    # TODO: Calculate safety stock using function library
    # Use input$confidence_level / 100 to convert to probability
    
    demand <- demand_stats()
    lead_time <- lead_time_stats()
    service_level <- input$confidence_level / 100
    
    # Call function from scm_functions.R
    calculate_safety_stock(
      demand_mean = demand$mean,
      demand_sd = demand$sd,
      lead_time_mean = lead_time$mean,
      lead_time_sd = lead_time$sd,
      service_level = service_level,
      include_supply_uncertainty = input$include_supply_uncertainty
    )
  })
  
  # Reorder point calculation
  reorder_point_calc <- reactive({
    # TODO: ROP = (demand per day * lead time in days) + safety stock
    
    demand <- demand_stats()
    lead_time <- lead_time_stats()
    ss <- safety_stock_calc()
    
    (demand$mean / 30) * lead_time$mean + ss  # Assuming monthly demand
  })
  
  # Purchase quantity needed
  purchase_qty_calc <- reactive({
    # TODO: Calculate if purchase is needed
    # If current inventory < reorder point, order EOQ
    
    current_inv <- selected_inventory()$on_hand
    rop <- reorder_point_calc()
    
    if (current_inv < rop) {
      # Calculate EOQ
      demand_annual <- demand_stats()$mean * 12
      setup_cost <- 10.25  # From problem statement
      unit_cost <- selected_inventory()$unit_cost
      holding_rate <- 0.05 + 0.04  # 5% storage + 4% capital cost
      
      eoq <- calculate_eoq(demand_annual, setup_cost, unit_cost * holding_rate)
      return(ceiling(eoq))
    } else {
      return(0)
    }
  })
  
  # All parts requiring purchases
  all_purchases_required <- reactive({
    # TODO: Calculate for ALL parts, not just selected one
    # Return data frame with: part_number, description, qty_to_order, dollar_amount
    
    # Placeholder - students should implement full logic
    data.frame(
      part_number = character(),
      description = character(),
      qty_to_order = numeric(),
      dollar_amount = numeric()
    )
  })
  
  #============================================================================
  # OUTPUTS - Value Boxes
  #============================================================================
  
  output$safety_stock_box <- renderValueBox({
    ss <- safety_stock_calc()
    
    valueBox(
      value = format(round(ss), big.mark = ","),
      subtitle = "Safety Stock (units)",
      icon = icon("shield-alt"),
      color = "blue"
    )
  })
  
  output$reorder_point_box <- renderValueBox({
    rop <- reorder_point_calc()
    
    valueBox(
      value = format(round(rop), big.mark = ","),
      subtitle = "Reorder Point (units)",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$current_dos_box <- renderValueBox({
    # TODO: Calculate current DOS
    # DOS = (current inventory / average daily demand)
    
    current_inv <- selected_inventory()$on_hand
    daily_demand <- demand_stats()$mean / 30
    dos <- current_inv / daily_demand
    
    valueBox(
      value = round(dos, 1),
      subtitle = "Current DOS (days)",
      icon = icon("calendar-alt"),
      color = if(dos < 15) "red" else if(dos > 60) "orange" else "green"
    )
  })
  
  output$expected_dos_box <- renderValueBox({
    # TODO: Calculate expected DOS after purchase
    
    current_inv <- selected_inventory()$on_hand
    purchase_qty <- purchase_qty_calc()
    daily_demand <- demand_stats()$mean / 30
    expected_dos <- (current_inv + purchase_qty) / daily_demand
    
    valueBox(
      value = round(expected_dos, 1),
      subtitle = "Expected DOS After Purchase (days)",
      icon = icon("calendar-check"),
      color = "green"
    )
  })
  
  #============================================================================
  # OUTPUTS - Purchase Warning
  #============================================================================
  
  output$purchase_warning <- renderUI({
    purchase_qty <- purchase_qty_calc()
    
    if (purchase_qty > 0) {
      div(
        class = "alert alert-danger",
        style = "font-size: 18px; font-weight: bold;",
        icon("exclamation-triangle"),
        " PURCHASE ORDER REQUIRED"
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        " No purchase required at this time"
      )
    }
  })
  
  output$purchase_details <- renderUI({
    purchase_qty <- purchase_qty_calc()
    
    if (purchase_qty > 0) {
      unit_cost <- selected_inventory()$unit_cost
      total_cost <- purchase_qty * unit_cost
      
      tagList(
        h4("Purchase Details:"),
        p(strong("Quantity to Order:"), format(purchase_qty, big.mark = ","), "units"),
        p(strong("Unit Cost:"), paste0("$", format(unit_cost, nsmall = 2))),
        p(strong("Total Purchase Cost:"), paste0("$", format(total_cost, big.mark = ",", nsmall = 2)))
      )
    }
  })
  
  output$total_purchase_amount <- renderValueBox({
    # TODO: Sum up all purchase requirements across all parts
    
    total <- sum(all_purchases_required()$dollar_amount, na.rm = TRUE)
    
    valueBox(
      value = paste0("$", format(total, big.mark = ",", nsmall = 2)),
      subtitle = "Total Purchase Requirements (All Parts)",
      icon = icon("dollar-sign"),
      color = if(total > 0) "red" else "green",
      width = 12
    )
  })
  
  #============================================================================
  # OUTPUTS - Purchases Table
  #============================================================================
  
  output$purchases_table <- DT::renderDataTable({
    # TODO: Show only parts that need orders
    
    purchases <- all_purchases_required()
    
    DT::datatable(
      purchases,
      options = list(
        pageLength = 25,
        order = list(list(3, 'desc'))  # Sort by dollar amount descending
      ),
      rownames = FALSE
    ) %>%
      formatCurrency('dollar_amount', '$')
  })
  
  #============================================================================
  # EVENT HANDLERS
  #============================================================================
  
  observeEvent(input$generate_purchase_report, {
    # TODO: Regenerate purchase requirements table
    
    showNotification("Purchase report generated!", 
                    type = "message",
                    duration = 2)
  })
}

#==============================================================================
# RUN THE APP
#==============================================================================

shinyApp(ui = ui, server = server)
