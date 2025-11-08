#==============================================================================
# SCM Function Library
# Supply Chain Management helper functions
# Source this file in your Shiny apps: source("scm_functions.R")
#==============================================================================

library(tidyverse)

#==============================================================================
# INVENTORY FUNCTIONS
#==============================================================================

#' Calculate Economic Order Quantity (EOQ)
#' 
#' @param demand_annual Annual demand
#' @param setup_cost Cost per order
#' @param holding_cost_rate Annual holding cost rate (as decimal)
#' @param unit_cost Cost per unit
#' @return Optimal order quantity
calculate_eoq <- function(demand_annual, setup_cost, holding_cost_rate, unit_cost) {
  holding_cost <- holding_cost_rate * unit_cost
  eoq <- sqrt((2 * demand_annual * setup_cost) / holding_cost)
  return(eoq)
}

#' Calculate Safety Stock
#' 
#' @param demand_mean Average demand per period
#' @param demand_sd Standard deviation of demand
#' @param lead_time_mean Average lead time
#' @param lead_time_sd Standard deviation of lead time (0 if deterministic)
#' @param service_level Desired service level (0-1)
#' @param include_supply_uncertainty Include lead time variability?
#' @return Safety stock quantity
calculate_safety_stock <- function(demand_mean, demand_sd, lead_time_mean, 
                                  lead_time_sd = 0, service_level = 0.95,
                                  include_supply_uncertainty = FALSE) {
  
  z_score <- qnorm(service_level)
  
  if (include_supply_uncertainty && lead_time_sd > 0) {
    # Include both demand and supply uncertainty
    variance <- (lead_time_mean * demand_sd^2) + (demand_mean^2 * lead_time_sd^2)
    safety_stock <- z_score * sqrt(variance)
  } else {
    # Only demand uncertainty
    safety_stock <- z_score * demand_sd * sqrt(lead_time_mean)
  }
  
  return(ceiling(safety_stock))
}

#' Calculate Reorder Point
#' 
#' @param demand_mean Average demand per period
#' @param lead_time Lead time in same units as demand
#' @param safety_stock Safety stock quantity
#' @return Reorder point
calculate_reorder_point <- function(demand_mean, lead_time, safety_stock) {
  rop <- demand_mean * lead_time + safety_stock
  return(ceiling(rop))
}

#' Calculate Days of Supply
#' 
#' @param on_hand Current inventory
#' @param daily_demand Average daily demand
#' @return Days of supply
calculate_dos <- function(on_hand, daily_demand) {
  if (daily_demand == 0) return(Inf)
  dos <- on_hand / daily_demand
  return(dos)
}

#==============================================================================
# FORECAST ACCURACY FUNCTIONS
#==============================================================================

#' Calculate Mean Absolute Error (MAE)
#' 
#' @param actual Vector of actual values
#' @param forecast Vector of forecast values
#' @return MAE
calculate_mae <- function(actual, forecast) {
  mae <- mean(abs(actual - forecast), na.rm = TRUE)
  return(mae)
}

#' Calculate Mean Absolute Percentage Error (MAPE)
#' 
#' @param actual Vector of actual values
#' @param forecast Vector of forecast values
#' @return MAPE as percentage
calculate_mape <- function(actual, forecast) {
  # Avoid division by zero
  valid_idx <- actual != 0
  if (sum(valid_idx) == 0) return(NA)
  
  mape <- mean(abs((actual[valid_idx] - forecast[valid_idx]) / actual[valid_idx]), 
              na.rm = TRUE) * 100
  return(mape)
}

#' Calculate Forecast Bias
#' 
#' @param actual Vector of actual values
#' @param forecast Vector of forecast values
#' @return Bias (positive = over-forecast, negative = under-forecast)
calculate_bias <- function(actual, forecast) {
  bias <- mean(forecast - actual, na.rm = TRUE)
  return(bias)
}

#' Calculate Root Mean Squared Error (RMSE)
#' 
#' @param actual Vector of actual values
#' @param forecast Vector of forecast values
#' @return RMSE
calculate_rmse <- function(actual, forecast) {
  rmse <- sqrt(mean((actual - forecast)^2, na.rm = TRUE))
  return(rmse)
}

#' Calculate Tracking Signal
#' 
#' @param actual Vector of actual values
#' @param forecast Vector of forecast values
#' @return Tracking signal
calculate_tracking_signal <- function(actual, forecast) {
  cumulative_error <- sum(forecast - actual, na.rm = TRUE)
  mae <- calculate_mae(actual, forecast)
  
  if (mae == 0) return(0)
  ts <- cumulative_error / mae
  return(ts)
}

#==============================================================================
# MRP FUNCTIONS
#==============================================================================

#' Calculate Gross Requirements from Production Plan
#' 
#' @param production_plan Data frame with menu items and weekly quantities
#' @param bom Bill of materials data frame
#' @param ingredient_name Name of ingredient to calculate requirements for
#' @return Vector of gross requirements by week
calculate_gross_requirements <- function(production_plan, bom, ingredient_name) {
  
  # Filter BOM for this ingredient
  bom_ingredient <- bom %>%
    filter(ingredient == ingredient_name)
  
  # Calculate requirements for each menu item
  gross_req <- rep(0, ncol(production_plan) - 1)  # Assuming first col is menu item name
  
  for (i in 1:nrow(bom_ingredient)) {
    menu_item <- bom_ingredient$menu_item[i]
    qty_per <- bom_ingredient$quantity_per_item[i]
    
    # Find row in production plan for this menu item
    if (menu_item %in% production_plan$menu_item) {
      production_qty <- production_plan %>%
        filter(menu_item == !!menu_item) %>%
        select(-menu_item) %>%
        unlist()
      
      gross_req <- gross_req + (production_qty * qty_per)
    }
  }
  
  return(gross_req)
}

#' Calculate Net Requirements
#' 
#' @param gross_req Vector of gross requirements
#' @param on_hand Initial on-hand inventory
#' @param scheduled_receipts Vector of scheduled receipts by period
#' @param safety_stock Safety stock to maintain
#' @return List with net requirements and ending inventory by period
calculate_net_requirements <- function(gross_req, on_hand, scheduled_receipts, 
                                      safety_stock = 0) {
  
  n_periods <- length(gross_req)
  net_req <- numeric(n_periods)
  ending_inventory <- numeric(n_periods)
  
  current_inventory <- on_hand
  
  for (i in 1:n_periods) {
    # Available = current inventory + scheduled receipts
    available <- current_inventory + scheduled_receipts[i]
    
    # Net requirement = max(0, gross requirement - available + safety stock)
    net_req[i] <- max(0, gross_req[i] - available + safety_stock)
    
    # Ending inventory
    ending_inventory[i] <- available - gross_req[i]
    
    # Update current inventory for next period
    current_inventory <- ending_inventory[i]
  }
  
  return(list(
    net_requirements = net_req,
    ending_inventory = ending_inventory
  ))
}

#' Offset for Lead Time
#' 
#' @param planned_receipts Vector of planned receipts
#' @param lead_time Lead time in periods
#' @return Vector of planned releases (offsetplanned receipts by lead time)
offset_for_lead_time <- function(planned_receipts, lead_time) {
  
  n_periods <- length(planned_receipts)
  planned_releases <- numeric(n_periods)
  
  for (i in 1:n_periods) {
    release_period <- i - lead_time
    if (release_period >= 1) {
      planned_releases[release_period] <- planned_receipts[i]
    }
  }
  
  return(planned_releases)
}

#' Complete MRP Calculation
#' 
#' @param ingredient_name Name of ingredient
#' @param gross_requirements Vector of gross requirements
#' @param on_hand Initial inventory
#' @param scheduled_receipts Vector of scheduled receipts
#' @param lead_time Lead time in periods
#' @param safety_stock Safety stock level
#' @param order_quantity Fixed order quantity (if using lot-for-lot, set to NULL)
#' @return Data frame with complete MRP report
calculate_mrp <- function(ingredient_name, gross_requirements, on_hand, 
                         scheduled_receipts, lead_time, safety_stock = 0,
                         order_quantity = NULL) {
  
  n_periods <- length(gross_requirements)
  
  # Initialize result vectors
  on_hand_beginning <- numeric(n_periods)
  net_req <- numeric(n_periods)
  planned_receipts <- numeric(n_periods)
  on_hand_ending <- numeric(n_periods)
  
  current_oh <- on_hand
  
  for (i in 1:n_periods) {
    # Beginning inventory
    on_hand_beginning[i] <- current_oh
    
    # Available = on hand + scheduled receipts
    available <- current_oh + scheduled_receipts[i]
    
    # Net requirement
    if (available < gross_requirements[i] + safety_stock) {
      net_req[i] <- gross_requirements[i] + safety_stock - available
      
      # Planned order receipt
      if (is.null(order_quantity)) {
        # Lot-for-lot
        planned_receipts[i] <- net_req[i]
      } else {
        # Fixed order quantity
        planned_receipts[i] <- order_quantity
      }
    } else {
      net_req[i] <- 0
      planned_receipts[i] <- 0
    }
    
    # Ending inventory
    on_hand_ending[i] <- available + planned_receipts[i] - gross_requirements[i]
    
    # Update for next period
    current_oh <- on_hand_ending[i]
  }
  
  # Calculate planned releases (offset by lead time)
  planned_releases <- offset_for_lead_time(planned_receipts, lead_time)
  
  # Create MRP report data frame
  mrp_report <- data.frame(
    Period = 1:n_periods,
    Gross_Requirements = gross_requirements,
    Scheduled_Receipts = scheduled_receipts,
    On_Hand_Beginning = on_hand_beginning,
    Net_Requirements = net_req,
    Planned_Order_Receipts = planned_receipts,
    Planned_Order_Releases = planned_releases,
    On_Hand_Ending = on_hand_ending
  )
  
  return(mrp_report)
}

#==============================================================================
# DATA MANAGEMENT FUNCTIONS
#==============================================================================

#' Save forecast data to CSV
#' 
#' @param forecast_df Data frame with forecast data
#' @param file_path Path to CSV file
save_forecast_data <- function(forecast_df, file_path = "data/forecasts.csv") {
  
  # Add timestamp
  forecast_df$timestamp <- Sys.time()
  
  # Append to existing file or create new
  if (file.exists(file_path)) {
    write_csv(forecast_df, file_path, append = TRUE)
  } else {
    write_csv(forecast_df, file_path)
  }
  
  return(TRUE)
}

#' Load historical demand data
#' 
#' @param item_name Menu item or part name
#' @param n_periods Number of periods to retrieve
#' @param file_path Path to historical data CSV
#' @return Data frame with historical demand
load_historical_demand <- function(item_name, n_periods = 12, 
                                  file_path = "data/historical_demand.csv") {
  
  if (!file.exists(file_path)) {
    warning("Historical data file not found")
    return(NULL)
  }
  
  demand_data <- read_csv(file_path, show_col_types = FALSE)
  
  historical <- demand_data %>%
    filter(item == item_name) %>%
    arrange(desc(date)) %>%
    head(n_periods)
  
  return(historical)
}

#==============================================================================
# UTILITY FUNCTIONS
#==============================================================================

#' Format currency
#' 
#' @param value Numeric value
#' @return Formatted string
format_currency <- function(value) {
  paste0("$", format(round(value, 2), big.mark = ",", nsmall = 2))
}

#' Format percentage
#' 
#' @param value Numeric value (0-1 or 0-100)
#' @param as_decimal If TRUE, multiply by 100
#' @return Formatted string
format_percentage <- function(value, as_decimal = TRUE) {
  if (as_decimal) value <- value * 100
  paste0(format(round(value, 1), nsmall = 1), "%")
}

#' Create date sequence for forecasting
#' 
#' @param start_date Starting date
#' @param n_periods Number of periods
#' @param period_type "week", "month", "day"
#' @return Vector of dates
create_date_sequence <- function(start_date, n_periods, period_type = "week") {
  
  if (period_type == "week") {
    dates <- seq(start_date, by = "week", length.out = n_periods)
  } else if (period_type == "month") {
    dates <- seq(start_date, by = "month", length.out = n_periods)
  } else if (period_type == "day") {
    dates <- seq(start_date, by = "day", length.out = n_periods)
  } else {
    stop("Invalid period_type. Use 'week', 'month', or 'day'")
  }
  
  return(dates)
}

#==============================================================================
# END OF FUNCTION LIBRARY
#==============================================================================
