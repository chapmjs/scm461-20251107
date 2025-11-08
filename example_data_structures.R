# Example data structures for students to understand

# menu_items.csv
menu_items <- data.frame(
  menu_item = c("burger", "sub", "taco", "spaghetti", "pizza"),
  description = c("Hamburger", "Sub Sandwich", "Taco", "Spaghetti Plate", "Personal Pizza"),
  price = c(8.99, 7.99, 3.99, 9.99, 10.99)
)

# ingredients.csv
ingredients <- data.frame(
  ingredient = c("ground_beef", "buns", "lettuce", "tomato", "cheese", ...),
  description = c("Ground Beef", "Hamburger Buns", "Lettuce", "Tomato", "Cheese", ...),
  unit_cost = c(4.50, 2.00, 1.50, 2.25, 3.00, ...),
  unit_of_measure = c("lb", "package", "head", "lb", "lb", ...),
  on_hand = c(50, 100, 20, 30, 25, ...),
  lead_time_days = c(3, 2, 1, 2, 3, ...)
)

# bom.csv (Bill of Materials)
bom <- data.frame(
  menu_item = c("burger", "burger", "burger", "burger", "sub", "sub", ...),
  ingredient = c("ground_beef", "buns", "lettuce", "tomato", "sub_roll", "turkey", ...),
  quantity_per_item = c(0.25, 1, 0.1, 0.15, 1, 0.2, ...)
)

# forecasts.csv
forecasts <- data.frame(
  date = as.Date("2025-01-01"),
  menu_item = "burger",
  week_1 = 100,
  week_2 = 110,
  week_3 = 105,
  week_4 = 115,
  week_5 = 108,
  week_6 = 112
)

# production_plans.csv
production_plans <- data.frame(
  menu_item = c("burger", "sub", "taco", "spaghetti", "pizza"),
  week_1 = c(100, 80, 50, 40, 30),
  week_2 = c(110, 85, 55, 45, 35),
  week_3 = c(105, 82, 52, 42, 32),
  week_4 = c(115, 88, 58, 48, 38),
  week_5 = c(108, 84, 54, 44, 34),
  week_6 = c(112, 86, 56, 46, 36)
)

# inventory.csv
inventory <- data.frame(
  ingredient = c("ground_beef", "buns", "lettuce", ...),
  on_hand = c(50, 100, 20, ...),
  safety_stock = c(20, 40, 10, ...),
  reorder_point = c(35, 60, 15, ...)
)

# scheduled_receipts.csv
scheduled_receipts <- data.frame(
  ingredient = c("ground_beef", "buns", ...),
  week_1 = c(0, 50, ...),
  week_2 = c(100, 0, ...),
  week_3 = c(0, 0, ...),
  week_4 = c(0, 100, ...),
  week_5 = c(0, 0, ...),
  week_6 = c(50, 0, ...)
)
