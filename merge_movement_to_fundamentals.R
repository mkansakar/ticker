# Define file paths
price_movement_file <- "/Users/kasa/RStudio/price_movement_45d.csv"
fundamentals_file <- "/Users/kasa/RStudio/stock_fundamentals_out_1.csv"

# Read both files
price_movement <- read.csv(price_movement_file, stringsAsFactors = FALSE)
stock_fundamentals <- read.csv(fundamentals_file, stringsAsFactors = FALSE)

# Identify which columns are from price_movement (excluding the key column)
price_movement_cols <- setdiff(names(price_movement), "symbol")

# Remove existing price_movement columns from fundamentals if they exist
stock_fundamentals <- stock_fundamentals %>%
  select(-any_of(price_movement_cols))

# Merge the data
merged_data <- stock_fundamentals %>%
  full_join(price_movement, by = "symbol")

# Write back to fundamentals file
write.csv(merged_data, fundamentals_file, row.names = FALSE)

cat(paste("Merged", nrow(price_movement), "price movement records into", 
          fundamentals_file, "\n"))