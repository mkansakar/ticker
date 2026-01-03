# Load required libraries
library(tidyverse)
library(dplyr)
library(readr)
library(purrr)

# Define paths
INDICATOR_DIR <- "/Users/kasa/RStudio/indicators/"  # Update with your actual path
FUNDAMENTAL_FILE <- "/Users/kasa/RStudio/stock_fundamental_non_snp500.csv"

# Function to read latest indicators for specific symbols
read_indicators_for_symbols <- function(symbols, indicator_dir) {
  # Initialize empty dataframe
  all_indicators <- data.frame()
  
  # Track progress
  cat("Fetching indicators for", length(symbols), "symbols...\n")
  
  for (symbol in symbols) {
    # Construct file path
    file_path <- file.path(indicator_dir, paste0(symbol, ".csv"))
    
    if (file.exists(file_path)) {
      tryCatch({
        # Read the CSV file
        df <- read_csv(file_path, show_col_types = FALSE)
        
        # Get the most recent data
        latest <- df %>%
          filter(Date == max(Date, na.rm = TRUE)) %>%
          mutate(Symbol = symbol)  # Add symbol column
        
        # Append to results
        all_indicators <- bind_rows(all_indicators, latest)
        
        cat("✓", symbol, "\n")
        
      }, error = function(e) {
        cat("✗", symbol, "- Error:", e$message, "\n")
      })
    } else {
      cat("✗", symbol, "- File not found:", basename(file_path), "\n")
    }
  }
  
  cat("\nSuccessfully loaded indicators for", 
      length(unique(all_indicators$Symbol)), "out of", length(symbols), "symbols\n")
  
  return(all_indicators)
}

# Alternative: Vectorized version for faster processing
read_indicators_for_symbols_fast <- function(symbols, indicator_dir) {
  # Create all file paths
  file_paths <- file.path(indicator_dir, paste0(symbols, ".csv"))
  
  # Check which files exist
  file_exists <- file.exists(file_paths)
  existing_files <- file_paths[file_exists]
  existing_symbols <- symbols[file_exists]
  
  cat("Found", length(existing_files), "indicator files out of", length(symbols), "symbols\n")
  
  if (length(existing_files) == 0) {
    return(data.frame())
  }
  
  # Read all existing files
  indicator_data <- map2_df(existing_files, existing_symbols, function(f, sym) {
    tryCatch({
      df <- read_csv(f, show_col_types = FALSE)
      
      # Get the most recent data
      latest <- df %>%
        filter(Date == max(Date, na.rm = TRUE)) %>%
        mutate(Symbol = sym)  # Add symbol column
      
      return(latest)
      
    }, error = function(e) {
      cat("Error reading", sym, ":", e$message, "\n")
      return(NULL)
    })
  })
  
  return(indicator_data)
}

# Function to clean and convert data types
clean_data <- function(df) {
  df_clean <- df %>%
    mutate(
      # Convert market cap to numeric
      MarketCap = as.numeric(MarketCap),
      
      # Handle missing values in key metrics
      across(c(
        RevenueGrowth, OperatingMargin, ROE, ProfitMargin, 
        DebtToEquity, TrailingPE, ForwardPE, dividend_yield
      ), ~ifelse(is.na(.x) | .x == "Infinity" | .x == "-Infinity", 
                 NA, as.numeric(.x)))
    )
  
  return(df_clean)
}

# Main script
cat("=== MOMENTUM SCREENING PIPELINE ===\n\n")

# 1. Load the fundamental data
cat("1. Loading fundamental data...\n")
data <- read.csv(FUNDAMENTAL_FILE, stringsAsFactors = FALSE)

# Clean the data
data_clean <- clean_data(data)

# Get symbols from the fundamental data (assuming first column is Symbol)
symbols <- unique(data_clean$Symbol)
cat("   Found", length(symbols), "unique symbols in fundamental data\n")

# 2. Read indicators for these symbols
cat("\n2. Fetching indicator data...\n")
latest_indicators <- read_indicators_for_symbols_fast(symbols, INDICATOR_DIR)

# 3. Check what we got
if (nrow(latest_indicators) == 0) {
  cat("\nERROR: No indicator data loaded!\n")
  cat("Check that:\n")
  cat("1. INDICATOR_DIR is correct:", INDICATOR_DIR, "\n")
  cat("2. Files exist in the format: SYMBOL.csv (e.g., AAPL.csv, MSFT.csv)\n")
  cat("3. Files have the required columns: Date, close, sma_50, sma_200, etc.\n")
  
  # Show sample of what files exist
  existing_files <- list.files(INDICATOR_DIR, pattern = "\\.csv$")
  cat("\nFirst 10 files in indicator directory:\n")
  print(head(existing_files, 10))
  
  # Check for first few symbols
  cat("\nChecking first 5 symbols:\n")
  for (i in 1:min(5, length(symbols))) {
    sym <- symbols[i]
    file_path <- file.path(INDICATOR_DIR, paste0(sym, ".csv"))
    cat(sym, "-", ifelse(file.exists(file_path), "EXISTS", "MISSING"), "\n")
  }
  
  stop("Cannot proceed without indicator data")
}

cat("\n3. Indicator data loaded successfully\n")
cat("   Symbols with indicators:", length(unique(latest_indicators$Symbol)), "\n")
cat("   Columns available:", paste(colnames(latest_indicators), collapse = ", "), "\n")

# 4. Merge fundamental data with indicators
cat("\n4. Merging data...\n")
data_with_indicators <- data_clean %>%
  left_join(latest_indicators %>% 
              select(Symbol, Date, close, sma_10, sma_20, sma_50, sma_200, 
                     rsi_14, macd, macd_signal, volume),
            by = "Symbol")

# Check merge results
symbols_with_data <- sum(!is.na(data_with_indicators$sma_50) & !is.na(data_with_indicators$sma_200))
cat("   Symbols with SMA data:", symbols_with_data, "out of", nrow(data_with_indicators), "\n")

# 5. Create momentum screen
cat("\n5. Applying momentum screening...\n")
momentum_screen <- data_with_indicators %>%
  filter(
    # Ensure we have all required data
    !is.na(close) & !is.na(sma_50) & !is.na(sma_200),
    
    # Price above 50-day and 200-day SMA
    close > sma_50,
    close > sma_200,
    
    # 50-day SMA above 200-day SMA (golden cross)
    sma_50 > sma_200,
    
    # Minimum market cap
    !is.na(MarketCap) & MarketCap > 1000000000
  ) %>%
  mutate(
    # Calculate momentum ratios
    price_to_sma200 = close / sma_200,
    price_to_sma50 = close / sma_50,
    sma_cross_strength = sma_50 / sma_200
  ) %>%
  select(
    Symbol, Sector, Company,
    end_price = close,
    sma_10, sma_20, sma_50, sma_200,
    price_to_sma200,
    price_to_sma50,
    sma_cross_strength,
    rsi = rsi_14,
    macd, macd_signal,
    revenue_growth = RevenueGrowth, 
    ROE,
    OperatingMargin,
    DebtToEquity,
    TrailingPE,
    MarketCap,
    Date
  ) %>%
  arrange(desc(price_to_sma200))  # Sort by % above 200-day SMA

# Enhanced momentum screen with RSI and MACD filters
momentum_screen_enhanced <- momentum_screen %>%
  filter(
    !is.na(rsi) & rsi < 70,  # Not overbought
    !is.na(macd) & !is.na(macd_signal) & macd > macd_signal  # MACD bullish
  ) %>%
  mutate(
    rsi_zone = case_when(
      rsi < 30 ~ "Oversold",
      rsi < 50 ~ "Bearish",
      rsi < 70 ~ "Bullish",
      TRUE ~ "Overbought"
    ),
    macd_strength = macd - macd_signal
  )

# 6. Display results
cat("\n6. Results:\n")
cat("   Stocks passing basic momentum screen:", nrow(momentum_screen), "\n")
cat("   Stocks passing enhanced screen:", nrow(momentum_screen_enhanced), "\n")

# Function to print screen results
print_screen_results <- function(screen_name, screen_data, n = 10) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(str_pad(paste(" ", screen_name, " "), width = 60, pad = "=", side = "both"), "\n")
  cat(strrep("=", 60), "\n\n", sep = "")
  
  if (nrow(screen_data) == 0) {
    cat("No stocks passed the screen.\n")
  } else {
    # Format numeric columns for better display
    display_data <- screen_data %>%
      mutate(
        across(c(end_price, sma_10, sma_20, sma_50, sma_200, rsi, macd), ~ round(., 2)),
        across(c(price_to_sma200, price_to_sma50, sma_cross_strength), ~ round(., 3)),
        MarketCap = ifelse(!is.na(MarketCap), 
                           paste0(round(MarketCap/1e9, 2), "B"), 
                           NA),
        ROE = paste0(round(ROE * 100, 1), "%"),
        revenue_growth = paste0(round(revenue_growth * 100, 1), "%")
      ) %>%
      select(-Date)  # Remove Date for cleaner display
    
    print(head(display_data, n))
    cat("\nTotal stocks passing:", nrow(screen_data), "\n")
    
    # Show sector distribution
    if (nrow(screen_data) > 0 && "Sector" %in% colnames(screen_data)) {
      cat("\nSector distribution:\n")
      sector_summary <- screen_data %>%
        count(Sector, sort = TRUE) %>%
        mutate(Percentage = round(n / sum(n) * 100, 1))
      print(sector_summary)
    }
  }
}

# Display results
print_screen_results("MOMENTUM STOCKS (Golden Cross)", momentum_screen)
print_screen_results("ENHANCED MOMENTUM (with RSI & MACD filters)", momentum_screen_enhanced)

# 7. Save results
cat("\n7. Saving results...\n")
output_dir <- "/Users/kasa/RStudio/composite/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(momentum_screen, paste0(output_dir, "momentum_stocks_screen.csv"))
write_csv(momentum_screen_enhanced, paste0(output_dir, "momentum_stocks_enhanced.csv"))

cat("   Results saved to:", output_dir, "\n")

# 8. Summary statistics
if (nrow(momentum_screen) > 0) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(str_pad(" SUMMARY STATISTICS ", width = 60, pad = "=", side = "both"), "\n")
  cat(strrep("=", 60), "\n\n", sep = "")
  
  # Calculate averages
  summary_stats <- momentum_screen %>%
    summarise(
      n_stocks = n(),
      avg_price_to_sma200 = mean(price_to_sma200, na.rm = TRUE),
      avg_price_to_sma50 = mean(price_to_sma50, na.rm = TRUE),
      avg_ROE = mean(ROE, na.rm = TRUE),
      avg_revenue_growth = mean(revenue_growth, na.rm = TRUE),
      avg_rsi = mean(rsi, na.rm = TRUE),
      median_market_cap_b = median(MarketCap, na.rm = TRUE) / 1e9
    )
  
  cat("Total stocks passing:", summary_stats$n_stocks, "\n")
  cat("Average Price/sma200 ratio:", round(summary_stats$avg_price_to_sma200, 3), "\n")
  cat("Average Price/sma50 ratio:", round(summary_stats$avg_price_to_sma50, 3), "\n")
  cat("Average ROE:", round(summary_stats$avg_ROE * 100, 1), "%\n")
  cat("Average Revenue Growth:", round(summary_stats$avg_revenue_growth * 100, 1), "%\n")
  cat("Average RSI:", round(summary_stats$avg_rsi, 1), "\n")
  cat("Median Market Cap:", round(summary_stats$median_market_cap_b, 2), "B\n")
  
  # Top performers
  cat("\nTop 5 momentum stocks:\n")
  top_performers <- momentum_screen %>%
    head(5) %>%
    select(Symbol, Company, Sector, end_price, price_to_sma200, ROE, rsi)
  print(top_performers)
}

cat("\n=== PROCESS COMPLETE ===\n")