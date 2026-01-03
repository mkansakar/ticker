library(dplyr)
library(readr)
library(TTR)
library(purrr)

PRICE_DIR <- "/Users/kasa/RStudio/dow_non_snp500"
data_fl <- "/Users/kasa/RStudio/stock_fundamental_non_snp500.csv"

build_momentum_screen <- function(
    price_dir,
    fundamentals_file,
    min_history = 260,
    ma_short = 50,
    ma_long  = 200,
    trend_lookback = 60,
    ret_short_lb = 20,
    ret_long_lb  = 60,
    ret_short_min = 0.05,
    ret_long_min  = 0.15,
    vol_max = 0.40,
    min_marketcap = 1e9
) {
  
  price_files <- list.files(price_dir, "\\.csv$", full.names = TRUE)
  
  if (length(price_files) == 0) {
    stop("No price CSV files found in ", price_dir)
  }
  
  fundamentals <- read_csv(
    fundamentals_file,
    show_col_types = FALSE
  )
  
  technical_data <- map_dfr(price_files, function(f) {
    
    symbol <- tools::file_path_sans_ext(basename(f))
    
    df <- read_csv(f, show_col_types = FALSE) %>%
      rename_with(tolower) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date)
    
    if (!"close" %in% names(df)) return(NULL)
    if (nrow(df) < min_history) return(NULL)
    
    df <- df %>%
      mutate(
        ret_1d = close / dplyr::lag(close) - 1,
        MA_short = SMA(close, ma_short),
        MA_long  = SMA(close, ma_long),
        vol = rollapply(ret_1d, width = 20, FUN = sd, fill = NA, align = "right", na.rm = TRUE) * sqrt(252),
        ret_short = close / lag(close, ret_short_lb) - 1,
        ret_long  = close / lag(close, ret_long_lb) - 1
      )
    
    trend_confirm <- df %>%
      tail(trend_lookback) %>%
      summarise(pct = mean(MA_short > MA_long, na.rm = TRUE)) %>%
      pull(pct)
    
    df %>%
      tail(1) %>%
      transmute(
        symbol,
        end_price = close,
        MA_short,
        MA_long,
        ret_short,
        ret_long,
        vol,
        trend_confirm
      )
  })
  
  momentum_screen <- fundamentals %>%
    left_join(technical_data, by = "symbol") %>%
    filter(
      !is.na(MA_short),
      !is.na(MA_long),
      end_price > MA_short,
      MA_short > MA_long,
      trend_confirm >= 0.80,
      ret_short > ret_short_min,
      ret_long  > ret_long_min,
      vol < vol_max,
      MarketCap > min_marketcap
    ) %>%
    arrange(desc(ret_long))
  
  return(momentum_screen)
}

momentum_non_snp <- build_momentum_screen(
  price_dir = PRICE_DIR, 
  fundamentals_file = data_fl
)