library(dplyr)
library(TTR)
library(purrr)
library(zoo)

DOW_DIR <- "/Users/kasa/RStudio/dow_non_snp500"
FUNDAMENTAL_FILE <- "/Users/kasa/RStudio/stock_fundamental_non_snp500.csv"
OUTPUT <- "/Users/kasa/RStudio/composite_non_snp500"

price_files <- list.files(DOW_DIR, "\\.csv$", full.names = TRUE)

technical_data <- map(price_files, function(f) {
  Symbol <- tools::file_path_sans_ext(basename(f))
  
  df <- read.csv(f)
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>%
    filter(close > 0) %>%
    arrange(date)
  
  if (nrow(df) < 260) return(NULL)
  
  df <- df %>%
    mutate(
      ret_1d = close / lag(close) - 1,
      MA10 = SMA(close, 10),
      MA50 = SMA(close, 50),
      MA200 = SMA(close, 200),
      vol20 = rollapply(ret_1d, 20, sd, fill = NA, na.rm = TRUE) * sqrt(252),
      ret_20 = close / lag(close, 20) - 1,
      ret_60 = close / lag(close, 60) - 1
    )
  
  tail(df, 1) %>%
    transmute(Symbol, end_price.x = close, MA10, MA50, MA200, ret_20, ret_60, vol20)
}) %>%
  compact() %>%
  bind_rows()

data_clean <- read.csv(FUNDAMENTAL_FILE, stringsAsFactors = FALSE) %>%
  mutate(across(
    c(MarketCap, RevenueGrowth, OperatingMargin, ROE, ProfitMargin,
      DebtToEquity, TrailingPE, ForwardPE, dividend_yield, PayoutRatio, BookValue),
    ~ suppressWarnings(as.numeric(.x))
  )) %>%
  left_join(technical_data, by = "Symbol")
#df <- df %>% rename(end_price.x = end_price.x)

screens <- list(
  value = list(
    filter = "TrailingPE > 0 & TrailingPE < 15 & ForwardPE > 0 & ForwardPE < 15 & DebtToEquity < 100 & ROE > 0.15 & ProfitMargin > 0.10 & MarketCap > 5e8",
    select = c("Symbol", "Sector", "MarketCap", "TrailingPE", "ForwardPE", "ROE", "ProfitMargin", "DebtToEquity", "dividend_yield", "end_price.x"),
    arrange = "TrailingPE"
  ),
  
  growth = list(
    filter = "RevenueGrowth > 0.15 & ProfitMargin > 0.10 & ROE > 0.15 & MarketCap > 5e8 & DebtToEquity < 50",
    select = c("Symbol", "Sector", "MarketCap", "RevenueGrowth", "ROE", "ProfitMargin", "DebtToEquity", "end_price.x"),
    arrange = "desc(RevenueGrowth)"
  ),
  
  quality = list(
    filter = "ROE > 0.20 & ProfitMargin > 0.15 & OperatingMargin > 0.15 & DebtToEquity < 50 & MarketCap > 2e9",
    select = c("Symbol", "Sector", "MarketCap", "ROE", "ProfitMargin", "OperatingMargin", "DebtToEquity", "end_price.x"),
    arrange = "desc(ROE)"
  ),
  
  dividend = list(
    filter = "dividend_yield > 0.03 & PayoutRatio < 0.80 & ROE > 0.10 & DebtToEquity < 100 & MarketCap > 1e9",
    select = c("Symbol", "Sector", "MarketCap", "dividend_yield", "PayoutRatio", "ROE", "DebtToEquity", "end_price.x"),
    arrange = "desc(dividend_yield)"
  ),
  
  undervalued = list(
    filter = "TrailingPE > 0 & TrailingPE < 10 & BookValue > 0 & end_price.x > 0 & DebtToEquity < 50 & (end_price.x / BookValue) < 1.5",
    select = c("Symbol", "Sector", "end_price.x", "BookValue", "TrailingPE", "DebtToEquity", "ROE"),
    arrange = "(end_price.x / BookValue)"
  ),
  
  momentum = list(
    filter = "end_price.x > MA50 & end_price.x > MA200 & MA50 > MA200 & MarketCap > 1e9",
    select = c("Symbol", "Sector", "end_price.x", "MA10", "MA50", "MA200", "RevenueGrowth", "ROE"),
    arrange = "desc(end_price.x / MA200)"
  ),
  
  magic = list(
    filter = "ROE > 0.15 & TrailingPE > 0 & TrailingPE < 20 & OperatingMargin > 0.10 & DebtToEquity < 100 & MarketCap > 5e8",
    select = c("Symbol", "Sector", "MarketCap", "TrailingPE", "ROE", "OperatingMargin", "DebtToEquity"),
    arrange = "TrailingPE"
  )
)

run_screen <- function(df, filter_expr, select_cols, arrange_expr) {
  df %>%
    filter(eval(parse(text = filter_expr))) %>%
    select(all_of(select_cols)) %>%
    arrange(eval(parse(text = arrange_expr)))
}

results <- map(screens, ~ run_screen(data_clean, .x$filter, .x$select, .x$arrange))

results$magic <- results$magic %>%
  mutate(
    value_score = rank(TrailingPE),
    quality_score = rank(-ROE) + rank(-OperatingMargin),
    combined_score = value_score + quality_score
  ) %>%
  arrange(desc(combined_score))

walk(names(results), function(nm) {
  write.csv(results[[nm]], file.path(OUTPUT, paste0(nm, "_screen.csv")), row.names = FALSE)
})

summary_table <- data.frame(
  Screen = c("Value", "Growth", "Quality", "Dividend", "Undervalued", "Momentum", "Magic"),
  Count = sapply(results, nrow)
)

print(summary_table)

screen_counts <- table(unlist(map(results, ~ .x$Symbol)))
multi_screen <- as.data.frame(screen_counts) %>%
  filter(Freq >= 3) %>%
  arrange(desc(Freq))

if (nrow(multi_screen) > 0) {
  print(multi_screen)
}

