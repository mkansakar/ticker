library(dplyr)
library(readr)
library(TTR)
library(purrr)

#setwd("/Users/kasa/RStudio/trading")

DATA_DIR <- "/Users/kasa/RStudio/trading/dow"
OUTPUT <- "/Users/kasa/RStudio/trading/market_features.csv"

symbols <- tools::file_path_sans_ext(list.files(DATA_DIR, "\\.csv$"))

build_one <- function(sym) {
  
  file <- file.path(DATA_DIR, paste0(sym, ".csv"))
  if (!file.exists(file)) return(NULL)
  
  df <- read_csv(file, show_col_types = FALSE) |>
    mutate(Date = as.Date(Date)) |>
    filter(Date >= Sys.Date() - 365, !is.na(Close))
  
  if (nrow(df) < 60) return(NULL)
  
  bb <- BBands(df$Close)
  
  df |>
    mutate(
      Symbol = sym,
      ret_1d  = ROC(Close, 1, "continuous"),
      ret_5d  = ROC(Close, 5),
      ret_20d = ROC(Close, 20),
      vol_5d  = runSD(ret_1d, 5),
      vol_20d = runSD(ret_1d, 20),
      ma_5    = SMA(Close, 5),
      ma_20   = SMA(Close, 20),
      ma_50   = SMA(Close, 50),
      rsi_14  = RSI(Close, 14),
      macd    = MACD(Close)[, "macd"],
      bb_width = (bb[, "up"] - bb[, "dn"]) / bb[, "mavg"],
      target  = as.integer(lead(Close, 5) > Close)
    ) |>
    drop_na()
}

features <- map(symbols, build_one) |> compact() |> bind_rows()
write_csv(features, OUTPUT)

cat("Saved", nrow(features), "rows → market_features.csv\n")
