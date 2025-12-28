library(dplyr)
library(readr)
library(tidyverse)

INDICATOR_DIR <- "/Users/kasa/RStudio/indicators"
OUTPUT_FILE <- "/Users/kasa/RStudio/price_movement_45d.csv"

# ---------------- LOAD DATA ----------------
files <- list.files(INDICATOR_DIR, "\\.csv$", full.names = TRUE)

data <- map_df(files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  
  df$symbol <- as.character(
    tools::file_path_sans_ext(basename(f))
  )
  df
})

# ---------------- LAST 45 TRADING DAYS ----------------
recent <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(symbol) %>%
  filter(date >= max(date) - 45) %>%
  ungroup()

# ---------------- 45-DAY SUMMARY ----------------
summary_45d <- recent %>%
  group_by(symbol) %>%
  summarise(
    start_price = first(close),
    end_price   = last(close),
    return_45d  = (end_price / start_price - 1) * 100,
    vol_45d     = sd(ret_1d, na.rm = TRUE) * 100,
    avg_volume  = mean(volume, na.rm = TRUE),
    vol_spike   = max(volume, na.rm = TRUE) / avg_volume,
    n_days      = n(),
    .groups = "drop"
  ) %>%
  filter(n_days >= 20)

# ---------------- MARKET BASELINE ----------------
market_return <- median(summary_45d$return_45d, na.rm = TRUE)

# ---------------- CROSS-SECTIONAL Z-SCORES ----------------
summary_45d <- summary_45d %>%
  mutate(
    z_return = scale(return_45d)[,1],
    z_vol    = scale(vol_45d)[,1],
    z_volume = scale(vol_spike)[,1],
    
    anomaly_score = abs(z_return) + 0.5 * abs(z_vol) + 0.5 * abs(z_volume)
  ) %>%
  arrange(desc(anomaly_score)) %>%
  #slice_head(n = 25) %>%
  arrange(symbol) 

# ---------------- INTERPRETABLE LABELS ----------------
results <- summary_45d %>%
  mutate(
    category = case_when(
      z_return >=  2 ~ "STRONG_OUTPERFORMER",
      z_return <= -2 ~ "STRONG_UNDERPERFORMER",
      z_vol    >=  2 ~ "HIGH_VOLATILITY",
      z_volume >=  2 ~ "VOLUME_SPIKE",
      TRUE          ~ "NORMAL"
    ),
    
    market_context = case_when(
      return_45d > 0 & market_return < 0 ~ "UP_WHILE_MARKET_DOWN",
      return_45d < 0 & market_return > 0 ~ "DOWN_WHILE_MARKET_UP",
      return_45d > market_return * 1.5   ~ "OUTPERFORMING_MARKET",
      return_45d < market_return * 0.5   ~ "UNDERPERFORMING_MARKET",
      TRUE                               ~ "IN_LINE"
    )
  ) %>%
  arrange(symbol)

# ---------------- SAVE ----------------
write_csv(results, file.path("/Users/kasa/RStudio/price_movement_45d.csv"))



