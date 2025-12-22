library(quantmod)
library(xts)
library(ggplot2)
library(scales)
library(gridExtra)
library(TTR)
library(grid)
library(zoo)

tickers <- read.csv("c:/temp/sp500_full_fundamentals_1.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
financial_data <- tickers[, c("Symbol", "Company", "MarketCap", "TrailingPE", "ForwardPE", "DebtToEquity")]

start <- as.Date("2025-01-01")
end <- Sys.Date()

symbols <- financial_data$Symbol

path1 <- "C:/Temp/500-S_And_P/"

stock_data <- list()

for (sym in symbols) {
  tryCatch({
    cat("Downloading:", sym, "\n")
    x <- getSymbols(sym, src = "yahoo", from = start, to = end, auto.assign = FALSE)
    stock_data[[sym]] <- na.omit(x)
    Sys.sleep(0.5)
  }, error = function(e) {
    cat("Error downloading", sym, "\n")
  })
}

stock_data <- stock_data[lengths(stock_data) > 0]

pdf_file <- paste0(path1, "snp500_fundamental_ggplot_charts.pdf")
pdf(pdf_file, width = 16, height = 11)


compute_volume_profile <- function(df, n_bins = 40) {
  price_min <- min(df$Low, na.rm = TRUE)
  price_max <- max(df$High, na.rm = TRUE)
  
  bins <- seq(price_min, price_max, length.out = n_bins + 1)
  bin_mid <- (bins[-1] + bins[-length(bins)]) / 2
  
  volume_by_price <- numeric(length(bin_mid))
  
  for (i in seq_len(nrow(df))) {
    idx <- which(bin_mid >= df$Low[i] & bin_mid <= df$High[i])
    if (length(idx) > 0) {
      volume_by_price[idx] <- volume_by_price[idx] +
        df$Volume[i] / length(idx)
    }
  }
  
  vp <- data.frame(
    Price = bin_mid,
    Volume = volume_by_price
  )
  
  poc_price <- vp$Price[which.max(vp$Volume)]
  
  hvn_threshold <- quantile(vp$Volume, 0.80, na.rm = TRUE)
  hvn_prices <- vp$Price[vp$Volume >= hvn_threshold]
  
  list(
    profile = vp,
    POC = poc_price,
    HVN = hvn_prices
  )
}


for (symbol in names(stock_data)) {
  
  fin_info <- financial_data[financial_data$Symbol == symbol, , drop = FALSE  ]
  if (nrow(fin_info) == 0) next
  
  data_xts <- stock_data[[symbol]]
  if (nrow(data_xts) < 50) next
  
  df <- data.frame(
    Date = index(data_xts),
    Open = as.numeric(Op(data_xts)),
    High = as.numeric(Hi(data_xts)),
    Low = as.numeric(Lo(data_xts)),
    Close = as.numeric(Cl(data_xts)),
    Volume = as.numeric(Vo(data_xts))
  )
  
  df <- df[complete.cases(df), ]
  if (nrow(df) < 30) next
  
  df$SMA3 <- SMA(df$Close, 3)
  df$SMA8 <- SMA(df$Close, 8)
  df$RSI <- RSI(df$Close, 14)
  
  df <- na.omit(df)
  if (nrow(df) == 0) next
  
  current_price <- tail(df$Close, 1)
  
  resistance_20d <- round(max(tail(df$High, 20)), 2)
  support_20d <- round(min(tail(df$Low, 20)), 2)
  resistance_50d <- round(max(tail(df$High, 50)), 2)
  support_50d <- round(min(tail(df$Low, 50)), 2)
  
  comp <- paste(fin_info$Symbol, fin_info$Company)
  
  vp <- compute_volume_profile(df, n_bins = 50)
  poc_price <- vp$POC
  hvn_prices <- vp$HVN
  
  p1 <- ggplot(df, aes(x = Date)) +
    geom_linerange(aes(ymin = Low, ymax = High), linewidth = 0.4) +
    geom_rect(
      aes(xmin = Date - 0.3, xmax = Date + 0.3, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Close > Open), color = "black", linewidth = 0.2,show.legend = FALSE) +
    
    
    geom_line(aes(y = SMA3), color = "red", linewidth = 0.8) +
    geom_line(aes(y = SMA8), color = "blue", linewidth = 0.8) +
    
    geom_hline(yintercept = poc_price, color = "red", linewidth = 1) +
    geom_hline(yintercept = hvn_prices, color = "orange", alpha = 0.4) +
    
    scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
    labs(title = comp, y = "Price", x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
  
  fund_text_left <- paste0(
    "Market Cap: ", fin_info$MarketCap, "\n",
    "Trailing P/E: ", round(fin_info$TrailingPE, 2), "\n",
    "Forward P/E: ", round(fin_info$ForwardPE, 2), "\n",
    "Debt to equity: ", round(fin_info$DebtToEquity, 2)
  )
  
  
  fund_text_right <- paste0(
    "Price: ", round(current_price, 2), "\n",
    "Resistance20: ", resistance_20d, "\n" ,
    "Support20: ", support_20d, "\n", 
    "Resistance50: ", resistance_50d, "\n",
    "Support50: ", support_50d
  )
  
  
  p_fund <- ggplot() +
    annotate("text", x = 0.05, y = 0.5, label = fund_text_left,
             hjust = 0, vjust = 0.5, size = 4, fontface = "bold") +
    annotate("text", x = 0.55, y = 0.5, label = fund_text_right,
             hjust = 0, vjust = 0.5, size = 4, fontface = "bold") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.margin = unit(c(0, 1, 0, 1), "lines"),
      plot.background = element_rect(fill = "lightgreen", color = NA)
    )
  
  p_vp <- ggplot(vp$profile, aes(x = Volume, y = Price)) +
    geom_col(fill = "gray70") +
    geom_hline(yintercept = poc_price, color = "red") +
    theme_minimal() +
    labs(title = "Volume Profile", x = "Volume", y = "Price")
  
  p2 <- ggplot(df, aes(x = Date, y = Volume)) +
    geom_col(fill = "gray70") +
    labs(y = "Volume", x = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
  
  p3 <- ggplot(df, aes(x = Date, y = RSI)) +
    geom_line(color = "purple", linewidth = 0.9) +
    geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 50, linetype = "dotted", color = "gray70") +
    ylim(0, 100) +
    labs(y = "RSI(14)", x = "Date") +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
  
  grid.arrange(p1, p_fund, p2, p3, ncol = 1, heights = c(2.2, 0.5, 0.6, 1))
  
  cat("Created chart for:", symbol, "\n")
}

dev.off()
cat("PDF saved to:", pdf_file, "\n")
