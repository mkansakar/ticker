library(quantmod)
library(xts)
library(ggplot2)
library(scales)
library(gridExtra)
library(TTR)
library(grid)

tickers <- read.csv("/Users/kasa/RStudio/sp500_full_fundamentals.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
financial_data <- tickers[, c("Symbol", "Company", "MarketCap", "TrailingPE", "ForwardPE", "DebtToEquity")]

start <- as.Date("2025-01-01")
end <- Sys.Date()

symbols <- financial_data$Symbol

path1 <- "/Users/kasa/RStudio/S_And_P/"

stock_data <- list()
for (sym in symbols) {
  tryCatch({
    cat("Downloading:", sym, "\n")
    data <- getSymbols(sym, src = "yahoo", from = start, to = end, auto.assign = FALSE)
    stock_data[[sym]] <- data
    Sys.sleep(0.25)
  }, error = function(e) {
    cat("Error downloading", sym, "\n")
  })
}

stock_data <- stock_data[!sapply(stock_data, is.null)]

# while (dev.cur() > 1) dev.off()

# Create a single PDF file
pdf_file <- paste0(path1, "sp500_fundamental_charts.pdf")
pdf(pdf_file, width = 16, height = 11) 


for (symbol in names(stock_data)) {
  tryCatch({ 
  fin_info <- financial_data[financial_data$Symbol == symbol, , drop = FALSE]
  if (nrow(fin_info) == 0) next
  
  comp <- paste(fin_info$Symbol, fin_info$Company, sep = " ")
  
  market_cap <- ifelse(is.na(fin_info$MarketCap), "N/A", fin_info$MarketCap)
  trailing_pe <- ifelse(is.na(fin_info$TrailingPE), "N/A", round(as.numeric(fin_info$TrailingPE), 1))
  forward_pe <- ifelse(is.na(fin_info$ForwardPE), "N/A", round(as.numeric(fin_info$ForwardPE), 1))
  debt_eq <- ifelse(is.na(fin_info$DebtToEquity), "N/A", round(as.numeric(fin_info$DebtToEquity), 1))
  
  file <- paste0(path1, gsub("/", "_", symbol), ".jpeg")
  
  df <- data.frame(
    Date = index(stock_data[[symbol]]),
    Open = as.numeric(Op(stock_data[[symbol]])),
    High = as.numeric(Hi(stock_data[[symbol]])),
    Low = as.numeric(Lo(stock_data[[symbol]])),
    Close = as.numeric(Cl(stock_data[[symbol]])),
    Volume = as.numeric(Vo(stock_data[[symbol]]))
  )
  
  df$SMA3 <- SMA(df$Close, n = 3)
  df$SMA8 <- SMA(df$Close, n = 8)
  df$RSI <- RSI(df$Close, n = 14)
  
  current_price <- tail(df$Close, 1)
  
  # Time-based S/R
  resistance_20d <- round(max(tail(df$High, 20)), 2)
  support_20d <- round(min(tail(df$Low, 20)), 2)
  resistance_50d <- round(max(tail(df$High, 50)), 2)
  support_50d <- round(min(tail(df$Low, 50)), 2)

  
  df <- na.omit(df)
  
  if (nrow(df) == 0) next
  
  p1 <- ggplot(df, aes(x = Date)) +
    geom_linerange(aes(ymin = Low, ymax = High), size = 0.5) +
    geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                  ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                  fill = Close > Open), color = "black", size = 0.2) +
    geom_line(aes(y = SMA3), color = "red", size = 1) +
    geom_line(aes(y = SMA8), color = "blue", size = 1) +
    scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue"), guide = "none") +
    labs(y = "Price", x = "", title = comp) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 0, 1), "lines")
    )
  
  fund_text <- paste0(
    "Mkt Cap: ", market_cap, ";  ",
    "P/E: ", trailing_pe, ";  ",
    "Fwd P/E: ", forward_pe, ";  ",
    "D/E: ", debt_eq, " ----  ",
    "Price: $", round(current_price, 2), ";  ",
    "R20: $", resistance_20d, ";  ",
    "S20: $", support_20d, ";  ",
    "R50: $", resistance_50d, ";  ",
    "S50: $", support_50d
  )
  
  p_fund <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = fund_text, 
             size = 4, hjust = 0.5, vjust = 0.5, fontface = "bold") +
    theme_void() +
    theme(
      plot.margin = unit(c(0, 1, 0, 1), "lines"),
      plot.background = element_rect(fill = "green", color = NA)
    )
  
  p2 <- ggplot(df, aes(x = Date, y = Volume)) +
    geom_bar(stat = "identity", fill = "gray70") +
    labs(y = "Volume", x = "Date") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0, 1, 0, 1), "lines")
    )
  
  p3 <- ggplot(df, aes(x = Date, y = RSI)) +
    geom_line(color = "purple", size = 1) +
    geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 50, linetype = "dotted", color = "gray70") +
    ylim(0, 100) +
    labs(y = "RSI(14)", x = "Date") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0, 1, 1, 1), "lines")
    )
  
  #jpeg(file, width = 1280, height = 900, quality = 100)

 
    grid.arrange(p1, p_fund, p2, p3, ncol = 1, heights = c(2, 0.2, 0.5, 1))

  
  cat("Created ggplot chart for:", symbol, "\n")
  
  }, error = function(e) {
    cat("Error arranging chart for", symbol, "\n")
  })  
}
dev.off()
cat("PDF saved to:", pdf_file, "\n")