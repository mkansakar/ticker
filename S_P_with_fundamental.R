library(quantmod)
library(xts)

tickers <- read.csv("/Users/kasa/RStudio/sp500_full_fundamentals.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)
financial_data <- tickers[, c("Symbol", "Company", "MarketCap", "TrailingPE", "ForwardPE", "DebtToEquity")]

start <- as.Date("2025-05-01")
end <- Sys.Date()

stock_data <- list()
symbols <- financial_data$Symbol

path1 <- "/Users/kasa/RStudio/S_And_P/500SP_"
stock_data <- lapply(symbols, function(sym) {
  tryCatch({
    cat("Downloading:", sym, "\n")
    Sys.sleep(0.5)
    getSymbols(sym,src = "yahoo",from = start,to = end,auto.assign = FALSE,return.class = "xts")
  }, error = function(e) {
    cat("Error downloading", sym, "\n")
    NULL
  })
})

names(stock_data) <- symbols
stock_data <- stock_data[!sapply(stock_data, is.null)]

while (dev.cur() > 1) dev.off()

for (symbol in names(stock_data)) {
  fin_info <- financial_data[financial_data$Symbol == symbol, , drop = FALSE]
  if (nrow(fin_info) == 0) next
  comp = paste(fin_info$Symbol,fin_info$Company,sep = " ")
  market_cap <- ifelse(is.na(fin_info$MarketCap), "N/A", fin_info$MarketCap)
  trailing_pe <- ifelse(is.na(fin_info$TrailingPE), "N/A", round(as.numeric(fin_info$TrailingPE), 1))
  forward_pe <- ifelse(is.na(fin_info$ForwardPE), "N/A", round(as.numeric(fin_info$ForwardPE), 1))
  debt_eq <- ifelse(is.na(fin_info$DebtToEquity), "N/A", round(as.numeric(fin_info$DebtToEquity), 1))
  
  title <- paste("Symbol:", comp,"| Mkt Cap:", market_cap,"| Trailing P/E:", trailing_pe,"| Forward P/E:", forward_pe,"| Debt/Equity:", debt_eq)
  
  file <- paste0(path1, gsub("/", "_", symbol), ".jpeg")
  
  jpeg(file, width = 1280, height = 900)
  
  chartSeries(stock_data[[symbol]],name = title,show.grid = TRUE,theme = chartTheme("white"),bar.type = "ohlc",up.col = "blue",dn.col = "red")
  
  addSMA(n = 3, col = "pink")
  addSMA(n = 8, col = "blue")
  addRSI(n = 14, maType = "EMA", wilder = TRUE)
  
  dev.off()
  
  cat("Created chart for:", symbol, "\n")
}