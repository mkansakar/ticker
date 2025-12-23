library(quantmod)
library(ggplot2)
library(TTR)
library(gridExtra)
library(grid)

# ----------------------------
# INPUT DATA
# ----------------------------
tickers <- read.csv(
  "/Users/kasa/RStudio/S_and_P500_detailed_fundamentals_1.csv",
  stringsAsFactors = FALSE
)

financial_data <- tickers[, c(
  "Symbol", "Company", "Sector",
  "MarketCap", "ROE", "OperatingMargin",
  "FreeCashFlow", "EPS", "dividend_yield",
  "BookValue", "TrailingPE", "ForwardPE",
  "DebtToEquity"
)]

symbols <- unique(financial_data$Symbol)

start <- Sys.Date() - 366
end   <- Sys.Date()

path1 <- "/Users/kasa/RStudio/S_And_P"
dir.create(path1, showWarnings = FALSE)

# ----------------------------
# HELPERS
# ----------------------------
format_big <- function(x, digits = 2) {
  if (is.na(x)) return("N/A")
  x <- as.numeric(x)
  abs_x <- abs(x)
  suffix <- c(K = 1e3, M = 1e6, B = 1e9, T = 1e12)
  for (s in rev(names(suffix))) {
    if (abs_x >= suffix[[s]])
      return(paste0(round(x / suffix[[s]], digits), s))
  }
  round(x, digits)
}

compute_volume_profile <- function(df, n_bins = 50) {
  bins <- seq(min(df$Low), max(df$High), length.out = n_bins + 1)
  mids <- (bins[-1] + bins[-length(bins)]) / 2
  
  vol <- sapply(seq_along(mids), function(i) {
    idx <- df$Low <= mids[i] & df$High >= mids[i]
    sum(df$Volume[idx], na.rm = TRUE)
  })
  
  vp <- data.frame(Price = mids, Volume = vol)
  
  list(
    profile = vp,
    POC = vp$Price[which.max(vp$Volume)],
    HVN = vp$Price[vp$Volume >= quantile(vp$Volume, 0.8)]
  )
}

# ----------------------------
# DOWNLOAD DATA
# ----------------------------
stock_data <- lapply(symbols, function(sym) {
  tryCatch({
    Sys.sleep(0.25)
    getSymbols(sym, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  }, error = function(e) NULL)
})
names(stock_data) <- symbols
stock_data <- Filter(NROW, stock_data)

# ----------------------------
# OUTPUT
# ----------------------------
pdf_file <- file.path(path1, "snp500_fundamental_charts.pdf")
pdf(pdf_file, width = 16, height = 11)

for (symbol in names(stock_data)) {
  
  fin <- financial_data[financial_data$Symbol == symbol, ]
  if (nrow(fin) == 0) next
  
  xt <- stock_data[[symbol]]
  if (NROW(xt) < 100) next
  
  df <- data.frame(
    Date   = index(xt),
    Open   = as.numeric(Op(xt)),
    High   = as.numeric(Hi(xt)),
    Low    = as.numeric(Lo(xt)),
    Close  = as.numeric(Cl(xt)),
    Volume = as.numeric(Vo(xt))
  )
  
  df$SMA3 <- SMA(df$Close, 3)
  df$SMA8 <- SMA(df$Close, 8)
  df$RSI  <- RSI(df$Close, 14)
  
  bb <- as.data.frame(BBands(df$Close, n = 20, sd = 2))
  df$BB_up  <- bb$up
  df$BB_mid <- bb$mavg
  df$BB_dn  <- bb$dn
  
  
  df <- na.omit(df)
  
  current_price <- tail(df$Close, 1)
  
  last_52 <- tail(df, 252)
  high_52 <- max(last_52$High)
  low_52  <- min(last_52$Low)
  
  r50 <- max(tail(df$High, 50))
  s50 <- min(tail(df$Low, 50))
  
  vp <- compute_volume_profile(df)
  poc_price  <- vp$POC
  hvn_prices <- vp$HVN
  
  title <- paste(fin$Symbol, "-", fin$Company, "(", fin$Sector, ")")
  
  candle_width <- diff(range(df$Date)) / nrow(df) * 0.7
  
  # ----------------------------
  # PRICE PANEL
  # ----------------------------
  p_price <- ggplot(df, aes(Date)) +
    geom_linerange(aes(ymin = Low, ymax = High), linewidth = 0.4) +
    geom_rect(
      aes(xmin = Date - candle_width,
          xmax = Date + candle_width,
          ymin = pmin(Open, Close),
          ymax = pmax(Open, Close)),
      fill = "gray60", color = "black", linewidth = 0.2
    ) +
    geom_line(aes(y = SMA3), color = "red") +
    geom_line(aes(y = SMA8), color = "blue") +
    geom_line(aes(y = BB_up), color = "darkgreen") +
    geom_line(aes(y = BB_mid), color = "green") +
    geom_line(aes(y = BB_dn), color = "darkgreen") +
    geom_hline(yintercept = poc_price, color = "red", linewidth = 1) +
    { if (length(hvn_prices) > 0)
      geom_hline(yintercept = hvn_prices, color = "orange", alpha = 0.3)
    } +
    labs(title = title, y = "Price", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  # ----------------------------
  # FUNDAMENTALS PANEL
  # ----------------------------
  p_fund <- ggplot() +
    annotate("text", 0.05, 0.5,
             label = paste(
               "Market Cap:", format_big(fin$MarketCap),
               "\nTrailing P/E:", round(fin$TrailingPE, 2),
               "\nForward P/E:", round(fin$ForwardPE, 2),
               "\nDebt/Equity:", round(fin$DebtToEquity, 2),
               "\nDividend Yield:", round(fin$dividend_yield, 2)
             ), hjust = 0, size = 4) +
    annotate("text", 0.38, 0.5,
             label = paste(
               "ROE:", round(fin$ROE, 2),
               "\nOperating Margin:", round(fin$OperatingMargin, 2),
               "\nFree Cash Flow:", format_big(fin$FreeCashFlow),
               "\nBook Value:", fin$BookValue,
               "\nEPS:", fin$EPS
             ), hjust = 0, size = 4) +
    annotate("text", 0.70, 0.5,
             label = paste(
               "Price:", round(current_price, 2),
               "\n52W High:", round(high_52, 2),
               "\n52W Low:", round(low_52, 2),
               "\nResistance 50:", round(r50, 2),
               "\nSupport 50:", round(s50, 2)
             ), hjust = 0, size = 4) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgreen"))
  
  # ----------------------------
  # RSI PANEL
  # ----------------------------
  p_rsi <- ggplot(df, aes(Date, RSI)) +
    geom_line(color = "purple") +
    geom_hline(yintercept = c(30, 70), linetype = "dashed") +
    geom_hline(yintercept = 50, linetype = "dotted") +
    ylim(0, 100) +
    theme_minimal()
  
  grid.arrange(p_fund, p_price, p_rsi, ncol = 1,
               heights = c(0.6, 2.5, 1))
}

dev.off()
cat("PDF saved to:", pdf_file, "\n")
