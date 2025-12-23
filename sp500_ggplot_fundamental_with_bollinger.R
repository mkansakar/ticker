library(quantmod)
library(ggplot2)
library(TTR)
library(gridExtra)
library(grid)
library(scales)

tickers <- read.csv(
  "c:/temp/S_and_P500_detailed_fundamentals.csv",
  stringsAsFactors = FALSE
)

financial_data <- tickers[, c(
  "Symbol", "Company", "Sector",
  "MarketCap", "revenue", "net_income", "ROE", "OperatingMargin",
  "FreeCashFlow", "EPS", "dividend_yield",
  "BookValue", "TrailingPE", "ForwardPE",
  "DebtToEquity","gross_profits","ebitda"
)]

symbols <- unique(financial_data$Symbol)

start <- Sys.Date() - 366
end   <- Sys.Date()

output_path <- "C:/Temp/500-S_And_P"
dir.create(output_path, showWarnings = FALSE)

datetime_string <- format(Sys.time(), "%Y_%m_%d_%H_%M")
pdf_file <- file.path(
  output_path,
  paste0("snp500_fundamental_charts_", datetime_string, ".pdf")
)

format_big <- function(x) {
  if (is.na(x)) return("N/A")
  label_number(scale_cut = cut_short_scale())(x)
}

calculate_vwap <- function(df) {
  tp <- (df$High + df$Low + df$Close) / 3
  cumsum(tp * df$Volume) / cumsum(df$Volume)
}

calculate_anchored_vwap <- function(df, anchor_index) {
  tp <- (df$High + df$Low + df$Close) / 3
  v  <- df$Volume
  avwap <- rep(NA, nrow(df))
  
  avwap[anchor_index:nrow(df)] <-
    cumsum(tp[anchor_index:nrow(df)] * v[anchor_index:nrow(df)]) /
    cumsum(v[anchor_index:nrow(df)])
  avwap
}

stock_data <- list()

for (sym in symbols) {
  tryCatch({
    x <- getSymbols(sym, src = "yahoo",
                    from = start, to = end,
                    auto.assign = FALSE)
    if (NROW(x) > 100) {
      stock_data[[sym]] <- x
    }
    Sys.sleep(0.1)
  }, error = function(e) NULL)
}

pdf(pdf_file, width = 16, height = 11)

for (symbol in names(stock_data)) {
  
  fin <- financial_data[financial_data$Symbol == symbol, ]
  if (nrow(fin) == 0) next
  
  xt <- stock_data[[symbol]]
  
  df <- data.frame(
    Date   = index(xt),
    Open   = as.numeric(Op(xt)),
    High   = as.numeric(Hi(xt)),
    Low    = as.numeric(Lo(xt)),
    Close  = as.numeric(Cl(xt)),
    Volume = as.numeric(Vo(xt))
  )
  
  df <- na.omit(df)
  if (nrow(df) < 100) next
  
  df$VWAP <- calculate_vwap(df)
  df$RSI  <- RSI(df$Close, 14)
  
  lookback_52w <- tail(df, 252)
  anchor_row <- which.min(lookback_52w$Low)
  anchor_index <- (nrow(df) - nrow(lookback_52w)) + anchor_row
  
  df$AVWAP <- calculate_anchored_vwap(df, anchor_index)
  
  bb <- as.data.frame(BBands(df$Close, n = 20, sd = 2))
  df$BB_up  <- bb$up
  df$BB_mid <- bb$mavg
  df$BB_dn  <- bb$dn
  
  current_price <- tail(df$Close, 1)
  high_52 <- max(tail(df$High, 252))
  low_52  <- min(tail(df$Low, 252))
  
  title <- paste(fin$Symbol, "-", fin$Company, "(", fin$Sector, ")")
  candle_w <- diff(range(df$Date)) / nrow(df) * 0.6
  
  p_price <- ggplot(df, aes(Date)) +
    geom_linerange(aes(ymin = Low, ymax = High),
                   color = "gray60", linewidth = 0.4) +
    geom_rect(
      aes(
        xmin = Date - candle_w,
        xmax = Date + candle_w,
        ymin = pmin(Open, Close),
        ymax = pmax(Open, Close),
        fill = Close >= Open
      ),
      color = "black",
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    
    geom_line(aes(y = VWAP), color = "darkblue", linewidth = 1) +
    geom_line(aes(y = AVWAP), color = "blue", linewidth = 1, linetype = "dashed") +
    
    geom_line(aes(y = BB_up),  color = "darkgreen", alpha = 0.5) +
    geom_line(aes(y = BB_mid), color = "green", alpha = 0.5) +
    geom_line(aes(y = BB_dn),  color = "darkgreen", alpha = 0.5) +
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red")) +
    labs(title = title, y = "Price", x = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = element_blank()
    )
  
  p_fund <- ggplot() +
    annotate("text", 0.05, 0.5,
             label = paste(
               "Market Cap:", format_big(fin$MarketCap),
               "\nTrailing P/E:", round(fin$TrailingPE, 2),
               "\nForward P/E:", round(fin$ForwardPE, 2),
               "\nDebt/Equity:", round(fin$DebtToEquity, 2),
               "\nReturn/Equity:", round(fin$ROE, 2),
               "\nDividend Yield:", round(fin$dividend_yield, 2)
             ), hjust = 0, size = 4) +
    annotate("text", 0.38, 0.5,
             label = paste(
               "Return/Equity:", round(fin$ROE, 2),
               "\nOperating Margin:", round(fin$OperatingMargin, 2),
               "\nFree Cash Flow:", format_big(fin$FreeCashFlow),
               "\nBook Value:", fin$BookValue,
               "\n52W High:", round(high_52, 2),
               "\n52W Low:", round(low_52, 2)
               
             ), hjust = 0, size = 4) +
    annotate("text", 0.70, 0.5,
             label = paste(
               "Current Price:", round(current_price, 2),
               "\nEarning/Share:", fin$EPS,
               "\nRevenue:", format_big(fin$revenue),
               "\nGross Profit:", format_big(fin$gross_profits),
               "\nEBITDA:", format_big(fin$ebitda),
               "\nNet Income:", format_big(fin$net_income)
             ), hjust = 0, size = 4) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgreen"))
  
  p_volume <- ggplot(df, aes(Date, Volume)) +
    geom_col(aes(fill = Close >= Open), alpha = 0.7) +
    scale_fill_manual(values = c("TRUE"="green","FALSE"="red"), guide = FALSE) +
    labs(y = "Volume", x = "") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2)
    )
  
  p_rsi <- ggplot(df, aes(Date, RSI)) +
    geom_line(color = "purple", linewidth = 0.9) +
    geom_hline(yintercept = c(30,70),
               linetype = "dashed", color = "red") +
    geom_hline(yintercept = 50,
               linetype = "dotted", color = "blue") +
    ylim(0,100) +
    labs(y = "RSI (14)", x = "Date") +
    theme_minimal()
  
  grid.arrange(
    p_fund,
    p_price,
    arrangeGrob(p_volume, p_rsi, ncol = 2),
    ncol = 1,
    heights = c(0.6, 2.5, 1.1)
  )
}
dev.off()
cat("PDF saved to:", pdf_file, "\n")
