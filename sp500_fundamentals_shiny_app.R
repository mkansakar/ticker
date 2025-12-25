library(shiny)
library(quantmod)
library(ggplot2)
library(TTR)
library(gridExtra)
library(scales)
library(data.table)
library(shinyjs) 

#===================================
#Download fundamental Python module
#===================================
PYTHON <- "/Users/kasa/RStudio/studio/bin/python"  
SCRIPT <- "/Users/kasa/RStudio/download_fundamentals.py"
INPUT  <- "/Users/kasa/RStudio/symbols_1.csv"
OUTPUT <- "/Users/kasa/RStudio/stock_fundamentals_out.csv"

run_python_fundamentals <- function() {
  cmd <- sprintf(
    '"%s" "%s" "%s" "%s"',
    PYTHON, SCRIPT, INPUT, OUTPUT
  )
  cat("Running:\n", cmd, "\n\n")
  system(cmd, intern = FALSE)
  
  if (!file.exists(OUTPUT)) {
    stop("Python ran but output CSV was not created")
  }
}


if (file.exists("OUTPUT")) {
  tickers <- read.csv(OUTPUT, stringsAsFactors = FALSE)
} else {
  return(NULL)
}

financial_data <- tickers[, c(
  "Symbol", "Company", "Sector",
  "MarketCap", "revenue", "net_income", "ROE", "OperatingMargin",
  "FreeCashFlow", "EPS", "dividend_yield",
  "BookValue", "TrailingPE", "ForwardPE",
  "DebtToEquity","gross_profits","ebitda"
)]

symbols <- sort(unique(financial_data$Symbol))

format_big <- function(x) {
  if (is.na(x)) return("N/A")
  label_number(scale_cut = cut_short_scale())(x)
}

#===================================
#Calculate VWAP and AVWAP (not used)
#===================================
calculate_vwap <- function(df) {
  tp <- (df$High + df$Low + df$Close) / 3
  cumsum(tp * df$Volume) / cumsum(df$Volume)
}
calculate_anchored_vwap <- function(df, anchor_index) {
  if (length(anchor_index) == 0 || is.na(anchor_index)) {
    return(rep(NA, nrow(df)))
  }
  tp <- (df$High + df$Low + df$Close) / 3
  v  <- df$Volume
  avwap <- rep(NA, nrow(df))
  avwap[anchor_index:nrow(df)] <-
    cumsum(tp[anchor_index:nrow(df)] * v[anchor_index:nrow(df)]) /
    cumsum(v[anchor_index:nrow(df)])
  avwap
}

#===================================
#Read symbol data from DOW folder
#===================================
read_stock_data <- function(symbol) {
  file_path <- paste0("/Users/kasa/RStudio/dow/", symbol, ".csv")
  
  if (!file.exists(file_path)) {return(NULL)}
  
  data <- fread(file_path)
  
  data$Date <- as.Date(data$Date)
  one_year_ago <- Sys.Date() - 366
  data <- data[Date >= one_year_ago]
  
  if (nrow(data) == 0) {return(NULL)}
  
  xts_data <- xts(
    x = data[, .(Open, High, Low, Close, Volume, Adjusted)],
    order.by = data$Date
  )
  return(xts_data)
}
# =======================
# UI
# =======================
ui <- fluidPage(
  titlePanel("Stock Screening"),
  sidebarLayout(
    sidebarPanel(
      actionButton("runBtn", "Fundamentals", icon = icon("download")),
      actionButton("downloadBtn", "Market Data", icon = icon("chart-line")),
      actionButton("refreshBtn", "Reload Data",icon = icon("sync"),                   ),
      selectInput(
        "symbol",
        "Select Symbol",
        choices = symbols,
        selected = symbols[1]
      ),
      width = 3
    ),  # <-- Closing parenthesis for sidebarPanel() was missing
    mainPanel(
      plotOutput("chart", height = "800px"),
      width = 9
    )
  )
)
# =======================
# Server
# =======================
server <- function(input, output, session) {
  status <- reactiveVal("Ready")
  
  observeEvent(input$downloadBtn, {
    status("Updating Market data...")
    shinyjs::disable("downloadBtn")
    source("/Users/kasa/RStudio/download_dow_to_csv.R", local = TRUE)
    
    shinyjs::enable("downloadBtn")
  })
  
  observeEvent(input$runBtn, {
    status("Running Python script...")
    #shinyjs::disable("runBtn")
    
    tryCatch({
      success <- run_python_fundamentals()
      
      if (success) {
        if (!is.null(data)) {
          status(paste("Success! Loaded", nrow(data), "records"))
        } else {
          status("Python completed but no data loaded")
        }
      } else {
        status("Python script failed")
      }
    }, error = function(e) {
      status(paste("Error:", e$message))
    }, finally = {
      shinyjs::enable("downloadBtn")
    })
  })
  
  stock_data <- reactive({
    req(input$symbol)
    read_stock_data(input$symbol)
  })
  output$chart <- renderPlot({
    
    xt <- stock_data()
    if (is.null(xt) || nrow(xt) == 0) {
      # Create empty plot with message
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("No data found for", input$symbol), 
                 size = 6) +
        theme_void()
      return(p)
    }
    fin <- financial_data[financial_data$Symbol == input$symbol, , drop = FALSE]
    
    df <- data.frame(
      Date   = index(xt),
      Open   = as.numeric(Op(xt)),
      High   = as.numeric(Hi(xt)),
      Low    = as.numeric(Lo(xt)),
      Close  = as.numeric(Cl(xt)),
      Volume = as.numeric(Vo(xt))
    )
    df <- na.omit(df)
    
    if (nrow(df) < 20) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Insufficient data for", input$symbol, 
                               "\nOnly", nrow(df), "days available"), 
                 size = 6) +
        theme_void()
      return(p)
    }
    df$RSI  <- RSI(df$Close, 14)
    
    macd_xts <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9)
    
    df$MACD <- as.numeric(macd_xts[, "macd"])
    df$Signal <- as.numeric(macd_xts[, "signal"])
    df$MACD_Histogram <- df$MACD - df$Signal
    
    bb <- as.data.frame(BBands(df$Close, n = 20, sd = 2))
    df$BB_up  <- bb$up
    df$BB_mid <- bb$mavg
    df$BB_dn  <- bb$dn
    
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
        linewidth = 0.2, show.legend = FALSE
      ) +
      geom_line(aes(y = BB_up), color = "darkgreen", alpha = 0.5) +
      geom_line(aes(y = BB_mid), color = "green", alpha = 0.5) +
      geom_line(aes(y = BB_dn), color = "darkgreen", alpha = 0.5) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      labs(
        title = paste(fin$Symbol, "-", fin$Company, "(", fin$Sector, ")"),
        y = "Price", x = ""
      ) +
      theme_minimal()
    
    p_volume <- ggplot(df, aes(Date, Volume)) +
      geom_col(aes(fill = Close >= Open), alpha = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      theme_minimal() +
      labs(y = "Volume", x = "")
    
    p_rsi <- ggplot(df, aes(Date, RSI)) +
      geom_line(color = "purple") +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
      ylim(0, 100) +
      theme_minimal()
    
    p_macd <- ggplot(df, aes(Date)) +
      geom_line(aes(y = MACD), color = "blue", linewidth = 0.8) +
      geom_line(aes(y = Signal), color = "red", linewidth = 0.8) +
      geom_col(aes(y = MACD_Histogram), 
               fill = ifelse(df$MACD_Histogram >= 0, "green", "red"), 
               alpha = 0.6) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.3) +
      labs(y = "MACD", x = "") +
      theme_minimal()
    
    # =======================
    # FUNDAMENTAL PANEL
    # =======================
    current_price <- tail(df$Close, 1)
    high_52 <- max(tail(df$High, min(252, nrow(df))))
    low_52  <- min(tail(df$Low,  min(252, nrow(df))))
    
    p_fund <- ggplot() +
      annotate(
        "text", 0.05, 0.5,
        label = paste(
          "Trailing P/E:", round(fin$TrailingPE, 2),
          "\nForward P/E:", round(fin$ForwardPE, 2),
          "\nDebt/Equity:", round(fin$DebtToEquity, 2),
          "\nEarning/Share:", fin$EPS,
          "\nReturn/Equity:", round(fin$ROE, 2),
          "\nDividend Yield:", round(fin$dividend_yield, 2)
        ),
        hjust = 0, size = 4
      ) +
      annotate(
        "text", 0.38, 0.5,
        label = paste(
          "Operating Margin:", round(fin$OperatingMargin, 2),
          "\nBook Value:", fin$BookValue,
          "\nCurrent Price:", round(current_price, 2),
          "\n52W High:", round(high_52, 2),
          "\n52W Low:", round(low_52, 2)
        ),
        hjust = 0, size = 4
      ) +
      annotate(
        "text", 0.70, 0.5,
        label = paste(
          "Market Cap:", format_big(fin$MarketCap),
          "\nFree Cash Flow:", format_big(fin$FreeCashFlow),
          "\nRevenue:", format_big(fin$revenue),
          "\nGross Profit:", format_big(fin$gross_profits),
          "\nEBITDA:", format_big(fin$ebitda),
          "\nNet Income:", format_big(fin$net_income)
        ),
        hjust = 0, size = 4
      ) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "lightgreen", color = NA)
      )
    
    grid.arrange(
      p_fund,
      p_price,
      p_macd,
      p_rsi,
      ncol = 1,
      heights = c(0.6, 2.0, 1, 1)
    )
  })
}

shinyApp(ui, server)
