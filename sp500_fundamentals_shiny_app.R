library(shiny)
library(quantmod)
library(ggplot2)
library(TTR)
library(gridExtra)
library(scales)
library(data.table)
library(shinyjs) 
source("shap_engine.R")

#===================================
#Download fundamental Python module
#===================================
PYTHON <- "/Users/kasa/RStudio/studio/bin/python"  
SCRIPT <- "/Users/kasa/RStudio/download_fundamentals.py"
INPUTSYM  <- "/Users/kasa/RStudio/symbols.csv"
OUTPUT <- "/Users/kasa/RStudio/stock_fundamentals_out_1.csv"
MOVEMENTS <- "/Users/kasa/RStudio/price_movement_45d.csv"

run_python_fundamentals <- function() {
  cmd <- sprintf(
    '"%s" "%s" "%s" "%s"',
    PYTHON, SCRIPT, INPUTSYM, OUTPUT
  )
  cat("Running:\n", cmd, "\n\n")
  system(cmd, intern = FALSE)
  
  if (!file.exists(OUTPUT)) {
    stop("Python ran but output CSV was not created")
  }
}

top_n_by <- function(df, metric, n = 25, decreasing = TRUE) {
  # Filter out NA and invalid values based on metric type
  filtered <- switch(metric,
                     "TrailingPE" = df %>% filter(!is.na(TrailingPE), TrailingPE > 0),
                     "ForwardPE" = df %>% filter(!is.na(ForwardPE), ForwardPE > 0),
                     "ROE" = df %>% filter(!is.na(ROE)),
                     "DebtToEquity" = df %>% filter(!is.na(DebtToEquity), DebtToEquity >= 0),
                     "RevenueGrowth" = df %>% filter(!is.na(RevenueGrowth)),
                     "EPS" = df %>% filter(!is.na(EPS)),
                     "FreeCashFlow" = df %>% filter(!is.na(FreeCashFlow)),
                     "CurrentRatio" = df %>% filter(!is.na(CurrentRatio), CurrentRatio > 0),
                     "dividend_yield" = df %>% filter(!is.na(dividend_yield), dividend_yield >= 0),
                     "MarketCap" = df %>% filter(!is.na(MarketCap), MarketCap > 0),
                     # Default for other metrics
                     df %>% filter(!is.na(.data[[metric]]))
  )
  
  # Sort and get top N
  if (decreasing) {
    filtered %>% arrange(desc(.data[[metric]])) %>% head(n)
  } else {
    filtered %>% arrange(.data[[metric]]) %>% head(n)
  }
}

if (file.exists(OUTPUT)) {
  fundamental_data <- read.csv(OUTPUT, stringsAsFactors = FALSE)
} else {
  stop("Fundamental output CSV not found: ", OUTPUT)
}

financial_data <- fundamental_data[, c(
  "Symbol", "Company", "Sector",
  "MarketCap", "revenue", "net_income", "ROE", 
  "OperatingMargin",  "FreeCashFlow", "EPS", 
  "dividend_yield", "BookValue", "TrailingPE",
  "ForwardPE", "DebtToEquity","gross_profits","ebitda","CurrentRatio",
  "Recommendation","EnterpriseToEBITDA","FiveYearAvgDividendYield",
  "TargetMeanPrice","anomaly_score","category","return_45d","market_context"
)]

symbols <- sort(unique(financial_data$Symbol))

format_big <- function(x) {
  if (is.na(x)) return("N/A")
  label_number(scale_cut = cut_short_scale())(x)
}

financial_data <- financial_data %>%
  mutate(
    Symbol = trimws(Symbol),
    Sector = trimws(Sector)
  ) %>%
  filter(!is.na(Symbol), !is.na(Sector))

sectors <- sort(unique(financial_data$Sector))

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
      actionButton("downloadBtn", "Market Data", icon = icon("sync")),
      
      #actionButton("runBtn", "Run SHAP", icon = icon("play")), br(), br(), tableOutput("shapTable"),
      
      selectInput("rank_filter", "Filter Stocks By:",
        choices = c(
          "All Symbols" = "all",
          "Top 25 High Movement" = "movement",
          "Top 25 Market Cap" = "market_cap",
          "Top 25 Low P/E" = "low_pe",
          "Top 25 Low Forward P/E" = "low_forward_pe",
          "Top 25 High ROE" = "high_roe",
          "Top 25 Low Debt to Equity" = "low_debt",
          "Top 25 Revenue Growth" = "revenue_growth",
          "Top 25 High EPS" = "high_eps",
          "Top 25 Free Cash Flow" = "high_fcf",
          "Top 25 High Current Ratio" = "high_current_ratio",
          "Top 25 Dividend Yield" = "high_dividend"
        ),
        selected = "All Stocks" ),
      selectInput("symbol", "Select Symbol:", choices = NULL),
      width = 3),
    
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

  ranked_symbols <- reactive({
    df <- fundamental_data
    
    res <- switch(input$rank_filter,
                  "all"                  = df,
                  "movement"             = top_n_by(df, "anomaly_score"),
                  "market_cap"           = top_n_by(df, "MarketCap"),
                  "low_pe"               = top_n_by(df, "TrailingPE", decreasing = FALSE),
                  "low_forward_pe"       = top_n_by(df, "ForwardPE", decreasing = FALSE),
                  "high_roe"             = top_n_by(df, "ROE"),
                  "low_debt"             = top_n_by(df, "DebtToEquity", decreasing = FALSE),
                  "revenue_growth"       = top_n_by(df, "RevenueGrowth"),
                  "high_eps"             = top_n_by(df, "EPS"),
                  "high_fcf"             = top_n_by(df, "FreeCashFlow"),
                  "high_current_ratio"   = top_n_by(df, "CurrentRatio"),
                  "high_dividend"        = top_n_by(df, "dividend_yield"),
                  df 
    )
    arrange(res, Symbol)
  })
  
  selected_fundamental <- reactive({
    req(input$symbol)
    
    fundamental_data[
      fundamental_data$Symbol == input$symbol,
      ,
      drop = FALSE
    ]
  })
  
  is_high_movement <- reactive({
    fin <- selected_fundamental()
    
    nrow(fin) == 1 &&
      !is.na(fin$anomaly_score) &&
      fin$anomaly_score >= 2
  })

  observeEvent(ranked_symbols(), {
    
    syms <- ranked_symbols()$Symbol
    
    updateSelectInput(
      session,
      "symbol",
      choices = syms,
      selected = syms[1]
    )
  })
#==============================
# Sector and symbol selection
#==============================
  observeEvent(input$sector, {
    
    available_symbols <- financial_data %>%
      filter(Sector == input$sector) %>%
      arrange(Symbol) %>%
      pull(Symbol)
    
    updateSelectInput(
      session,
      "symbol",
      choices = available_symbols,
      selected = ifelse(length(symbols) > 0, symbols[1], NULL)
    )
  }, ignoreInit = FALSE)
  
  selected_fundamental <- reactive({
    req(input$symbol)
    
    financial_data %>%
      filter(Symbol == input$symbol)
  })
  
  observeEvent(input$downloadBtn, {
    status("Updating Market data...")
    shinyjs::disable("downloadBtn")
    source("/Users/kasa/RStudio/download_dow_to_csv.R", local = TRUE)
    
    shinyjs::enable("downloadBtn")
  })
  
  shap_data <- eventReactive(input$runShap, {
    shinyjs::disable("runShap")
    run_shap_engine(
      symbols_file = "/Users/kasa/RStudio/symbols.csv",
      data_file    = "/Users/kasa/RStudio/market_features.csv"
    )
    read_csv("/Users/kasa/RStudio/SHAP_IMPORTANCE_BY_SYMBOL.csv", show_col_types = FALSE)
  })
  
  output$shapTable <- renderTable({
    shap_data()
    shinyjs::enable("runShap")
  })
  
  observeEvent(input$runBtn, {
    status("Running Python script...")
    shinyjs::disable("runBtn")

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
  
#===================================================  
# Chart Starts here
#===================================================    
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
    fin <- selected_fundamental()
    title_text <- paste(fin$Symbol, "-", fin$Company, "(", fin$Sector, ")")
    
    if (is_high_movement()) {
      title_text <- paste0(
        "HIGH MOVEMENT: ",
        title_text,
        " | Score: ", round(fin$anomaly_score, 2),
        " | 45D Return: ", round(fin$return_45d, 2)
      )
    }
    
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
        #title = paste(fin$Symbol, "-", fin$Company, "(", fin$Sector, ")"),
        title =title_text,
        y = "Price", x = ""
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(
          fill = if (is_high_movement()) "mistyrose" else "white",
          color = NA
        )
      )
    
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
          "\nEarning/Share:", round(fin$EPS,2),
          "\nReturn/Equity:", round(fin$ROE, 2),
          "\nDividend Yield:", round(fin$dividend_yield, 2),
          "\nTarget Price:", round(fin$TargetMeanPrice)
        ),
        hjust = 0, size = 4
      ) +
      annotate(
        "text", 0.38, 0.5,
        label = paste(
          "Operating Margin:", round(fin$OperatingMargin, 2),
          "\nBook Value:", fin$BookValue,
          "\n5 Yrs Avg Dividend:",fin$FiveYearAvgDividendYield,
          "\nEnterpriseToEBITDA: ", round(fin$EnterpriseToEBITDA,2),
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
          "\nNet Income:", format_big(fin$net_income),
          "\nRecommend:" , fin$Recommendation
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
      heights = c(0.7, 2.0, 1, 1)
    )
  })
}

shinyApp(ui, server)
# shinyApp(ui, server, options = list(launch.browser = FALSE,  quiet = TRUE,  port = 3838, warn=-1,message=FALSE))
