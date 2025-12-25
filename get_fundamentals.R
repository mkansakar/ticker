library(dplyr)
library(readr)
library(processx)
library(lubridate)

# ==========================
# CONFIG
# ==========================
PYTHON <- "c:/temp/.venv/Scripts/python.exe"   # Windows
# PYTHON <- "venv/bin/python"         # macOS/Linux

PY_SCRIPT <- "c:/temp/download_fundamentals.py"
SYMBOL_FILE <- "c:/temp/symbols_1.csv"
OUTPUT_FILE <- "c:/temp/SP500_detailed_fundamentals.csv"
SLEEP_SEC <- 1

# ==========================
# LOAD EXISTING DATA
# ==========================
load_existing <- function() {
  if (file.exists(OUTPUT_FILE)) {
    read_csv(OUTPUT_FILE, show_col_types = FALSE)
  } else {
    tibble()
  }
}

# ==========================
# CALL PYTHON
# ==========================
fetch_symbol <- function(symbol) {
  
  result <- tryCatch({
    out <- processx::run(
      PYTHON,
      args = c(PY_SCRIPT, symbol),
      error_on_status = FALSE,
      echo = FALSE
    )
    
    if (nchar(out$stdout) < 10) return(NULL)
    
    df <- read_csv(I(out$stdout), show_col_types = FALSE)
    df$LastUpdated <- Sys.time()
    df
  }, error = function(e) NULL)
  
  result
}

# ==========================
# MAIN
# ==========================
main <- function() {
  
  symbols <- read_csv(SYMBOL_FILE, show_col_types = FALSE)$Symbol %>%
    unique() %>%
    na.omit()
  
  existing <- load_existing()
  
  done <- if (nrow(existing) > 0) existing$Ticker else character()
  symbols <- setdiff(symbols, done)
  
  message("Fetching ", length(symbols), " symbols")
  
  results <- list()
  
  for (i in seq_along(symbols)) {
    sym <- symbols[i]
    message(sprintf("[%d/%d] %s", i, length(symbols), sym))
    
    df <- fetch_symbol(sym)
    
    if (!is.null(df)) {
      results[[length(results) + 1]] <- df
    }
    
    Sys.sleep(SLEEP_SEC)
  }
  
  if (length(results) == 0 && nrow(existing) == 0) {
    stop("No data collected.")
  }
  
  new_data <- bind_rows(results)
  final <- bind_rows(existing, new_data) %>%
    arrange(desc(LastUpdated)) %>%
    distinct(Ticker, .keep_all = TRUE)
  
  write_csv(final, OUTPUT_FILE)
  
  message("Saved ", nrow(final), " rows → ", OUTPUT_FILE)
}

main()
