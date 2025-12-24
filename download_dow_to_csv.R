library(quantmod)
library(data.table)

tickers <- fread("/Users/kasa/RStudio/S_and_P500_detailed_fundamentals.csv")
symbols <- unique(trimws(tickers$Symbol))

data_dir <- "/Users/kasa/RStudio/dow"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

start_default <- as.Date("2010-01-01")
end_date <- Sys.Date()

update_symbol <- function(sym) {
  
  file_path <- file.path(data_dir, paste0(sym, ".csv"))

  if (file.exists(file_path)) {
    old_data <- tryCatch(fread(file_path), error = function(e) NULL)
    if (is.null(old_data) || nrow(old_data) == 0) {
      message(sym, ": corrupted file – redownloading")
      from_date <- start_default
      old_data <- NULL
    } else {
      old_data[, Date := as.Date(Date)]
      from_date <- max(old_data$Date, na.rm = TRUE) + 1
    }
    if (from_date >= end_date) {
      message(sym, ": already up to date")
      return(invisible(NULL))
    }
    message(sym, ": updating from ", from_date)
  } else {
    message(sym, ": downloading full history")
    old_data <- NULL
    from_date <- start_default
  }
  xt <- tryCatch(
    getSymbols(sym, src = "yahoo", from = from_date, to = end_date, auto.assign = FALSE, warnings = FALSE),
    error = function(e) NULL
  )
  if (is.null(xt) || NROW(xt) == 0) {
    message(sym, ": no data (possibly delisted)")
    if (sym %in% tickers$Symbol) {
      tickers <<- tickers[Symbol != sym]
    }
    return()
  }
  new_data <- data.table(
    Date     = index(xt),
    Open     = as.numeric(Op(xt)),
    High     = as.numeric(Hi(xt)),
    Low      = as.numeric(Lo(xt)),
    Close    = as.numeric(Cl(xt)),
    Volume   = as.numeric(Vo(xt)),
    Adjusted = as.numeric(Ad(xt))
  )
    if (!is.null(old_data)) {
    combined <- rbind(old_data, new_data, fill = TRUE)
    combined <- unique(combined, by = "Date")
  } else {
    combined <- new_data
  }
  setorder(combined, Date)
  fwrite(combined, file_path)
  
  Sys.sleep(runif(1, 0.8, 1.5))  
}
for (sym in symbols) {
  update_symbol(sym)
}
cat("All symbols processed successfully\n")

fwrite(tickers, "/Users/kasa/RStudio/S_and_P500_detailed_fundamentals.csv")
cat("Saved", length(successful_symbols), "symbols, removed delisted symbols\n")

