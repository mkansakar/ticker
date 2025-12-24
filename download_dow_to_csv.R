library(quantmod)

tickers <- read.csv("c:/temp/500SANDP_1.csv", stringsAsFactors = FALSE)
symbols <- tickers$Symbol

data_dir <- "c:/temp/500-S_And_P/dow"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

start_default <- as.Date("2021-01-01")
end_date <- Sys.Date()

update_symbol <- function(sym) {
  file_path <- file.path(data_dir, paste0(sym, ".csv"))
  if (file.exists(file_path)) {
    old_data <- read.csv(file_path, stringsAsFactors = FALSE)
    old_data$Date <- as.Date(old_data$Date)

    last_date <- max(old_data$Date, na.rm = TRUE)
    from_date <- last_date + 1

    if (from_date >= end_date) {
      message(sym, ": already up to date")
      return(NULL)
    }
    message(sym, ": updating from ", from_date)
    
    new_xts <- tryCatch(
      getSymbols(sym, src = "yahoo",
                 from = from_date, to = end_date,
                 auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(new_xts) || NROW(new_xts) == 0) return(NULL)
    
    new_df <- data.frame(Date= index(new_xts),Open= as.numeric(Op(new_xts)),High= as.numeric(Hi(new_xts)),Low= as.numeric(Lo(new_xts)),Close= as.numeric(Cl(new_xts)),Volume = as.numeric(Vo(new_xts)),Adjusted = as.numeric(Ad(new_xts))    )
    
    combined <- rbind(old_data, new_df)
    combined <- combined[!duplicated(combined$Date), ]
    
    write.csv(combined, file_path, row.names = FALSE)
  }
  else {
    message(sym, ": downloading full history")
    
    xt <- tryCatch(
      getSymbols(sym, src = "yahoo",
                 from = start_default, to = end_date,
                 auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(xt)) return(NULL)
    
    df <- data.frame(Date= index(xt),Open= as.numeric(Op(xt)),High= as.numeric(Hi(xt)),Low= as.numeric(Lo(xt)),Close= as.numeric(Cl(xt)),Volume = as.numeric(Vo(xt)),Adjusted = as.numeric(Ad(xt))    )
    
    write.csv(df, file_path, row.names = FALSE)
  }
}
for (sym in symbols) {
  update_symbol(sym)
  Sys.sleep(0.5)  
}
cat("All symbols updated successfully\n")
