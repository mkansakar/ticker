library(readr)
library(dplyr)

PYTHON <- "c:/temp/.venv/Scripts/python.exe"
SYMBOL_FILE <- "c:/temp/symbols_1.csv"
OUTPUT_FILE <- "c:/temp/fundamentals_out.csv"

run_python <- function() {
  python <- normalizePath(PYTHON, winslash = "/")
  script <- normalizePath("c:/temp/download_fundamentals.py", winslash = "/")
  input  <- normalizePath("c:/temp/symbols_1.csv", winslash = "/")
  output <- normalizePath("c:/temp/fundamentals_out.csv", winslash = "/")
  
  cmd <- sprintf('"%s" "%s" "%s" "%s"', python, script, input, output)
  
  cat("Running:\n", cmd, "\n\n")
  
  status <- system(cmd, intern = FALSE)
  
  if (!file.exists(output)) {
    stop("Python ran but output CSV not created")
  }
}


main <- function() {
  run_python()
  
  df <- read_csv(OUTPUT_FILE, show_col_types = FALSE)
  
  if (nrow(df) == 0) {
    stop("No data collected")
  }
  
  df <- df %>%
    arrange(Symbol)
  
  write_csv(df, "SP500_fundamentals_FINAL.csv")
  
  cat("Fundamentals loaded:", nrow(df), "rows\n")
}

main()
