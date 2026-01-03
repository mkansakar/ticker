library(dplyr)
library(readr)
library(tidyr)
library(corrplot)

# Configuration
FUNDAMENTALS_FILE <- "/Users/kasa/RStudio/stock_fundamentals_out_1.csv"
OUTPUT_DIR <- "/Users/kasa/RStudio/correlation"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Read fundamentals data
fundamentals <- read_csv(FUNDAMENTALS_FILE, show_col_types = FALSE)

# Standardize column names
colnames(fundamentals) <- tolower(colnames(fundamentals))

# Check what columns we have
cat("Columns in fundamentals data:\n")
print(names(fundamentals))

# Find numeric columns - SAFER APPROACH
numeric_cols <- names(fundamentals)[sapply(fundamentals, is.numeric)]
cat("\nNumeric columns found:\n")
print(numeric_cols)

# If no numeric columns found, try to convert
if (length(numeric_cols) == 0) {
  cat("\nNo numeric columns found. Attempting to convert...\n")
  fundamentals <- fundamentals %>%
    mutate(across(where(is.character), ~ as.numeric(.x)))
  numeric_cols <- names(fundamentals)[sapply(fundamentals, is.numeric)]
}

# Define metrics we want to check
common_metrics <- c(
  "trailingpe", "forwardpe", "pegratio", "pricetobook", "pricetosales",
  "enterprisetorevenue", "enterprisetoebitda", "roe", "roa", "profitmargin",
  "grossmargin", "operatingmargin", "ebitdamargin", "revenuegrowth", 
  "earningsgrowth", "quarterlyearningsgrowth", "revenue", "ebitda", 
  "net_income", "gross_profits", "operatingcashflow", "freecashflow",
  "totalcash", "totaldebt", "currentratio", "quickratio", "debtequity",
  "trailingeps", "eps", "bookvalue", "cashpershare", "dividend_yield",
  "dividendrate", "payoutratio", "fiveyearavgdividendyield", 
  "sharesoutstanding", "floatshares", "sharesshort", "shortratio",
  "shortpercentoffloat", "enterprisevalue", "marketcap"
)

# Keep only metrics that exist in our data
available_metrics <- intersect(common_metrics, numeric_cols)

cat("\nAvailable metrics for correlation analysis:\n")
print(available_metrics)

# If we have enough metrics, proceed
if (length(available_metrics) < 5) {
  cat("\nWarning: Not enough metrics found. Using all numeric columns instead.\n")
  available_metrics <- numeric_cols[!numeric_cols %in% c("symbol", "x1", "rowid")]
}

# Select data and clean it
fundamental_data <- fundamentals %>%
  # Keep only symbol and selected metrics
  select(any_of(c("symbol", available_metrics))) %>%
  # Remove rows with all NAs in metric columns
  filter(rowSums(!is.na(select(., -symbol))) > 0)

# Remove problematic columns manually
clean_data <- fundamental_data
for (col in available_metrics) {
  if (col %in% names(clean_data)) {
    # Check if column has valid numeric data
    col_data <- clean_data[[col]]
    non_na <- col_data[!is.na(col_data)]
    
    if (length(non_na) < 5) {
      # Too few non-NA values
      cat("Removing", col, "- too few values\n")
      clean_data <- clean_data %>% select(-all_of(col))
    } else if (sd(non_na, na.rm = TRUE) == 0) {
      # Zero variance
      cat("Removing", col, "- zero variance\n")
      clean_data <- clean_data %>% select(-all_of(col))
    }
  }
}

# Check if we have enough data
cat("\nFinal columns for correlation analysis:\n")
print(setdiff(names(clean_data), "symbol"))

if (nrow(clean_data) < 10 || ncol(clean_data) < 3) {
  cat("\nError: Insufficient data for correlation analysis.\n")
  cat("Rows:", nrow(clean_data), "\n")
  cat("Columns:", ncol(clean_data), "\n")
} else {
  # Calculate correlation matrix
  cor_matrix <- cor(clean_data %>% select(-symbol), 
                    use = "pairwise.complete.obs",
                    method = "pearson")
  
  # Create correlation dataframe
  n_metrics <- ncol(cor_matrix)
  cor_values <- cor_matrix[lower.tri(cor_matrix)]
  pairs <- which(lower.tri(cor_matrix), arr.ind = TRUE)
  
  cor_df <- data.frame(
    metric1 = colnames(cor_matrix)[pairs[, 1]],
    metric2 = colnames(cor_matrix)[pairs[, 2]],
    correlation = cor_values,
    abs_correlation = abs(cor_values)
  ) %>%
    arrange(desc(abs_correlation))
  
  # Save correlation results
  write_csv(cor_df, file.path(OUTPUT_DIR, "fundamental_metric_correlations.csv"))
  
  # Print summary
  cat("\n=== FUNDAMENTAL METRIC CORRELATION ANALYSIS ===\n")
  cat("Total metrics analyzed:", n_metrics, "\n")
  cat("Total stocks analyzed:", nrow(clean_data), "\n")
  
  cat("\nTop 10 Strongest Correlations:\n")
  print(head(cor_df, 10))
  
  # Create visualization if we have data
  if (n_metrics > 2) {
    png(file.path(OUTPUT_DIR, "fundamental_correlations_heatmap.png"), 
        width = 1000, height = 800, res = 150)
    
    corrplot(cor_matrix, 
             method = "color",
             type = "upper",
             tl.col = "black",
             tl.cex = 0.7,
             title = "Fundamental Metric Correlations",
             mar = c(0, 0, 2, 0))
    
    dev.off()
    cat("\nCorrelation heatmap saved.\n")
  }
}

cat("\nAnalysis complete. Results saved to:", OUTPUT_DIR, "\n")