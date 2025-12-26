library(data.table)
library(dplyr)
library(fastshap)
library(xgboost)
library(tidyr)

setwd("/Users/kasa/RStudio/trading")

train_model <- function(df) {
  xgboost(
    data = as.matrix(df |> select(-target)),
    label = df$target,
    nrounds = 50,
    objective = "binary:logistic",
    verbose = 0
  )
}

run_shap_engine <- function(
    symbols_file = "symbols.csv",
    data_file = "market_features.csv",
    out_shap = "SHAP_OUTPUT.csv",
    out_imp = "SHAP_IMPORTANCE.csv",
    min_rows = 30
) {
  
  symbols <- fread(symbols_file)$Symbol
  data <- fread(data_file)
  
  shap_all <- lapply(symbols, function(sym) {
    
    df <- data |> filter(Symbol == sym)
    if (nrow(df) < min_rows) return(NULL)
    
    model <- train_model(df |> select(-Symbol))
    X <- df |> select(-Symbol, -target)
    
    shap <- fastshap::explain(
      model, X,
      pred_wrapper = function(m, x) predict(m, as.matrix(x)),
      nsim = 100
    )
    
    as.data.frame(shap) |>
      mutate(Symbol = sym) |>
      pivot_longer(-Symbol, names_to = "Feature", values_to = "SHAP")
  })
  
  shap_tbl <- bind_rows(shap_all)
  fwrite(shap_tbl, out_shap)
  
  shap_tbl |>
    group_by(Symbol, Feature) |>
    summarise(MeanAbsSHAP = mean(abs(SHAP)), .groups = "drop") |>
    arrange(desc(MeanAbsSHAP)) |>
    fwrite(out_imp)
  
  invisible(TRUE)
}
