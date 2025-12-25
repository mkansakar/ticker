import yfinance as yf
import pandas as pd
import sys
import time

symbol = sys.argv[1]

def get_fundamentals(ticker):
    t = yf.Ticker(ticker)
    info = t.info

    return {
        "Ticker": ticker,
        "Name": info.get("shortName"),
        "Sector": info.get("sector"),
        "MarketCap": info.get("marketCap"),
        "EnterpriseValue": info.get("enterpriseValue"),
        "TrailingPE": info.get("trailingPE"),
        "ForwardPE": info.get("forwardPE"),
        "PEGRatio": info.get("pegRatio"),
        "PriceToBook": info.get("priceToBook"),
        "PriceToSales": info.get("priceToSalesTrailing12Months"),
        "EnterpriseToRevenue": info.get("enterpriseToRevenue"),
        "EnterpriseToEBITDA": info.get("enterpriseToEbitda"),
        "ROE": info.get("returnOnEquity"),
        "ROA": info.get("returnOnAssets"),
        "ProfitMargin": info.get("profitMargins"),
        "GrossMargin": info.get("grossMargins"),
        "OperatingMargin": info.get("operatingMargins"),
        "EBITDAMargin": info.get("ebitdaMargins")
    }

if __name__ == "__main__":
    try:
        data = get_fundamentals(symbol)
        print(pd.DataFrame([data]).to_csv(index=False))
    except Exception:
        print("")
