#download_fundamentals.py

import yfinance as yf
import pandas as pd
import time
import sys
from datetime import datetime
from pathlib import Path
import random


INPUT = sys.argv[1]
OUTPUT = sys.argv[2]

symbols = pd.read_csv(INPUT)["Symbol"].dropna().unique()

rows = []

for i, sym in enumerate(symbols, 1):
    print(f"[{i}/{len(symbols)}] Fetching {sym}", flush=True)
    try:
        t = yf.Ticker(sym)
        info = t.info
        if not info:
            continue
        rows.append({
            "Symbol": sym,
           "Company": info.get("longName"),
            "Sector": info.get("sector"),

            # Valuation
            "MarketCap": info.get("marketCap"),
            "EnterpriseValue": info.get("enterpriseValue"),
            "TrailingPE": info.get("trailingPE"),
            "ForwardPE": info.get("forwardPE"),
            # "PEGRatio": info.get("pegRatio"),
            "PriceToBook": info.get("priceToBook"),
            "PriceToSales": info.get("priceToSalesTrailing12Months"),
            "EnterpriseToRevenue": info.get("enterpriseToRevenue"),
            "EnterpriseToEBITDA": info.get("enterpriseToEbitda"),
            
            # Profitability
            "ROE": info.get("returnOnEquity"),
            "ROA": info.get("returnOnAssets"),
            "ProfitMargin": info.get("profitMargins"),
            "GrossMargin": info.get("grossMargins"),
            "OperatingMargin": info.get("operatingMargins"),
            "EBITDAMargin": info.get("ebitdaMargins"),
            
            # Growth
            "RevenueGrowth": info.get("revenueGrowth"),
            "EarningsGrowth": info.get("earningsGrowth"),
            # "QuarterlyRevenueGrowth": info.get("revenueQuarterlyGrowth"),
            # "QuarterlyEarningsGrowth": info.get("earningsQuarterlyGrowth"),
            
            # Financials
            "revenue": info.get("totalRevenue"),
            "ebitda": info.get("ebitda"),
            "net_income": info.get("netIncomeToCommon"),
            "gross_profits": info.get("grossProfits"),
            "OperatingCashFlow": info.get("operatingCashflow"),
            "FreeCashFlow": info.get("freeCashflow"),
            "TotalCash": info.get("totalCash"),
            "TotalDebt": info.get("totalDebt"),
            
            # Liquidity
            "CurrentRatio": info.get("currentRatio"),
            "QuickRatio": info.get("quickRatio"),
            "DebtToEquity": info.get("debtToEquity"),
            #"CashToDebt": info.get("cashToDebt"),
            
            # Per Share Metrics
            "TrailingEPS": info.get("trailingEps"),
            "EPS": info.get("forwardEps"),
            "BookValue": info.get("bookValue"),
            "CashPerShare": info.get("totalCashPerShare"),
            
            # Dividends
            "dividend_yield": info.get("dividendYield"),
            "DividendRate": info.get("dividendRate"),
            "PayoutRatio": info.get("payoutRatio"),
            "ExDividendDate": info.get("exDividendDate"),
            # "DividendDate": info.get("dividendDate"),
            "FiveYearAvgDividendYield": info.get("fiveYearAvgDividendYield"),
            
            # Share Stats
            "SharesOutstanding": info.get("sharesOutstanding"),
            "FloatShares": info.get("floatShares"),
            "SharesShort": info.get("sharesShort"),
            "ShortRatio": info.get("shortRatio"),
            "ShortPercentOfFloat": info.get("shortPercentOfFloat"),
            # "InsiderOwnership": info.get("heldPercentInsiders"),
            # "InstitutionalOwnership": info.get("heldPercentInstitutions"),
            
            # Price & Volume
            # "CurrentPrice": info.get("regularMarketPrice"),
            # "PreviousClose": info.get("previousClose"),
            # "Open": info.get("open"),
            # "DayHigh": info.get("dayHigh"),
            # "DayLow": info.get("dayLow"),
            # "FiftyTwoWeekHigh": info.get("fiftyTwoWeekHigh"),
            # "FiftyTwoWeekLow": info.get("fiftyTwoWeekLow"),
            # "FiftyDayAverage": info.get("fiftyDayAverage"),
            # "TwoHundredDayAverage": info.get("twoHundredDayAverage"),
            # "Volume": info.get("volume"),
            # "AverageVolume": info.get("averageVolume"),
            # "AverageVolume10Day": info.get("averageVolume10days"),
            # "Beta": info.get("beta"),
            
            # Analyst Estimates
            "TargetMeanPrice": info.get("targetMeanPrice"),
            "TargetHighPrice": info.get("targetHighPrice"),
            "TargetLowPrice": info.get("targetLowPrice"),
            "NumberOfAnalysts": info.get("numberOfAnalystOpinions"),
            "Recommendation": info.get("recommendationKey"),
            "RecommendationMean": info.get("recommendationMean"),
            "LastUpdated": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        })
        time.sleep(random.uniform(1, 5))
    except Exception as e:
        print(f"Failed {sym}: {e}", flush=True)
df = pd.DataFrame(rows)
df.to_csv(OUTPUT, index=False)

print(f"Saved {len(df)} rows to {OUTPUT}", flush=True)
