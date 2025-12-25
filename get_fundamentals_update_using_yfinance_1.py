import yfinance as yf
import pandas as pd
import os
import time
from datetime import datetime

OUTPUT_FILE = "/Users/kasa/RStudio/sp500_stock_fundamentals.csv"
SLEEP_SEC = 1.0

def load_existing():
    if os.path.exists(OUTPUT_FILE):
        df = pd.read_csv(OUTPUT_FILE)
        return df
    return pd.DataFrame()

def get_fundamentals(symbol):
    t = yf.Ticker(symbol)

    try:
        fi = t.fast_info
        info = t.info

        return {
            "Ticker": symbol,
            "Name": info.get("longName"),
            "Sector": info.get("sector"),

            # Valuation
            "MarketCap": info.get("marketCap"),
            "EnterpriseValue": info.get("enterpriseValue"),
            "TrailingPE": info.get("trailingPE"),
            "ForwardPE": info.get("forwardPE"),
            "PEGRatio": info.get("pegRatio"),
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
            #"QuarterlyRevenueGrowth": info.get("revenueQuarterlyGrowth"),
            "QuarterlyEarningsGrowth": info.get("earningsQuarterlyGrowth"),
            
            # Financials
            "Revenue": info.get("totalRevenue"),
            "EBITDA": info.get("ebitda"),
            "NetIncome": info.get("netIncomeToCommon"),
            "GrossProfit": info.get("grossProfits"),
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
            "ForwardEPS": info.get("forwardEps"),
            "BookValuePerShare": info.get("bookValue"),
            "CashPerShare": info.get("totalCashPerShare"),
            
            # Dividends
            "DividendYield": info.get("dividendYield"),
            "DividendRate": info.get("dividendRate"),
            "PayoutRatio": info.get("payoutRatio"),
            "ExDividendDate": info.get("exDividendDate"),
            "DividendDate": info.get("dividendDate"),
            "FiveYearAvgDividendYield": info.get("fiveYearAvgDividendYield"),
            
            # Share Stats
            "SharesOutstanding": info.get("sharesOutstanding"),
            "FloatShares": info.get("floatShares"),
            "SharesShort": info.get("sharesShort"),
            "ShortRatio": info.get("shortRatio"),
            "ShortPercentOfFloat": info.get("shortPercentOfFloat"),
            "InsiderOwnership": info.get("heldPercentInsiders"),
            "InstitutionalOwnership": info.get("heldPercentInstitutions"),
            
            # Price & Volume
            "CurrentPrice": info.get("regularMarketPrice"),
            "PreviousClose": info.get("previousClose"),
            "Open": info.get("open"),
            "DayHigh": info.get("dayHigh"),
            "DayLow": info.get("dayLow"),
            "FiftyTwoWeekHigh": info.get("fiftyTwoWeekHigh"),
            "FiftyTwoWeekLow": info.get("fiftyTwoWeekLow"),
            "FiftyDayAverage": info.get("fiftyDayAverage"),
            "TwoHundredDayAverage": info.get("twoHundredDayAverage"),
            "Volume": info.get("volume"),
            "AverageVolume": info.get("averageVolume"),
            "AverageVolume10Day": info.get("averageVolume10days"),
            "Beta": info.get("beta"),
            
            # Analyst Estimates
            "TargetMeanPrice": info.get("targetMeanPrice"),
            "TargetHighPrice": info.get("targetHighPrice"),
            "TargetLowPrice": info.get("targetLowPrice"),
            "NumberOfAnalysts": info.get("numberOfAnalystOpinions"),
            "Recommendation": info.get("recommendationKey"),
            "RecommendationMean": info.get("recommendationMean"),
            "LastUpdated": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        }

    except Exception as e:
        print(f" {symbol} failed: {e}")
        return None

def main():
    # Load symbols
    symbols = pd.read_csv("/Users/kasa/RStudio/S_and_P500_detailed_fundamentals.csv")["Symbol"].dropna().unique().tolist()
    
    # Load existing data
    existing = load_existing()
    
    # Determine which symbols to process
    if not existing.empty:
        processed_symbols = set(existing["Ticker"].tolist())
        symbols = [s for s in symbols if s not in processed_symbols]
        print(f"Found {len(existing)} existing records, {len(symbols)} new symbols to fetch")
    else:
        processed_symbols = set()
        print(f"No existing file found, fetching all {len(symbols)} symbols")
    
    # Collect all new data
    results = []
    
    for i, symbol in enumerate(symbols, 1):
        print(f"[{i}/{len(symbols)}] Fetching {symbol}")
        
        data = get_fundamentals(symbol)
        if data:
            results.append(data)
        else:
            # Keep existing data for this symbol if it exists
            if symbol in processed_symbols:
                print(f"  Keeping existing data for {symbol}")
        
        # Simple progress indicator (optional)
        if i % 10 == 0:
            print(f"  Progress: {i}/{len(symbols)} ({i/len(symbols)*100:.1f}%)")
        
        time.sleep(SLEEP_SEC)
    
    # Combine and save everything at the end
    if results:
        # Create dataframe from new results
        new_df = pd.DataFrame(results)
        
        # Combine with existing data
        if existing.empty:
            final_df = new_df
        else:
            final_df = pd.concat([existing, new_df], ignore_index=True)
        
        # Remove any duplicates (keep the newest based on LastUpdated)
        final_df = final_df.sort_values("LastUpdated").drop_duplicates("Ticker", keep="last")
        
        # Save to CSV
        final_df.to_csv(OUTPUT_FILE, index=False)
        
        print(f"Saved {len(final_df)} total records to {OUTPUT_FILE}")
        print(f"  - Existing: {len(existing)}")
        print(f"  - New: {len(results)}")
        print(f"  - Failed: {len(symbols) - len(results)}")
    else:
        print("\n✗ No new data collected")
        if not existing.empty:
            print(f"  Keeping existing {len(existing)} records")

if __name__ == "__main__":
    main()