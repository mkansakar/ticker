import yfinance as yf
import pandas as pd
import time
import sys
from pathlib import Path

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
            "Name": info.get("longName"),
            "Sector": info.get("sector"),
            "MarketCap": info.get("marketCap"),
            "TrailingPE": info.get("trailingPE"),
            "ForwardPE": info.get("forwardPE"),
            "ROE": info.get("returnOnEquity"),
            "OperatingMargin": info.get("operatingMargins"),
            "ProfitMargin": info.get("profitMargins"),
            "DebtToEquity": info.get("debtToEquity"),
            "RevenueGrowth": info.get("revenueGrowth"),
        })

        time.sleep(0.5)

    except Exception as e:
        print(f"Failed {sym}: {e}", flush=True)

df = pd.DataFrame(rows)
df.to_csv(OUTPUT, index=False)

print(f"Saved {len(df)} rows to {OUTPUT}", flush=True)
