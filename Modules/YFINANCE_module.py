# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# YFINANCE MODULE
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Requirements setup 
# We import from config_API the API key for the call to the provider's server
import pandas as pd
import yfinance as yf

# Fetching function definition (fetch_YFINANCE)
# symbol: Ticker symbol (type=str, ex. "AAPL" or even "EURUSD" for exchange rate)
# start: start date (format YYYY-MM-DD, type=str, ex. "1947-01-01")
# end: end date (format YYYY-MM-DD, type=str, ex. "2025-04-01")
# (!!!) only the closing price fo the first trading day of the month to compute the monthly return 
def fetch_YFINANCE(symbol: str, start: str, end= str):
# Frequency settings (1mo) and progress=False to not dipslay the progress bar
    data = yf.download(symbol, start=start, end=end, interval="1mo", progress=False)
    data = data.reset_index()
    data["YearMonth"] = data["Date"].dt.to_period("M")
# We keep both close price (at YYYY-MM-01) and volume 
    data = data[["YearMonth", "Close", "Volume"]]
    return data