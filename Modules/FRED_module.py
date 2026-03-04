# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# FRED MODULE
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Requirements setup 
# We import from config_API the API key for the call to the provider's server
import pandas as pd
from fredapi import Fred
from config_API import FRED_api_key

# Fetching function definition (fetch_FRED)
# series_id: ID of the database according to FRED URL (located at the end of the search url, ex.https://fred.stlouisfed.org/series/GDP)
# start: start date (format YYYY-MM-DD, type=str, ex. "1947-01-01")
# end: end date (format YYYY-MM-DD, type=str, ex. "2025-04-01")
def fetch_FRED(series_ID: str, start=None, end=None) -> pd.DataFrame:
    fred = Fred(api_key=FRED_api_key)
    data = fred.get_series(series_ID, observation_start=start, observation_end=end)
    df = data.reset_index()
    df.columns = ["date", series_ID]
    return df