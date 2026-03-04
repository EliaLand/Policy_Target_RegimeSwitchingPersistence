# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# WORLD BANK MODULE
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Requirements setup 
# We import from config_API the API key for the call to the provider's server
import pandas as pd
import wbgapi as wb

# Fetching function definition (fetch_WB)
# (!!!) Annual data ONLY
# indicator: dabase code (ex. "NY.GDP.MKTP.CD")
# countries: list of countries (type=list, ex. ["USA", "FRA"])
# start: start year (format YYYY, type=str, ex. "1947")
# end: end year (format YYYY, type=str, ex. "2025")
def fetch_WB(indicator: str, countries: list, start, end) -> pd.DataFrame:
    data = wb.data.DataFrame(indicator, countries, time=range(start, end + 1))
    data = data.reset_index().rename(columns={"economy": "Country", "time": "Year"})
    return data