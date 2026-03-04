# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# DBNOMICS MODULE
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Requirements setup 
# We import from config_API the API key for the call to the provider's server
import pandas as pd
from dbnomics import fetch_series

# Fetching function definition (fetch_DBNOMICS)
# https://db.nomics.world/ -> once you serach by keywords and get to the desired database, the df code is on the right-hand side ("copy" button)
# series_code: code of the database according to DBNOMICS (not the provider (!!!), type=string, ex. "IMF/CPI/A.ZAF.PCPIT_IX")
def fetch_DBNOMICS(series_code: str) -> pd.DataFrame:
    df = fetch_series(series_code)
    df = df.reset_index()

    df = df.rename(columns={
        "period": "Period",
        "value": "Value"
    })
    return df[["Period", "Value"]]