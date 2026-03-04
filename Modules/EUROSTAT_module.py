# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# EUROSTAT MODULE
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Requirements setup 
# We import from config_API the API key for the call to the provider's server
import pandas as pd
import requests
from config_API import EUROSTAT_BASE

# Fetching function definition (fetch_EUROSTAT)
# EUROSTAT_BASE = base URL from init, different structure cuz we have no API, but we directly scrape the website
# dataset_code: Eurostat dataset name (ex. "nama_10_gdp")
# filters: dictionary of all available filters, i.e., subfolders in the target dataset URL (ex. {"geo": "EA20", "na_item": "B1GQ"})
def fetch_EUROSTAT(dataset_code: str, filters: dict = None) -> pd.DataFrame:
    url = f"{EUROSTAT_BASE}/{dataset_code}"
    response = requests.get(url, params=filters)
    response.raise_for_status()
    data = response.json()
    
    obs = data.get("value", {})
    dim_values = [v["category"]["index"] for v in data["dimension"].values()]
    keys = list(obs.keys())
    
    records = []
    for idx, val in obs.items():
        idx = int(idx)
        dims = []
        temp = idx
        for dim in reversed(dim_values):
            n = len(dim)
            dims.insert(0, list(dim.keys())[temp % n])
            temp //= n
        records.append(dims + [val])
    
    columns = list(data["dimension"].keys()) + ["value"]
    df = pd.DataFrame(records, columns=columns)
    return df