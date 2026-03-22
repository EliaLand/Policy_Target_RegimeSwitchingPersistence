# ============================================================
# Module 1 - Data Extraction & Manipulation  (R Translation)
# Author      : Elia Landini
# Student ID  : 12310239
# Course      : EESM2 – Financial Economics
# Supervisor  : Jean-Bernard Chatelain
# Repository  : https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
# ============================================================


# ============================================================
# 2) REQUIREMENTS SET-UP
# ============================================================

# Uncomment the block below to install all required packages
# install.packages(c(
#   "tidyverse",   # pandas + numpy equivalent
#   "fredr",       # FRED API  (replaces fredapi / custom fetch_FRED)
#   "rdbnomics",   # DBnomics  (replaces custom fetch_DBNOMICS)
#   "eurostat",    # Eurostat  (replaces custom fetch_EUROSTAT)
#   "WDI",         # World Bank(replaces custom fetch_WB)
#   "quantmod",    # Yahoo Finance (replaces custom fetch_YFINANCE)
#   "tidyquant",   # Financial time-series tools
#   "lubridate",   # Date manipulation
#   "zoo",         # na.locf (forward-fill), na.approx
#   "tseries",     # ADF unit-root test
#   "urca",        # Phillips-Perron test
#   "lmtest",      # Statistical tests (Breusch-Pagan, Durbin-Watson …)
#   "car",         # VIF (variance_inflation_factor)
#   "stlplus",     # STL decomposition
#   "caret",       # ML pipeline (sklearn equivalent)
#   "pROC",        # ROC / AUC curves
#   "e1071",       # SVM, naive Bayes
#   "stargazer",   # Regression tables
#   "ggplot2",     # matplotlib / seaborn equivalent
#   "plotly",      # Interactive plots
#   "scales",      # Scale formatting helpers
#   "nortest"      # Normality tests (KS, Lilliefors …)
# ))

# Load libraries
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, ggplot2, readr, purrr …
  library(fredr)       # FRED API
  library(rdbnomics)   # DBnomics API
  library(eurostat)    # Eurostat
  library(WDI)         # World Bank
  library(quantmod)    # getSymbols (Yahoo Finance)
  library(tidyquant)   # tq_get, to.monthly helpers
  library(lubridate)   # floor_date, as.Date …
  library(zoo)         # na.locf, as.yearmon
  library(tseries)     # adf.test
  library(urca)        # ur.pp (Phillips-Perron)
  library(lmtest)      # coeftest, bgtest …
  library(car)         # vif()
  library(stlplus)     # stlplus() decomposition
  library(caret)       # train/test split, cross-validation
  library(pROC)        # roc(), auc()
  library(e1071)       # Support vector machines
  library(stargazer)   # Publication-quality regression tables
  library(ggplot2)     # Plotting
  library(plotly)      # Interactive charts
  library(scales)      # Number / date formatting
  library(nortest)     # ks.test, lillie.test
})

# Suppress warnings globally (mirrors warnings.filterwarnings("ignore"))
options(warn = -1)


# ============================================================
# 3) HELPER FUNCTIONS & GENERAL VARIABLES
# ============================================================

# Set your FRED API key (obtain at https://fred.stlouisfed.org/docs/api/api_key.html)
fredr_set_key("YOUR_FRED_API_KEY")

# ── Statistical significance labelling ────────────────────
# Python: def significance_stars(p)
significance_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

# ── Helper: expand quarterly data to monthly (flat within quarter) ──
# Replicates the Python pattern:
#   for i in range(3): month = start_month + DateOffset(months=i)
expand_quarterly_to_monthly <- function(df, value_cols) {
  # df must have columns: Country, Time (as Date or "YYYY-MM-DD"), <value_cols>
  df %>%
    mutate(quarter_start = lubridate::floor_date(as.Date(Time), "quarter")) %>%
    rowwise() %>%
    reframe(
      Country       = Country,
      Time          = format(seq(quarter_start, by = "month", length.out = 3), "%Y-%m"),
      across(all_of(value_cols), ~ .x)
    ) %>%
    select(Country, Time, all_of(value_cols))
}

# ── Helper: fetch from FRED and format Time as "YYYY-MM" ──
fetch_fred_monthly <- function(series_id, col_name) {
  fredr(series_id = series_id) %>%
    rename(Time = date, !!col_name := value) %>%
    mutate(
      Time    = format(as.Date(Time), "%Y-%m"),
      Country = "JP"
    ) %>%
    select(Country, Time, all_of(col_name))
}

# ── Helper: fetch from FRED (daily) and aggregate to monthly mean ──
fetch_fred_daily_to_monthly <- function(series_id, col_name) {
  fredr(series_id = series_id) %>%
    rename(Time = date, !!col_name := value) %>%
    mutate(Time = lubridate::floor_date(as.Date(Time), "month")) %>%
    group_by(Time) %>%
    summarise(!!col_name := mean(.data[[col_name]], na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Time    = format(Time, "%Y-%m"),
      Country = "JP"
    ) %>%
    select(Country, Time, all_of(col_name))
}

# ── Helper: fetch from FRED (annual/quarterly) and forward-fill to monthly ──
fetch_fred_ffill_to_monthly <- function(series_id, col_name) {
  fredr(series_id = series_id) %>%
    rename(Time = date, !!col_name := value) %>%
    mutate(Time = as.Date(Time)) %>%
    complete(Time = seq.Date(min(Time, na.rm = TRUE),
                             max(Time, na.rm = TRUE), by = "month")) %>%
    tidyr::fill(all_of(col_name), .direction = "down") %>%
    mutate(
      Time    = format(Time, "%Y-%m"),
      Country = "JP"
    ) %>%
    select(Country, Time, all_of(col_name))
}


# ============================================================
# 4) FC-FAVAR DATA RETRIEVAL
# ============================================================

# ── 4.1  REAL EFFECTIVE EXCHANGE RATES (REER) ─────────────
# Source: BIS via rdbnomics | CPI-based, Index 2015=100
# Python: fetch_DBNOMICS  /  fetch_EUROSTAT

reer_raw <- rdb("BIS", "eer", mask = "M.R.JP") %>%
  select(period, value) %>%
  rename(
    Time = period,
    `USD-JPY reer CPI-based (Index 2015=100)` = value
  ) %>%
  mutate(
    Time    = format(as.Date(Time), "%Y-%m"),
    Country = "JP"
  ) %>%
  select(Country, Time, `USD-JPY reer CPI-based (Index 2015=100)`)

tail(reer_raw)

# ── 4.2  HICP – HARMONISED INDEX OF CONSUMER PRICES (SA) ──
# Source: ECB via rdbnomics
# Python: fetch_DBNOMICS

hicp_raw <- rdb("ECB", "ICP", mask = "M.JP.N.000000.4.INX") %>%
  select(period, value) %>%
  rename(Time = period, `HICP (SA)` = value) %>%
  mutate(
    Time    = format(as.Date(Time), "%Y-%m"),
    Country = "JP"
  ) %>%
  select(Country, Time, `HICP (SA)`)

tail(hicp_raw)

# ── 4.3  JPY-USD SPOT EXCHANGE RATE ───────────────────────
# Source: FRED (DEXJPUS) – daily → monthly mean
# Python: fetch_FRED("DEXJPUS")  +  resample("M").mean()

jpyusd_raw <- fetch_fred_daily_to_monthly("DEXJPUS", "JPY-USD Spot Exchange Rate")

tail(jpyusd_raw)

# ── 4.4  STOCK INDICES / BOND ETFs  ───────────────────────
# Source: Yahoo Finance via quantmod
# Python: fetch_YFINANCE  with log monthly return + volume
# Tickers: 2561.T  (iShares Core Japan Gov Bond ETF)
#          Add further tickers with bind_rows() below

fetch_yfinance_monthly <- function(ticker, from_date, to_date, label) {
  raw <- tryCatch(
    getSymbols(ticker, src = "yahoo",
               from = from_date, to = to_date, auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(NULL)

  # to.monthly uses zoo::as.yearmon internally
  monthly <- to.monthly(raw, indexAt = "lastof", OHLC = FALSE)
  df <- as.data.frame(monthly) %>%
    rownames_to_column("Time") %>%
    # Standardise column names before selecting
    setNames(c("Time", "Open", "High", "Low", "Close", "Volume", "Adj")) %>%
    mutate(
      Time               = format(as.Date(as.yearmon(Time)), "%Y-%m"),
      `Log Monthly Return` = c(NA_real_, diff(log(Close))),
      `Stock Index / Bond-related Instrument` = label,
      `Stock Index`      = ticker
    ) %>%
    select(`Stock Index / Bond-related Instrument`,
           `Stock Index`, Time,
           `Log Monthly Return`, Volume) %>%
    filter(!is.na(`Log Monthly Return`))
  df
}

jp_stocks_raw <- bind_rows(
  fetch_yfinance_monthly("2561.T", "2020-01-01", Sys.Date(),
                          "iShares Core Japan Government Bond ETF")
  # Add more tickers here, e.g.:
  # fetch_yfinance_monthly("^N225", "1990-01-01", Sys.Date(), "Nikkei 225")
)

tail(jp_stocks_raw)

# ── 4.5  TOTAL TREASURY RESERVES (excl. Gold) ─────────────
# Source: FRED (TRESEGSJPM052N) – monthly
# Python: fetch_FRED("TRESEGSJPM052N")

jp_reserves_raw <- fetch_fred_monthly("TRESEGSJPM052N",
                                       "Total Treasury Reserves (- Gold)")

tail(jp_reserves_raw)

# ── 4.6  MONETARY AGGREGATES – M3 ─────────────────────────
# Source: FRED (MABMM301JPM189S)

jp_m3_raw <- fetch_fred_monthly("MABMM301JPM189S",
                                  "Monetary Aggregates - M3 (JPY)")

tail(jp_m3_raw)

# ── 4.7  MONETARY AGGREGATES – M2 ─────────────────────────
# Source: FRED (MYAGM2JPM189N)

jp_m2_raw <- fetch_fred_monthly("MYAGM2JPM189N",
                                  "Monetary Aggregates - M2 (JPY)")

tail(jp_m2_raw)

# ── 4.8  MONETARY AGGREGATES – M1 ─────────────────────────
# Source: FRED (MANMM101JPM189S)
# Python: fetch_FRED("MANMM101JPM189S")

jp_m1_raw <- fetch_fred_monthly("MANMM101JPM189S",
                                  "Monetary Aggregates - M1 (JPY)")

tail(jp_m1_raw)

# ── 4.9  TOTAL CREDIT – PRIVATE NON-FINANCIAL (%GDP) ──────
# Source: FRED (QJPPAM770A) – quarterly → monthly (flat expansion)
# Python: quarterly loop with DateOffset(months=i)

jp_credit_pnf_raw <- fredr(series_id = "QJPPAM770A") %>%
  rename(Time = date,
         `Total Credit - Private Non-Financial (%GDP)` = value) %>%
  mutate(Country = "JP") %>%
  expand_quarterly_to_monthly("Total Credit - Private Non-Financial (%GDP)")

tail(jp_credit_pnf_raw)

# ── 4.10  TOTAL CREDIT – GENERAL GOVERNMENT (%GDP) ────────
# Source: FRED (QJPGAM770A) – quarterly → monthly

jp_credit_gg_raw <- fredr(series_id = "QJPGAM770A") %>%
  rename(Time = date,
         `Total Credit - General Government (%GDP)` = value) %>%
  mutate(Country = "JP") %>%
  expand_quarterly_to_monthly("Total Credit - General Government (%GDP)")

tail(jp_credit_gg_raw)

# ── 4.11  TOTAL CREDIT – HOUSEHOLDS & NPISHs (%GDP) ───────
# Source: FRED (QJPHAM770A) – quarterly → monthly

jp_credit_hh_raw <- fredr(series_id = "QJPHAM770A") %>%
  rename(Time = date,
         `Total Credit - Households & NPISHs (%GDP)` = value) %>%
  mutate(Country = "JP") %>%
  expand_quarterly_to_monthly("Total Credit - Households & NPISHs (%GDP)")

tail(jp_credit_hh_raw)

# ── 4.12  REAL GDP (billions chained 2015 JPY) ────────────
# Source: FRED (JPNRGDPEXP) – quarterly → monthly
# Python: fetch_FRED("JPNRGDPEXP") + quarterly loop

jp_rgdp_raw <- fredr(series_id = "JPNRGDPEXP") %>%
  rename(Time = date,
         `Real GDP (billions chained 2015 JPY)` = value) %>%
  mutate(Country = "JP") %>%
  expand_quarterly_to_monthly("Real GDP (billions chained 2015 JPY)")

tail(jp_rgdp_raw)

# ── 4.13  10-YEAR GOVERNMENT BOND YIELDS ──────────────────
# Source: FRED (IRLTLT01JPM156N)

jp_10ygb_raw <- fetch_fred_monthly("IRLTLT01JPM156N",
                                     "10-Year Gov Bond Yields (%)")

tail(jp_10ygb_raw)

# ── 4.14  CALL MONEY / INTERBANK IMMEDIATE RATE ───────────
# Source: FRED (IRSTCI01JPM156N)

jp_cmibr_raw <- fetch_fred_monthly("IRSTCI01JPM156N",
                                     "Call Money/Interbank Immediate (%)")

tail(jp_cmibr_raw)

# ── 4.15  NATURAL RATE OF INTEREST (1-year & 10-year) ─────
# Source: Nakajima et al. (2023) – local CSV, quarterly → monthly
# Python: pd.read_csv("Data/nakajima et al. 2023.csv")
#         + quarterly expansion loop

jp_nir_csv <- read.csv("Data/nakajima et al. 2023.csv",
                        header = FALSE, stringsAsFactors = FALSE)

# Drop header row; keep YYYYQ (col 1), 1YMean (col 2), 10YMean (col 5)
jp_nir_csv <- jp_nir_csv[-1, c(1, 2, 5)]
colnames(jp_nir_csv) <- c("YYYYQ", "YMean1", "YMean10")

quarter_to_month <- c("1" = 1L, "2" = 4L, "3" = 7L, "4" = 10L)

jp_nir_raw <- jp_nir_csv %>%
  mutate(
    YYYYQ    = as.character(YYYYQ),
    Year     = as.integer(substr(YYYYQ, 1, 4)),
    Quarter  = substr(YYYYQ, nchar(YYYYQ), nchar(YYYYQ)),
    StartMon = quarter_to_month[Quarter],
    QStart   = as.Date(paste(Year, StartMon, "01", sep = "-")),
    YMean1   = as.numeric(YMean1),
    YMean10  = as.numeric(YMean10)
  ) %>%
  rowwise() %>%
  reframe(
    Country = "JP",
    Time    = format(seq(QStart, by = "month", length.out = 3), "%Y-%m"),
    `Est. 1-year Neutral Interest Rate (%)`  = YMean1,
    `Est. 10-year Neutral Interest Rate (%)` = YMean10
  ) %>%
  select(Country, Time,
         `Est. 1-year Neutral Interest Rate (%)`,
         `Est. 10-year Neutral Interest Rate (%)`)

tail(jp_nir_raw)

# ── 4.16  BOJ TOTAL ASSETS ────────────────────────────────
# Source: FRED (JPNASSETS)

jp_bojta_raw <- fetch_fred_monthly("JPNASSETS",
                                     "BoJ's Total Assets (100 Million Yen)")

tail(jp_bojta_raw)

# ── 4.17  10-YEAR US TREASURY YIELD (daily → monthly mean) ─
# Source: FRED (DGS10)
# Python: resample("M").mean()

jp_ust_raw <- fetch_fred_daily_to_monthly("DGS10",
                                           "10-Year US T-Bills Yield (%)")

tail(jp_ust_raw)

# ── 4.18  CBOE VIX (daily → monthly mean) ─────────────────
# Source: FRED (VIXCLS)

jp_vix_raw <- fetch_fred_daily_to_monthly("VIXCLS", "CBOE-VIX")

tail(jp_vix_raw)

# ── 4.19  CENTRAL GOVERNMENT DEBT (% GDP) – annual → monthly ──
# Source: FRED (DEBTTLJPA188A)
# Python: resample("MS").ffill()

jp_cgdebt_raw <- fetch_fred_ffill_to_monthly("DEBTTLJPA188A",
                                               "Central Government Debt (% GDP)")

tail(jp_cgdebt_raw)

# ── 4.20  DOMESTIC PRIVATE DEBT SECURITIES (% GDP) ────────
# Source: FRED (DDDM03JPA156NWDB) – annual → monthly ffill

jp_dpdebt_raw <- fetch_fred_ffill_to_monthly("DDDM03JPA156NWDB",
                                               "Domestic Private Debt Securities (% GDP)")

tail(jp_dpdebt_raw)

# ── 4.21  DOMESTIC PUBLIC DEBT SECURITIES (% GDP) ─────────
# Source: FRED (DDDM04JPA156NWDB) – annual → monthly ffill

jp_dudebt_raw <- fetch_fred_ffill_to_monthly("DDDM04JPA156NWDB",
                                               "Domestic Public Debt Securities (% GDP)")

tail(jp_dudebt_raw)

# ── 4.22  DEPOSIT INTEREST RATE  (BoJ local CSV files) ────
# Source: BoJ website – two series spliced together at 2022-03
# Python: pd.read_csv + merge(how="outer")

# Series 1: Certificates of Deposit 60-89 days (until 2022-03)
jp_depi1_raw <- read.csv(
  "Data/BoJdeposits/IR02STRDCDNITL03.csv",
  stringsAsFactors = FALSE
) %>%
  rename_with(~ c("Time", "Deposit Interest Rate (%)")) %>%
  mutate(Time = format(as.Date(Time), "%Y-%m")) %>%
  select(Time, `Deposit Interest Rate (%)`) %>%
  drop_na()

# Series 2: Time Deposits ≥10 M JPY / 3 months (2022-03 onwards)
jp_depi2_raw <- read.csv(
  "Data/BoJdeposits/extendedIR02DLDR43TL43MN.csv",
  stringsAsFactors = FALSE
) %>%
  rename_with(~ c("Time", "Deposit Interest Rate (%)")) %>%
  mutate(Time = format(as.Date(Time), "%Y-%m")) %>%
  select(Time, `Deposit Interest Rate (%)`) %>%
  drop_na()

# Outer-join the two series (equivalent to pd.merge(..., how="outer"))
jp_depi_raw <- full_join(
  jp_depi1_raw,
  jp_depi2_raw,
  by = c("Time", "Deposit Interest Rate (%)")
) %>%
  mutate(Country = "JP") %>%
  select(Country, Time, `Deposit Interest Rate (%)`) %>%
  arrange(Time)

tail(jp_depi_raw)

# ── 4.23  LOAN INTEREST RATE (BoJ local CSV) ──────────────
# Source: BoJ – Average Contract Interest Rates on Loans and Discounts

jp_loai_raw <- read.csv(
  "Data/BoJ/loansIR02STRDCDNITL03.csv",
  stringsAsFactors = FALSE
) %>%
  rename_with(~ c("Time", "Loan Interest Rate (%)")) %>%
  mutate(
    Time    = format(as.Date(Time), "%Y-%m"),
    Country = "JP"
  ) %>%
  select(Country, Time, `Loan Interest Rate (%)`) %>%
  drop_na()

tail(jp_loai_raw)

# ── 4.24  BANKING SECTOR DEFAULT PROXY (Yahoo Finance: 1615.T) ──
# Source: Yahoo Finance – NEXT FUNDS TOPIX Banks ETF
# Python: fetch_YFINANCE("1615.T", …)

jp_def_raw <- tryCatch({
  raw_1615 <- getSymbols(
    "1615.T", src = "yahoo",
    from = "2000-01-01", to = Sys.Date(),
    auto.assign = FALSE
  )
  to.monthly(raw_1615, indexAt = "lastof", OHLC = FALSE) %>%
    as.data.frame() %>%
    rownames_to_column("Time") %>%
    setNames(c("Time", "Open", "High", "Low", "Close", "Volume", "Adj")) %>%
    transmute(
      Country        = "JP",
      Time           = format(as.Date(as.yearmon(Time)), "%Y-%m"),
      `1615.T-Price` = Close
    ) %>%
    drop_na()
}, error = function(e) {
  message("Could not fetch 1615.T: ", e$message)
  data.frame(Country = character(), Time = character(), `1615.T-Price` = numeric())
})

tail(jp_def_raw)


# ============================================================
# 5) DATA MERGING
# ============================================================
# Each block replicates a pd.merge(..., how="outer") chain
# followed by .to_csv(...)

dir.create("Data/Aggregated", showWarnings = FALSE, recursive = TRUE)

# ── Monetary Aggregates ─────────────────────────────────────
jp_money_agg_df <- jp_m1_raw %>%
  full_join(jp_m2_raw, by = c("Country", "Time")) %>%
  full_join(jp_m3_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_money_agg_df,
          "Data/Aggregated/jpmoneyaggdf.csv", row.names = FALSE)
tail(jp_money_agg_df)

# ── Exchange Rate ───────────────────────────────────────────
jp_exchange_rate_df <- reer_raw %>%
  full_join(jpyusd_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_exchange_rate_df,
          "Data/Aggregated/jpexchangeratedf.csv", row.names = FALSE)
tail(jp_exchange_rate_df)

# ── Inflation (HICP) ────────────────────────────────────────
jp_inflation_df <- hicp_raw

write.csv(jp_inflation_df,
          "Data/Aggregated/jpinflationdf.csv", row.names = FALSE)
tail(jp_inflation_df)

# ── Real GDP ────────────────────────────────────────────────
jp_gdp_df <- jp_rgdp_raw

write.csv(jp_gdp_df,
          "Data/Aggregated/jpgdpdf.csv", row.names = FALSE)
tail(jp_gdp_df)

# ── Stock Indices / Bond ETFs ───────────────────────────────
jp_stocks_pv_df <- jp_stocks_raw

write.csv(jp_stocks_pv_df,
          "Data/Aggregated/jpstockspvdf.csv", row.names = FALSE)
tail(jp_stocks_pv_df)

# ── Debt Levels ─────────────────────────────────────────────
jp_debt_df <- jp_cgdebt_raw %>%
  full_join(jp_dpdebt_raw, by = c("Country", "Time")) %>%
  full_join(jp_dudebt_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_debt_df,
          "Data/Aggregated/jpdebtdf.csv", row.names = FALSE)
tail(jp_debt_df)

# ── Net Interest Margin (NIM) ───────────────────────────────
jp_nim_df <- jp_loai_raw %>%
  full_join(jp_depi_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_nim_df,
          "Data/Aggregated/jpnimdf.csv", row.names = FALSE)
tail(jp_nim_df)

# ── Default Rate Proxy (1615.T) ─────────────────────────────
jp_stress_df <- jp_def_raw

write.csv(jp_stress_df,
          "Data/Aggregated/jpstressdf.csv", row.names = FALSE)
tail(jp_stress_df)

# ── Bank Reserves ────────────────────────────────────────────
jp_bank_reserves_df <- jp_reserves_raw

write.csv(jp_bank_reserves_df,
          "Data/Aggregated/jpbankreservesdf.csv", row.names = FALSE)
tail(jp_bank_reserves_df)

# ── BoJ Total Assets ─────────────────────────────────────────
jp_bojta_df <- jp_bojta_raw

write.csv(jp_bojta_df,
          "Data/Aggregated/jpbojtotalassetsdf.csv", row.names = FALSE)
tail(jp_bojta_df)

# ── Policy Rate ───────────────────────────────────────────────
jp_policy_rate_df <- jp_10ygb_raw %>%
  full_join(jp_cmibr_raw,  by = c("Country", "Time")) %>%
  full_join(jp_nir_raw,    by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_policy_rate_df,
          "Data/Aggregated/jppolicyratedf.csv", row.names = FALSE)
tail(jp_policy_rate_df)

# ── External / Control Variables ─────────────────────────────
jp_control_df <- jp_ust_raw %>%
  full_join(jp_vix_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_control_df,
          "Data/Aggregated/jpcontroldf.csv", row.names = FALSE)
tail(jp_control_df)

# ── Credit Decomposition ──────────────────────────────────────
jp_credit_df <- jp_credit_pnf_raw %>%
  full_join(jp_credit_gg_raw, by = c("Country", "Time")) %>%
  full_join(jp_credit_hh_raw, by = c("Country", "Time")) %>%
  arrange(Time)

write.csv(jp_credit_df,
          "Data/Aggregated/jpcreditdf.csv", row.names = FALSE)
tail(jp_credit_df)
