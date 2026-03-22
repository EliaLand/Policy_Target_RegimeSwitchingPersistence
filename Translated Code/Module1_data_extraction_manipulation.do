
/*******************************************************************************
* Module 1 - Data Extraction & Manipulation
* Author    : Elia Landini
* Student ID: 12310239
* Course    : EESM2-Financial Economics
* Supervisor: Jean-Bernard Chatelain
* Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
*
* Translation of Python/Jupyter notebook to Stata do-file.
*
* REQUIREMENTS:
*   - Stata 16+ (for native `import fred`)
*   - fredkey set via:  set fredkey "YOUR_FRED_API_KEY", permanently
*   - x13as package installed for seasonal adjustment:  ssc install x13as
*   - Data files in ./Data/ folder:
*       HICP_ECB_extracted_raw.csv
*       nakajima et al. (2023).csv
*       BoJ_deposits_i_IR02STRDCDNITL03.csv
*       BoJ_deposits_i_extended_IR02DLDR43TL43MN.csv
*       BoJ_loans_i_IR04DLLR2CIDBNL1.csv
*   - Yahoo Finance data (pre-downloaded as CSVs) in ./Data/:
*       yfinance_N225.csv, yfinance_JPN.csv,
*       yfinance_236AT.csv, yfinance_2561T.csv
*       yfinance_1615T.csv
*   - Output folder:  ./Data/Aggregated/
*******************************************************************************/

clear all
set more off
capture mkdir "Data/Aggregated"

* --------------------------------------------------------------------------
* SECTION 2 – REQUIREMENTS SET-UP
* (Stata uses built-in commands; no library imports needed)
* --------------------------------------------------------------------------

* Set FRED API key (run once; stored permanently)
* set fredkey "YOUR_FRED_API_KEY", permanently


/*******************************************************************************
* SECTION 4 – FC-FAVAR DATA RETRIEVAL
*******************************************************************************/

* --------------------------------------------------------------------------
* 4.1  REAL EFFECTIVE EXCHANGE RATES
*      FRED: CCRETT01JPM661N
*      Monthly, Index 2015=100, NSA, 1970-01 to 2025-09
*      https://fred.stlouisfed.org/series/CCRETT01JPM661N
* --------------------------------------------------------------------------
import fred CCRETT01JPM661N, clear

rename datestr         Time
rename CCRETT01JPM661N reer_cpi

gen     time_m = monthly(Time, "YM")
format  time_m %tm
drop    Time
rename  time_m Time

gen Country = "JP"
order Country Time reer_cpi
label variable reer_cpi "USD-JPY reer CPI-based (Index 2015=100)"

save "Data/Aggregated/REXUSDJPY_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.2  CONSUMER PRICES (HICP) – STL SEASONAL ADJUSTMENT
*      Source: ECB, file HICP_ECB_extracted_raw.csv
*      Monthly, NSA → seasonally adjusted via X-13ARIMA-SEATS
* --------------------------------------------------------------------------
import delimited "Data/HICP_ECB_extracted_raw.csv", varnames(1) clear

* Keep only the time and value columns (drop DATE column if present)
capture drop date DATE

* Rename columns
rename v1 Time         // "TIME PERIOD"
rename v2 hicp_nsa    // long ECB variable name column

* Parse YYYY-MMM format (e.g. 2020-JAN → monthly Stata date)
gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

* Sort by time and set as time series
sort Time
tsset Time

* Seasonal adjustment using X-13ARIMA-SEATS
* (requires x13as installed: ssc install x13as)
* x13as generates the seasonally adjusted series as a new variable
x13as hicp_nsa, sa(hicp_sa) replace

gen Country = "JP"
label variable hicp_sa "HICP (SA)"
keep Country Time hicp_sa

save "Data/Aggregated/jp_HICP_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.3  JPY-USD SPOT EXCHANGE RATE
*      FRED: EXJPUS
*      Monthly, NSA, 1971-01 to 2025-09
*      https://fred.stlouisfed.org/series/EXJPUS
* --------------------------------------------------------------------------
import fred EXJPUS, clear

rename datestr Time
rename EXJPUS   spot_jpy_usd

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable spot_jpy_usd "JPY-USD Spot Exchange Rate"
order Country Time spot_jpy_usd

save "Data/Aggregated/JPYUSD_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.4  STOCK INDICES / BOND-RELATED INSTRUMENTS (Yahoo Finance)
*      Monthly, Log Return + Volume, 2015-01 to 2025-10
*      Tickers: ^N225, ^JPN, 236A.T, 2561.T
*      NOTE: Yahoo Finance data must be pre-downloaded as monthly OHLCV CSVs.
*            Expected columns: Date, Open, High, Low, Close, Volume
* --------------------------------------------------------------------------

* Helper: define a program to process each Yahoo Finance CSV
capture program drop process_yfinance
program define process_yfinance
    args filepath ticker_name instrument_label

    import delimited "`filepath'", varnames(1) clear

    * Standardise date column (assumes "Date" column in YYYY-MM format)
    gen time_m = monthly(date, "YM")
    format time_m %tm
    sort time_m
    tsset time_m

    * Log monthly return: ln(Close_t / Close_{t-1})
    gen log_return = ln(close / L.close)
    label variable log_return "Log Monthly Return"

    rename volume Volume
    label variable Volume "Volume"

    gen str20 stock_index    = "`instrument_label'"
    gen str10 stock_ticker   = "`ticker_name'"

    rename time_m Time
    gen Country = "JP"

    keep stock_index stock_ticker Time log_return Volume Country
    order stock_index stock_ticker Time log_return Volume Country
end

process_yfinance "Data/yfinance_N225.csv"  "^N225"   "Nikkei 225"
save "Data/Aggregated/tmp_N225.dta", replace

process_yfinance "Data/yfinance_JPN.csv"   "^JPN"    "NYSE Arca Japan Index"
save "Data/Aggregated/tmp_JPN.dta", replace

process_yfinance "Data/yfinance_236AT.csv" "236A.T"  "iShares 7-10 Year Japan Government Bond ETF"
save "Data/Aggregated/tmp_236AT.dta", replace

process_yfinance "Data/yfinance_2561T.csv" "2561.T"  "iShares Core Japan Government Bond ETF"
save "Data/Aggregated/tmp_2561T.dta", replace

* Stack all four into one long dataset
use "Data/Aggregated/tmp_N225.dta", clear
append using "Data/Aggregated/tmp_JPN.dta"
append using "Data/Aggregated/tmp_236AT.dta"
append using "Data/Aggregated/tmp_2561T.dta"

save "Data/Aggregated/jp_stocks_pv_df.dta", replace
export delimited "Data/Aggregated/jp_stocks_pv_df.csv", replace


* --------------------------------------------------------------------------
* 4.5  TOTAL RESERVES EXCLUDING GOLD
*      FRED: TRESEGJPM052N
*      Monthly, Millions USD, NSA, 1950-01 to 2025-07
*      https://fred.stlouisfed.org/series/TRESEGJPM052N
* --------------------------------------------------------------------------
import fred TRESEGJPM052N, clear

rename datestr        Time
rename TRESEGJPM052N  total_reserves

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable total_reserves "Total Treasury Reserves (- Gold)"
order Country Time total_reserves

save "Data/Aggregated/jp_bank_reserves_df.dta", replace
export delimited "Data/Aggregated/jp_bank_reserves_df.csv", replace


* --------------------------------------------------------------------------
* 4.6  MONETARY AGGREGATE M3
*      FRED: MABMM301JPM189S
*      Monthly, JPY, SA, 1980-01 to 2023-11
*      https://fred.stlouisfed.org/series/MABMM301JPM189S
* --------------------------------------------------------------------------
import fred MABMM301JPM189S, clear

rename datestr         Time
rename MABMM301JPM189S m3_jpy

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable m3_jpy "Monetary Aggregates - M3 (JPY)"
order Country Time m3_jpy

save "Data/Aggregated/jp_m3_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.7  MONETARY AGGREGATE M2
*      FRED: MYAGM2JPM189S
*      Monthly, JPY, SA, 1955-01 to 2017-02
*      https://fred.stlouisfed.org/series/MYAGM2JPM189S
* --------------------------------------------------------------------------
import fred MYAGM2JPM189S, clear

rename datestr        Time
rename MYAGM2JPM189S  m2_jpy

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable m2_jpy "Monetary Aggregates - M2 (JPY)"
order Country Time m2_jpy

save "Data/Aggregated/jp_m2_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.8  MONETARY AGGREGATE M1
*      FRED: MANMM101JPM189S
*      Monthly, JPY, SA, 1960-01 to 2023-11
*      https://fred.stlouisfed.org/series/MANMM101JPM189S
* --------------------------------------------------------------------------
import fred MANMM101JPM189S, clear

rename datestr         Time
rename MANMM101JPM189S m1_jpy

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable m1_jpy "Monetary Aggregates - M1 (JPY)"
order Country Time m1_jpy

save "Data/Aggregated/jp_m1_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.9  TOTAL CREDIT – PRIVATE NON-FINANCIAL SECTOR
*      FRED: QJPPAM770A  (quarterly → expanded to monthly, constant within quarter)
*      Quarterly, % GDP, breaks-adjusted, 1964-Q1 to 2025-Q1
*      https://fred.stlouisfed.org/series/QJPPAM770A
* --------------------------------------------------------------------------
import fred QJPPAM770A, clear

rename datestr      Time_q_str
rename QJPPAM770A   credit_pnf

* Parse quarterly date string (format "YYYY-MM-DD", first month of quarter)
gen time_q = quarterly(substr(Time_q_str, 1, 7), "YQ")
format time_q %tq
drop Time_q_str

* Expand each quarterly observation to 3 monthly rows
expand 3
bysort time_q: gen m_offset = _n - 1          // 0, 1, 2

* Convert to monthly Stata date
gen Time = yq(year(dofq(time_q)), quarter(dofq(time_q))) * 3 + m_offset + ///
           mofd(mdy(1, 1, 1960))               // anchor to Stata monthly epoch
* Simpler equivalent:
drop Time
gen Time = mofd(dofq(time_q)) + m_offset
format Time %tm

drop time_q m_offset
sort Time

gen Country = "JP"
label variable credit_pnf "Total Credit - Private Non-Financial (%GDP)"
order Country Time credit_pnf

save "Data/Aggregated/jp_credit_pnf_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.10  TOTAL CREDIT – GENERAL GOVERNMENT
*       FRED: QJPGAM770A  (quarterly → monthly, constant within quarter)
*       Quarterly, % GDP, 1997-Q1 to 2025-Q1
*       https://fred.stlouisfed.org/series/QJPGAM770A
* --------------------------------------------------------------------------
import fred QJPGAM770A, clear

rename datestr     Time_q_str
rename QJPGAM770A  credit_gg

gen time_q = quarterly(substr(Time_q_str, 1, 7), "YQ")
format time_q %tq
drop Time_q_str

expand 3
bysort time_q: gen m_offset = _n - 1

gen Time = mofd(dofq(time_q)) + m_offset
format Time %tm

drop time_q m_offset
sort Time

gen Country = "JP"
label variable credit_gg "Total Credit - General Government (%GDP)"
order Country Time credit_gg

save "Data/Aggregated/jp_credit_gg_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.11  TOTAL CREDIT – HOUSEHOLDS & NPISHs
*       FRED: QJPHAM770A  (quarterly → monthly, constant within quarter)
*       Quarterly, % GDP, 1964-Q1 to 2025-Q1
*       https://fred.stlouisfed.org/series/QJPHAM770A
* --------------------------------------------------------------------------
import fred QJPHAM770A, clear

rename datestr     Time_q_str
rename QJPHAM770A  credit_hh

gen time_q = quarterly(substr(Time_q_str, 1, 7), "YQ")
format time_q %tq
drop Time_q_str

expand 3
bysort time_q: gen m_offset = _n - 1

gen Time = mofd(dofq(time_q)) + m_offset
format Time %tm

drop time_q m_offset
sort Time

gen Country = "JP"
label variable credit_hh "Total Credit - Households & NPISHs (%GDP)"
order Country Time credit_hh

save "Data/Aggregated/jp_credit_hh_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.12  REAL GDP
*       FRED: JPNRGDPEXP  (quarterly → monthly, constant within quarter)
*       Quarterly, Billions of Chained 2015 JPY, SA, 1994-Q1 to 2025-Q2
*       https://fred.stlouisfed.org/series/JPNRGDPEXP
* --------------------------------------------------------------------------
import fred JPNRGDPEXP, clear

rename datestr     Time_q_str
rename JPNRGDPEXP  rgdp

gen time_q = quarterly(substr(Time_q_str, 1, 7), "YQ")
format time_q %tq
drop Time_q_str

expand 3
bysort time_q: gen m_offset = _n - 1

gen Time = mofd(dofq(time_q)) + m_offset
format Time %tm

drop time_q m_offset
sort Time

gen Country = "JP"
label variable rgdp "Real GDP (billions chained 2015 JPY)"
order Country Time rgdp

save "Data/Aggregated/jp_rgdp_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.13  LONG-TERM GOVERNMENT BOND YIELDS – 10-YEAR
*       FRED: IRLTLT01JPM156N
*       Monthly, %, NSA, 1989-01 to 2025-09
*       https://fred.stlouisfed.org/series/IRLTLT01JPM156N
* --------------------------------------------------------------------------
import fred IRLTLT01JPM156N, clear

rename datestr          Time
rename IRLTLT01JPM156N  gb10y

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable gb10y "10-Year Gov Bond Yields (%)"
order Country Time gb10y

save "Data/Aggregated/jp_10ygb_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.14  CALL MONEY / INTERBANK IMMEDIATE RATE
*       FRED: IRSTCI01JPM156N
*       Monthly, %, NSA, 1985-01 to 2025-09
*       https://fred.stlouisfed.org/series/IRSTCI01JPM156N
* --------------------------------------------------------------------------
import fred IRSTCI01JPM156N, clear

rename datestr          Time
rename IRSTCI01JPM156N  call_money

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable call_money "Call Money/Interbank Immediate (%)"
order Country Time call_money

save "Data/Aggregated/jp_cmibr_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.15  NATURAL RATE OF INTEREST (Nakajima et al. 2023)
*       Source: nakajima et al. (2023).csv
*       Quarterly (1Y_Mean, 10Y_Mean) → expanded to monthly
*       Reference: https://sites.google.com/site/jnakajimaweb/rstar
* --------------------------------------------------------------------------
import delimited "Data/nakajima et al. (2023).csv", varnames(nonames) clear

* Drop header row (first row of data)
drop if _n == 1

* The raw CSV has unlabelled columns; pick col 1 (YYYYQ), col 2 (1Y_Mean),
* col 5 (10Y_Mean) — adjust column indices if your CSV differs
rename v1 YYYYQ
rename v2 nir_1y
rename v5 nir_10y
keep YYYYQ nir_1y nir_10y

destring nir_1y nir_10y, replace force

* Parse year and quarter from YYYYQ (e.g. "20051" → year=2005, quarter=1)
gen str4 yr_str = substr(YYYYQ, 1, 4)
gen str1 qr_str = substr(YYYYQ, 5, 1)
destring yr_str qr_str, gen(yr qr)

gen time_q = yq(yr, qr)
format time_q %tq
drop YYYYQ yr_str qr_str yr qr

* Expand each quarter to 3 monthly rows
expand 3
bysort time_q: gen m_offset = _n - 1

gen Time = mofd(dofq(time_q)) + m_offset
format Time %tm

drop time_q m_offset
sort Time

gen Country = "JP"
label variable nir_1y  "Est. 1-year Neutral Interest Rate (%)"
label variable nir_10y "Est. 10-year Neutral Interest Rate (%)"
order Country Time nir_1y nir_10y

save "Data/Aggregated/jp_nir_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.16  BANK OF JAPAN TOTAL ASSETS (Balance Sheet)
*       FRED: JPNASSETS
*       Monthly, 100 Million Yen, NSA, 1998-04 to 2025-12
*       https://fred.stlouisfed.org/series/JPNASSETS
* --------------------------------------------------------------------------
import fred JPNASSETS, clear

rename datestr    Time
rename JPNASSETS  boj_assets

gen  time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

gen Country = "JP"
label variable boj_assets "BoJ's Total Assets (100 Million Yen)"
order Country Time boj_assets

save "Data/Aggregated/jp_bojta_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.17  U.S. 10-YEAR TREASURY YIELD (daily → monthly mean)
*       FRED: DGS10
*       Daily → averaged to monthly
*       https://fred.stlouisfed.org/series/DGS10
* --------------------------------------------------------------------------
import fred DGS10, clear

rename datestr Time_d
rename DGS10   ust_10y

gen time_d = daily(Time_d, "YMD")
format time_d %td

gen Time = mofd(time_d)
format Time %tm

* Collapse daily observations to monthly mean
collapse (mean) ust_10y, by(Time)

gen Country = "JP"
label variable ust_10y "10-Year US T-Bills Yield (%)"
order Country Time ust_10y

save "Data/Aggregated/jp_ust_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.18  CBOE VIX (daily → monthly mean)
*       FRED: VIXCLS
*       Daily → averaged to monthly
*       https://fred.stlouisfed.org/series/VIXCLS
* --------------------------------------------------------------------------
import fred VIXCLS, clear

rename datestr Time_d
rename VIXCLS  vix

gen time_d = daily(Time_d, "YMD")
format time_d %td

gen Time = mofd(time_d)
format Time %tm

collapse (mean) vix, by(Time)

gen Country = "JP"
label variable vix "CBOE-VIX"
order Country Time vix

save "Data/Aggregated/jp_vix_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.19  CENTRAL GOVERNMENT DEBT (% GDP) — annual → monthly (forward-fill)
*       FRED: DEBTTLJPA188A
*       Annual, % GDP, NSA, 1990–2022
*       https://fred.stlouisfed.org/series/DEBTTLJPA188A
* --------------------------------------------------------------------------
import fred DEBTTLJPA188A, clear

rename datestr       Time_a
rename DEBTTLJPA188A cg_debt

gen yr = year(daily(Time_a, "YMD"))
drop Time_a

* Expand each annual observation to 12 monthly rows
expand 12
bysort yr: gen m_offset = _n          // 1..12

gen Time = ym(yr, m_offset)
format Time %tm

drop yr m_offset
sort Time

gen Country = "JP"
label variable cg_debt "Central Government Debt (% GDP)"
order Country Time cg_debt

save "Data/Aggregated/jp_cgdebt_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.20  OUTSTANDING DOMESTIC PRIVATE DEBT SECURITIES (% GDP) — annual → monthly
*       FRED: DDDM03JPA156NWDB
*       Annual, % GDP, NSA, 1997–2020
*       https://fred.stlouisfed.org/series/DDDM03JPA156NWDB
* --------------------------------------------------------------------------
import fred DDDM03JPA156NWDB, clear

rename datestr          Time_a
rename DDDM03JPA156NWDB dp_debt

gen yr = year(daily(Time_a, "YMD"))
drop Time_a

expand 12
bysort yr: gen m_offset = _n

gen Time = ym(yr, m_offset)
format Time %tm

drop yr m_offset
sort Time

gen Country = "JP"
label variable dp_debt "Domestic Private Debt Securities (% GDP)"
order Country Time dp_debt

save "Data/Aggregated/jp_dpdebt_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.21  OUTSTANDING DOMESTIC PUBLIC DEBT SECURITIES (% GDP) — annual → monthly
*       FRED: DDDM04JPA156NWDB
*       Annual, % GDP, NSA, 1997–2020
*       https://fred.stlouisfed.org/series/DDDM04JPA156NWDB
* --------------------------------------------------------------------------
import fred DDDM04JPA156NWDB, clear

rename datestr          Time_a
rename DDDM04JPA156NWDB du_debt

gen yr = year(daily(Time_a, "YMD"))
drop Time_a

expand 12
bysort yr: gen m_offset = _n

gen Time = ym(yr, m_offset)
format Time %tm

drop yr m_offset
sort Time

gen Country = "JP"
label variable du_debt "Domestic Public Debt Securities (% GDP)"
order Country Time du_debt

save "Data/Aggregated/jp_dudebt_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.22  DEPOSIT INTEREST RATES (BoJ, two spliced series)
*       Source 1 (up to 2022-03): BoJ_deposits_i_IR02STRDCDNITL03.csv
*           CD 60–89 days (City Banks)
*       Source 2 (from 2022-03): BoJ_deposits_i_extended_IR02DLDR43TL43MN.csv
*           Time Deposits ≥10M JPY / 3 Months
*       Output: spliced monthly series
* --------------------------------------------------------------------------

* --- Series 1 ---
import delimited "Data/BoJ_deposits_i_IR02STRDCDNITL03.csv", varnames(1) clear

* Column name is very long — just rename both columns
rename (v1 v2) (Time dep_rate_1)
capture rename (*) (Time dep_rate_1)     // fallback if varnames read correctly

* If variable names are already correct from varnames(1), adapt:
* rename time Time
* rename *IR02STRDCDNITL03* dep_rate_1

gen time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

drop if missing(dep_rate_1)
sort Time
save "Data/Aggregated/tmp_dep1.dta", replace

* --- Series 2 ---
import delimited "Data/BoJ_deposits_i_extended_IR02DLDR43TL43MN.csv", varnames(1) clear

rename (v1 v2) (Time dep_rate_2)

gen time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

drop if missing(dep_rate_2)
sort Time
save "Data/Aggregated/tmp_dep2.dta", replace

* --- Outer merge and unify ---
use "Data/Aggregated/tmp_dep1.dta", clear
merge 1:1 Time using "Data/Aggregated/tmp_dep2.dta", nogen

* Combine: use dep_rate_2 where dep_rate_1 is missing (extension)
gen dep_rate = dep_rate_1
replace dep_rate = dep_rate_2 if missing(dep_rate) & !missing(dep_rate_2)
drop dep_rate_1 dep_rate_2

gen Country = "JP"
label variable dep_rate "Deposit Interest Rate (%)"
order Country Time dep_rate
sort Time

save "Data/Aggregated/jp_depi_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.23  LOAN INTEREST RATES (BoJ)
*       Source: BoJ_loans_i_IR04DLLR2CIDBNL1.csv
*       New Loans and Discounts / Total / Domestically Licensed Banks
*       Monthly, %, NSA, 1993-10 to 2025-12
*       https://www.boj.or.jp/en/statistics/dl/loan/yaku/index.htm
* --------------------------------------------------------------------------
import delimited "Data/BoJ_loans_i_IR04DLLR2CIDBNL1.csv", varnames(1) clear

rename (v1 v2) (Time loan_rate)

gen time_m = monthly(Time, "YM")
format time_m %tm
drop Time
rename time_m Time

drop if missing(loan_rate)

gen Country = "JP"
label variable loan_rate "Loan Interest Rate (%)"
order Country Time loan_rate
sort Time

save "Data/Aggregated/jp_loai_m_raw.dta", replace


* --------------------------------------------------------------------------
* 4.24  BANKING SECTOR DEFAULT PROXY — TOPIX Banks ETF
*       Yahoo Finance: 1615.T
*       Monthly price, 2000-01 to 2026-02
*       NOTE: Pre-download as monthly OHLCV CSV from Yahoo Finance.
* --------------------------------------------------------------------------
import delimited "Data/yfinance_1615T.csv", varnames(1) clear

rename date  Time_str
rename close price_1615T

gen Time = monthly(Time_str, "YM")
format Time %tm
drop Time_str

drop if missing(price_1615T)

gen Country = "JP"
label variable price_1615T "1615.T-Price"
order Country Time price_1615T
sort Time

save "Data/Aggregated/jp_def_m_raw.dta", replace


/*******************************************************************************
* SECTION 5 – DATA MERGING & AGGREGATED DATASETS
*******************************************************************************/

* --------------------------------------------------------------------------
* 5.1  BROAD MONEY (M1 + M2 + M3)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_m1_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_m2_m_raw.dta",  nogen
merge 1:1 Time using "Data/Aggregated/jp_m3_m_raw.dta",  nogen
sort Time
save   "Data/Aggregated/jp_broad_money_df.dta", replace
export delimited "Data/Aggregated/jp_broad_money_df.csv", replace


* --------------------------------------------------------------------------
* 5.2  CREDIT DEMAND (Private Non-Financial + General Gov. + Households)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_credit_pnf_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_credit_gg_m_raw.dta",  nogen
merge 1:1 Time using "Data/Aggregated/jp_credit_hh_m_raw.dta",  nogen
sort Time
save   "Data/Aggregated/jp_credit_demand_df.dta", replace
export delimited "Data/Aggregated/jp_credit_demand_df.csv", replace


* --------------------------------------------------------------------------
* 5.3  BANK RESERVES (already saved in 4.5)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_bank_reserves_df.dta", clear
export delimited "Data/Aggregated/jp_bank_reserves_df.csv", replace


* --------------------------------------------------------------------------
* 5.4  BOJ TOTAL ASSETS (already saved in 4.16)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_bojta_m_raw.dta", clear
save   "Data/Aggregated/jp_boj_total_assets_df.dta", replace
export delimited "Data/Aggregated/jp_boj_total_assets_df.csv", replace


* --------------------------------------------------------------------------
* 5.5  POLICY RATE (10-Year Gov Bond + Call Money + Natural IR)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_10ygb_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_cmibr_m_raw.dta",  nogen
merge 1:1 Time using "Data/Aggregated/jp_nir_m_raw.dta",    nogen
sort Time
save   "Data/Aggregated/jp_policy_rate_df.dta", replace
export delimited "Data/Aggregated/jp_policy_rate_df.csv", replace


* --------------------------------------------------------------------------
* 5.6  EXCHANGE RATE (REER + Spot)
* --------------------------------------------------------------------------
use "Data/Aggregated/REXUSDJPY_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/JPYUSD_m_raw.dta", nogen
sort Time
save   "Data/Aggregated/jp_exchange_rate_df.dta", replace
export delimited "Data/Aggregated/jp_exchange_rate_df.csv", replace


* --------------------------------------------------------------------------
* 5.7  INFLATION (HICP, seasonally adjusted — already saved in 4.2)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_HICP_m_raw.dta", clear
save   "Data/Aggregated/jp_inflation_df.dta", replace
export delimited "Data/Aggregated/jp_inflation_df.csv", replace


* --------------------------------------------------------------------------
* 5.8  REAL GDP (already saved in 4.12)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_rgdp_m_raw.dta", clear
save   "Data/Aggregated/jp_rgdp_df.dta", replace
export delimited "Data/Aggregated/jp_rgdp_df.csv", replace


* --------------------------------------------------------------------------
* 5.9  STOCK INDICES (already saved in 4.4)
* --------------------------------------------------------------------------
* jp_stocks_pv_df.dta already written; CSV also exported above.


* --------------------------------------------------------------------------
* 5.10  DEBT LEVEL (Central Gov + Private Debt Securities + Public Debt Sec.)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_cgdebt_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_dpdebt_m_raw.dta", nogen
merge 1:1 Time using "Data/Aggregated/jp_dudebt_m_raw.dta", nogen
sort Time
save   "Data/Aggregated/jp_debt_df.dta", replace
export delimited "Data/Aggregated/jp_debt_df.csv", replace


* --------------------------------------------------------------------------
* 5.11  NET INTEREST MARGIN — NIM (Loan Rate + Deposit Rate)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_loai_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_depi_m_raw.dta", nogen
sort Time
save   "Data/Aggregated/jp_nim_df.dta", replace
export delimited "Data/Aggregated/jp_nim_df.csv", replace


* --------------------------------------------------------------------------
* 5.12  DEFAULT RATE PROXY / STRESS (TOPIX Banks ETF — already saved in 4.24)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_def_m_raw.dta", clear
save   "Data/Aggregated/jp_stress_df.dta", replace
export delimited "Data/Aggregated/jp_stress_df.csv", replace


* --------------------------------------------------------------------------
* 5.13  CONTROLS (US 10-Year Treasury + VIX)
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_ust_m_raw.dta", clear
merge 1:1 Time using "Data/Aggregated/jp_vix_m_raw.dta", nogen
sort Time
save   "Data/Aggregated/jp_control_df.dta", replace
export delimited "Data/Aggregated/jp_control_df.csv", replace


* --------------------------------------------------------------------------
* 5.14  FULLY AGGREGATED DATASET — jp_aggregated_df
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_broad_money_df.dta",       clear
merge 1:1 Time using "Data/Aggregated/jp_credit_demand_df.dta",    nogen
merge 1:1 Time using "Data/Aggregated/jp_bank_reserves_df.dta",    nogen
merge 1:1 Time using "Data/Aggregated/jp_policy_rate_df.dta",      nogen
merge 1:1 Time using "Data/Aggregated/jp_exchange_rate_df.dta",    nogen
merge 1:1 Time using "Data/Aggregated/jp_inflation_df.dta",        nogen
merge 1:1 Time using "Data/Aggregated/jp_rgdp_df.dta",             nogen
merge 1:1 Time using "Data/Aggregated/jp_debt_df.dta",             nogen
merge 1:1 Time using "Data/Aggregated/jp_boj_total_assets_df.dta", nogen
merge 1:1 Time using "Data/Aggregated/jp_nim_df.dta",              nogen
merge 1:1 Time using "Data/Aggregated/jp_stress_df.dta",           nogen
merge 1:1 Time using "Data/Aggregated/jp_control_df.dta",          nogen

* Fill Country identifier where missing (all obs are Japan)
replace Country = "JP" if missing(Country)

* Sort by time and set as monthly time series
sort Time
tsset Time

* Preview last 5 rows
list in -5/l

* Save final dataset
save   "Data/Aggregated/jp_aggregated_df.dta", replace
export delimited "Data/Aggregated/jp_aggregated_df.csv", replace

di "Module 1 complete. All datasets saved to Data/Aggregated/"
