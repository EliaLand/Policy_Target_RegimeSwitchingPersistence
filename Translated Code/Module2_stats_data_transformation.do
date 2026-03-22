
/*******************************************************************************
* Module 2 - Statistics & Data Transformation
* Author    : Elia Landini
* Student ID: 12310239
* Course    : EESM2-Financial Economics
* Supervisor: Jean-Bernard Chatelain
* Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
*
* Translation of Python/Jupyter notebook to Stata do-file.
*
* PREREQUISITES:
*   - Module 1 do-file must have been run first (produces jp_aggregated_df.dta)
*   - Working directory set to project root
*   - Output folder ./Data/Transformed/ must exist
*
* VARIABLE NAME MAPPING (Module 1 short names → original long names):
*   m1_jpy         ← Monetary Aggregates - M1 (JPY)
*   m2_jpy         ← Monetary Aggregates - M2 (JPY)
*   m3_jpy         ← Monetary Aggregates - M3 (JPY)
*   total_reserves ← Total Treasury Reserves (- Gold)
*   reer_cpi       ← USD-JPY reer CPI-based (Index 2015=100)
*   spot_jpy_usd   ← JPY-USD Spot Exchange Rate
*   hicp_sa        ← HICP (SA)
*   price_1615T    ← 1615.T-Price
*   boj_assets     ← BoJ's Total Assets (100 Million Yen)
*   call_money     ← Call Money/Interbank Immediate (%)
*   credit_pnf     ← Total Credit - Private Non-Financial (%GDP)
*   credit_gg      ← Total Credit - General Government (%GDP)
*   credit_hh      ← Total Credit - Households & NPISHs (%GDP)
*   gb10y          ← 10-Year Gov Bond Yields (%)
*   nir_1y         ← Est. 1-year Neutral Interest Rate (%)
*   nir_10y        ← Est. 10-year Neutral Interest Rate (%)
*   rgdp           ← Real GDP (billions chained 2015 JPY)
*   cg_debt        ← Central Government Debt (% GDP)
*   dp_debt        ← Domestic Private Debt Securities (% GDP)
*   du_debt        ← Domestic Public Debt Securities (% GDP)
*   loan_rate      ← Loan Interest Rate (%)
*   dep_rate       ← Deposit Interest Rate (%)
*   ust_10y        ← 10-Year US T-Bills Yield (%)
*   vix            ← CBOE-VIX
*******************************************************************************/

clear all
set more off
capture mkdir "Data/Transformed"


/*******************************************************************************
* SECTION 1 – REQUIREMENTS SET-UP
* (No library imports needed in Stata)
*******************************************************************************/


/*******************************************************************************
* SECTION 2 – DESCRIPTIVE STATISTICS (RAW DATA)
*******************************************************************************/

* --------------------------------------------------------------------------
* Load aggregated dataset produced by Module 1
* --------------------------------------------------------------------------
use "Data/Aggregated/jp_aggregated_df.dta", clear

* Set as monthly time series (Time variable is already formatted %tm from Module 1)
tsset Time

* --------------------------------------------------------------------------
* Descriptive statistics — full aggregated dataset (equivalent to df.describe())
* --------------------------------------------------------------------------
summarize m1_jpy m2_jpy m3_jpy                          ///
          credit_pnf credit_gg credit_hh                 ///
          total_reserves                                  ///
          gb10y call_money nir_1y nir_10y                 ///
          reer_cpi spot_jpy_usd                           ///
          rgdp                                            ///
          hicp_sa                                         ///
          cg_debt dp_debt du_debt                         ///
          loan_rate dep_rate price_1615T                  ///
          ust_10y vix                                     ///
          boj_assets,                                     ///
          detail


/*******************************************************************************
* SECTION 3 – NON-STATIONARITY CORRECTIONS
*
* Transformation strategy:
*   Log-Difference  → Monetary aggregates, reserves, exchange rates,
*                     HICP (annualised), banking price, BoJ assets, call money
*   AR(1) detrend   → Credit metrics, policy yields, neutral rates,
*                     debt metrics, banking rates, controls (VIX, US yield)
*   HP-filter cycle → Real GDP (log, λ=1600)
*******************************************************************************/

* Keep Country and Time; all transformed variables will be added here
* (work in-memory; save at end)

* --------------------------------------------------------------------------
* 3.1  LOG-DIFFERENCE TRANSFORMATIONS
*      Python: trans_df[f"LogDiff-{var}"] = np.log(df[var]).diff()
*      HICP annualised: * 12 * 100
*      Stata: first-difference of log using L. (lag) operator after tsset
* --------------------------------------------------------------------------

* Monetary Aggregates
gen ld_m1_jpy    = D.ln(m1_jpy)
gen ld_m2_jpy    = D.ln(m2_jpy)
gen ld_m3_jpy    = D.ln(m3_jpy)

label variable ld_m1_jpy    "LogDiff - Monetary Aggregates M1 (JPY)"
label variable ld_m2_jpy    "LogDiff - Monetary Aggregates M2 (JPY)"
label variable ld_m3_jpy    "LogDiff - Monetary Aggregates M3 (JPY)"

* Total Reserves
gen ld_reserves  = D.ln(total_reserves)
label variable ld_reserves  "LogDiff - Total Treasury Reserves (- Gold)"

* Exchange Rates
gen ld_reer      = D.ln(reer_cpi)
gen ld_spot      = D.ln(spot_jpy_usd)
label variable ld_reer      "LogDiff - USD-JPY REER CPI-based"
label variable ld_spot      "LogDiff - JPY-USD Spot Exchange Rate"

* HICP (SA) — annualised: log-difference × 12 × 100
gen ld_hicp_sa   = D.ln(hicp_sa) * 12 * 100
label variable ld_hicp_sa   "LogDiff - HICP (SA), annualised %"

* Banking sector price proxy
gen ld_price_1615T = D.ln(price_1615T)
label variable ld_price_1615T "LogDiff - 1615.T Price"

* BoJ Total Assets
gen ld_boj_assets = D.ln(boj_assets)
label variable ld_boj_assets "LogDiff - BoJ Total Assets"

* Call Money / Interbank rate (also log-differenced — rate ≠ 0 throughout)
gen ld_call_money = D.ln(call_money)
label variable ld_call_money "LogDiff - Call Money/Interbank Immediate (%)"


* --------------------------------------------------------------------------
* 3.2  AR(1) DETRENDING
*      Python: AutoReg(df[var].dropna(), lags=1).fit() → residuals
*      Stata:  regress var L.var; predict ar1_var, residuals
*
*      Rationale: credit, yields, and debt series are near-unit-root I(0);
*      first-differences destroy medium-term cycles.  AR(1) residuals capture
*      cyclical deviations (policy shocks, credit gaps) without over-differencing.
* --------------------------------------------------------------------------

* Macro list of variables requiring AR(1) detrending
local ar1_vars credit_pnf credit_gg credit_hh    ///
               gb10y call_money nir_1y nir_10y   ///
               cg_debt dp_debt du_debt           ///
               loan_rate dep_rate               ///
               ust_10y vix

foreach var of local ar1_vars {

    * Fit AR(1): regress var on its own first lag
    * (!!!) `if !missing(`var') & !missing(L.`var')` restricts to valid obs only
    quietly regress `var' L.`var' if !missing(`var') & !missing(L.`var')

    * Predict residuals (fitted = α̂ + ρ̂·var_{t-1})
    quietly predict _resid_`var', residuals

    * Assign to clean name with ar1_ prefix
    gen ar1_`var' = _resid_`var'
    drop _resid_`var'

    label variable ar1_`var' "AR(1) detrended - `var'"
}

* Explicit labels for key series
label variable ar1_credit_pnf  "AR(1) detrend - Total Credit: Private Non-Financial (%GDP)"
label variable ar1_credit_gg   "AR(1) detrend - Total Credit: General Government (%GDP)"
label variable ar1_credit_hh   "AR(1) detrend - Total Credit: Households & NPISHs (%GDP)"
label variable ar1_gb10y       "AR(1) detrend - 10-Year Gov Bond Yields (%)"
label variable ar1_call_money  "AR(1) detrend - Call Money/Interbank Immediate (%)"
label variable ar1_nir_1y      "AR(1) detrend - Est. 1-year Neutral Interest Rate (%)"
label variable ar1_nir_10y     "AR(1) detrend - Est. 10-year Neutral Interest Rate (%)"
label variable ar1_cg_debt     "AR(1) detrend - Central Government Debt (% GDP)"
label variable ar1_dp_debt     "AR(1) detrend - Domestic Private Debt Securities (% GDP)"
label variable ar1_du_debt     "AR(1) detrend - Domestic Public Debt Securities (% GDP)"
label variable ar1_loan_rate   "AR(1) detrend - Loan Interest Rate (%)"
label variable ar1_dep_rate    "AR(1) detrend - Deposit Interest Rate (%)"
label variable ar1_ust_10y     "AR(1) detrend - 10-Year US T-Bills Yield (%)"
label variable ar1_vix         "AR(1) detrend - CBOE-VIX"


* --------------------------------------------------------------------------
* 3.3  HP-FILTER CYCLE
*      Python: cycle, trend = hpfilter(np.log(df["Real GDP"]).dropna(), lamb=1600)
*      Stata:  tsfilter hp hp_rgdp = ln_rgdp, smooth(1600)
*
*      Rationale: HP-filter isolates the business-cycle component of log-GDP,
*      removing the long-run trend (λ=1600 is the standard for monthly data).
* --------------------------------------------------------------------------

* Generate log real GDP (drop missing before filtering)
gen ln_rgdp = ln(rgdp)
label variable ln_rgdp "Log Real GDP (billions chained 2015 JPY)"

* HP filter: smooth(1600) = λ; output: hp_rgdp (cycle), trend stored implicitly
tsfilter hp hp_rgdp = ln_rgdp, smooth(1600) trend(hptrend_rgdp)

label variable hp_rgdp      "HP-filter cycle - Log Real GDP (λ=1600)"
label variable hptrend_rgdp "HP-filter trend - Log Real GDP (λ=1600)"


/*******************************************************************************
* SECTION 4 – SAVE TRANSFORMED DATASETS
*******************************************************************************/

* --------------------------------------------------------------------------
* 4.1  Full transformed dataset — jp_trans_df
*      All original raw variables + all transformed variables
* --------------------------------------------------------------------------
save   "Data/Transformed/jp_trans_df.dta", replace
export delimited "Data/Transformed/jp_trans_df.csv", replace


* --------------------------------------------------------------------------
* 4.2  Core transformed dataset — jp_core_trans_df
*      Country, Time, HICP (SA) log-diff, AR(1) call money, AR(1) 10Y bond
*      Python: jp_core_trans_df = jp_trans_df[
*                  ["Country","Time","LogDiff-HICP (SA)",
*                   "AR(1)detrend-Call Money/Interbank Immediate (%)",
*                   "AR(1)detrend-10-Year Gov Bond Yields (%)"]]
* --------------------------------------------------------------------------
preserve
    keep Country Time ld_hicp_sa ar1_call_money ar1_gb10y
    save   "Data/Transformed/jp_core_trans_df.dta", replace
    export delimited "Data/Transformed/jp_core_trans_df.csv", replace
restore


* --------------------------------------------------------------------------
* 4.3  Core hybrid dataset — jp_core_hybrid_df
*      Transformed target (HICP log-diff) + RAW policy instruments
*      Python: merges core_trans (HICP only) with raw call_money & gb10y,
*              then drops rows with any missing value
* --------------------------------------------------------------------------
preserve
    keep Country Time ld_hicp_sa call_money gb10y
    * Drop any rows with missing in any of the three series
    drop if missing(ld_hicp_sa) | missing(call_money) | missing(gb10y)
    save   "Data/Transformed/jp_core_hybrid_df.dta", replace
    export delimited "Data/Transformed/jp_core_hybrid_df.csv", replace
restore


/*******************************************************************************
* SECTION 5 – DESCRIPTIVE STATISTICS (TRANSFORMED DATA)
*******************************************************************************/

* --------------------------------------------------------------------------
* 5.1  Descriptive statistics — raw core series (Cell 12)
*      Python: df = jp_aggregated_df[["Country","Time","HICP (SA)",
*                  "Call Money/Interbank Immediate (%)","10-Year Gov Bond Yields (%)"]]
*              df.describe()
* --------------------------------------------------------------------------
di _newline "=== Descriptive Statistics — RAW Core Series ==="
summarize hicp_sa call_money gb10y, detail


* --------------------------------------------------------------------------
* 5.2  Descriptive statistics — transformed core series (Cell 13)
*      Python: df = jp_core_trans_df.describe()
* --------------------------------------------------------------------------
di _newline "=== Descriptive Statistics — TRANSFORMED Core Series ==="
summarize ld_hicp_sa ar1_call_money ar1_gb10y, detail


di _newline "Module 2 complete. Transformed datasets saved to Data/Transformed/"
