
/*******************************************************************************
* Module 3 - AR Analysis
* Author    : Elia Landini
* Student ID: 12310239
* Course    : EESM2-Financial Economics
* Supervisor: Jean-Bernard Chatelain
* Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
*
* Translation of Python/Jupyter notebook (Module3_ar_analysis.ipynb) to Stata.
*
* PREREQUISITES:
*   Module 2 do-file must have been run first (produces jp_trans_df.dta etc.)
*
* VARIABLE KEY (short names from Module 2):
*   ld_hicp_sa    = LogDiff-HICP (SA)         [policy target, transformed]
*   call_money    = Call Money/Interbank (%)   [policy instrument, raw]
*   ld_call_money = LogDiff-Call Money         [policy instrument, log-diff]
*   gb10y         = 10-Year Gov Bond Yields (%)
*   hp_rgdp       = HP-filter output gap (log real GDP)
*   hp_reserves   = ld_reserves (log-diff treasury reserves)
*   hp_rgdp       = HP-filter cycle of log real GDP
*
* YCC KEY DATES (reference vertical lines throughout):
*   YCC Adoption          : 21 Sep 2016  (monthly: 2016m9)
*   YCC De-facto Widening : 31 Jul 2018  (monthly: 2018m7)
*   YCC Explicit Widening : 19 Mar 2021  (monthly: 2021m3)
*******************************************************************************/

clear all
set more off
capture mkdir "Output/Module3"


/*******************************************************************************
* SECTION 1 – DATA LOADING
* Python: jp_aggregated_df, jp_trans_df, jp_core_trans_df, jp_core_hybrid_df
*******************************************************************************/

* Full transformed dataset (all variables)
use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Convenience: store the three YCC dates as scalars (in %tm units)
scalar ycc_adopt    = ym(2016, 9)
scalar ycc_defacto  = ym(2018, 7)
scalar ycc_explicit = ym(2021, 3)


/*******************************************************************************
* SECTION 2 – OVERTIME PLOTTING
* Sections 5–8 in notebook: time-series plots of policy target & instruments.
* Sample restricted to observations >= Jan 1990.
*
* Python:  matplotlib line charts with YCC reference lines
* Stata:   twoway tsline with xline() for YCC dates
*******************************************************************************/

* ── Plot 2.1: Raw policy target + instruments (1990–, no smoothing) ────────
twoway                                                                        ///
    (tsline ld_hicp_sa if Time >= ym(1990,1),                                 ///
        lcolor("31 158 137") lwidth(thin)                                     ///
        legend(label(1 "LogDiff-HICP (SA)")))                                 ///
    (tsline call_money if Time >= ym(1990,1),                                 ///
        lcolor("68 1 84") lwidth(thin)                                        ///
        legend(label(2 "Call Money/Interbank (%)")))                          ///
    (tsline gb10y if Time >= ym(1990,1),                                      ///
        lcolor("253 231 37") lwidth(thin)                                     ///
        legend(label(3 "10-Year Gov Bond Yields (%)"))),                      ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("Policy Target & Policy Instrument Evolution Overtime (Raw Data)")   ///
    xtitle("Year") ytitle("Policy Target/Instrument")                          ///
    legend(rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_plot_raw.png", replace

* ── Plot 2.2: Same with 12-month rolling average for inflation ─────────────
* (!!!) Python: df1["LogDiff-HICP (SA)-smooth"] = df1["LogDiff-HICP (SA)"].rolling(12).mean()
tssmooth ma ld_hicp_smooth = ld_hicp_sa if Time >= ym(1990,1), window(6 1 5)
label variable ld_hicp_smooth "LogDiff-HICP (SA, 12-lag rolling mean)"

twoway                                                                        ///
    (tsline ld_hicp_smooth if Time >= ym(1990,1),                             ///
        lcolor("31 158 137") lwidth(thin)                                     ///
        legend(label(1 "LogDiff-HICP (SA, 12-lag rolling mean)")))            ///
    (tsline call_money if Time >= ym(1990,1),                                 ///
        lcolor("68 1 84") lwidth(thin)                                        ///
        legend(label(2 "Call Money/Interbank (%)")))                          ///
    (tsline gb10y if Time >= ym(1990,1),                                      ///
        lcolor("253 231 37") lwidth(thin)                                     ///
        legend(label(3 "10-Year Gov Bond Yields (%)"))),                      ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("Policy Target & Policy Instrument Evolution Overtime (Smoothed)")  ///
    xtitle("Year") ytitle("Policy Target/Instrument")                          ///
    legend(rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_plot_smoothed.png", replace

* ── Plot 2.3: Transformed variables (jp_core_trans_df) ─────────────────────
twoway                                                                        ///
    (tsline ld_hicp_sa,                                                       ///
        lcolor("31 158 137") lwidth(thin)                                     ///
        legend(label(1 "LogDiff-HICP (SA)")))                                 ///
    (tsline ar1_call_money,                                                   ///
        lcolor("68 1 84") lwidth(thin)                                        ///
        legend(label(2 "AR(1)detrend-Call Money (%)")))                       ///
    (tsline ar1_gb10y,                                                        ///
        lcolor("253 231 37") lwidth(thin)                                     ///
        legend(label(3 "AR(1)detrend-10Y Gov Bond Yields (%)"))),             ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("Policy Target & Instrument Evolution Overtime (Transformed Data)") ///
    xtitle("Year") ytitle("Policy Target/Instrument")                          ///
    legend(rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_plot_transformed.png", replace


/*******************************************************************************
* SECTION 3 – AR(1) PROCESS ESTIMATION
* Sections 9–12: OLS AR(1) for inflation, policy instrument, output gap.
*
* Python: sm.OLS(y_t, add_constant(y_tm1)).fit()
* Stata:  regress var L.var   (requires tsset)
*
* AR(1) residuals tested with AR(1)-on-residuals (Breusch-Godfrey logic):
* Python: sm.OLS(u_t, add_constant(u_tm1)).fit()
* Stata:  predict resid; regress resid L.resid
*******************************************************************************/

* ── 3.1 AR(1) – Inflation (LogDiff-HICP (SA)) ──────────────────────────────
di _newline "=== AR(1) – Inflation ==="
regress ld_hicp_sa L.ld_hicp_sa if !missing(ld_hicp_sa) & !missing(L.ld_hicp_sa)
scalar ar1_phi_pi = _b[L.ld_hicp_sa]
scalar ar1_const_pi = _b[_cons]
di "AR(1) phi(inflation) = " ar1_phi_pi

* AR(1) on residuals (test for residual autocorrelation)
predict u_pi if e(sample), residuals
di _newline "=== AR(1) on Residuals – Inflation ==="
regress u_pi L.u_pi if !missing(u_pi) & !missing(L.u_pi)
drop u_pi

* ── 3.2 AR(1) – Policy Instrument, raw (Call Money) ───────────────────────
di _newline "=== AR(1) – Call Money/Interbank (raw) ==="
regress call_money L.call_money if !missing(call_money) & !missing(L.call_money)
scalar ar1_phi_i = _b[L.call_money]
scalar ar1_const_i = _b[_cons]
di "AR(1) phi(call money) = " ar1_phi_i

predict u_i if e(sample), residuals
di _newline "=== AR(1) on Residuals – Call Money ==="
regress u_i L.u_i if !missing(u_i) & !missing(L.u_i)
drop u_i

* ── 3.3 AR(1) – Output Gap (HP-filter log Real GDP) ───────────────────────
* (!!!) HP-filter output gap is the standard choice for DSGE/Taylor rule models
di _newline "=== AR(1) – HP Output Gap ==="
regress hp_rgdp L.hp_rgdp if !missing(hp_rgdp) & !missing(L.hp_rgdp)
scalar ar1_phi_y = _b[L.hp_rgdp]
scalar ar1_const_y = _b[_cons]
di "AR(1) phi(output gap) = " ar1_phi_y

predict u_y if e(sample), residuals
di _newline "=== AR(1) on Residuals – Output Gap ==="
regress u_y L.u_y if !missing(u_y) & !missing(L.u_y)
drop u_y

* ── 3.4 AR(1) Autocorrelation Table (jp_core_hybrid_df) ───────────────────
* Python: series.autocorr(lag=1) for each column
* Stata:  corr var L.var  or  acf var, lags(1)
di _newline "=== AR(1) Autocorrelation Coefficients ==="
foreach var in ld_hicp_sa call_money gb10y {
    quietly corr `var' L.`var' if !missing(`var') & !missing(L.`var')
    di "AR(1) autocorr [" "`var'" "] = " r(rho)
}


/*******************************************************************************
* SECTION 4 – AR(1) UNIT ROOT TESTING (RAW DATA)
* Sections 13–15: ADF + PP tests on core hybrid variables.
*
* Python:  adfuller(series, autolag="AIC") + PhillipsPerron(series)
* Stata:   dfuller var, lags(#) regress  +  pperron var
*
* (!!!) ADF lag length: Stata's dfuller with lags(#) where # is AIC-selected.
*       Use dfuller var to let Stata auto-select, or dfuller var, lags(0) for ADF(0).
*       pperron provides the Newey-West heteroskedasticity-consistent PP test.
*******************************************************************************/

* ── 4.1 Augmented Dickey-Fuller Test ───────────────────────────────────────
di _newline "=== ADF Unit Root Tests – Core Hybrid Variables ==="
foreach var in ld_hicp_sa call_money gb10y {
    di _newline "ADF test: `var'"
    * (!!!) Python uses autolag="AIC"; Stata's dfuller uses maxlags with AIC via regress option
    dfuller `var' if !missing(`var'), lags(12) regress
    * Display: reject H0 (unit root) if p-value < 0.05
    * (!!!) Stata does not report p-value directly; use critical values at 1%, 5%, 10%
}

* ── 4.2 Phillips-Perron Test ────────────────────────────────────────────────
di _newline "=== Phillips-Perron Unit Root Tests – Core Hybrid Variables ==="
foreach var in ld_hicp_sa call_money gb10y {
    di _newline "PP test: `var'"
    pperron `var' if !missing(`var'), lags(12)
}

* ── 4.3 AR(1) Autocorrelation + Unit Circle (tabular output) ───────────────
* (!!!) Python: complex matplotlib unit-circle plot (eigenvalues on real axis)
* Stata:  display the AR(1) phi values relative to ±1 boundary
di _newline "=== AR(1) Coefficients vs Unit Boundary ==="
foreach var in ld_hicp_sa call_money gb10y {
    quietly regress `var' L.`var' if !missing(`var') & !missing(L.`var')
    scalar phi = _b[L.`var']
    di "`var': phi = " phi  " | |phi|<1 (stationary): " cond(abs(phi)<1,"Yes","No")
}

* AC / PAC plots (visual autocorrelation diagnostics)
ac  ld_hicp_sa, lags(20) title("ACF – LogDiff-HICP (SA)") graphregion(color(white))
graph export "Output/Module3/fig_acf_pi.png", replace
pac ld_hicp_sa, lags(20) title("PACF – LogDiff-HICP (SA)") graphregion(color(white))
graph export "Output/Module3/fig_pacf_pi.png", replace


/*******************************************************************************
* SECTION 5 – AR(2) PROCESS ESTIMATION
* Sections 16–18: OLS AR(2) for inflation, call money, output gap.
*
* Python: sm.OLS(pi_t, add_constant([pi_tm1, pi_tm2])).fit()
* Stata:  regress var L.var L2.var
*******************************************************************************/

* ── 5.1 AR(2) – Inflation ───────────────────────────────────────────────────
di _newline "=== AR(2) – Inflation ==="
regress ld_hicp_sa L.ld_hicp_sa L2.ld_hicp_sa if                            ///
    !missing(ld_hicp_sa) & !missing(L.ld_hicp_sa) & !missing(L2.ld_hicp_sa)
scalar phi1_pi = _b[L.ld_hicp_sa]
scalar phi2_pi = _b[L2.ld_hicp_sa]
di "AR(2) phi1(pi) = " phi1_pi "  phi2(pi) = " phi2_pi

* ── 5.2 AR(2) – Call Money (raw) ────────────────────────────────────────────
di _newline "=== AR(2) – Call Money (raw) ==="
regress call_money L.call_money L2.call_money if                             ///
    !missing(call_money) & !missing(L.call_money) & !missing(L2.call_money)
scalar phi1_i = _b[L.call_money]
scalar phi2_i = _b[L2.call_money]
di "AR(2) phi1(i) = " phi1_i "  phi2(i) = " phi2_i

* ── 5.3 AR(2) – Output Gap ──────────────────────────────────────────────────
di _newline "=== AR(2) – Output Gap ==="
regress hp_rgdp L.hp_rgdp L2.hp_rgdp if                                     ///
    !missing(hp_rgdp) & !missing(L.hp_rgdp) & !missing(L2.hp_rgdp)
scalar phi1_y = _b[L.hp_rgdp]
scalar phi2_y = _b[L2.hp_rgdp]


/*******************************************************************************
* SECTION 6 – AR(2) UNIT ROOT TESTING
* Sections 19–22: Characteristic polynomial roots + stability triangle.
*
* Python: coeffs = [1, -(phi1+phi2), -(phi1*phi2)]; np.roots(coeffs)
*         Stability conditions:
*         (1) phi1*phi2 < 1
*         (2) phi1*phi2 > (phi1+phi2) - 1
*         (3) phi1*phi2 > -(phi1+phi2) - 1
*
* Stata:  compute roots analytically from scalar estimates.
*         The characteristic polynomial used in the notebook is:
*         X² - (φ1+φ2)X - φ1φ2 = 0
*         Discriminant Δ = (φ1+φ2)² + 4φ1φ2 = (φ1+φ2+2√(φ1φ2))... 
*         → roots: X = [(φ1+φ2) ± √Δ] / 2
*******************************************************************************/

di _newline "=== AR(2) Characteristic Polynomial Roots ==="

foreach sfx in pi i {

    * Load correct phi scalars
    if "`sfx'" == "pi" {
        scalar p1 = phi1_pi
        scalar p2 = phi2_pi
        local varname "Inflation"
    }
    else {
        scalar p1 = phi1_i
        scalar p2 = phi2_i
        local varname "Call Money"
    }

    * Characteristic polynomial: X² - (p1+p2)X - p1*p2 = 0
    scalar sum_p   = p1 + p2
    scalar prod_p  = p1 * p2
    * Discriminant
    scalar delta   = sum_p^2 + 4*prod_p

    if delta >= 0 {
        scalar root1   = (sum_p + sqrt(delta)) / 2
        scalar root2   = (sum_p - sqrt(delta)) / 2
        scalar mod1    = abs(root1)
        scalar mod2    = abs(root2)
        di "`varname': root1=" root1 " (|root1|=" mod1 "); root2=" root2 " (|root2|=" mod2 ")"
        di "  Inside unit disk? root1: " cond(mod1<1,"Yes","No") " root2: " cond(mod2<1,"Yes","No")
    }
    else {
        * Complex conjugate pair: real ± i*imaginary
        scalar re_root = sum_p / 2
        scalar im_root = sqrt(-delta) / 2
        scalar modulus = sqrt(re_root^2 + im_root^2)
        di "`varname': complex conjugate roots: " re_root " ± " im_root "i (|modulus|=" modulus ")"
        di "  Inside unit disk? " cond(modulus<1,"Yes","No") " (complex conjugate pair)"
    }
}

* AR(2) Stability Triangle conditions
di _newline "=== AR(2) Stability Triangle Conditions ==="
foreach sfx in pi i {
    if "`sfx'" == "pi" {
        scalar p1 = phi1_pi
        scalar p2 = phi2_pi
        local varname "Inflation"
    }
    else {
        scalar p1 = phi1_i
        scalar p2 = phi2_i
        local varname "Call Money"
    }
    di _newline "`varname': phi1=" p1 " phi2=" p2
    di "  (1) phi1*phi2 < 1:                   " cond(p1*p2 < 1, "✓", "✗")
    di "  (2) phi1*phi2 > (phi1+phi2) - 1:     " cond(p1*p2 > (p1+p2)-1, "✓", "✗")
    di "  (3) phi1*phi2 > -(phi1+phi2) - 1:    " cond(p1*p2 > -(p1+p2)-1, "✓", "✗")
}


/*******************************************************************************
* SECTION 7 – AR(p) LIMIT TO STABILITY (PACF)
* Sections 23–25: PACF-based lag order selection for inflation and call money.
*
* Python: pacf(series, nlags=10); significant if |PACF| > 1.96/sqrt(T)
* Stata:  pac var, lags(10)
*         Significance bands drawn automatically at ±1.96/sqrt(T)
*******************************************************************************/

di _newline "=== AR(p) Lag Selection via PACF ==="

* ── Inflation
pac ld_hicp_sa, lags(10)                                                      ///
    title("PACF – LogDiff-HICP (SA) – AR(p) Lag Selection")                  ///
    graphregion(color(white))
graph export "Output/Module3/fig_pacf_lag_pi.png", replace

* Manual significance table (mimics Python DataFrame output)
quietly ac ld_hicp_sa, lags(10) generate(acf_pi)
quietly count if !missing(ld_hicp_sa)
scalar T_pi = r(N)
scalar ci95_pi = 1.96 / sqrt(T_pi)
di "95% confidence bound (±1.96/√T): " ci95_pi
drop acf_pi

* ── Call Money (raw)
pac call_money, lags(10)                                                      ///
    title("PACF – Call Money/Interbank (%) – AR(p) Lag Selection")           ///
    graphregion(color(white))
graph export "Output/Module3/fig_pacf_lag_i.png", replace

quietly count if !missing(call_money)
scalar T_i = r(N)
scalar ci95_i = 1.96 / sqrt(T_i)
di "95% confidence bound for call money: " ci95_i


/*******************************************************************************
* SECTION 8 – 10-YEAR ROLLING WINDOW AR(1)
* Sections 26–29: Rolling AR(1) coefficient (window=120) for inflation and call money.
*
* Python:  manual loop over windows; sm.OLS(pi_t, pi_tm1).fit()
* Stata:   rolling command — saves coefficient at each window end-point
*
* (!!!) rolling _b[L.var], window(120): AR(1) coefficient from OLS on L.var
* (!!!) Results stored in a separate .dta; merged back for plotting
*******************************************************************************/

* ── 8.1 Rolling AR(1) – Inflation ──────────────────────────────────────────
* (!!!) window(120) = 120 months = 10 years; saving(filename) stores results
rolling rollAR1_pi=_b[L.ld_hicp_sa],                                         ///
    window(120) saving("Output/Module3/roll_ar1_pi.dta", replace):            ///
    regress ld_hicp_sa L.ld_hicp_sa

* Load and label
use "Output/Module3/roll_ar1_pi.dta", clear
rename end Time
tsset Time
label variable rollAR1_pi "10-Year Rolling Window AR(1) – Inflation"

* Save for merging
save "Output/Module3/roll_ar1_pi_clean.dta", replace

* ── 8.2 Rolling AR(1) – Call Money (raw) ────────────────────────────────────
use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

rolling rollAR1_i=_b[L.call_money],                                          ///
    window(120) saving("Output/Module3/roll_ar1_i.dta", replace):             ///
    regress call_money L.call_money

use "Output/Module3/roll_ar1_i.dta", clear
rename end Time
tsset Time
label variable rollAR1_i "10-Year Rolling Window AR(1) – Call Money"
save "Output/Module3/roll_ar1_i_clean.dta", replace

* ── 8.3 Merge rolling AR(1) results ────────────────────────────────────────
use "Output/Module3/roll_ar1_pi_clean.dta", clear
merge 1:1 Time using "Output/Module3/roll_ar1_i_clean.dta", nogen keep(1 3)
tsset Time

* ── 8.4 Plot: 10-Year Rolling AR(1) – Inflation ─────────────────────────────
* (!!!) Python: fill_between for stationary (AR(1)<1) and unit-root (AR(1)>=1) regions
*       Stata: separate line for global mean + reference line at 1
quietly summarize rollAR1_pi
scalar mean_ar1_pi = r(mean)
di "Global average AR(1) – Inflation: " mean_ar1_pi

twoway                                                                        ///
    (tsline rollAR1_pi, lcolor(gs4) lwidth(thin))                             ///
    (function y = 1, range(rollAR1_pi) lcolor(red) lpattern(shortdash))       ///
    (function y = `=mean_ar1_pi', range(rollAR1_pi)                           ///
        lcolor("31 158 137") lpattern(dash) lwidth(medthin)),                  ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("10-Year Rolling Window AR(1) – Inflation (LogDiff-HICP (SA))")     ///
    ytitle("10-Year Rolling Window AR(1)") xtitle("Year")                     ///
    yline(1, lcolor(red) lpattern(shortdash))                                  ///
    legend(order(1 "Rolling AR(1)" 3 "Global avg AR(1)")                      ///
        rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_roll_ar1_pi.png", replace

* ── 8.5 Plot: 10-Year Rolling AR(1) – Call Money ───────────────────────────
quietly summarize rollAR1_i
scalar mean_ar1_i = r(mean)
di "Global average AR(1) – Call Money: " mean_ar1_i

twoway                                                                        ///
    (tsline rollAR1_i, lcolor(gs4) lwidth(thin))                              ///
    (function y = `=mean_ar1_i', range(rollAR1_i)                             ///
        lcolor("68 1 84") lpattern(dash) lwidth(medthin)),                     ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("10-Year Rolling Window AR(1) – Call Money/Interbank (%)")          ///
    ytitle("10-Year Rolling Window AR(1)") xtitle("Year")                     ///
    yline(1, lcolor(red) lpattern(shortdash))                                  ///
    legend(order(1 "Rolling AR(1)" 2 "Global avg AR(1)")                      ///
        rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_roll_ar1_i.png", replace


/*******************************************************************************
* SECTION 9 – 3-YEAR ROLLING WINDOW DETERMINISTIC TREND (INFLATION)
* Section 29 (estimation) + 30 (plot) in notebook.
*
* Python: OLS(pi_window, add_constant(t)).fit() → save params[1] (g = time trend)
* Stata:  rolling using _b[t_trend] after regressing on a time variable
*
* (!!!) Python window = 36 months = 3 years
* (!!!) g > 0 → trend inflation; g < 0 → trend deflation
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Generate a numeric time trend counter (1, 2, 3, …)
gen t_trend = _n
label variable t_trend "Numeric time trend (1,2,3,...)"

* Rolling 3-year deterministic trend coefficient (g)
rolling roll_g_pi=_b[t_trend],                                                ///
    window(36) saving("Output/Module3/roll_trend_pi.dta", replace):           ///
    regress ld_hicp_sa t_trend

use "Output/Module3/roll_trend_pi.dta", clear
rename end Time
tsset Time
label variable roll_g_pi "3-Year Rolling Deterministic Trend g – Inflation"
save "Output/Module3/roll_trend_pi_clean.dta", replace

* Plot
twoway                                                                        ///
    (tsline roll_g_pi, lcolor(gs4) lwidth(thin)),                             ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("3-Year Rolling Window Deterministic Trend – Inflation")            ///
    ytitle("Rolling g (time trend)") xtitle("Year")                           ///
    graphregion(color(white))
graph export "Output/Module3/fig_roll_trend_pi.png", replace


/*******************************************************************************
* SECTION 10 – AR(1) & g(TREND) CORRELATION (SUBPERIOD TRENDS)
* Sections 31–33: OLS trend on subperiods (YCC adoption, post-financial crisis).
*
* Python: pi[start:end] → OLS on time index → save g, plot fitted trend line
* Stata:  regress ld_hicp_sa t_trend if inrange(Time, start, end)
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time
gen t_trend = _n

* ── YCC Adoption subperiod (Sep 2016 – Jan 2020, excl. COVID) ───────────────
di _newline "=== Deterministic Trend – YCC Adoption Period (2016m9 – 2020m1) ==="
regress ld_hicp_sa t_trend                                                    ///
    if Time >= ym(2016,9) & Time <= ym(2020,1) & !missing(ld_hicp_sa)
scalar g_ycc    = _b[t_trend]
scalar a_ycc    = _b[_cons]
di "YCC Adoption trend g = " g_ycc

* ── Post-Financial Crisis subperiod (Jan 2008 – Jan 2013) ──────────────────
di _newline "=== Deterministic Trend – Post-Financial Crisis (2008m1 – 2013m1) ==="
regress ld_hicp_sa t_trend                                                    ///
    if Time >= ym(2008,1) & Time <= ym(2013,1) & !missing(ld_hicp_sa)
scalar g_crisis = _b[t_trend]
scalar a_crisis = _b[_cons]
di "Post-Crisis trend g = " g_crisis

* Generate fitted trend lines for plotting
gen fitted_ycc    = a_ycc    + g_ycc    * t_trend                             ///
    if Time >= ym(2016,9) & Time <= ym(2020,1)
gen fitted_crisis = a_crisis + g_crisis * t_trend                             ///
    if Time >= ym(2008,1) & Time <= ym(2013,1)
label variable fitted_ycc    "OLS trend – YCC Adoption (2016–2020)"
label variable fitted_crisis "OLS trend – Post-Financial Crisis (2008–2013)"

* Plot: inflation + subperiod trend overlays
twoway                                                                        ///
    (tsline ld_hicp_sa, lcolor(gs6) lwidth(vthin) lpattern(solid))            ///
    (tsline fitted_ycc,    lcolor("31 158 137") lwidth(medthick))              ///
    (tsline fitted_crisis, lcolor("68 1 84")    lwidth(medthick)),             ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    title("Deterministic Trend on Unit Root Subperiods – Inflation")          ///
    subtitle("ln(π{sub:t+1}) = a + gt + ε")                                   ///
    ytitle("π{sub:t} (LogDiff-HICP (SA))") xtitle("Year")                    ///
    legend(order(1 "Inflation" 2 "YCC (2016-2020)" 3 "Post-Crisis (2008-2013)") ///
        rows(1)) graphregion(color(white))
graph export "Output/Module3/fig_subperiod_trend.png", replace

drop fitted_ycc fitted_crisis


/*******************************************************************************
* SECTION 11 – POST-YCC UNIT ROOT TESTING
* Sections 34–35: ADF + PP on jp_core_hybrid_df variables, restricted to post-2016.
*
* Python: df[df["Time"] >= "2016-01"] → adfuller + PhillipsPerron
* Stata:  dfuller / pperron with if condition: Time >= ym(2016,1)
*******************************************************************************/

di _newline "=== POST-YCC ADF + PP Unit Root Tests (from 2016m1) ==="

foreach var in ld_hicp_sa call_money gb10y {
    di _newline "ADF test (post-YCC): `var'"
    dfuller `var' if Time >= ym(2016,1) & !missing(`var'), lags(12) regress
    di "PP  test (post-YCC): `var'"
    pperron `var' if Time >= ym(2016,1) & !missing(`var'), lags(12)
}


/*******************************************************************************
* SECTION 12 – LAGGED CROSS-CORRELATION
* Sections 36–37: Rolling 10-year cross-correlations corr(i_t, π_{t+k}) for k=−2..2
*
* Python: i.rolling(120).corr(pi.shift(k))
* Stata:  rolling command using corr; or xcorr for the full sample;
*         manual approach: gen shifted lags, then rolling regression.
*
* (!!!) Stata's xcorr gives the full-sample cross-correlation at each lag.
*       For rolling window cross-correlations, we manually gen lagged vars
*       and run rolling corr(pi, L#.call_money).
*******************************************************************************/

* Full-sample cross-correlogram (replaces static lag-correlation table)
di _newline "=== Cross-Correlation: call_money & ld_hicp_sa (lags ±5) ==="
xcorr call_money ld_hicp_sa, lags(5) table
* (!!!) xcorr: corr(call_money_t, ld_hicp_sa_{t-k}) for k=−5…5

* ── Rolling 10-year cross-correlations for k = −2, −1, 0, 1, 2 ─────────────
* Generate all needed shifted series
foreach k in m2 m1 0 p1 p2 {
    if "`k'" == "m2" { gen pi_shift_`k' = F2.ld_hicp_sa }  // π_{t+2}
    if "`k'" == "m1" { gen pi_shift_`k' = F.ld_hicp_sa  }  // π_{t+1}
    if "`k'" == "0"  { gen pi_shift_`k' = ld_hicp_sa    }  // π_t
    if "`k'" == "p1" { gen pi_shift_`k' = L.ld_hicp_sa  }  // π_{t-1}
    if "`k'" == "p2" { gen pi_shift_`k' = L2.ld_hicp_sa }  // π_{t-2}
}

* Rolling correlations for each lag
foreach k in m2 m1 0 p1 p2 {
    rolling rollcorr_`k'=r(rho), window(120)                                  ///
        saving("Output/Module3/rollcorr_`k'.dta", replace):                   ///
        corr call_money pi_shift_`k'
}
drop pi_shift_*

* Merge and plot
use "Output/Module3/rollcorr_m2.dta", clear
rename end Time
foreach k in m1 0 p1 p2 {
    merge 1:1 end using "Output/Module3/rollcorr_`k'.dta", nogen keep(1 3)
    rename end Time
}
* (!!!) Correct: just merge after the first use
use "Output/Module3/rollcorr_m2.dta", clear
rename (end rollcorr_m2) (Time rollcorr_k_m2)
foreach k in m1 0 p1 p2 {
    preserve
    use "Output/Module3/rollcorr_`k'.dta", clear
    rename (end rollcorr_`k') (Time rollcorr_k_`k')
    save "Output/Module3/rollcorr_`k'_clean.dta", replace
    restore
    merge 1:1 Time using "Output/Module3/rollcorr_`k'_clean.dta", nogen keep(1 3)
}
tsset Time

twoway                                                                        ///
    (tsline rollcorr_k_m2, lcolor("68 1 84")   lwidth(medthin))              ///
    (tsline rollcorr_k_m1, lcolor("59 82 139")  lwidth(medthin))              ///
    (tsline rollcorr_k_0,  lcolor("33 144 140") lwidth(medthin))              ///
    (tsline rollcorr_k_p1, lcolor("93 200 99")  lwidth(medthin))              ///
    (tsline rollcorr_k_p2, lcolor("253 231 37") lwidth(medthin)),             ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    legend(order(1 "k=-2" 2 "k=-1" 3 "k=0" 4 "k=+1" 5 "k=+2") rows(1))     ///
    title("10-Year Rolling Cross-Correlations: Policy Instrument & Lagged Inflation") ///
    ytitle("Corr(i{sub:t}, π{sub:t+k})") xtitle("Year")                      ///
    yscale(range(-1 1)) graphregion(color(white))
graph export "Output/Module3/fig_rollcorr_xcorr.png", replace


/*******************************************************************************
* SECTION 13 – FIRST-ORDER SISO MODEL (ROLLING 10-YEAR WINDOW)
* Section 38: π_t = A π_{t-1} + B i_{t-1} + u_t  AND  i_{t-1} = F π_{t-1} + e_{t-1}
*
* Python: manual loop over 120-month windows; save A, B, F, u_rho, e_gamma
* Stata:  two rolling regressions; merge; compute λ=A+BF; |A+BF|>1 flag
*
* (!!!) Two separate rolling() calls are needed:
*       (1) regress call_money L.ld_hicp_sa  → F
*       (2) regress ld_hicp_sa L.ld_hicp_sa L.call_money → A, B
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* ── SISO Equation 1: i_{t-1} = F π_{t-1} + e_{t-1} → extract F ─────────────
rolling siso_F=_b[L.ld_hicp_sa],                                             ///
    window(120) saving("Output/Module3/siso_F.dta", replace):                 ///
    regress call_money L.ld_hicp_sa

* ── SISO Equation 2: π_t = A π_{t-1} + B i_{t-1} + u_t → extract A, B ─────
rolling siso_A=_b[L.ld_hicp_sa] siso_B=_b[L.call_money],                    ///
    window(120) saving("Output/Module3/siso_AB.dta", replace):                ///
    regress ld_hicp_sa L.ld_hicp_sa L.call_money

* ── Merge A, B, F ────────────────────────────────────────────────────────────
use "Output/Module3/siso_F.dta", clear
rename end Time
merge 1:1 Time using "Output/Module3/siso_AB.dta", nogen keep(1 3)
rename end Time
tsset Time

* ── Compute λ = A + BF (closed-loop eigenvalue) ──────────────────────────────
gen siso_lambda = siso_A + siso_B * siso_F
label variable siso_A      "SISO rolling A (π_{t-1} coeff in π_t equation)"
label variable siso_B      "SISO rolling B (i_{t-1} coeff in π_t equation)"
label variable siso_F      "SISO rolling F (π_{t-1} coeff in i_{t-1} equation)"
label variable siso_lambda "SISO λ = A+BF (closed-loop eigenvalue)"

* Instability flag: |A+BF| > 1
gen siso_unstable = abs(siso_lambda) > 1 if !missing(siso_lambda)
label variable siso_unstable "|A+BF|>1 instability flag"

save "Output/Module3/siso_abf_10roll.dta", replace

* ── Plot: SISO coefficients A, B, F, λ overtime ─────────────────────────────
twoway                                                                        ///
    (tsline siso_A,      lcolor("68 1 84")   lwidth(medthin))                 ///
    (tsline siso_B,      lcolor("59 82 139")  lwidth(medthin))                 ///
    (tsline siso_F,      lcolor("33 144 140") lwidth(medthin))                 ///
    (tsline siso_lambda, lcolor("253 231 37") lwidth(medthin)),                ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    yscale(range(-10 10))                                                     ///
    legend(order(1 "A" 2 "B" 3 "F" 4 "λ=A+BF") rows(1))                     ///
    title("10-Year Rolling Window First-order SISO Coefficients (A, B, F, λ)")///
    ytitle("SISO Coefficient") xtitle("Year")                                  ///
    graphregion(color(white))
graph export "Output/Module3/fig_siso_abf.png", replace


/*******************************************************************************
* SECTION 14 – AR(1) vs SISO |A+BF| COMPARISON
* Section 39–40: overlay rolling AR(1) with rolling λ=A+BF
*******************************************************************************/

* Merge rolling AR(1) and SISO lambda
use "Output/Module3/siso_abf_10roll.dta", clear
merge 1:1 Time using "Output/Module3/roll_ar1_pi_clean.dta", nogen keep(1 3)
tsset Time

twoway                                                                        ///
    (tsline siso_lambda, lcolor("33 144 140") lwidth(medthin))                ///
    (tsline rollAR1_pi,  lcolor("68 1 84")    lwidth(medthin)),               ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    yline(1, lcolor(red) lpattern(shortdash))                                 ///
    legend(order(1 "SISO λ=A+BF" 2 "Rolling AR(1)") rows(1))                 ///
    title("10-Year Rolling Window: SISO λ=A+BF vs AR(1)")                    ///
    ytitle("Coefficient") xtitle("Year")                                       ///
    graphregion(color(white))
graph export "Output/Module3/fig_siso_vs_ar1.png", replace


/*******************************************************************************
* SECTION 15 – LUCAS CRITIQUE
* Section 41: Plot showing A, B, F, λ with negative-feedback regions (0<λ<A).
*******************************************************************************/

use "Output/Module3/siso_abf_10roll.dta", clear
tsset Time

* Flag negative feedback region: 0 < A+BF < A
gen neg_feedback = (siso_lambda > 0 & siso_lambda < siso_A) if !missing(siso_lambda)
label variable neg_feedback "Negative Feedback Region (0 < A+BF < A)"

twoway                                                                        ///
    (tsline siso_A,      lcolor("68 1 84")   lwidth(medthin))                 ///
    (tsline siso_B,      lcolor("59 82 139")  lwidth(medthin))                 ///
    (tsline siso_F,      lcolor("33 144 140") lwidth(medthin))                 ///
    (tsline siso_lambda, lcolor("253 231 37") lwidth(medthin)),                ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    yscale(range(-10 10))                                                     ///
    legend(order(1 "A" 2 "B" 3 "F" 4 "λ=A+BF") rows(1))                     ///
    title("Lucas Critique: SISO Coefficients & λ=A+BF (Negative Feedback Regions)")  ///
    ytitle("Coefficient") xtitle("Year")                                       ///
    graphregion(color(white))
graph export "Output/Module3/fig_lucas_critique.png", replace


/*******************************************************************************
* SECTION 16 – SISO ANALYSIS WITH LOG-DIFF POLICY INSTRUMENT
* Sections 42–49: Repeat AR(1) unit root tests + SISO rolling using ld_call_money.
*
* Python: replace ±Inf (from log of zero) with 0 before analysis
* Stata:  replace ld_call_money = 0 if missing(ld_call_money) due to log(0)
*         (!!!) More conservatively: drop infinite values and proceed
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
* Replace any infinite values produced by log-diff of zero
replace ld_call_money = . if !inrange(ld_call_money, -1e10, 1e10)
tsset Time

* ── 16.1 AR(1) autocorrelation + ADF + PP for logdiff variables ─────────────
di _newline "=== AR(1) Tests – LogDiff Policy Instrument ==="
foreach var in ld_hicp_sa ld_call_money {
    quietly corr `var' L.`var' if !missing(`var') & !missing(L.`var')
    di "AR(1) [" "`var'" "] = " r(rho)
    dfuller `var' if !missing(`var'), lags(12) regress
    pperron `var' if !missing(`var'), lags(12)
}

* ── 16.2 Rolling SISO with LogDiff policy instrument ────────────────────────
* SISO Eq 1: ld_call_money_{t-1} = F π_{t-1} + e  → F
rolling siso_ld_F=_b[L.ld_hicp_sa],                                          ///
    window(120) saving("Output/Module3/siso_ld_F.dta", replace):              ///
    regress ld_call_money L.ld_hicp_sa if !missing(ld_call_money) & !missing(ld_hicp_sa)

* SISO Eq 2: π_t = A π_{t-1} + B ld_i_{t-1} + u  → A, B
rolling siso_ld_A=_b[L.ld_hicp_sa] siso_ld_B=_b[L.ld_call_money],          ///
    window(120) saving("Output/Module3/siso_ld_AB.dta", replace):             ///
    regress ld_hicp_sa L.ld_hicp_sa L.ld_call_money                          ///
    if !missing(ld_call_money) & !missing(ld_hicp_sa)

use "Output/Module3/siso_ld_F.dta", clear
rename end Time
merge 1:1 Time using "Output/Module3/siso_ld_AB.dta", nogen keep(1 3)
rename end Time
tsset Time

gen siso_ld_lambda = siso_ld_A + siso_ld_B * siso_ld_F
label variable siso_ld_A      "SISO-LD rolling A"
label variable siso_ld_B      "SISO-LD rolling B"
label variable siso_ld_F      "SISO-LD rolling F"
label variable siso_ld_lambda "SISO-LD λ=A+BF (logdiff instrument)"
save "Output/Module3/siso_ld_abf_10roll.dta", replace

* Plot (time-restricted to 2000+, mimics Cell 47)
twoway                                                                        ///
    (tsline siso_ld_A       if Time >= ym(2000,1), lcolor("68 1 84")   lwidth(medthin))  ///
    (tsline siso_ld_B       if Time >= ym(2000,1), lcolor("59 82 139")  lwidth(medthin)) ///
    (tsline siso_ld_F       if Time >= ym(2000,1), lcolor("33 144 140") lwidth(medthin)) ///
    (tsline siso_ld_lambda  if Time >= ym(2000,1), lcolor("253 231 37") lwidth(medthin)),///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    legend(order(1 "A" 2 "B" 3 "F" 4 "λ=A+BF") rows(1))                     ///
    title("10-Year Rolling SISO Coefficients with LogDiff Policy Instrument (2000+)") ///
    ytitle("Coefficient") xtitle("Year")                                       ///
    graphregion(color(white))
graph export "Output/Module3/fig_siso_logdiff.png", replace

* λ vs AR(1) comparison with logdiff instrument
merge 1:1 Time using "Output/Module3/roll_ar1_pi_clean.dta", nogen keep(1 3)
tsset Time

twoway                                                                        ///
    (tsline siso_ld_lambda if Time >= ym(1999,1), lcolor("33 144 140") lwidth(medthin)) ///
    (tsline rollAR1_pi     if Time >= ym(1999,1), lcolor("68 1 84")    lwidth(medthin)),///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    yline(1, lcolor(red) lpattern(shortdash))                                 ///
    legend(order(1 "SISO λ=A+BF (logdiff i)" 2 "Rolling AR(1)") rows(1))    ///
    title("SISO λ=A+BF vs AR(1) – LogDiff Policy Instrument")                ///
    ytitle("Coefficient") xtitle("Year")                                       ///
    graphregion(color(white))
graph export "Output/Module3/fig_siso_ld_vs_ar1.png", replace


/*******************************************************************************
* SECTION 17 – LONGEST PERIOD SINGLE ESTIMATES: r, σ_π/σ_i, F̂_π
* Sections 50–52: 10-year rolling window statistics.
*
* Python: r = pi.corr(i); ratio = σ_pi/σ_i; F_hat = r * σ_i/σ_pi
* Stata:  rolling corr(pi,i) + sd(pi) + sd(i) → compute ratio and F_hat
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Rolling correlation π_t ~ i_t
rolling roll_r=r(rho), window(120)                                            ///
    saving("Output/Module3/roll_r.dta", replace):                             ///
    corr ld_hicp_sa call_money if !missing(ld_hicp_sa) & !missing(call_money)

* Rolling SD of inflation
rolling roll_sd_pi=r(sd), window(120)                                         ///
    saving("Output/Module3/roll_sd_pi.dta", replace):                         ///
    summarize ld_hicp_sa

* Rolling SD of call money
rolling roll_sd_i=r(sd), window(120)                                          ///
    saving("Output/Module3/roll_sd_i.dta", replace):                          ///
    summarize call_money

* Merge
use "Output/Module3/roll_r.dta", clear
rename end Time
merge 1:1 Time using "Output/Module3/roll_sd_pi.dta", nogen keep(1 3)
rename end Time
merge 1:1 Time using "Output/Module3/roll_sd_i.dta", nogen keep(1 3)
rename end Time
tsset Time

* Compute F_hat and sigma ratio
gen roll_sigma_ratio = roll_sd_pi / roll_sd_i
gen roll_F_hat       = roll_r * (roll_sd_i / roll_sd_pi)
label variable roll_r           "Rolling 10Y: r(π_t, i_t)"
label variable roll_sigma_ratio "Rolling 10Y: σ_π/σ_i"
label variable roll_F_hat       "Rolling 10Y: F̂_π = r × σ_i/σ_π"

save "Output/Module3/roll_stats10.dta", replace

* Plot (3-panel chart)
twoway                                                                        ///
    (tsline roll_r, lcolor("68 1 84") lwidth(medthin)),                       ///
    xline(`=ycc_adopt', lcolor(gs4) lpattern(dash))                           ///
    xline(`=ycc_defacto', lcolor(gs4) lpattern(dash))                         ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    title("10-Year Rolling r(π{sub:t}, i{sub:t})")                            ///
    ytitle("r") xtitle("Year") graphregion(color(white))
graph export "Output/Module3/fig_roll_r.png", replace

twoway                                                                        ///
    (tsline roll_sigma_ratio, lcolor("33 144 140") lwidth(medthin)),          ///
    xline(`=ycc_adopt', lcolor(gs4) lpattern(dash))                           ///
    xline(`=ycc_defacto', lcolor(gs4) lpattern(dash))                         ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    title("10-Year Rolling σ{sub:π}/σ{sub:i}")                                ///
    ytitle("σ{sub:π}/σ{sub:i}") xtitle("Year") graphregion(color(white))
graph export "Output/Module3/fig_roll_sigma_ratio.png", replace

twoway                                                                        ///
    (tsline roll_F_hat, lcolor("253 231 37") lwidth(medthin)),                ///
    xline(`=ycc_adopt', lcolor(gs4) lpattern(dash))                           ///
    xline(`=ycc_defacto', lcolor(gs4) lpattern(dash))                         ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    title("10-Year Rolling F̂{sub:π} = r × σ{sub:i}/σ{sub:π}")               ///
    ytitle("F̂{sub:π}") xtitle("Year") graphregion(color(white))
graph export "Output/Module3/fig_roll_Fhat.png", replace


/*******************************************************************************
* SECTION 18 – INDIRECT LEAST SQUARES (ILS)
* Sections 53–55: Structural system recovered via reduced-form OLS.
*
* Structural system:
*   π_t = B i_t + G y_t + O z_t + ε_t
*   i_t = F π_t + H z_t + η_t
* Reduced form (SR):
*   π_t = β_πy y_t + β_πz z_t + ε_R,t
*   i_t = β_iy y_t + β_iz z_t + η_R,t
* ILS recovery:
*   F̂ = β_iy / β_πy    (feedback rule slope)
*   B̂ = β_πz / β_iz   (policy instrument slope in inflation)
*
* Instruments: y_t = hp_rgdp (HP-filter output gap), z_t = ld_reserves
* Python: ivregress-equivalent via manual two-equation OLS
* Stata:  regress reduced forms; compute F_hat and B_hat as ratios
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Align all series to common non-missing sample
keep if !missing(ld_hicp_sa) & !missing(call_money) & !missing(hp_rgdp) & !missing(ld_reserves)

* ── Reduced Form 1: π_t = β_πy y_t + β_πz z_t ──────────────────────────────
di _newline "=== ILS – Reduced Form 1: π_t on instruments ==="
regress ld_hicp_sa hp_rgdp ld_reserves
scalar b_pi_y = _b[hp_rgdp]
scalar b_pi_z = _b[ld_reserves]
di "β_πy = " b_pi_y "  β_πz = " b_pi_z

* ── Reduced Form 2: i_t = β_iy y_t + β_iz z_t ──────────────────────────────
di _newline "=== ILS – Reduced Form 2: i_t on instruments ==="
regress call_money hp_rgdp ld_reserves
scalar b_i_y = _b[hp_rgdp]
scalar b_i_z = _b[ld_reserves]
di "β_iy = " b_i_y "  β_iz = " b_i_z

* ── ILS Recovery ─────────────────────────────────────────────────────────────
scalar F_hat_ils = b_i_y / b_pi_y
scalar B_hat_ils = b_pi_z / b_i_z
di _newline "=== ILS Structural Parameters ==="
di "F̂ = β_iy/β_πy = " F_hat_ils "  (feedback rule slope)"
di "B̂ = β_πz/β_iz = " B_hat_ils "  (policy instrument slope in inflation eq.)"

* Summary table of structural parameters
matrix ILS = (b_pi_y \ b_pi_z \ b_i_y \ b_i_z \ F_hat_ils \ B_hat_ils)
matrix rownames ILS = "β_πy" "β_πz" "β_iy" "β_iz" "F̂=β_iy/β_πy" "B̂=β_πz/β_iz"
matrix colnames ILS = "Value"
matlist ILS, format(%9.4f)

* ── Instrument Combination Search (top R² combinations) ─────────────────────
* Python: itertools.combinations over all candidate variables, size 1 and 2
* (!!!) Stata does not have native combinatorial search; use stepwise-style loop
* We test all single instruments from the transformed dataset
di _newline "=== ILS Instrument Search – Single Instruments ==="

* Re-load full transformed data for search
use "Data/Transformed/jp_trans_df.dta", clear
tsset Time
replace ld_call_money = . if !inrange(ld_call_money, -1e10, 1e10)

* Candidate instruments: all numeric vars excluding target, instrument, IDs
* (!!!) Python excludes LogDiff-HICP, call_money, Country, index
local cand_vars hp_rgdp ld_reserves ld_reer ld_spot ld_boj_assets           ///
                ld_m1_jpy ld_m2_jpy ld_m3_jpy                               ///
                ar1_gb10y ar1_nir_1y ar1_nir_10y                             ///
                ar1_credit_pnf ar1_credit_gg ar1_credit_hh                   ///
                ar1_cg_debt ar1_dp_debt ar1_du_debt                          ///
                ar1_loan_rate ar1_dep_rate ar1_ust_10y ar1_vix

local results_pi = ""
local results_i  = ""

foreach var of local cand_vars {
    quietly reg ld_hicp_sa `var' if !missing(ld_hicp_sa) & !missing(`var')
    local r2_pi = round(e(r2), 0.0001)
    quietly reg call_money `var' if !missing(call_money) & !missing(`var')
    local r2_i  = round(e(r2), 0.0001)
    local r2_g  = `r2_pi' + `r2_i'
    di "Instrument: `var'  |  R²_π=" `r2_pi' "  R²_i=" `r2_i' "  R²_global=" `r2_g'
}


/*******************************************************************************
* SECTION 19 – ROLLING REGRESSION WITH 6-QUARTER LAGS
* Section 55–56: π_{t+3} = A π_{t-3} + B i_{t-18} + u  AND  i_t = F π_t + u
*
* Python: lead_pi=-3, lag_pi=3, lag_i=18 (months)
*         π_{t+3} (3m lead), π_{t-3} (3m lag), i_{t-18} (18m lag = 6 quarters)
* Stata:  generate shifted vars using F3. (lead) and L3./L18. (lags)
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Generate lagged/lead variables
gen pi_lead3  = F3.ld_hicp_sa    // π_{t+3}
gen pi_lag3   = L3.ld_hicp_sa    // π_{t-3}
gen i_lag18   = L18.call_money   // i_{t-18} (6 quarters)
label variable pi_lead3  "π_{t+3}: ld_hicp_sa led 3 months"
label variable pi_lag3   "π_{t-3}: ld_hicp_sa lagged 3 months"
label variable i_lag18   "i_{t-18}: call_money lagged 18 months (6 quarters)"

* ── SISO Eq 1: π_{t+3} = A π_{t-3} + B i_{t-18} + u ───────────────────────
rolling siso6q_A=_b[pi_lag3] siso6q_B=_b[i_lag18],                          ///
    window(120) saving("Output/Module3/siso_6q_AB.dta", replace):             ///
    regress pi_lead3 pi_lag3 i_lag18

* ── SISO Eq 2: i_t = F π_t + u ─────────────────────────────────────────────
rolling siso6q_F=_b[ld_hicp_sa],                                              ///
    window(120) saving("Output/Module3/siso_6q_F.dta", replace):              ///
    regress call_money ld_hicp_sa

* Merge
use "Output/Module3/siso_6q_AB.dta", clear
rename end Time
merge 1:1 Time using "Output/Module3/siso_6q_F.dta", nogen keep(1 3)
rename end Time
tsset Time

gen siso6q_lambda = siso6q_A + siso6q_B * siso6q_F
label variable siso6q_A      "6Q-lag SISO rolling A (π_{t-3} coeff)"
label variable siso6q_B      "6Q-lag SISO rolling B (i_{t-18} coeff)"
label variable siso6q_F      "6Q-lag SISO rolling F (π_t coeff in i_t eq.)"
label variable siso6q_lambda "6Q-lag SISO λ = A+BF"

* Flag negative feedback: B < 0
gen siso6q_negfeedback = (siso6q_B < 0) if !missing(siso6q_B)
label variable siso6q_negfeedback "Negative Feedback (B<0)"

save "Output/Module3/siso_6q_results.dta", replace

* Plot
twoway                                                                        ///
    (tsline siso6q_A,      lcolor("68 1 84")   lwidth(medthin))               ///
    (tsline siso6q_B,      lcolor("59 82 139")  lwidth(medthin))               ///
    (tsline siso6q_F,      lcolor("33 144 140") lwidth(medthin))               ///
    (tsline siso6q_lambda, lcolor("253 231 37") lwidth(medthin)),              ///
    xline(`=ycc_adopt'   , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_defacto' , lcolor(gs4) lpattern(dash))                        ///
    xline(`=ycc_explicit', lcolor(gs4) lpattern(dash))                        ///
    yline(0, lcolor(gs6) lpattern(dash))                                      ///
    legend(order(1 "A" 2 "B" 3 "F" 4 "λ=A+BF") rows(1))                     ///
    title("6-Quarter Lag SISO Rolling Window Coefficients (A, B, F, λ)")      ///
    ytitle("Coefficient") xtitle("Year")                                       ///
    graphregion(color(white))
graph export "Output/Module3/fig_siso_6q.png", replace


di _newline "Module 3 complete. All outputs saved to Output/Module3/"
