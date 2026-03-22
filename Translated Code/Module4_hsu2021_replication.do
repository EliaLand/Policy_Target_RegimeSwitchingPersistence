
/*******************************************************************************
* Module 4 - Hsu et al. (2021) Replication: Local FAVAR
* Author    : Elia Landini
* Student ID: 12310239
* Course    : EESM2-Financial Economics
* Supervisor: Jean-Bernard Chatelain
* Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
*
* Translation of Python/Jupyter notebook (Module4_hsu2021_replication.ipynb)
* to Stata do-file.
*
* PREREQUISITES:
*   Module 2 do-file must have been run first (jp_trans_df.dta available).
*
* MODEL OVERVIEW (Hsu et al., 2021 — Local FAVAR):
*   1. PCA (5 factors) compresses 23 transformed variables into F1–F5.
*   2. Y = [ld_hicp_sa, ld_call_money] + [F1,…,F5] forms the FAVAR state vector.
*   3. Smoothing variable Z = L6.ld_call_money; z_grid = 20th/40th/60th/80th pct.
*   4. For each z0 in z_grid: Gaussian kernel-weighted VAR(2) estimated via WLS
*      (Local FAVAR). Companion matrix eigenvalues verify stability.
*   5. Cholesky-identified IRFs: 12-horizon response of inflation to policy shock.
*
* VARIABLE KEY:
*   ld_hicp_sa     = LogDiff-HICP (SA)                 [policy target]
*   ld_call_money  = LogDiff-Call Money/Interbank (%)  [policy instrument]
*   F1–F5          = PCA factors from transformed dataset
*   Z_smooth       = L6.ld_call_money                  [regime smoothing var]
*******************************************************************************/

clear all
set more off
capture mkdir "Output/Module4"


/*******************************************************************************
* SECTION 1 – DATA LOADING & CLEANING
* Python: jp_trans_df → replace ±inf with NaN → filter columns → dropna
*******************************************************************************/

use "Data/Transformed/jp_trans_df.dta", clear
tsset Time

* Replace any ±inf values produced by log-diff of zero (e.g. call money)
* (!!!) Python: df.replace([np.inf, -np.inf], np.nan)
foreach var of varlist _all {
    capture {
        replace `var' = . if `var' == .a  // extended missing (SAS-style, harmless)
        replace `var' = . if !inrange(`var', -1e10, 1e10)
    }
}

* Keep only the variables used in the FAVAR
* (!!!) Python excludes "Country" and "AR(1)detrend-Call Money/Interbank Immediate (%)"
* Stata: keep all transformed vars except ar1_call_money (the AR(1) detrended call money)
drop ar1_call_money Country


/*******************************************************************************
* SECTION 2 – PCA FACTOR EXTRACTION (5 COMPONENTS)
* Python: sklearn.decomposition.PCA(n_components=5).fit_transform(favar_df.values)
*
* Stata: pca varlist, components(5) covariance
*        → predict F1 F2 F3 F4 F5
*
* (!!!) Python's PCA uses covariance matrix (centers but does NOT scale).
*       Stata default is correlation matrix (standardised). We use the
*       `covariance` option to match Python exactly.
*
* (!!!) Candidate factor variables: ALL transformed vars except policy target
*       and policy instrument (they go into Y directly), and non-numeric IDs.
*       Python favar_vars = all cols except "Country" and "AR(1)detrend-Call Money"
*******************************************************************************/

* Build list of variables to compress: all transformed vars except the two Y vars
* (ld_hicp_sa and ld_call_money are kept out of PCA input – they form Y directly)
* (!!!) Stata PCA requires complete cases; use only obs with no missing values
* Drop obs with any missing across all candidate variables
local pca_vars ld_m1_jpy ld_m2_jpy ld_m3_jpy ld_reserves ld_reer ld_spot    ///
               ld_price_1615T ld_boj_assets                                    ///
               ar1_credit_pnf ar1_credit_gg ar1_credit_hh                     ///
               ar1_gb10y ar1_nir_1y ar1_nir_10y                               ///
               ar1_cg_debt ar1_dp_debt ar1_du_debt                            ///
               ar1_loan_rate ar1_dep_rate ar1_ust_10y ar1_vix                  ///
               hp_rgdp

* Drop rows with any missing in PCA variables (mirrors Python .dropna())
* (!!!) Python: favar_df = df[favar_vars].dropna()
marksample touse
foreach v of local pca_vars {
    replace touse = 0 if missing(`v')
}
replace touse = 0 if missing(ld_hicp_sa) | missing(ld_call_money)

* PCA on covariance matrix (5 components)
pca `pca_vars' if touse, components(5) covariance
di "Cumulative explained variance (5 components):"
estat proportion, cum

* Extract factor scores
predict F1 F2 F3 F4 F5 if touse
label variable F1 "PCA Factor 1"
label variable F2 "PCA Factor 2"
label variable F3 "PCA Factor 3"
label variable F4 "PCA Factor 4"
label variable F5 "PCA Factor 5"


/*******************************************************************************
* SECTION 3 – Y TARGET VARIABLES & FAVAR CORE DATASET
* Python: Y = [ld_hicp_sa, ld_call_money]; favar_core_df = [Y, F1..F5].dropna()
*
* (!!!) Python: Y.fillna(method="ffill") before concat
*       Stata equivalent: ipolate or carryforward (here carryforward = ffill)
*******************************************************************************/

* Forward-fill missing values in Y variables (Python: .fillna(method="ffill"))
* (!!!) Only fills isolated missing values; does NOT extrapolate beyond data range
tssmooth ma _tmp_pi = ld_hicp_sa, window(1 0 0)  // carry-forward approx
drop _tmp_pi
* (!!!) More precisely: use carryforward (SSC: carryforward package, or gen manually)
* Stata base: replace with lag if missing
foreach var in ld_hicp_sa ld_call_money {
    replace `var' = L.`var' if missing(`var') & !missing(L.`var')
}

* Keep only complete cases across all 7 FAVAR variables
keep if touse & !missing(F1) & !missing(F2) & !missing(F3) & !missing(F4) & !missing(F5)

* Save FAVAR core dataset
keep Time ld_hicp_sa ld_call_money F1 F2 F3 F4 F5
save "Output/Module4/favar_core_df.dta", replace

di _newline "=== FAVAR Core Dataset Summary ==="
describe
summarize


/*******************************************************************************
* SECTION 4 – LAG ORDER SELECTION (AIC)
* Python: VAR(Y).select_order(maxlags=10)
*
* (!!!) Python fits VAR only on Y = [ld_hicp_sa, ld_call_money] for lag selection
* Stata: varsoc ld_hicp_sa ld_call_money, maxlag(10)
*
* For the full FAVAR (7 variables), degrees of freedom are too low for AIC;
* we follow the notebook: select lag on Y variables only, apply to full system.
*******************************************************************************/

di _newline "=== VAR Lag Order Selection on Y Variables (AIC) ==="
varsoc ld_hicp_sa ld_call_money, maxlag(10)
* (!!!) Lag = 2 is selected (as hardcoded in Python Cell 9: lag = 2)


/*******************************************************************************
* SECTION 5 – SMOOTHING VARIABLE Z & REGIME GRID
* Python: Z = L6.ld_call_money; z_grid = np.percentile(Z, [20,40,60,80])
*
* Stata: gen Z_smooth = L6.ld_call_money
*        _pctile Z_smooth, percentiles(20 40 60 80)
*******************************************************************************/

* Smoothing variable: 6-lag of log-differenced call money
gen Z_smooth = L6.ld_call_money
label variable Z_smooth "Smoothing Variable Z: L6.ld_call_money"

* Compute percentile grid (z_grid)
* (!!!) Python: z_grid = np.percentile(Z, [20,40,60,80])
_pctile Z_smooth if !missing(Z_smooth), percentiles(20 40 60 80)
scalar z_p20 = r(r1)
scalar z_p40 = r(r2)
scalar z_p60 = r(r3)
scalar z_p80 = r(r4)

di _newline "=== Smoothing Variable Z — Percentile Grid ==="
di "z_grid[1] (p20) = " z_p20
di "z_grid[2] (p40) = " z_p40
di "z_grid[3] (p60) = " z_p60
di "z_grid[4] (p80) = " z_p80

save "Output/Module4/favar_core_df.dta", replace


/*******************************************************************************
* SECTION 6 – LOCAL FAVAR ESTIMATION (GAUSSIAN KERNEL-WEIGHTED VAR)
* Python: for each z0 in z_grid → Gaussian kernel weights → weighted OLS VAR(2)
*
* MODEL:
*   P_t = Φ_0 + Φ_1 P_{t-1} + Φ_2 P_{t-2} + ε_t
*   where P_t = [ld_hicp_sa, ld_call_money, F1, F2, F3, F4, F5]
*   Kernel: w_t(z0) = φ((Z_t - z0)/h) / h,  h = 0.1 * std(Z)
*
* Python: manual matrix loop → la.pinv(X_w.T @ X_w) @ (X_w.T @ y_w)
* Stata:  Mata implementation of weighted OLS VAR
*
* (!!!) Key structural detail from Python code:
*   X has (lag+1)*K columns where:
*     cols 0:K     = ones (K constant terms, one per equation)
*     cols K:2K    = P_{t-1}
*     cols 2K:3K   = P_{t-2}
*   y has K columns = P_t
*   This is a simultaneous system estimated equation-by-equation (via matrix inversion)
*
* Output: Phi matrices for each regime (stored in Mata)
*******************************************************************************/

* ── Mata Implementation ──────────────────────────────────────────────────────

mata:

/* ─── Helper: build VAR lag matrix X and response Y ─────────────────────── */
real matrix function build_var_data(real matrix Ydata, real scalar lag) {
    real scalar n, K, T
    real matrix X, y, y_list
    n  = rows(Ydata)
    K  = cols(Ydata)
    T  = n - lag
    /* y: response (rows lag+1 .. n → index lag+1:n in 1-based = rows lag+1..n) */
    y  = Ydata[(lag+1)::n, .]
    /* X: constant (ones) + P_{t-1} + P_{t-2} */
    X  = J(T, (lag+1)*K, 0)
    X[., 1..K] = J(T, K, 1)          // constant block (K ones per row)
    for (q = 1; q <= lag; q++) {
        X[., (q*K+1)::((q+1)*K)] = Ydata[(lag+1-q)::(n-q), .]
    }
    return((X, y))                    // return stacked [X | y]
}

/* ─── Main: Local FAVAR loop over z_grid ──────────────────────────────────── */
void function local_favar(string scalar varnames, real scalar lag,
                           real vector z_grid, string scalar savepfx) {

    real matrix Ydata, Z, Xfull, X, y
    real scalar n, K, i, z0, h, T
    real vector kern, K_w, w
    real matrix X_w, y_w, beta, Phi0, Phi1, Phi2

    /* Load FAVAR core variables from Stata memory */
    Ydata = st_data(., varnames)
    Z     = st_data(., "Z_smooth")

    /* Drop rows where Z is missing (first 6 obs due to L6 shift) */
    real vector notmiss
    notmiss = rowmissing(Ydata) :== 0 :& (Z :!= .)
    Ydata   = select(Ydata, notmiss)
    Z       = select(Z, notmiss)

    n = rows(Ydata)
    K = cols(Ydata)

    /* Build common X and y (no weights yet) */
    real matrix combined
    combined = build_var_data(Ydata, lag)
    T = n - lag
    X = combined[., 1..(lag+1)*K]
    y = combined[., (lag+1)*K+1..cols(combined)]

    /* Bandwidth: h = 0.1 * std(Z) — Python: 0.1 * np.std(Z) = population std */
    /* Stata Mata variance() uses N-1; adjust to match Python's np.std (N) */
    h = 0.1 * sqrt(variance(Z) * (rows(Z)-1) / rows(Z))

    /* Loop over regimes */
    for (i = 1; i <= length(z_grid); i++) {

        z0 = z_grid[i]

        /* Gaussian kernel: φ((Z_t - z0)/h) / h  (normalden = standard normal pdf) */
        kern = normalden((Z :- z0) :/ h) :/ h

        /* (!!!) Python uses kern from t=2 onward (after 1-period lag used to build X)
           So kernel aligns with rows (lag+1)..n of Ydata, i.e., indices lag+2..n+1 in 1-based
           Python: K_w = kern.iloc[1:] / kern.iloc[1:].sum()
           Here: Z is already aligned after removing missing; rows lag+1..n correspond
           to the response rows. Use kern rows (lag+1)..n.                       */
        K_w = kern[(lag+1)::n] :/ sum(kern[(lag+1)::n])

        /* Weight matrix: diag(sqrt(K_w)) */
        w   = sqrt(K_w)

        /* Weighted X and y: element-wise multiply each row by w_t */
        X_w = X :* w
        y_w = y :* w

        /* Weighted OLS: β = (X_w'X_w)^{-1} X_w' y_w (using pseudo-inverse) */
        /* (!!!) Python uses la.pinv which handles near-singular matrices gracefully */
        beta = invsym(X_w' * X_w) * (X_w' * y_w)

        /* Reshape: beta has shape ((lag+1)*K, K)
           Phi_z0[0] = beta[1..K, .]     (constant, K×K)
           Phi_z0[1] = beta[K+1..2K, .]  (Phi1, K×K)
           Phi_z0[2] = beta[2K+1..3K, .] (Phi2, K×K) */
        Phi0 = beta[1..K, .]
        Phi1 = beta[K+1..2*K, .]
        Phi2 = beta[2*K+1..3*K, .]

        /* Store Phi matrices in Stata as named matrices */
        st_matrix(savepfx + "_const_" + strofreal(i), Phi0)
        st_matrix(savepfx + "_Phi1_"  + strofreal(i), Phi1)
        st_matrix(savepfx + "_Phi2_"  + strofreal(i), Phi2)

        /* Eigenvalue stability check */
        real vector eigenvalues
        real matrix V
        eigenvalues = C_eigenvalues(Phi1)
        printf("z0=%6.4f: Phi1 dominant |λ| = %6.4f\n",
               z0, max(abs(eigenvalues)))
        printf("  All eigenvalues inside unit circle: %s\n",
               (max(abs(eigenvalues)) < 1 ? "Yes" : "No"))
    }
}

end  // end mata block


* ── Run Local FAVAR ──────────────────────────────────────────────────────────
di _newline "=== Running Local FAVAR (Gaussian Kernel-Weighted VAR(2)) ==="

* Build z_grid vector as a Mata vector
mata:
z_grid = (st_numscalar("z_p20"), st_numscalar("z_p40"),
          st_numscalar("z_p60"), st_numscalar("z_p80"))
end

* Variable list passed to Mata (7-variable FAVAR state vector)
* (!!!) Order: Y1=ld_hicp_sa, Y2=ld_call_money, F1…F5
local favar_varnames "ld_hicp_sa ld_call_money F1 F2 F3 F4 F5"

mata: local_favar("`favar_varnames'", 2, z_grid, "favar")

* Results are now stored as Stata matrices:
*   favar_const_1 … favar_const_4  (constant matrices, K×K)
*   favar_Phi1_1  … favar_Phi1_4   (Phi1 matrices, K×K)
*   favar_Phi2_1  … favar_Phi2_4   (Phi2 matrices, K×K)

di _newline "=== Phi1 matrix – Regime 1 (z_p20) ==="
matrix list favar_Phi1_1
di _newline "=== Phi1 matrix – Regime 2 (z_p40) ==="
matrix list favar_Phi1_2
di _newline "=== Phi1 matrix – Regime 3 (z_p60) ==="
matrix list favar_Phi1_3
di _newline "=== Phi1 matrix – Regime 4 (z_p80) ==="
matrix list favar_Phi1_4


/*******************************************************************************
* SECTION 7 – COMPANION MATRIX EIGENVALUE STABILITY DIAGNOSTICS
* Python: eigvals(results[z0][1]) → all |λ| < 1 check
*
* Stata Mata: C_eigenvalues(Phi1) → complex eigenvalue vector
*
* (!!!) C_eigenvalues returns complex eigenvalues of a real matrix.
*       abs() of a complex vector gives the modulus.
*******************************************************************************/

di _newline "=== Eigenvalue Stability Diagnostics – All Regimes ==="

mata:
regime_labels = ("z_p20 (p20)", "z_p40 (p40)", "z_p60 (p60)", "z_p80 (p80)")
for (r = 1; r <= 4; r++) {
    Phi1_r = st_matrix("favar_Phi1_" + strofreal(r))
    eigs   = C_eigenvalues(Phi1_r)
    mods   = abs(eigs)
    printf("\n%s\n", regime_labels[r])
    printf("  Eigenvalue moduli: ")
    printf("%6.4f  ", mods)
    printf("\n")
    printf("  Dominant |λ| = %6.4f | All inside unit circle: %s\n",
           max(mods), (max(mods) < 1 ? "Yes" : "No"))
}
end


/*******************************************************************************
* SECTION 8 – CHOLESKY-IDENTIFIED IMPULSE RESPONSE FUNCTIONS
* Python: P_cov = np.cov(favar_core_df.T); P_chol = cholesky(P_cov, lower=True)
*         IRF recursion: B[h] = Φ1 @ B[h-1] + Φ2 @ B[h-2]  (h=1..11)
*         Shock: policy instrument (col 1); Response: inflation (row 0)
*
* Stata Mata:
*   - Covariance: variance(Ydata)' * (N-1)/N  (match Python np.cov N-1 default)
*   - Cholesky: cholesky(P_cov) = lower triangular
*   - IRF recursion: identical to Python
*
* IDENTIFICATION NOTE:
*   Shock col = 1 (0-indexed in Python) = col 2 (1-indexed in Stata/Mata)
*     → ld_call_money (policy instrument shock)
*   Target row = 0 (Python) = row 1 (Mata)
*     → ld_hicp_sa (policy target response = HICP inflation)
*******************************************************************************/

mata:

void function compute_irfs(string scalar varnames, real scalar lag,
                            real scalar irf_h,
                            real scalar shock_col,
                            real scalar target_row,
                            real vector z_grid,
                            string scalar matpfx,
                            string scalar out_pfx) {

    real matrix Ydata, P_cov, P_chol, Phi1, Phi2, B
    real vector hicp_resp, eigs
    real scalar z0, r, h, k, n, K, sign_flip

    Ydata = st_data(., varnames)
    /* Drop missing rows */
    Ydata = select(Ydata, rowmissing(Ydata) :== 0)

    n = rows(Ydata)
    K = cols(Ydata)

    /* Covariance matrix: Python np.cov uses N-1 denominator (default) */
    P_cov  = (Ydata :- mean(Ydata))' * (Ydata :- mean(Ydata)) :/ (n-1)
    P_chol = cholesky(P_cov)       /* lower-triangular Cholesky */

    for (r = 1; r <= length(z_grid); r++) {

        z0   = z_grid[r]
        Phi1 = st_matrix(matpfx + "_Phi1_" + strofreal(r))
        Phi2 = st_matrix(matpfx + "_Phi2_" + strofreal(r))

        /* IRF matrices: B[h] is K×K, horizon h = 0..irf_h-1 */
        /* Initialise 3D as irf_h×K×K stored as a list of matrices */
        /* Stata Mata has no 3D arrays; use vector of matrices indexed by h */
        B_list = J(1, irf_h, NULL)
        B_list[1] = &P_chol              /* B[0] = Cholesky impact */

        for (h = 2; h <= irf_h; h++) {
            B_h = J(K, K, 0)
            for (k = 1; k <= min(h-1, lag); k++) {
                if      (k == 1) B_h = B_h + Phi1 * (*B_list[h-k])
                else if (k == 2) B_h = B_h + Phi2 * (*B_list[h-k])
            }
            B_list[h] = &B_h
        }

        /* Extract inflation response to policy instrument shock */
        hicp_resp = J(irf_h, 1, 0)
        for (h = 1; h <= irf_h; h++) {
            hicp_resp[h] = (*B_list[h])[target_row, shock_col]
        }

        /* Sign-flip test: any negative response in h=2..6 (Python: h=1..5 in 0-index) */
        sign_flip = any(hicp_resp[2..6] :< 0)
        printf("z0=%6.4f: π response h=1-5: ", z0)
        printf("%7.4f  ", hicp_resp[2..6]')
        printf("| Flip: %s\n", (sign_flip ? "Yes" : "No"))

        /* Store IRF vector as Stata matrix (irf_h × 1) */
        st_matrix(out_pfx + "_irf_" + strofreal(r), hicp_resp)
    }
}

end  /* end mata block */


di _newline "=== Computing Cholesky-Identified IRFs (12 horizons) ==="

* (!!!) Python: shock_col=1 (0-indexed) = ld_call_money → Mata col 2 (1-indexed)
* (!!!) Python: target_row=0 (0-indexed) = ld_hicp_sa → Mata row 1 (1-indexed)
mata: compute_irfs("`favar_varnames'", 2, 12, 2, 1, z_grid, "favar", "favar")

* Display IRF results
di _newline "=== Impulse Response Functions: Inflation → Policy Instrument Shock ==="
forvalues r = 1/4 {
    di _newline "Regime `r': IRF (inflation response to policy shock)"
    matrix list favar_irf_`r'
}


/*******************************************************************************
* SECTION 9 – IRF PLOTTING (REGIME-DEPENDENT)
* Python: 2×2 subplot grid, one IRF per regime, zero reference line
* Stata:  build a dataset of IRF values and plot with twoway line
*******************************************************************************/

* Assemble IRF data from stored matrices
clear

* Create a long dataset: horizon × regime
set obs 48  // 12 horizons × 4 regimes
gen horizon = mod(_n-1, 12)
gen regime  = ceil(_n/12)
gen irf_val = .

forvalues r = 1/4 {
    forvalues h = 1/12 {
        local obs_n = (`r'-1)*12 + `h'
        local val   = favar_irf_`r'[`h',1]
        replace irf_val = `val' in `obs_n'
    }
}

* Regime labels
gen regime_label = ""
local zlabels `"p20"' `"p40"' `"p60"' `"p80"'
forvalues r = 1/4 {
    local lbl : word `r' of `zlabels'
    replace regime_label = "z0 = `lbl'" if regime == `r'
}

label variable horizon  "Horizon (months)"
label variable irf_val  "IRF: inflation response to policy shock"

* Plot IRFs (2×2 layout approximated via twoway with by())
* (!!!) Stata's twoway does not have native 2×2 subplot layout;
*       use by(regime) which creates a panel of plots
twoway (line irf_val horizon, lwidth(medthick))                               ///
       (function y = 0, range(0 11) lcolor(gs6) lpattern(dash)),              ///
       by(regime_label, total cols(2)                                          ///
          title("Local FAVAR — Regime-Dependent IRFs")                         ///
          note("Impulse: LogDiff Call Money; Response: LogDiff-HICP (SA)"))    ///
       xtitle("Horizon (months)") ytitle("IRF")                                ///
       legend(off) graphregion(color(white))
graph export "Output/Module4/fig_local_favar_irfs.png", replace

di _newline "Module 4 complete. Outputs saved to Output/Module4/"
