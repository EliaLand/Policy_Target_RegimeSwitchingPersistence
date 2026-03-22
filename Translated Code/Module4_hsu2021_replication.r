
################################################################################
# Module 4 - Hsu et al. (2021) Replication: Local FAVAR
# Author    : Elia Landini
# Student ID: 12310239
# Course    : EESM2-Financial Economics
# Supervisor: Jean-Bernard Chatelain
# Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
#
# Translation of Python/Jupyter notebook (Module4_hsu2021_replication.ipynb) to R.
#
# PREREQUISITES:
#   Module 2 outputs must exist (jp_trans_df.csv).
#
# MODEL OVERVIEW (Hsu et al., 2021 — Local FAVAR):
#   1. PCA (5 components) compresses 22+ transformed vars into F1–F5 factors.
#   2. FAVAR state vector: P_t = [ld_hicp_sa, ld_call_money, F1, F2, F3, F4, F5].
#   3. Smoothing variable Z = lag(ld_call_money, 6); regime grid = pctiles 20/40/60/80.
#   4. For each z0 in z_grid: Gaussian kernel-weighted VAR(2) estimated via WLS.
#   5. Companion matrix eigenvalue stability check per regime.
#   6. Cholesky-identified IRFs (12 horizons): inflation response to policy shock.
#
# REQUIRED PACKAGES:
#   install.packages(c("tidyverse","MASS","vars","patchwork","ggplot2","ggrepel"))
################################################################################

library(tidyverse)
library(MASS)       # ginv() — pseudo-inverse matching Python's la.pinv()
library(vars)       # VARselect() for AIC lag order selection
library(patchwork)  # multi-panel ggplot layouts
library(ggplot2)
library(ggrepel)    # label repel for eigenvalue plot

dir.create("Output/Module4", recursive = TRUE, showWarnings = FALSE)


################################################################################
# SECTION 1 – DATA LOADING & CLEANING
# Python: pd.read_csv("jp_trans_df.csv"); replace ±inf with NaN
################################################################################

jp_trans_df <- read.csv("Data/Transformed/jp_trans_df.csv")
jp_trans_df$Time <- as.Date(jp_trans_df$Time)

# Replace ±Inf with NA (Python: df.replace([np.inf, -np.inf], np.nan))
jp_trans_df <- jp_trans_df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))

# Friendly column names (mirrors Module 2 naming)
jp_trans_df <- jp_trans_df %>%
  rename_with(~ gsub("LogDiff\\.HICP\\.\\.SA\\.", "ld_hicp_sa", .)) %>%
  rename_with(~ gsub("LogDiff\\.Call\\.Money.*",  "ld_call_money", .)) %>%
  rename_with(~ gsub("AR\\.1\\.detrend\\.Call\\.Money.*", "ar1_call_money", .)) %>%
  rename_with(~ gsub("HPfilter.*",                "hp_rgdp", .))

# Drop AR(1)-detrended call money (excluded in Python: favar_vars filter)
# Python: col not in ["Country", "AR(1)detrend-Call Money/Interbank Immediate (%)"]
if ("ar1_call_money" %in% names(jp_trans_df)) {
  jp_trans_df <- jp_trans_df %>% select(-ar1_call_money)
}
if ("Country" %in% names(jp_trans_df)) {
  jp_trans_df <- jp_trans_df %>% select(-Country)
}


################################################################################
# SECTION 2 – PCA FACTOR EXTRACTION (5 COMPONENTS)
# Python: sklearn.decomposition.PCA(n_components=5).fit_transform(favar_df.values)
#
# CRITICAL: sklearn PCA centers but does NOT scale → covariance-matrix PCA.
#   R equivalent: prcomp(X, center=TRUE, scale.=FALSE)$x[, 1:5]
#   (NOT princomp or PCA with correlation matrix — those standardise first.)
#
# Python favar_vars = all cols except "Country" & "AR(1)detrend-Call Money"
# PCA input = favar_vars; Y = [ld_hicp_sa, ld_call_money] kept separate.
################################################################################

# Variables to compress via PCA:
# all transformed vars EXCEPT the two Y variables (policy target + instrument)
# (!!!) These enter as factors; Y enters directly into the FAVAR state vector
pca_exclude <- c("Time", "ld_hicp_sa", "ld_call_money")
pca_candidates <- setdiff(names(jp_trans_df), pca_exclude)

# Drop rows with any missing across PCA candidate variables + Y variables
# (Python: favar_df = df[favar_vars].dropna())
favar_complete <- jp_trans_df %>%
  select(Time, ld_hicp_sa, ld_call_money, all_of(pca_candidates)) %>%
  drop_na()

cat(sprintf("Complete cases for FAVAR: %d observations\n", nrow(favar_complete)))

# PCA on covariance matrix (center=TRUE, scale.=FALSE matches sklearn.PCA)
pca_input <- favar_complete %>% select(all_of(pca_candidates)) %>% as.matrix()
pca_fit   <- prcomp(pca_input, center = TRUE, scale. = FALSE)

# Cumulative explained variance (Python: pca.explained_variance_ratio_.cumsum())
var_explained <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
cumvar        <- cumsum(var_explained)
cat("\nCumulative explained variance (5 components):",
    paste(round(cumvar[1:5], 4), collapse = " | "), "\n")

# Extract 5 factor scores
factors_df <- as.data.frame(pca_fit$x[, 1:5])
colnames(factors_df) <- paste0("F", 1:5)
factors_df$Time <- favar_complete$Time


################################################################################
# SECTION 3 – Y TARGET VARIABLES & FAVAR CORE DATASET
# Python: Y = [ld_hicp_sa, ld_call_money]; forward-fill NaN; favar_core_df = [Y, F1..F5]
################################################################################

Y_df <- favar_complete %>% select(Time, ld_hicp_sa, ld_call_money)

# Forward-fill NaN in Y variables (Python: Y.fillna(method="ffill"))
Y_df <- Y_df %>%
  arrange(Time) %>%
  fill(ld_hicp_sa, ld_call_money, .direction = "down")

# Build FAVAR core: P_t = [ld_hicp_sa, ld_call_money, F1, F2, F3, F4, F5]
favar_core_df <- Y_df %>%
  inner_join(factors_df, by = "Time") %>%
  drop_na()

cat(sprintf("\nFAVAR core dataset: %d obs × %d variables\n",
            nrow(favar_core_df), ncol(favar_core_df) - 1)) # -1 for Time

write.csv(favar_core_df, "Output/Module4/favar_core_df.csv", row.names = FALSE)


################################################################################
# SECTION 4 – LAG ORDER SELECTION (AIC)
# Python: VAR(Y).select_order(maxlags=10)
# R:      vars::VARselect(Y, lag.max=10, type="const")
#
# (!!!) Python fits VAR on Y = [ld_hicp_sa, ld_call_money] only for lag selection.
#       Optimal lag = 2 (hardcoded in Python Cell 9: lag = 2).
################################################################################

cat("\n=== VAR Lag Order Selection on Y Variables (AIC) ===\n")
Y_mat <- favar_core_df %>% select(ld_hicp_sa, ld_call_money) %>% as.matrix()
lag_sel <- VARselect(Y_mat, lag.max = 10, type = "const")
print(lag_sel$selection)
cat("AIC-selected lag:", lag_sel$selection["AIC(n)"], "\n")
# (!!!) lag = 2 applied throughout (consistent with notebook)
lag <- 2


################################################################################
# SECTION 5 – SMOOTHING VARIABLE Z & REGIME GRID
# Python: Z = favar_core_df["ld_call_money"].shift(6)
#         z_grid = np.percentile(Z, [20, 40, 60, 80])
# R:      Z_smooth = lag(ld_call_money, 6); quantile(Z, probs=c(0.2,0.4,0.6,0.8))
################################################################################

favar_core_df <- favar_core_df %>%
  arrange(Time) %>%
  mutate(Z_smooth = lag(ld_call_money, 6))

# Drop rows where Z_smooth is NA (first 6 obs)
reg_favar_core_df <- favar_core_df %>% drop_na(Z_smooth)

# Regime percentile grid
Z_vals <- reg_favar_core_df$Z_smooth
z_grid <- quantile(Z_vals, probs = c(0.20, 0.40, 0.60, 0.80), na.rm = TRUE)
cat("\n=== Smoothing Variable Z — Percentile Grid ===\n")
print(z_grid)


################################################################################
# SECTION 6 – LOCAL FAVAR ESTIMATION (GAUSSIAN KERNEL-WEIGHTED VAR)
# Python: for each z0 → Gaussian kernel weights → weighted OLS VAR(2)
#
# MODEL:
#   P_t = Φ_0 + Φ_1 P_{t-1} + Φ_2 P_{t-2} + ε_t
#   P_t = [ld_hicp_sa, ld_call_money, F1, F2, F3, F4, F5]
#   Kernel: w_t(z0) = φ((Z_t − z0)/h) / h,   h = 0.1 × σ_Z (population std)
#
# Python lag matrix structure:
#   X has (lag+1)*K columns:
#     cols 1..K        = ones (K constant terms)
#     cols (K+1)..2K   = P_{t-1}
#     cols (2K+1)..3K  = P_{t-2}
#   y has K columns = P_t
#   beta = pinv(X_w' X_w) @ (X_w' y_w)  → shape ((lag+1)*K, K)
#   Phi_z0 = beta.reshape(lag+1, K, K)
#     Phi_z0[0] = constant (K×K)
#     Phi_z0[1] = Phi1    (K×K)
#     Phi_z0[2] = Phi2    (K×K)
#
# Python bandwidth: h = 0.1 * np.std(Z) = 0.1 * population std (÷ N not N-1)
# R: sd() uses N-1; correct to population std: sd(Z) * sqrt((N-1)/N)
################################################################################

# FAVAR state variable matrix (K=7): P_t
P_vars <- c("ld_hicp_sa","ld_call_money","F1","F2","F3","F4","F5")
K      <- length(P_vars)

# Use reg_favar_core_df (aligned with Z_smooth)
Ydata <- reg_favar_core_df %>% select(all_of(P_vars)) %>% as.matrix()
Z     <- reg_favar_core_df$Z_smooth
n     <- nrow(Ydata)
T_reg <- n - lag

# Build response matrix y (rows lag+1 .. n) and regressor X
# (!!!) X[:,1..K] = ones (K constant terms per row), then P_{t-1}, P_{t-2}
y_mat <- Ydata[(lag + 1):n, ]                           # T_reg × K
X_mat <- matrix(1, nrow = T_reg, ncol = (lag + 1) * K) # initialise to ones
for (q in 1:lag) {
  col_start <- q * K + 1
  col_end   <- (q + 1) * K
  X_mat[, col_start:col_end] <- Ydata[(lag + 1 - q):(n - q), ]
}

# Population std (matches Python np.std which divides by N)
N_Z <- length(Z)
h   <- 0.1 * (sd(Z) * sqrt((N_Z - 1) / N_Z))

# Results container: list indexed by regime
local_favar_results <- list()

cat("\n=== Local FAVAR – Gaussian Kernel-Weighted VAR(2) ===\n")

for (iz in seq_along(z_grid)) {
  z0 <- z_grid[iz]

  # Gaussian kernel: φ((Z_t − z0)/h) / h
  # (!!!) Python: kern = exp(−0.5*((Z−z0)/h)²) / (h*sqrt(2π)) = dnorm((Z−z0)/h)/h
  kern <- dnorm((Z - z0) / h) / h

  # Normalised weights aligned with response rows (lag+1 .. n)
  # Python: K_w = kern.iloc[1:] / kern.iloc[1:].sum()  (skips first obs)
  kern_aligned <- kern[(lag + 1):n]
  K_w          <- kern_aligned / sum(kern_aligned)

  # Diagonal weight matrix applied row-wise: X_w = diag(sqrt(K_w)) @ X
  w   <- sqrt(K_w)
  X_w <- X_mat * w        # broadcast: each row of X multiplied by w[row]
  y_w <- y_mat * w        # same for y

  # Weighted OLS via pseudo-inverse (matches Python la.pinv())
  # β = pinv(X_w'X_w) @ (X_w' y_w)
  XtX  <- t(X_w) %*% X_w
  Xty  <- t(X_w) %*% y_w
  beta <- tryCatch(
    ginv(XtX) %*% Xty,        # MASS::ginv = pseudo-inverse
    error = function(e) { warning("ginv failed for z0=", z0); matrix(NA, nrow(XtX), K) }
  )

  # Reshape beta: ((lag+1)*K rows, K cols) → 3 matrices each K×K
  # Python: Phi_z0 = beta.reshape(lag+1, K, K)
  Phi0 <- beta[1:K,          ]     # constant block
  Phi1 <- beta[(K+1):(2*K),  ]     # Phi1
  Phi2 <- beta[(2*K+1):(3*K),]     # Phi2
  rownames(Phi1) <- rownames(Phi2) <- P_vars
  colnames(Phi1) <- colnames(Phi2) <- P_vars

  # Eigenvalue stability check
  eigs    <- eigen(Phi1)$values
  dom_mod <- max(Mod(eigs))
  stable  <- all(Mod(eigs) < 1)
  cat(sprintf("z0=%7.4f: AR(1) diags %s\n",
              z0, paste(round(diag(Phi1)[1:4], 3), collapse=" ")))
  cat(sprintf("         Dominant |λ|=%.4f | All inside unit circle: %s\n",
              dom_mod, ifelse(stable, "Yes", "No")))

  local_favar_results[[iz]] <- list(
    z0    = z0,
    Phi0  = Phi0,
    Phi1  = Phi1,
    Phi2  = Phi2,
    eigs  = eigs,
    stable = stable
  )
}

# Average eigenvalue of Phi1 across all regimes (Python: "Roots check (avg A1)")
avg_Phi1   <- Reduce("+", lapply(local_favar_results, `[[`, "Phi1")) / length(z_grid)
avg_eigs   <- Mod(eigen(avg_Phi1)$values)
cat(sprintf("\nRoots check (avg Phi1): %s\n", paste(round(avg_eigs, 4), collapse=" ")))


################################################################################
# SECTION 7 – COMPANION MATRIX EIGENVALUE STABILITY + UNIT ROOT CIRCLE PLOT
# Python: complex plane scatter + simulated VAR processes (2-panel figure)
# R:      ggplot unit root circle with geom_point; ggplot simulation traces
################################################################################

# Build eigenvalue data frame for all regimes
eig_df <- map_dfr(seq_along(z_grid), function(iz) {
  eigs <- local_favar_results[[iz]]$eigs
  data.frame(
    regime  = paste0("z0=", round(z_grid[iz], 3)),
    real    = Re(eigs),
    imag    = Im(eigs),
    modulus = Mod(eigs)
  )
})

# Unit circle data
theta    <- seq(0, 2 * pi, length.out = 300)
circle_df <- data.frame(x = cos(theta), y = sin(theta))
palette4  <- c("#440154","#3B528B","#21908C","#FDE725")

p_circle <- ggplot() +
  geom_path(data = circle_df, aes(x = x, y = y), color = "#393939", linewidth = 1) +
  geom_hline(yintercept = 0, color = "#393939", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "#393939", linewidth = 0.5) +
  geom_vline(xintercept =  1, color = "#FF0000", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = -1, color = "#FF0000", linetype = "dashed", alpha = 0.5) +
  geom_point(data = eig_df,
             aes(x = real, y = imag, color = regime, shape = regime),
             size = 3.5, alpha = 0.9) +
  scale_color_manual(values = setNames(palette4, unique(eig_df$regime))) +
  coord_equal(xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35)) +
  labs(title    = "VAR Companion Matrix Eigenvalues on Unit Circle",
       subtitle = "Complex plane, all regimes z\u2080",
       x = "Real Part", y = "Imaginary Part",
       color = "Regime", shape = "Regime") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

# ── Simulated VAR(2) processes (right panel of Python figure) ────────────────
# Python: random seed=42; N=300; simulate y_t = Phi1 @ y_{t-1} + Phi2 @ y_{t-2} + ε_t
#         plot first variable (index 0 = ld_hicp_sa) for each regime
set.seed(42)
N_sim <- 300
sim_list <- map_dfr(seq_along(z_grid), function(iz) {
  Phi1_r <- local_favar_results[[iz]]$Phi1
  Phi2_r <- local_favar_results[[iz]]$Phi2
  dom_mod <- max(Mod(local_favar_results[[iz]]$eigs))
  stable  <- local_favar_results[[iz]]$stable

  eps <- matrix(rnorm(N_sim * K), nrow = N_sim, ncol = K)
  Y_s <- matrix(0,             nrow = N_sim, ncol = K)
  Y_s[1, ] <- eps[1, ]
  Y_s[2, ] <- Phi1_r %*% Y_s[1, ] + eps[2, ]
  for (t in 3:N_sim) {
    Y_s[t, ] <- Phi1_r %*% Y_s[t-1, ] + Phi2_r %*% Y_s[t-2, ] + eps[t, ]
  }
  data.frame(
    t       = 1:N_sim,
    x       = Y_s[, 1],
    regime  = paste0("z0=", round(z_grid[iz], 3),
                     " | dom|λ|=", round(dom_mod, 4),
                     " | stable:", stable)
  )
})

p_sims <- ggplot(sim_list, aes(x = t, y = x)) +
  geom_line(aes(color = regime), linewidth = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#393939", linewidth = 0.4) +
  facet_wrap(~ regime, ncol = 1, scales = "free_y") +
  scale_color_manual(values = setNames(palette4, unique(sim_list$regime))) +
  labs(title = "Simulated FAVAR Processes – First Variable (π_t)",
       x = "lag t", y = "x(t)") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none", strip.text = element_text(size = 7))

# Combined figure (Python: 2-panel matplotlib figure)
p_stability <- p_circle | p_sims
p_stability <- p_stability +
  plot_annotation(title = "FAVAR Companion Matrix Unit Root Diagnostics – Local FAVAR All Regimes")

ggsave("Output/Module4/fig_unit_root_circle.png", p_stability, width = 14, height = 7)
cat("\nUnit root circle + simulation plot saved.\n")


################################################################################
# SECTION 8 – CHOLESKY-IDENTIFIED IMPULSE RESPONSE FUNCTIONS
# Python: P_cov = np.cov(favar_core_df.T); P_chol = cholesky(P_cov, lower=True)
#         IRF recursion: B[h] = Σ_{k=1}^{min(h,lag)} Phi[k] @ B[h-k]
#
# Python identification:
#   shock_col  = 1 (0-indexed) = ld_call_money (policy instrument shock)
#   target_row = 0 (0-indexed) = ld_hicp_sa    (inflation response)
#
# R translations:
#   np.cov(X.T)                → cov(X) [N-1 denominator, matches numpy default]
#   cholesky(P_cov, lower=True) → t(chol(P_cov))  [R's chol() is UPPER triangular]
#   B[h-k] accessed by list index h-k+1 (1-indexed in R)
#   shock_col=2, target_row=1  (1-indexed in R)
################################################################################

# Covariance matrix on aligned FAVAR state variables
P_data <- reg_favar_core_df %>% select(all_of(P_vars)) %>% drop_na() %>% as.matrix()
P_cov  <- cov(P_data)               # matches Python np.cov (N-1 denominator)

# Lower-triangular Cholesky decomposition
# Python: scipy.linalg.cholesky(P_cov, lower=True)
# R: chol() returns UPPER triangular U where t(U) %*% U = P_cov
#    So lower triangular L = t(chol(P_cov))
P_chol <- t(chol(P_cov))

irf_h      <- 12    # 12 periods horizon
shock_col  <- 2     # ld_call_money (1-indexed: col 2 = Python col 1)
target_row <- 1     # ld_hicp_sa    (1-indexed: row 1 = Python row 0)
irfs       <- list()

cat("\n=== Cholesky-Identified IRFs (12-horizon, all regimes) ===\n")

for (iz in seq_along(z_grid)) {
  z0   <- z_grid[iz]
  Phi1 <- local_favar_results[[iz]]$Phi1
  Phi2 <- local_favar_results[[iz]]$Phi2

  # B_list[[h+1]] stores B[h] (K×K matrix at horizon h = 0..irf_h-1)
  # Python: B[0] = P_chol
  B_list    <- vector("list", irf_h)
  B_list[[1]] <- P_chol   # h = 0

  for (h in 2:irf_h) {   # h = 1..11 in Python (0-indexed) = 2..12 in R loop
    B_h <- matrix(0, K, K)
    for (k in 1:min(h - 1, lag)) {
      if (k == 1) B_h <- B_h + Phi1 %*% B_list[[h - k]]
      if (k == 2) B_h <- B_h + Phi2 %*% B_list[[h - k]]
    }
    B_list[[h]] <- B_h
  }

  # Extract inflation (target_row=1) response to policy shock (shock_col=2)
  hicp_resp <- sapply(B_list, function(B) B[target_row, shock_col])

  # Sign-flip test: any negative in h=1..5 (Python: h=1..5 in 0-index → R index 2..6)
  sign_flip <- any(hicp_resp[2:6] < 0)
  cat(sprintf("z0=%7.4f: π response h=1-5: %s | Flip: %s\n",
              z0,
              paste(round(hicp_resp[2:6], 3), collapse="  "),
              ifelse(sign_flip, "Yes", "No")))

  irfs[[iz]] <- list(z0 = z0, hicp_resp = hicp_resp, sign_flip = sign_flip)
}


################################################################################
# SECTION 9 – IRF PLOTTING (2×2 REGIME-DEPENDENT)
# Python: plt.subplots(2, 2); one IRF per regime; zero reference line
# R:      ggplot facet_wrap(~ regime, ncol=2)
################################################################################

# Build IRF data frame
irf_df <- map_dfr(seq_along(z_grid), function(iz) {
  data.frame(
    horizon     = 0:(irf_h - 1),
    irf_val     = irfs[[iz]]$hicp_resp,
    regime      = paste0("Regime z\u2080 = ", round(z_grid[iz], 3)),
    sign_flip   = irfs[[iz]]$sign_flip,
    regime_idx  = iz
  )
})

# Ribbon for sign-flip marker (h=1..5)
irf_df <- irf_df %>%
  mutate(in_test_window = horizon >= 1 & horizon <= 5)

p_irfs <- ggplot(irf_df, aes(x = horizon, y = irf_val, color = regime)) +
  geom_hline(yintercept = 0, color = "#393939", linetype = "dashed", linewidth = 0.8) +
  geom_rect(data = irf_df %>% filter(in_test_window) %>% distinct(regime, regime_idx),
            aes(xmin = 1, xmax = 5, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = setNames(palette4,
                                        paste0("Regime z\u2080 = ", round(z_grid, 3)))) +
  facet_wrap(~ regime, ncol = 2) +
  scale_x_continuous(breaks = 0:(irf_h - 1)) +
  labs(
    title    = "Local FAVAR — Regime-Dependent IRFs",
    subtitle = "Impulse: Policy Instrument (LogDiff Call Money) → Response: Inflation (LogDiff-HICP SA)",
    x        = "Horizon (months)",
    y        = "IRF",
    color    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank())

ggsave("Output/Module4/fig_local_favar_irfs.png", p_irfs,
       width = 14, height = 9, dpi = 150)

# Overlay plot: all 4 IRFs on single panel for direct comparison
p_irfs_overlay <- ggplot(irf_df, aes(x = horizon, y = irf_val,
                                       color = regime, linetype = regime)) +
  geom_hline(yintercept = 0, color = "#393939", linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = setNames(palette4,
                                        paste0("Regime z\u2080 = ", round(z_grid, 3)))) +
  scale_x_continuous(breaks = 0:(irf_h - 1)) +
  labs(
    title    = "Local FAVAR — Overlaid Regime-Dependent IRFs",
    subtitle = "Impulse: LogDiff Call Money | Response: LogDiff-HICP (SA)",
    x        = "Horizon (months)",
    y        = "IRF",
    color    = "Regime", linetype = "Regime"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("Output/Module4/fig_local_favar_irfs_overlay.png", p_irfs_overlay,
       width = 12, height = 6, dpi = 150)

cat("\nModule 4 complete. All outputs saved to Output/Module4/\n")
