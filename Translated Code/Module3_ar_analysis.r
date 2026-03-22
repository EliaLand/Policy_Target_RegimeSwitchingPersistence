
################################################################################
# Module 3 - AR Analysis
# Author    : Elia Landini
# Student ID: 12310239
# Course    : EESM2-Financial Economics
# Supervisor: Jean-Bernard Chatelain
# Repository: https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
#
# Translation of Python/Jupyter notebook (Module3_ar_analysis.ipynb) to R.
#
# PREREQUISITES:
#   Module 2 outputs must exist (jp_trans_df.csv, jp_core_trans_df.csv,
#   jp_core_hybrid_df.csv, jp_aggregated_df.csv)
#
# REQUIRED PACKAGES:
#   install.packages(c("tidyverse","zoo","urca","tseries","forecast",
#                      "vars","lmtest","sandwich","ggplot2","patchwork"))
################################################################################

library(tidyverse)
library(zoo)
library(urca)        # ur.df (ADF), ur.pp (Phillips-Perron)
library(tseries)     # adf.test
library(forecast)    # Acf, Pacf
library(vars)        # VAR, causality
library(lmtest)      # bgtest (Breusch-Godfrey)
library(sandwich)    # vcovHAC
library(ggplot2)
library(patchwork)   # multi-panel ggplots

dir.create("Output/Module3", recursive = TRUE, showWarnings = FALSE)

# YCC key dates (reference lines throughout)
ycc_adopt    <- as.Date("2016-09-21")
ycc_defacto  <- as.Date("2018-07-31")
ycc_explicit <- as.Date("2021-03-19")

ycc_vlines <- function(p) {
  p +
    geom_vline(xintercept = as.numeric(ycc_adopt),    linetype = "dashed", color = "#393939") +
    geom_vline(xintercept = as.numeric(ycc_defacto),  linetype = "dashed", color = "#393939") +
    geom_vline(xintercept = as.numeric(ycc_explicit), linetype = "dashed", color = "#393939") +
    annotate("text", x = ycc_adopt,    y = Inf, label = "YCC adoption",          angle = 90, vjust = 1.2, hjust = 1, size = 2.8) +
    annotate("text", x = ycc_defacto,  y = Inf, label = "YCC de-facto widening", angle = 90, vjust = 1.2, hjust = 1, size = 2.8) +
    annotate("text", x = ycc_explicit, y = Inf, label = "YCC explicit widening", angle = 90, vjust = 1.2, hjust = 1, size = 2.8)
}

viridis_pal <- c("#1F9E89", "#440154", "#FDE725")


################################################################################
# SECTION 1 – DATA LOADING
# Python: pd.read_csv(...)
################################################################################

jp_aggregated_df  <- read.csv("Data/Aggregated/jp_aggregated_df.csv")
jp_trans_df       <- read.csv("Data/Transformed/jp_trans_df.csv")
jp_core_trans_df  <- read.csv("Data/Transformed/jp_core_trans_df.csv")
jp_core_hybrid_df <- read.csv("Data/Transformed/jp_core_hybrid_df.csv")

# Parse Time column to Date
for (df_name in c("jp_aggregated_df","jp_trans_df","jp_core_trans_df","jp_core_hybrid_df")) {
  df <- get(df_name)
  df$Time <- as.Date(df$Time)
  assign(df_name, df)
}

# Rename columns to short R-friendly names (mirrors Module 2 Stata variable names)
# Core columns used throughout this module
names(jp_core_trans_df)[names(jp_core_trans_df) == "LogDiff.HICP..SA."]                                <- "ld_hicp_sa"
names(jp_core_trans_df)[names(jp_core_trans_df) == "AR.1.detrend.Call.Money.Interbank.Immediate...."] <- "ar1_call_money"
names(jp_core_trans_df)[names(jp_core_trans_df) == "AR.1.detrend.10.Year.Gov.Bond.Yields...."]        <- "ar1_gb10y"

names(jp_core_hybrid_df)[names(jp_core_hybrid_df) == "LogDiff.HICP..SA."]                              <- "ld_hicp_sa"
names(jp_core_hybrid_df)[names(jp_core_hybrid_df) == "Call.Money.Interbank.Immediate...."]             <- "call_money"
names(jp_core_hybrid_df)[names(jp_core_hybrid_df) == "X10.Year.Gov.Bond.Yields...."]                   <- "gb10y"

names(jp_aggregated_df)[names(jp_aggregated_df) == "Call.Money.Interbank.Immediate...."]               <- "call_money"
names(jp_aggregated_df)[names(jp_aggregated_df) == "X10.Year.Gov.Bond.Yields...."]                     <- "gb10y"

names(jp_trans_df)[names(jp_trans_df) == "LogDiff.HICP..SA."]                                          <- "ld_hicp_sa"
names(jp_trans_df)[names(jp_trans_df) == "LogDiff.Call.Money.Interbank.Immediate...."]                  <- "ld_call_money"
names(jp_trans_df)[names(jp_trans_df) == "HPfilter.Real.GDP..billions.chained.2015.JPY."]               <- "hp_rgdp"


################################################################################
# SECTION 2 – OVERTIME PLOTTING
# Python: matplotlib line charts with YCC reference lines
# R: ggplot2 with geom_vline for YCC dates
################################################################################

# Restrict to 1990+
df1 <- jp_core_trans_df %>% filter(Time >= as.Date("1990-01-01"))
df2 <- jp_aggregated_df  %>% filter(Time >= as.Date("1990-01-01"))

# Plot 2.1: Raw data (inflation log-diff + call money + 10Y bond)
p_raw <- ggplot() +
  geom_line(data = df1, aes(x = Time, y = ld_hicp_sa,  color = "LogDiff-HICP (SA)"),          linewidth = 0.5) +
  geom_line(data = df2, aes(x = Time, y = call_money,  color = "Call Money/Interbank (%)"),    linewidth = 0.5) +
  geom_line(data = df2, aes(x = Time, y = gb10y,       color = "10-Year Gov Bond Yields (%)"), linewidth = 0.5) +
  scale_color_manual(values = viridis_pal) +
  labs(title = "Policy Target & Policy Instrument Evolution Overtime (Raw Data)",
       x = "Year", y = "Policy Target/Instrument", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
p_raw <- ycc_vlines(p_raw)
ggsave("Output/Module3/fig_plot_raw.png", p_raw, width = 10, height = 5)

# Plot 2.2: Smoothed inflation (12-month rolling mean)
# Python: df1["LogDiff-HICP (SA)"].rolling(window=12, center=True).mean()
df1 <- df1 %>% mutate(ld_hicp_smooth = zoo::rollmean(ld_hicp_sa, k = 12, fill = NA, align = "center"))

p_smooth <- ggplot() +
  geom_line(data = df1, aes(x = Time, y = ld_hicp_smooth, color = "LogDiff-HICP (SA, 12-lag rolling mean)"), linewidth = 0.5) +
  geom_line(data = df2, aes(x = Time, y = call_money,     color = "Call Money/Interbank (%)"),               linewidth = 0.5) +
  geom_line(data = df2, aes(x = Time, y = gb10y,          color = "10-Year Gov Bond Yields (%)"),            linewidth = 0.5) +
  scale_color_manual(values = viridis_pal) +
  labs(title = "Policy Target & Policy Instrument Evolution Overtime (Smoothed)",
       x = "Year", y = "Policy Target/Instrument", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_smooth <- ycc_vlines(p_smooth)
ggsave("Output/Module3/fig_plot_smoothed.png", p_smooth, width = 10, height = 5)

# Plot 2.3: Transformed variables (jp_core_trans_df)
df_trans <- jp_core_trans_df %>%
  pivot_longer(cols = c(ld_hicp_sa, ar1_call_money, ar1_gb10y), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
    ld_hicp_sa     = "LogDiff-HICP (SA)",
    ar1_call_money = "AR(1)detrend-Call Money (%)",
    ar1_gb10y      = "AR(1)detrend-10Y Gov Bond Yields (%)"))

p_trans <- ggplot(df_trans, aes(x = Time, y = value, color = variable)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = setNames(viridis_pal, unique(df_trans$variable))) +
  labs(title = "Policy Target & Instrument Overtime (Transformed Data)",
       x = "Year", y = "Value", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_trans <- ycc_vlines(p_trans)
ggsave("Output/Module3/fig_plot_transformed.png", p_trans, width = 10, height = 5)


################################################################################
# SECTION 3 – AR(1) PROCESS ESTIMATION
# Python: sm.OLS(pi_t, add_constant(pi_tm1)).fit()
# R:      lm(pi_t ~ pi_tm1) = lm(y ~ lag(y, 1))
#
# Residual AR(1) test: lm on residuals; equivalent to Breusch-Godfrey lag-1
################################################################################

ar1_estimate <- function(series, name) {
  series <- na.omit(series)
  n      <- length(series)
  y      <- series[2:n]
  y_lag  <- series[1:(n-1)]
  fit    <- lm(y ~ y_lag)
  cat("\n=== AR(1) –", name, "===\n")
  print(summary(fit))
  cat("AR(1) phi =", coef(fit)["y_lag"], "\n")

  # AR(1) on residuals
  resid   <- fit$residuals
  n_r     <- length(resid)
  u_t     <- resid[2:n_r]
  u_tm1   <- resid[1:(n_r-1)]
  fit_r   <- lm(u_t ~ u_tm1)
  cat("--- AR(1) on residuals:", name, "---\n")
  print(summary(fit_r))

  return(list(fit = fit, phi = coef(fit)["y_lag"], const = coef(fit)["(Intercept)"]))
}

# 3.1 Inflation
ar1_pi <- ar1_estimate(jp_core_trans_df$ld_hicp_sa, "Inflation (LogDiff-HICP SA)")

# 3.2 Call Money (raw)
ar1_i  <- ar1_estimate(jp_aggregated_df$call_money,  "Call Money/Interbank (raw)")

# 3.3 Output Gap (HP-filter)
ar1_y  <- ar1_estimate(jp_trans_df$hp_rgdp,           "HP Output Gap")

# 3.4 Autocorrelation table (jp_core_hybrid_df)
cat("\n=== AR(1) Autocorrelation Coefficients (jp_core_hybrid_df) ===\n")
for (vname in c("ld_hicp_sa","call_money","gb10y")) {
  x   <- na.omit(jp_core_hybrid_df[[vname]])
  rho <- cor(x[-1], x[-length(x)])
  cat(sprintf("AR(1) [%s] = %.6f\n", vname, rho))
}


################################################################################
# SECTION 4 – AR(1) UNIT ROOT TESTING (RAW DATA)
# Python: adfuller(series, autolag="AIC") + PhillipsPerron(series)
# R:      urca::ur.df (ADF)  +  urca::ur.pp (Phillips-Perron)
#
# ADF:  ur.df(series, type="none", lags=p, selectlags="AIC")
# PP:   ur.pp(series, type="Z-tau", model="constant", lags="long")
################################################################################

unit_root_test <- function(series, name) {
  series <- na.omit(series)
  cat("\n=== Unit Root Tests –", name, "===\n")

  # ADF (AIC lag selection, max 12 lags — mirrors Python autolag="AIC")
  adf <- ur.df(series, type = "none", lags = 12, selectlags = "AIC")
  cat("ADF statistic:", adf@teststat, "\n")
  cat("Critical values (1%/5%/10%):", adf@cval, "\n")
  cat("Stationary (|ADF| > 5% CV):", abs(adf@teststat) > abs(adf@cval[2]), "\n")

  # Phillips-Perron
  pp  <- ur.pp(series, type = "Z-tau", model = "constant", lags = "long")
  cat("PP statistic:", pp@teststat, "\n")
  cat("Critical values:", pp@cval, "\n")
  cat("Stationary (|PP| > 5% CV):", abs(pp@teststat) > abs(pp@cval[2]), "\n")
}

for (vname in c("ld_hicp_sa","call_money","gb10y")) {
  unit_root_test(na.omit(jp_core_hybrid_df[[vname]]), vname)
}

# AR(1) autocorrelations vs unit circle
cat("\n=== AR(1) Coefficients vs Unit Boundary ===\n")
for (vname in c("ld_hicp_sa","call_money","gb10y")) {
  x   <- na.omit(jp_core_hybrid_df[[vname]])
  rho <- cor(x[-1], x[-length(x)])
  cat(sprintf("%s: phi = %.6f | Stationary (|phi|<1): %s\n",
              vname, rho, ifelse(abs(rho) < 1, "Yes", "No")))
}

# ACF / PACF plots
png("Output/Module3/fig_acf_pi.png", width = 800, height = 400)
  Acf(na.omit(jp_core_trans_df$ld_hicp_sa), lag.max = 20,
      main = "ACF – LogDiff-HICP (SA)")
dev.off()
png("Output/Module3/fig_pacf_pi.png", width = 800, height = 400)
  Pacf(na.omit(jp_core_trans_df$ld_hicp_sa), lag.max = 20,
       main = "PACF – LogDiff-HICP (SA)")
dev.off()


################################################################################
# SECTION 5 – AR(2) PROCESS ESTIMATION
# Python: sm.OLS(pi_t, add_constant([pi_tm1, pi_tm2])).fit()
# R:      lm(y[3:n] ~ y[2:(n-1)] + y[1:(n-2)])
################################################################################

ar2_estimate <- function(series, name) {
  series <- na.omit(series)
  n      <- length(series)
  y      <- series[3:n]
  y_tm1  <- series[2:(n-1)]
  y_tm2  <- series[1:(n-2)]
  fit    <- lm(y ~ y_tm1 + y_tm2)
  cat("\n=== AR(2) –", name, "===\n")
  print(summary(fit))
  cat("phi1 =", coef(fit)["y_tm1"], " phi2 =", coef(fit)["y_tm2"], "\n")
  return(list(fit = fit, phi1 = coef(fit)["y_tm1"], phi2 = coef(fit)["y_tm2"]))
}

ar2_pi <- ar2_estimate(jp_core_trans_df$ld_hicp_sa, "Inflation")
ar2_i  <- ar2_estimate(jp_aggregated_df$call_money,  "Call Money (raw)")
ar2_y  <- ar2_estimate(jp_trans_df$hp_rgdp,           "HP Output Gap")


################################################################################
# SECTION 6 – AR(2) UNIT ROOT TESTING
# Python: coeffs = [1, -(phi1+phi2), -(phi1*phi2)]; np.roots(coeffs)
# R:      polyroot(c(coef_x0, coef_x1, coef_x2))
#         NOTE: polyroot() takes coefficients from LOWEST to HIGHEST degree
#         Polynomial: X² - (phi1+phi2)X - phi1*phi2 = 0
#           → polyroot(c(-phi1*phi2, -(phi1+phi2), 1))
#
# Stability triangle:
#   (1) phi1*phi2 < 1
#   (2) phi1*phi2 > (phi1+phi2) - 1
#   (3) phi1*phi2 > -(phi1+phi2) - 1
################################################################################

ar2_roots <- function(phi1, phi2, name) {
  cat("\n=== AR(2) Characteristic Polynomial Roots –", name, "===\n")
  # Python: np.roots([1, -(phi1+phi2), -(phi1*phi2)])
  # R poly: X^2 - (phi1+phi2)X - phi1*phi2  →  polyroot(c(-phi1*phi2, -(phi1+phi2), 1))
  roots  <- polyroot(c(-phi1 * phi2, -(phi1 + phi2), 1))
  moduli <- Mod(roots)
  for (i in seq_along(roots)) {
    root_type <- ifelse(Im(roots[i]) == 0, "Real Root", "Complex conjugate")
    cat(sprintf("  Root %d: %s | Modulus=%.6f | Inside unit disk: %s | %s\n",
                i, format(roots[i], digits = 6), moduli[i],
                ifelse(moduli[i] < 1, "Yes", "No"), root_type))
  }
  cat("Stability triangle:\n")
  cat("  (1) phi1*phi2 < 1:              ", phi1*phi2 < 1, "\n")
  cat("  (2) phi1*phi2 > (phi1+phi2)-1:  ", phi1*phi2 > (phi1+phi2)-1, "\n")
  cat("  (3) phi1*phi2 > -(phi1+phi2)-1: ", phi1*phi2 > -(phi1+phi2)-1, "\n")
  return(roots)
}

pi_ar2_roots <- ar2_roots(ar2_pi$phi1, ar2_pi$phi2, "Inflation")
i_ar2_roots  <- ar2_roots(ar2_i$phi1,  ar2_i$phi2,  "Call Money")


################################################################################
# SECTION 7 – AR(p) LAG SELECTION VIA PACF
# Python: pacf(series, nlags=10); significant if |PACF| > 1.96/sqrt(T)
# R:      pacf(series, lag.max=10, plot=FALSE)$acf; CI = 1.96/sqrt(T)
################################################################################

arp_pacf <- function(series, name) {
  series  <- na.omit(series)
  T_obs   <- length(series)
  ci_95   <- 1.96 / sqrt(T_obs)
  pf      <- pacf(series, lag.max = 10, plot = FALSE)
  cat("\n=== AR(p) PACF –", name, "(95% CI: ±", round(ci_95, 5), ") ===\n")
  df_pacf <- data.frame(
    Lag                          = 1:10,
    PACF                         = round(pf$acf[1:10], 5),
    Significant_95pct            = abs(pf$acf[1:10]) > ci_95
  )
  print(df_pacf)
  return(df_pacf)
}

pi_ARp <- arp_pacf(jp_core_trans_df$ld_hicp_sa, "Inflation (LogDiff-HICP SA)")
i_ARp  <- arp_pacf(jp_aggregated_df$call_money,  "Call Money (raw)")

# PACF plots
png("Output/Module3/fig_pacf_lag_pi.png", width = 800, height = 400)
  pacf(na.omit(jp_core_trans_df$ld_hicp_sa), lag.max = 10,
       main = "PACF – LogDiff-HICP (SA) – AR(p) Lag Selection")
dev.off()
png("Output/Module3/fig_pacf_lag_i.png", width = 800, height = 400)
  pacf(na.omit(jp_aggregated_df$call_money), lag.max = 10,
       main = "PACF – Call Money/Interbank (%) – AR(p) Lag Selection")
dev.off()


################################################################################
# SECTION 8 – 10-YEAR ROLLING WINDOW AR(1)
# Python: manual loop; window=120; OLS(pi_t, pi_tm1).fit() per window
# R:      for loop over windows; lm(y[2:w] ~ y[1:(w-1)]) per chunk
#
# Output: roll_ar1_pi (inflation), roll_ar1_i (call money) data frames
################################################################################

rolling_ar1 <- function(series, dates, window = 120, name = "") {
  series    <- na.omit(series)
  dates_vec <- dates[!is.na(series)]    # keep aligned dates
  n         <- length(series)
  phi_list  <- numeric(n - window)
  date_list <- dates_vec[(window + 1):n]

  for (end_w in (window + 1):n) {
    chunk  <- series[(end_w - window):(end_w - 1)]
    y_t    <- chunk[2:window]
    y_tm1  <- chunk[1:(window - 1)]
    fit    <- lm(y_t ~ y_tm1)
    phi_list[end_w - window] <- coef(fit)["y_tm1"]
  }

  df <- data.frame(Time = date_list, phi = phi_list)
  names(df)[2] <- paste0("rollAR1_", gsub(" ", "_", name))
  return(df)
}

cat("\nComputing 10-year rolling AR(1) — Inflation...\n")
roll_ar1_pi <- rolling_ar1(jp_core_trans_df$ld_hicp_sa,
                            jp_core_trans_df$Time, window = 120, name = "pi")

cat("Computing 10-year rolling AR(1) — Call Money...\n")
roll_ar1_i  <- rolling_ar1(jp_aggregated_df$call_money,
                            jp_aggregated_df$Time,  window = 120, name = "i")

roll_ar1_df <- full_join(roll_ar1_pi, roll_ar1_i, by = "Time")

mean_ar1_pi <- mean(roll_ar1_df$rollAR1_pi, na.rm = TRUE)
mean_ar1_i  <- mean(roll_ar1_df$rollAR1_i,  na.rm = TRUE)
cat(sprintf("Global avg AR(1) – Inflation: %.6f\n", mean_ar1_pi))
cat(sprintf("Global avg AR(1) – Call Money: %.6f\n", mean_ar1_i))

# Plot: Rolling AR(1) – Inflation
p_roll_pi <- ggplot(roll_ar1_df, aes(x = Time, y = rollAR1_pi)) +
  geom_line(color = "#393939", linewidth = 0.5, alpha = 0.8) +
  geom_ribbon(aes(ymin = pmin(rollAR1_pi, 1), ymax = 1,
                  fill = rollAR1_pi >= 1), alpha = 0.25, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#FF0000", "FALSE" = "green")) +
  geom_hline(yintercept = 1,          color = "#FF0000", linetype = "dotdash") +
  geom_hline(yintercept = mean_ar1_pi, color = "#1F9E89", linetype = "dashed") +
  labs(title = "10-Year Rolling Window AR(1) – Inflation (LogDiff-HICP SA)",
       x = "Year", y = "Rolling AR(1)") +
  theme_minimal(base_size = 11)
p_roll_pi <- ycc_vlines(p_roll_pi)
ggsave("Output/Module3/fig_roll_ar1_pi.png", p_roll_pi, width = 10, height = 5)

# Plot: Rolling AR(1) – Call Money
p_roll_i <- ggplot(roll_ar1_df, aes(x = Time, y = rollAR1_i)) +
  geom_line(color = "#393939", linewidth = 0.5, alpha = 0.8) +
  geom_hline(yintercept = 1,         color = "#FF0000", linetype = "dotdash") +
  geom_hline(yintercept = mean_ar1_i, color = "#440154", linetype = "dashed") +
  labs(title = "10-Year Rolling Window AR(1) – Call Money/Interbank (%)",
       x = "Year", y = "Rolling AR(1)") +
  theme_minimal(base_size = 11)
p_roll_i <- ycc_vlines(p_roll_i)
ggsave("Output/Module3/fig_roll_ar1_i.png", p_roll_i, width = 10, height = 5)


################################################################################
# SECTION 9 – 3-YEAR ROLLING DETERMINISTIC TREND (INFLATION)
# Python: OLS(pi_window, add_constant(t)).fit() → save params[1] (g = trend)
# R:      lm(chunk ~ seq_along(chunk)) → coef["seq_along(chunk)"]
################################################################################

cat("\nComputing 3-year rolling deterministic trend – Inflation...\n")
pi_series  <- na.omit(jp_core_trans_df$ld_hicp_sa)
pi_dates   <- jp_core_trans_df$Time[!is.na(jp_core_trans_df$ld_hicp_sa)]
n_pi       <- length(pi_series)
window_3y  <- 36
g_list     <- numeric(n_pi - window_3y)
g_dates    <- pi_dates[(window_3y + 1):n_pi]

for (end_w in (window_3y + 1):n_pi) {
  chunk   <- pi_series[(end_w - window_3y):(end_w - 1)]
  t_idx   <- seq_along(chunk)
  fit_g   <- lm(chunk ~ t_idx)
  g_list[end_w - window_3y] <- coef(fit_g)["t_idx"]
}

pi_rolltrend <- data.frame(Time = g_dates, roll_g = g_list)

p_rolltrend <- ggplot(pi_rolltrend, aes(x = Time, y = roll_g)) +
  geom_line(color = "#393939", linewidth = 0.5, alpha = 0.5) +
  geom_ribbon(aes(ymin = pmin(roll_g, 0), ymax = 0,
                  fill = roll_g >= 0), alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#440154", "FALSE" = "#1F9E89")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.4) +
  labs(title = "3-Year Rolling Window Deterministic Trend – Inflation",
       x = "Year", y = "Rolling g (time trend)") +
  theme_minimal(base_size = 11)
p_rolltrend <- ycc_vlines(p_rolltrend)
ggsave("Output/Module3/fig_roll_trend_pi.png", p_rolltrend, width = 10, height = 5)


################################################################################
# SECTION 10 – SUBPERIOD DETERMINISTIC TRENDS (AR(1) & g CORRELATION)
# Python: pi[start:end] → OLS on time index → fitted trend line
# R:      subset; lm(pi ~ t); predict()
################################################################################

pi_df <- jp_core_trans_df %>% select(Time, ld_hicp_sa) %>% na.omit() %>%
  mutate(t_idx = seq_len(n()))

# YCC Adoption: Sep 2016 – Jan 2020 (excl. COVID)
sub_ycc    <- pi_df %>% filter(Time >= as.Date("2016-09-01") & Time <= as.Date("2020-01-01"))
fit_ycc    <- lm(ld_hicp_sa ~ t_idx, data = sub_ycc)
g_ycc      <- coef(fit_ycc)["t_idx"]
sub_ycc$fitted_ycc <- predict(fit_ycc)

# Post-Financial Crisis: Jan 2008 – Jan 2013
sub_crisis <- pi_df %>% filter(Time >= as.Date("2008-01-01") & Time <= as.Date("2013-01-01"))
fit_crisis <- lm(ld_hicp_sa ~ t_idx, data = sub_crisis)
g_crisis   <- coef(fit_crisis)["t_idx"]
sub_crisis$fitted_crisis <- predict(fit_crisis)

cat(sprintf("\nYCC Adoption trend g      = %.6f\n", g_ycc))
cat(sprintf("Post-Crisis   trend g      = %.6f\n", g_crisis))

p_subtrend <- ggplot(pi_df, aes(x = Time, y = ld_hicp_sa)) +
  geom_line(color = "#393939", linewidth = 0.4, alpha = 0.5) +
  geom_line(data = sub_ycc,    aes(y = fitted_ycc),    color = "#1F9E89", linewidth = 1.8) +
  geom_line(data = sub_crisis, aes(y = fitted_crisis),  color = "#440154", linewidth = 1.8) +
  geom_rect(aes(xmin = as.Date("2016-09-01"), xmax = as.Date("2020-01-01"),
                ymin = -Inf, ymax = Inf), fill = "#1F9E89", alpha = 0.03, inherit.aes = FALSE) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), xmax = as.Date("2013-01-01"),
                ymin = -Inf, ymax = Inf), fill = "#440154", alpha = 0.03, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.4) +
  labs(title    = "Deterministic Trend on Unit Root Subperiods – Inflation",
       subtitle = expression(ln(pi[t+1]) == a + gt + epsilon),
       x = "Year", y = expression(pi[t])) +
  theme_minimal(base_size = 11)
p_subtrend <- ycc_vlines(p_subtrend)
ggsave("Output/Module3/fig_subperiod_trend.png", p_subtrend, width = 13, height = 5)


################################################################################
# SECTION 11 – POST-YCC UNIT ROOT TESTING
# Python: df[df["Time"] >= "2016-01"] → adfuller + PhillipsPerron
# R:      filter Time >= "2016-01-01"; ur.df + ur.pp
################################################################################

cat("\n=== POST-YCC ADF + PP Unit Root Tests (from 2016-01) ===\n")
df_postycc <- jp_core_hybrid_df %>% filter(Time >= as.Date("2016-01-01"))

for (vname in c("ld_hicp_sa","call_money","gb10y")) {
  unit_root_test(na.omit(df_postycc[[vname]]), paste("post-YCC:", vname))
}


################################################################################
# SECTION 12 – LAGGED CROSS-CORRELATION
# Python: xcorr + 10-year rolling corr(i_t, pi_{t+k}) for k = -2..2
# R:      ccf() for full-sample; manual rolling corr per lag k
################################################################################

# Full-sample cross-correlogram
cat("\n=== Cross-Correlation: call_money & ld_hicp_sa (lags ±5) ===\n")
df_ccf <- jp_core_hybrid_df %>% select(ld_hicp_sa, call_money) %>% na.omit()
png("Output/Module3/fig_ccf.png", width = 800, height = 400)
  ccf(df_ccf$call_money, df_ccf$ld_hicp_sa, lag.max = 5,
      main = "CCF: call_money vs ld_hicp_sa (±5 lags)")
dev.off()

# 10-year rolling cross-correlations for k = -2, -1, 0, +1, +2
cat("Computing 10-year rolling cross-correlations...\n")
df_xcorr    <- jp_core_hybrid_df %>% select(Time, ld_hicp_sa, call_money) %>% na.omit()
n_xcorr     <- nrow(df_xcorr)
window_xcr  <- 120
lag_values  <- c(-2, -1, 0, 1, 2)

rollcorr_list <- list()
for (k in lag_values) {
  corr_vec <- rep(NA_real_, n_xcorr)
  for (end_w in (window_xcr + 1):n_xcorr) {
    i_chunk  <- df_xcorr$call_money[(end_w - window_xcr):(end_w - 1)]
    pi_chunk <- df_xcorr$ld_hicp_sa[(end_w - window_xcr):(end_w - 1)]
    # Python: if k<0: corr(i_t, pi.shift(k)) i.e. pi leads by |k|
    #          if k>0: corr(i.shift(-k), pi)    i.e. i lags by k
    if (k == 0) {
      r_val <- cor(i_chunk, pi_chunk, use = "complete.obs")
    } else if (k < 0) {
      # pi leads: shift pi left by |k|  → use pi[1:(w-|k|)] with i[1:(w-|k|)]
      abs_k <- abs(k)
      r_val <- tryCatch(cor(i_chunk[1:(window_xcr - abs_k)],
                            pi_chunk[(abs_k + 1):window_xcr], use = "complete.obs"),
                        error = function(e) NA)
    } else {
      # i lags: shift i right by k → use i[1:(w-k)] with pi[1:(w-k)]
      r_val <- tryCatch(cor(i_chunk[1:(window_xcr - k)],
                            pi_chunk[(k + 1):window_xcr], use = "complete.obs"),
                        error = function(e) NA)
    }
    corr_vec[end_w] <- r_val
  }
  rollcorr_list[[as.character(k)]] <- corr_vec
}

rollcorr_df <- data.frame(
  Time = df_xcorr$Time,
  as.data.frame(rollcorr_list) %>%
    rename_with(~ paste0("k_", .), everything())
)

rollcorr_long <- rollcorr_df %>%
  pivot_longer(cols = starts_with("k_"), names_to = "lag", values_to = "corr") %>%
  mutate(lag = recode(lag, "k_-2"="k=-2","k_-1"="k=-1","k_0"="k=0","k_1"="k=+1","k_2"="k=+2")) %>%
  na.omit()

p_rollcorr <- ggplot(rollcorr_long, aes(x = Time, y = corr, color = lag)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = c("k=-2"="#440154","k=-1"="#3B528B","k=0"="#21908C",
                                 "k=+1"="#5DC863","k=+2"="#FDE725")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.6) +
  ylim(-1, 1) +
  labs(title = "10-Year Rolling Cross-Correlations: Policy Instrument & Lagged Inflation",
       x = "Year", y = "Corr(i_t, π_{t+k})", color = "Lag k") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_rollcorr <- ycc_vlines(p_rollcorr)
ggsave("Output/Module3/fig_rollcorr.png", p_rollcorr, width = 14, height = 6)


################################################################################
# SECTION 13 – FIRST-ORDER SISO MODEL (ROLLING 10-YEAR WINDOW)
# Python: π_t = A π_{t-1} + B i_{t-1} + u_t   AND   i_{t-1} = F π_{t-1} + e
# R:      lm(pi_t ~ pi_tm1 + i_tm1) + lm(i_tm1 ~ pi_tm1) per window
################################################################################

cat("\nComputing 10-year rolling SISO model...\n")
df_siso    <- jp_core_hybrid_df %>% select(Time, ld_hicp_sa, call_money) %>% na.omit()
n_siso     <- nrow(df_siso)
siso_results <- vector("list", n_siso - window_xcr)

for (start in (window_xcr + 1):n_siso) {
  chunk  <- df_siso[(start - window_xcr):(start - 1), ]
  pi_t   <- chunk$ld_hicp_sa[2:window_xcr]
  pi_tm1 <- chunk$ld_hicp_sa[1:(window_xcr - 1)]
  i_tm1  <- chunk$call_money[1:(window_xcr - 1)]

  # SISO Eq 1: i_{t-1} = F * pi_{t-1} + e
  fit_F   <- lm(i_tm1  ~ pi_tm1)
  F_val   <- coef(fit_F)["pi_tm1"]
  e_gamma <- tryCatch({
    e   <- fit_F$residuals
    cor(e[-1], e[-length(e)])
  }, error = function(e) NA)

  # SISO Eq 2: π_t = A * π_{t-1} + B * i_{t-1} + u
  fit_AB  <- lm(pi_t ~ pi_tm1 + i_tm1)
  A_val   <- coef(fit_AB)["pi_tm1"]
  B_val   <- coef(fit_AB)["i_tm1"]
  u_rho   <- tryCatch({
    u   <- fit_AB$residuals
    cor(u[-1], u[-length(u)])
  }, error = function(e) NA)

  siso_results[[start - window_xcr]] <- data.frame(
    Time    = df_siso$Time[start],
    A       = A_val, B = B_val, F = F_val,
    e_gamma = e_gamma, u_rho = u_rho
  )
}

abf_10roll_df <- bind_rows(siso_results) %>%
  mutate(lambda = A + B * F)

# Plot: SISO coefficients A, B, F, λ
siso_long <- abf_10roll_df %>%
  select(Time, A, B, F, lambda) %>%
  pivot_longer(-Time, names_to = "coef", values_to = "value")

p_siso <- ggplot(siso_long, aes(x = Time, y = value, color = coef)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("A"="#440154","B"="#3B528B","F"="#21908C","lambda"="#FDE725")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.6) +
  ylim(-10, 10) +
  labs(title = "10-Year Rolling Window First-order SISO Coefficients (A, B, F, λ=A+BF)",
       x = "Year", y = "Coefficient", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_siso <- ycc_vlines(p_siso)
ggsave("Output/Module3/fig_siso_abf.png", p_siso, width = 14, height = 7)


################################################################################
# SECTION 14 & 15 – AR(1) vs λ=A+BF COMPARISON & LUCAS CRITIQUE
################################################################################

abf_ar1_df <- abf_10roll_df %>%
  left_join(roll_ar1_pi, by = "Time") %>%
  rename(rollAR1_pi = starts_with("rollAR1"))

p_siso_ar1 <- ggplot(abf_ar1_df) +
  geom_line(aes(x = Time, y = lambda,      color = "SISO λ=A+BF"),   linewidth = 0.8) +
  geom_line(aes(x = Time, y = rollAR1_pi,  color = "Rolling AR(1)"), linewidth = 0.8) +
  scale_color_manual(values = c("SISO λ=A+BF"="#21908C","Rolling AR(1)"="#440154")) +
  geom_hline(yintercept = c(0, 1), linetype = c("dashed","dotdash"),
             color = c("#393939","#FF0000"), alpha = 0.6) +
  labs(title = "10-Year Rolling Window: SISO λ=A+BF vs AR(1)",
       x = "Year", y = "Coefficient", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_siso_ar1 <- ycc_vlines(p_siso_ar1)
ggsave("Output/Module3/fig_siso_vs_ar1.png", p_siso_ar1, width = 14, height = 7)

# Lucas Critique: negative feedback regions (0 < A+BF < A)
abf_10roll_df <- abf_10roll_df %>%
  mutate(neg_feedback = lambda > 0 & lambda < A)

siso_lucas_long <- abf_10roll_df %>%
  select(Time, A, B, F, lambda) %>%
  pivot_longer(-Time, names_to = "coef", values_to = "value")

p_lucas <- ggplot(siso_lucas_long, aes(x = Time, y = value, color = coef)) +
  geom_line(linewidth = 0.8) +
  geom_rect(data = abf_10roll_df %>% filter(neg_feedback),
            aes(xmin = Time, xmax = Time + 30, ymin = -10, ymax = 10),
            fill = "green", alpha = 0.08, inherit.aes = FALSE) +
  scale_color_manual(values = c("A"="#440154","B"="#3B528B","F"="#21908C","lambda"="#FDE725")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.6) +
  ylim(-10, 10) +
  labs(title = "Lucas Critique: SISO Coefficients & λ=A+BF (Negative Feedback Regions)",
       x = "Year", y = "Coefficient", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_lucas <- ycc_vlines(p_lucas)
ggsave("Output/Module3/fig_lucas_critique.png", p_lucas, width = 14, height = 7)


################################################################################
# SECTION 16 – SISO WITH LOG-DIFF POLICY INSTRUMENT
# Python: replace ±Inf with NaN; use ld_call_money instead of call_money
################################################################################

jp_trans_df <- jp_trans_df %>%
  mutate(ld_call_money = ifelse(is.infinite(ld_call_money) | is.nan(ld_call_money),
                                NA, ld_call_money))

# AR(1) + unit root on logdiff variables
cat("\n=== AR(1) + Unit Root – LogDiff Policy Instrument ===\n")
ar1_ld <- ar1_estimate(na.omit(jp_trans_df$ld_call_money), "LogDiff-Call Money")
unit_root_test(na.omit(jp_trans_df$ld_call_money), "LogDiff-Call Money")

# Rolling SISO with ld_call_money
cat("Computing 10-year rolling SISO (logdiff instrument)...\n")
df_siso_ld <- jp_trans_df %>% select(Time, ld_hicp_sa, ld_call_money) %>% na.omit()
n_siso_ld  <- nrow(df_siso_ld)
siso_ld_results <- vector("list", n_siso_ld - window_xcr)

for (start in (window_xcr + 1):n_siso_ld) {
  chunk  <- df_siso_ld[(start - window_xcr):(start - 1), ]
  pi_t   <- chunk$ld_hicp_sa[2:window_xcr]
  pi_tm1 <- chunk$ld_hicp_sa[1:(window_xcr - 1)]
  ld_i   <- chunk$ld_call_money[1:(window_xcr - 1)]
  if (sum(!is.na(ld_i)) < window_xcr / 2) {
    siso_ld_results[[start - window_xcr]] <- NULL; next
  }
  fit_F  <- lm(ld_i  ~ pi_tm1)
  F_val  <- coef(fit_F)["pi_tm1"]
  fit_AB <- lm(pi_t ~ pi_tm1 + ld_i)
  A_val  <- coef(fit_AB)["pi_tm1"]
  B_val  <- coef(fit_AB)["ld_i"]
  siso_ld_results[[start - window_xcr]] <- data.frame(
    Time = df_siso_ld$Time[start], A = A_val, B = B_val, F = F_val
  )
}

abf_ld_df <- bind_rows(siso_ld_results) %>%
  mutate(lambda = A + B * F)

siso_ld_long <- abf_ld_df %>%
  filter(Time >= as.Date("2000-01-01")) %>%
  select(Time, A, B, F, lambda) %>%
  pivot_longer(-Time, names_to = "coef", values_to = "value")

p_siso_ld <- ggplot(siso_ld_long, aes(x = Time, y = value, color = coef)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("A"="#440154","B"="#3B528B","F"="#21908C","lambda"="#FDE725")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.6) +
  labs(title = "10-Year Rolling SISO Coefficients – LogDiff Policy Instrument (2000+)",
       x = "Year", y = "Coefficient", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_siso_ld <- ycc_vlines(p_siso_ld)
ggsave("Output/Module3/fig_siso_logdiff.png", p_siso_ld, width = 14, height = 7)


################################################################################
# SECTION 17 – ROLLING r, σ_π/σ_i, F̂_π
# Python: r = corr(pi, i); ratio = sd(pi)/sd(i); F_hat = r * sd(i)/sd(pi)
################################################################################

cat("Computing 10-year rolling r, sigma ratio, F_hat...\n")
df_stats   <- jp_core_hybrid_df %>% select(Time, ld_hicp_sa, call_money) %>% na.omit()
n_stats    <- nrow(df_stats)
stats_results <- vector("list", n_stats - window_xcr)

for (start in (window_xcr + 1):n_stats) {
  chunk  <- df_stats[(start - window_xcr):(start - 1), ]
  r_val  <- cor(chunk$ld_hicp_sa, chunk$call_money, use = "complete.obs")
  s_pi   <- sd(chunk$ld_hicp_sa, na.rm = TRUE)
  s_i    <- sd(chunk$call_money,  na.rm = TRUE)
  stats_results[[start - window_xcr]] <- data.frame(
    Time        = df_stats$Time[start],
    r           = r_val,
    sigma_ratio = s_pi / s_i,
    F_hat       = r_val * (s_i / s_pi)
  )
}

stats_roll10 <- bind_rows(stats_results)

p_r     <- ggplot(stats_roll10, aes(x = Time, y = r))     + geom_line(color="#440154", linewidth=0.8) +
  geom_hline(yintercept = 0, linetype="dashed", color="#393939", alpha=0.6) +
  labs(title = "10-Year Rolling r(π_t, i_t)", x = "Year", y = "r") +
  theme_minimal(base_size = 11)
p_sigma <- ggplot(stats_roll10, aes(x = Time, y = sigma_ratio)) + geom_line(color="#21908C", linewidth=0.8) +
  geom_hline(yintercept = 0, linetype="dashed", color="#393939", alpha=0.6) +
  labs(title = "10-Year Rolling σ_π/σ_i", x = "Year", y = "σ_π/σ_i") +
  theme_minimal(base_size = 11)
p_Fhat  <- ggplot(stats_roll10, aes(x = Time, y = F_hat)) + geom_line(color="#FDE725", linewidth=0.8) +
  geom_hline(yintercept = 0, linetype="dashed", color="#393939", alpha=0.6) +
  labs(title = "10-Year Rolling F̂_π = r × σ_i/σ_π", x = "Year", y = "F̂") +
  theme_minimal(base_size = 11)

p_stats_combined <- (ycc_vlines(p_r) / ycc_vlines(p_sigma) / ycc_vlines(p_Fhat)) +
  plot_annotation(title = "10-Year Rolling Window: r, σ_π/σ_i, F̂_π")
ggsave("Output/Module3/fig_roll_stats.png", p_stats_combined, width = 14, height = 12)


################################################################################
# SECTION 18 – INDIRECT LEAST SQUARES (ILS)
# Python: structural system → reduced forms → F̂ = β_iy/β_πy, B̂ = β_πz/β_iz
# R:      two lm() reduced forms; recover structural parameters as ratios
################################################################################

df_ils <- jp_trans_df %>%
  select(Time, ld_hicp_sa, call_money, hp_rgdp, ld_reserves) %>%
  na.omit()

cat("\n=== ILS – Reduced Form 1: π_t on instruments ===\n")
rf_pi  <- lm(ld_hicp_sa ~ hp_rgdp + ld_reserves, data = df_ils)
print(summary(rf_pi))
b_pi_y <- coef(rf_pi)["hp_rgdp"]
b_pi_z <- coef(rf_pi)["ld_reserves"]

cat("\n=== ILS – Reduced Form 2: i_t on instruments ===\n")
rf_i   <- lm(call_money ~ hp_rgdp + ld_reserves, data = df_ils)
print(summary(rf_i))
b_i_y  <- coef(rf_i)["hp_rgdp"]
b_i_z  <- coef(rf_i)["ld_reserves"]

F_hat_ils <- b_i_y / b_pi_y
B_hat_ils <- b_pi_z / b_i_z
cat(sprintf("\nF̂ = β_iy/β_πy = %.6f  (feedback rule slope)\n", F_hat_ils))
cat(sprintf("B̂ = β_πz/β_iz = %.6f  (policy instrument slope)\n", B_hat_ils))

ils_params <- data.frame(
  Parameter = c("β_πy","β_πz","β_iy","β_iz","F̂=β_iy/β_πy","B̂=β_πz/β_iz"),
  Value     = round(c(b_pi_y, b_pi_z, b_i_y, b_i_z, F_hat_ils, B_hat_ils), 4)
)
print(ils_params)

# Instrument Combination Search (single instruments, maximise joint R²)
cat("\n=== ILS Instrument Search – Single Instruments ===\n")
cand_vars <- c("hp_rgdp","ld_reserves","ld_reer","ld_spot","ld_boj_assets",
               "ld_m1_jpy","ld_m2_jpy","ld_m3_jpy","ar1_gb10y",
               "ar1_nir_1y","ar1_nir_10y","ar1_credit_pnf","ar1_credit_gg",
               "ar1_credit_hh","ar1_cg_debt","ar1_dp_debt","ar1_du_debt",
               "ar1_loan_rate","ar1_dep_rate","ar1_ust_10y","ar1_vix")

search_df <- jp_trans_df %>% select(all_of(c("ld_hicp_sa","call_money", cand_vars))) %>% na.omit()

combo_results <- map_dfr(cand_vars, function(v) {
  if (!(v %in% names(search_df))) return(NULL)
  r2_pi <- tryCatch(summary(lm(reformulate(v, "ld_hicp_sa"), data = search_df))$r.squared, error = function(e) NA)
  r2_i  <- tryCatch(summary(lm(reformulate(v, "call_money"),  data = search_df))$r.squared, error = function(e) NA)
  data.frame(Instrument = v, R2_pi = round(r2_pi, 4), R2_i = round(r2_i, 4),
             R2_global = round(r2_pi + r2_i, 4))
}) %>% arrange(desc(R2_global))

cat("Top 10 instrument combinations (by R²_global):\n")
print(head(combo_results, 10))


################################################################################
# SECTION 19 – ROLLING REGRESSION WITH 6-QUARTER LAGS
# Python: π_{t+3} = A π_{t-3} + B i_{t-18} + u; i_t = F π_t + u
# R:      lead_pi = lead(pi, 3); lag_pi = lag(pi, 3); lag_i = lag(i, 18)
################################################################################

cat("\nComputing 6-quarter lag SISO rolling regression...\n")
df_6q <- jp_core_hybrid_df %>%
  select(Time, ld_hicp_sa, call_money) %>%
  arrange(Time) %>%
  mutate(
    pi_lead3 = lead(ld_hicp_sa, 3),
    pi_lag3  = lag(ld_hicp_sa, 3),
    i_lag18  = lag(call_money, 18)
  ) %>%
  na.omit()

n_6q <- nrow(df_6q)
siso_6q_results <- vector("list", n_6q - window_xcr)

for (start in (window_xcr + 1):n_6q) {
  chunk <- df_6q[(start - window_xcr):(start - 1), ]

  # SISO Eq 1: π_{t+3} = A π_{t-3} + B i_{t-18}
  fit1  <- lm(pi_lead3 ~ pi_lag3 + i_lag18, data = chunk)
  A_val <- coef(fit1)["pi_lag3"]
  B_val <- coef(fit1)["i_lag18"]
  u_rho <- tryCatch({
    u <- fit1$residuals; cor(u[-1], u[-length(u)])
  }, error = function(e) NA)

  # SISO Eq 2: i_t = F π_t
  fit2  <- lm(call_money ~ ld_hicp_sa, data = chunk)
  F_val <- coef(fit2)["ld_hicp_sa"]

  siso_6q_results[[start - window_xcr]] <- data.frame(
    Time = df_6q$Time[start], A = A_val, B = B_val, F = F_val, u_rho = u_rho
  )
}

results_6qlag_df <- bind_rows(siso_6q_results) %>%
  mutate(lambda = A + B * F, neg_feedback = B < 0)

siso_6q_long <- results_6qlag_df %>%
  select(Time, A, B, F, lambda) %>%
  pivot_longer(-Time, names_to = "coef", values_to = "value")

p_siso_6q <- ggplot(siso_6q_long, aes(x = Time, y = value, color = coef)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("A"="#440154","B"="#3B528B","F"="#21908C","lambda"="#FDE725")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#393939", alpha = 0.6) +
  labs(title = "6-Quarter Lag SISO Rolling Window Coefficients (A, B, F, λ=A+BF)",
       x = "Year", y = "Coefficient", color = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
p_siso_6q <- ycc_vlines(p_siso_6q)
ggsave("Output/Module3/fig_siso_6q.png", p_siso_6q, width = 14, height = 7)

cat("\nModule 3 complete. All outputs saved to Output/Module3/\n")
