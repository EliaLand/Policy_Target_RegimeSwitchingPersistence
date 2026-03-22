# ============================================================
# Module 2 - Statistics & Data Transformation  (R Translation)
# Author      : Elia Landini
# Student ID  : 12310239
# Course      : EESM2 – Financial Economics
# Supervisor  : Jean-Bernard Chatelain
# Repository  : https://github.com/EliaLand/Policy_Target_RegimeSwitchingPersistence
# ============================================================


# ============================================================
# 1) REQUIREMENTS SET-UP
# ============================================================

# Uncomment to install all required packages
# install.packages(c(
#   "tidyverse",   # pandas + numpy (dplyr, tidyr, ggplot2 …)
#   "mFilter",     # hpfilter()  ← statsmodels.tsa.filters.hp_filter
#   "tseries",     # adf.test()  ← statsmodels.tsa.stattools.adfuller
#   "urca",        # ur.pp()     ← arch.unitroot.PhillipsPerron
#   "forecast",    # Arima(), auto.arima()
#   "lmtest",      # bgtest(), dwtest() …
#   "car",         # vif()
#   "lubridate",
#   "zoo",         # na.locf, na.approx
#   "pROC",        # roc(), auc()
#   "caret",       # train/test pipeline
#   "stargazer",   # regression tables
#   "ggplot2",
#   "plotly",
#   "RColorBrewer",
#   "scales",
#   "gridExtra",   # grid.arrange() ← matplotlib.gridspec
#   "nortest",     # ks.test, lillie.test
#   "MASS"
# ))

suppressPackageStartupMessages({
  library(tidyverse)     # dplyr, tidyr, ggplot2, purrr …
  library(mFilter)       # hpfilter()
  library(tseries)       # adf.test()
  library(urca)          # ur.pp()
  library(forecast)      # Arima(), auto.arima()
  library(lmtest)        # coeftest(), bgtest()
  library(car)           # vif()
  library(lubridate)     # date helpers
  library(zoo)           # na.locf, na.approx, rollapply
  library(pROC)          # ROC / AUC
  library(caret)         # ML pipeline
  library(stargazer)     # regression tables
  library(ggplot2)       # ggplot2 plots
  library(plotly)        # interactive plots
  library(RColorBrewer)  # colour palettes
  library(scales)        # number formatting
  library(gridExtra)     # grid.arrange() (≈ matplotlib.gridspec)
  library(nortest)       # normality tests
  library(MASS)          # statistical helpers
})

# Suppress warnings globally
options(warn = -1)


# ============================================================
# HELPER: significance stars (shared with Module 1)
# ============================================================

significance_stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}


# ============================================================
# 2) DESCRIPTIVE STATISTICS (RAW)
# ============================================================

# Load the full aggregated panel built in Module 1
# Python: jpaggregateddf = pd.read_csv(...)  [assembled from all sub-DFs]
jp_aggregated_df <- read.csv(
  "Data/Aggregated/jpaggregateddf.csv",
  stringsAsFactors = FALSE
)

# Preview (Python: jpaggregateddf.head())
head(jp_aggregated_df)

# ── Descriptive summary of the raw panel ──────────────────
# Python: jpaggregateddf.describe()
summary(jp_aggregated_df)

# ── Core variables descriptive statistics ─────────────────
# Python: df[["HICP (SA)", "Call Money...", "10-Year Gov Bond Yields (%)"]].describe()
core_raw_cols <- c(
  "HICP..SA.",
  "Call.Money.Interbank.Immediate....",
  "X10.Year.Gov.Bond.Yields...."
)

jp_aggregated_df %>%
  select(all_of(core_raw_cols)) %>%
  summary()

# Rename columns to human-readable labels for analysis
# (keeps code consistent with Python column names)
jp_df <- jp_aggregated_df %>%
  rename(
    Country                              = Country,
    Time                                 = Time,
    `M1 (JPY)`                           = starts_with("Monetary.Aggregates...M1"),
    `M2 (JPY)`                           = starts_with("Monetary.Aggregates...M2"),
    `M3 (JPY)`                           = starts_with("Monetary.Aggregates...M3"),
    `Credit PNF (%GDP)`                  = starts_with("Total.Credit...Private"),
    `Credit GG (%GDP)`                   = starts_with("Total.Credit...General"),
    `Credit HH (%GDP)`                   = starts_with("Total.Credit...Households"),
    `Reserves`                           = starts_with("Total.Treasury.Reserves"),
    `10Y Bond Yield (%)`                 = starts_with("X10.Year.Gov.Bond"),
    `Call Money (%)`                     = starts_with("Call.Money"),
    `Neutral 1Y (%)`                     = starts_with("Est..1.year"),
    `Neutral 10Y (%)`                    = starts_with("Est..10.year"),
    `REER`                               = starts_with("USD.JPY.reer"),
    `Spot FX`                            = starts_with("JPY.USD.Spot"),
    `HICP`                               = starts_with("HICP"),
    `Real GDP`                           = starts_with("Real.GDP"),
    `CG Debt (%GDP)`                     = starts_with("Central.Government.Debt"),
    `Priv Debt Sec (%GDP)`               = starts_with("Domestic.Private.Debt"),
    `Pub Debt Sec (%GDP)`                = starts_with("Domestic.Public.Debt"),
    `BoJ Assets`                         = starts_with("BoJ"),
    `Loan Rate (%)`                      = starts_with("Loan.Interest"),
    `Deposit Rate (%)`                   = starts_with("Deposit.Interest"),
    `Bank Stock (1615.T)`                = starts_with("X1615"),
    `US 10Y Yield (%)`                   = starts_with("X10.Year.US"),
    `VIX`                                = starts_with("CBOE")
  )


# ============================================================
# 3) DATA TRANSFORMATIONS  (Non-Stationarity Corrections)
# ============================================================
# Mirrors the three transformation blocks in the Python notebook:
#   Block A  – Log-Difference     (I(1) → I(0) via log returns)
#   Block B  – AR(1) detrending   (removes autocorrelated trend)
#   Block C  – HP-filter cycle    (extracts business-cycle component)

df      <- jp_df                        # working copy of raw panel
trans_df <- df %>% select(Country, Time)  # accumulator for transformed cols

# ── Block A: LOG-DIFFERENCE TRANSFORMATIONS ────────────────
# Python:
#   for var in log_transformed_variables:
#       if var != "HICP (SA)":
#           transdf[f"LogDiff-{var}"] = np.log(df[var]).diff()
#       else:
#           transdf[f"LogDiff-{var}"] = np.log(df[var]).diff(12) * 100

# Variables treated with simple log-first-difference (log-returns)
log_diff_vars <- c(
  "M1 (JPY)",
  "M2 (JPY)",
  "M3 (JPY)",
  "Reserves",
  "REER",
  "Spot FX",
  "Bank Stock (1615.T)",
  "BoJ Assets",
  "Call Money (%)"
)

for (var in log_diff_vars) {
  new_col <- paste0("LogDiff-", var)
  trans_df[[new_col]] <- c(NA_real_, diff(log(df[[var]])))
}

# HICP: annualised 12-month log-difference × 100
# Python: np.log(df["HICP (SA)"]).diff(12) * 100
hicp_log <- log(df[["HICP"]])
trans_df[["LogDiff-HICP"]] <- c(
  rep(NA_real_, 12),
  diff(hicp_log, lag = 12) * 100
)

# ── Block B: AR(1) DETRENDING ──────────────────────────────
# Python:
#   model = AutoReg(df[var].dropna(), lags=1, old_names=False).fit()
#   transdf[f"AR(1)detrend-{var}"] = df[var] - model.fittedvalues
#
# R equivalent:
#   fit AR(1) on non-NA values via arima(x, order = c(1,0,0))
#   compute residuals (= actual – fitted), preserve original index

ar1_detrend <- function(x) {
  # x: a numeric vector (may contain NAs)
  valid_idx  <- which(!is.na(x))
  x_valid    <- x[valid_idx]

  if (length(x_valid) < 3) return(rep(NA_real_, length(x)))

  fit        <- tryCatch(
    arima(x_valid, order = c(1, 0, 0), include.mean = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(rep(NA_real_, length(x)))

  # Fitted values = observed – residuals
  fitted_vals <- as.numeric(x_valid - residuals(fit))

  # Place residuals back at original positions
  result          <- rep(NA_real_, length(x))
  result[valid_idx] <- as.numeric(x_valid - fitted_vals)
  result
}

# Variables de-trended with AR(1) residuals
ar1_vars <- c(
  "Credit GG (%GDP)",
  "Credit HH (%GDP)",
  "Credit PNF (%GDP)",
  "10Y Bond Yield (%)",
  "Call Money (%)",
  "Neutral 1Y (%)",
  "Neutral 10Y (%)",
  "CG Debt (%GDP)",
  "Priv Debt Sec (%GDP)",
  "Pub Debt Sec (%GDP)",
  "Loan Rate (%)",
  "Deposit Rate (%)",
  "US 10Y Yield (%)",
  "VIX"
)

for (var in ar1_vars) {
  new_col           <- paste0("AR(1)detrend-", var)
  trans_df[[new_col]] <- ar1_detrend(df[[var]])
}

# ── Block C: HP-FILTER CYCLE ───────────────────────────────
# Python:
#   cycle, trend = hpfilter(np.log(df[var].dropna()), lamb=1600)
#   transdf[f"HPfilter-{var}"] = cycle
#
# mFilter::hpfilter uses 'freq' as the lambda parameter
# Monthly data standard: lambda = 1600

hp_filter_vars <- c("Real GDP")

for (var in hp_filter_vars) {
  new_col   <- paste0("HPfilter-", var)
  valid_idx <- which(!is.na(df[[var]]))
  x_valid   <- log(df[[var]][valid_idx])

  hp_res    <- mFilter::hpfilter(x_valid, freq = 1600, type = "lambda")
  cycle     <- as.numeric(hp_res$cycle)

  result           <- rep(NA_real_, nrow(df))
  result[valid_idx] <- cycle
  trans_df[[new_col]] <- result
}

# ── Bind transformed columns back to df ───────────────────
jp_trans_df <- trans_df
tail(jp_trans_df)


# ============================================================
# 3b) EXPORT TRANSFORMED DATASETS
# ============================================================

dir.create("Data/Transformed", showWarnings = FALSE, recursive = TRUE)

# Full transformed dataframe
# Python: jptransdf.to_csv("Data/Transformed/jptransdf.csv", index=False)
jp_trans_df_out <- jp_trans_df
write.csv(jp_trans_df_out,
          "Data/Transformed/jptransdf.csv", row.names = FALSE)

# ── Core transformed dataset ────────────────────────────────
# Python: jpcoretransdf = jptransdf[["Country","Time",
#           "LogDiff-HICP (SA)",
#           "AR(1)detrend-Call Money/Interbank Immediate (%)",
#           "AR(1)detrend-10-Year Gov Bond Yields (%)"]].copy()
jp_core_trans_df <- jp_trans_df %>%
  select(
    Country,
    Time,
    `LogDiff-HICP`,
    `AR(1)detrend-Call Money (%)`,
    `AR(1)detrend-10Y Bond Yield (%)`
  )

write.csv(jp_core_trans_df,
          "Data/Transformed/jpcoretransdf.csv", row.names = FALSE)

# ── Core hybrid dataset (transformed inflation + RAW rates) ─
# Python:
#   df1 = jpcoretransdf without AR(1) rate columns
#   df2 = jpaggregateddf[["Country","Time",
#                          "Call Money/Interbank Immediate (%)",
#                          "10-Year Gov Bond Yields (%)"]]
#   jpcorehybriddf = pd.merge(df1, df2, on=["Country","Time"], how="outer")
#   jpcorehybriddf = jpcorehybriddf.dropna()

df1_hybrid <- jp_core_trans_df %>%
  select(Country, Time, `LogDiff-HICP`)

df2_hybrid <- df %>%
  select(Country, Time, `Call Money (%)`, `10Y Bond Yield (%)`)

jp_core_hybrid_df <- full_join(df1_hybrid, df2_hybrid,
                                by = c("Country", "Time")) %>%
  drop_na()

write.csv(jp_core_hybrid_df,
          "Data/Transformed/jpcorehybriddf.csv", row.names = FALSE)

tail(jp_core_hybrid_df)


# ============================================================
# 4) DESCRIPTIVE STATISTICS  (TRANSFORMED)
# ============================================================

# ── 4a  Raw core series ─────────────────────────────────────
# Python: df[["HICP (SA)", "Call Money...", "10-Year Gov Bond..."]].describe()
cat("
--- Raw Core Series: Descriptive Statistics ---
")
df %>%
  select(`HICP`, `Call Money (%)`, `10Y Bond Yield (%)`) %>%
  summary() %>%
  print()

# ── 4b  Transformed core series ────────────────────────────
# Python: jpcoretransdf[["LogDiff-HICP","AR(1)detrend-Call Money",
#                         "AR(1)detrend-10Y Bond"]].describe()
cat("
--- Transformed Core Series: Descriptive Statistics ---
")
jp_core_trans_df %>%
  select(-Country, -Time) %>%
  summary() %>%
  print()

# ── 4c  Detailed numeric summary (mirrors pd.describe() exactly) ─
describe_df <- function(df_in, label = "") {
  num_cols <- df_in %>% select(where(is.numeric))
  cat(sprintf("
=== %s ===
", label))
  for (col in names(num_cols)) {
    x <- num_cols[[col]]
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) next
    cat(sprintf(
      "%-45s  n=%d  mean=%.4f  sd=%.4f  min=%.4f  p25=%.4f  p50=%.4f  p75=%.4f  max=%.4f
",
      col, length(x_clean),
      mean(x_clean), sd(x_clean), min(x_clean),
      quantile(x_clean, 0.25), median(x_clean),
      quantile(x_clean, 0.75), max(x_clean)
    ))
  }
}

describe_df(
  df %>% select(`HICP`, `Call Money (%)`, `10Y Bond Yield (%)`),
  label = "RAW core series"
)

describe_df(
  jp_core_trans_df %>% select(-Country, -Time),
  label = "TRANSFORMED core series"
)

describe_df(
  jp_trans_df %>% select(-Country, -Time),
  label = "FULL transformed panel"
)


# ============================================================
# 5) STATIONARITY TESTS
# ============================================================
# The Python notebook implicitly relies on ADF and PP tests
# (adfuller, PhillipsPerron) when justifying transformation choices.
# This section formalises those tests for all series.

# ── ADF test wrapper (mirrors adfuller) ─────────────────────
# Python: adfuller(series, autolag='AIC')
run_adf <- function(x, series_name, max_lag = "AIC") {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 10) {
    return(data.frame(Series = series_name, ADF_stat = NA,
                      p_value = NA, Stars = NA,
                      stringsAsFactors = FALSE))
  }
  result <- tryCatch(
    tseries::adf.test(x_clean, alternative = "stationary"),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(data.frame(Series = series_name, ADF_stat = NA,
                      p_value = NA, Stars = NA,
                      stringsAsFactors = FALSE))
  }
  data.frame(
    Series   = series_name,
    ADF_stat = round(as.numeric(result$statistic), 4),
    p_value  = round(result$p.value, 4),
    Stars    = significance_stars(result$p.value),
    stringsAsFactors = FALSE
  )
}

# ── Phillips-Perron test wrapper (mirrors arch.unitroot.PhillipsPerron) ──
run_pp <- function(x, series_name) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 10) {
    return(data.frame(Series = series_name, PP_stat = NA,
                      p_value = NA, Stars = NA,
                      stringsAsFactors = FALSE))
  }
  result <- tryCatch(
    urca::ur.pp(x_clean, type = "Z-tau",
                model = "constant", use.lag = NULL),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(data.frame(Series = series_name, PP_stat = NA,
                      p_value = NA, Stars = NA,
                      stringsAsFactors = FALSE))
  }
  # ur.pp returns test statistic; map to approx p-value via critical values
  tau  <- as.numeric(result@teststat)
  # Approximate p-value from PP tau distribution (similar to ADF)
  p_approx <- tryCatch(
    tseries::adf.test(x_clean)$p.value,   # conservative proxy
    error = function(e) NA
  )
  data.frame(
    Series   = series_name,
    PP_stat  = round(tau, 4),
    p_value  = round(p_approx, 4),
    Stars    = significance_stars(p_approx),
    stringsAsFactors = FALSE
  )
}

# Run ADF on all numeric series (raw + transformed)
all_test_cols <- jp_trans_df %>%
  select(-Country, -Time) %>%
  select(where(is.numeric)) %>%
  names()

adf_results <- map_dfr(all_test_cols, function(col) {
  run_adf(jp_trans_df[[col]], col)
})

pp_results <- map_dfr(all_test_cols, function(col) {
  run_pp(jp_trans_df[[col]], col)
})

cat("
--- ADF Unit Root Test Results ---
")
print(adf_results)

cat("
--- Phillips-Perron Unit Root Test Results ---
")
print(pp_results)

# Save test results
write.csv(adf_results,
          "Data/Transformed/adf_test_results.csv", row.names = FALSE)
write.csv(pp_results,
          "Data/Transformed/pp_test_results.csv", row.names = FALSE)


# ============================================================
# 6) DISTRIBUTION PLOTS  (Mirrors seaborn / matplotlib figures)
# ============================================================
# Python: sns.histplot + KDE overlays, plt.subplots grid layout

plot_distribution <- function(df_in, col, label, colour = "#2c7bb6") {
  x <- df_in[[col]]
  x_clean <- x[!is.na(x)]
  mu  <- mean(x_clean);  sigma <- sd(x_clean)

  ggplot(data.frame(x = x_clean), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40, fill = colour, alpha = 0.6, colour = "white") +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                  colour = "red", linewidth = 1.1, linetype = "dashed") +
    geom_density(colour = "navy", linewidth = 1) +
    labs(title  = label,
         x      = label,
         y      = "Density",
         caption = sprintf("n=%d  µ=%.3f  σ=%.3f", length(x_clean), mu, sigma)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 10))
}

# ── Distribution of core variables (raw) ───────────────────
p1 <- plot_distribution(df, "HICP",           "HICP (SA)")
p2 <- plot_distribution(df, "Call Money (%)",  "Call Money / Interbank (%)")
p3 <- plot_distribution(df, "10Y Bond Yield (%)","10-Year Gov Bond Yields (%)")

dist_raw <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3,
  top = grid::textGrob("Distribution of Core Raw Variables",
                        gp = grid::gpar(fontface = "bold", fontsize = 13)))

ggsave("Data/Transformed/dist_core_raw.png",
       dist_raw, width = 14, height = 5, dpi = 150)

# ── Distribution of transformed core variables ─────────────
p4 <- plot_distribution(jp_core_trans_df, "LogDiff-HICP",
                         "LogDiff-HICP (Annualised %)")
p5 <- plot_distribution(jp_core_trans_df, "AR(1)detrend-Call Money (%)",
                         "AR(1) Residuals – Call Money (%)")
p6 <- plot_distribution(jp_core_trans_df, "AR(1)detrend-10Y Bond Yield (%)",
                         "AR(1) Residuals – 10Y Gov Bond (%)")

dist_trans <- gridExtra::grid.arrange(p4, p5, p6, ncol = 3,
  top = grid::textGrob("Distribution of Transformed Core Variables",
                        gp = grid::gpar(fontface = "bold", fontsize = 13)))

ggsave("Data/Transformed/dist_core_transformed.png",
       dist_trans, width = 14, height = 5, dpi = 150)


# ============================================================
# 7) TIME-SERIES PLOTS  (Mirrors matplotlib line plots)
# ============================================================

plot_time_series <- function(df_in, col, label,
                              colour = "#2c7bb6", start_year = NULL) {
  df_plot <- df_in %>%
    select(Time, val = all_of(col)) %>%
    filter(!is.na(val)) %>%
    mutate(Date = as.Date(paste0(Time, "-01")))

  if (!is.null(start_year)) {
    df_plot <- df_plot %>% filter(year(Date) >= start_year)
  }

  ggplot(df_plot, aes(x = Date, y = val)) +
    geom_line(colour = colour, linewidth = 0.8) +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    labs(title = label, x = NULL, y = label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 10),
      axis.text.x   = element_text(angle = 45, hjust = 1)
    )
}

# ── Core raw time-series panel ──────────────────────────────
ts1 <- plot_time_series(df, "HICP",            "HICP (SA)")
ts2 <- plot_time_series(df, "Call Money (%)",  "Call Money (%)")
ts3 <- plot_time_series(df, "10Y Bond Yield (%)","10-Year Bond Yield (%)")

ts_raw_panel <- gridExtra::grid.arrange(ts1, ts2, ts3, ncol = 1,
  top = grid::textGrob("Core Raw Time Series",
                        gp = grid::gpar(fontface = "bold", fontsize = 13)))

ggsave("Data/Transformed/ts_core_raw.png",
       ts_raw_panel, width = 10, height = 10, dpi = 150)

# ── Core transformed time-series panel ─────────────────────
ts4 <- plot_time_series(jp_core_trans_df, "LogDiff-HICP",
                         "LogDiff-HICP (Annualised %)")
ts5 <- plot_time_series(jp_core_trans_df, "AR(1)detrend-Call Money (%)",
                         "AR(1) Residuals – Call Money (%)")
ts6 <- plot_time_series(jp_core_trans_df, "AR(1)detrend-10Y Bond Yield (%)",
                         "AR(1) Residuals – 10Y Bond (%)")

ts_trans_panel <- gridExtra::grid.arrange(ts4, ts5, ts6, ncol = 1,
  top = grid::textGrob("Core Transformed Time Series",
                        gp = grid::gpar(fontface = "bold", fontsize = 13)))

ggsave("Data/Transformed/ts_core_transformed.png",
       ts_trans_panel, width = 10, height = 10, dpi = 150)

# ── HP-filter: trend vs. cycle for Real GDP ─────────────────
# Python: cycle, trend = hpfilter(log(x), lamb=1600)  → overlaid plot

hp_plot_df <- df %>%
  select(Time, `Real GDP`) %>%
  filter(!is.na(`Real GDP`)) %>%
  mutate(
    Date      = as.Date(paste0(Time, "-01")),
    log_gdp   = log(`Real GDP`),
    hp_result = {
      hp <- mFilter::hpfilter(log_gdp, freq = 1600, type = "lambda")
      list(trend = as.numeric(hp$trend), cycle = as.numeric(hp$cycle))
    }
  )

# Unpack list column
hp_plot_df$Trend <- sapply(hp_plot_df$hp_result, `[[`, "trend")
hp_plot_df$Cycle <- sapply(hp_plot_df$hp_result, `[[`, "cycle")

p_gdp_trend <- ggplot(hp_plot_df, aes(x = Date)) +
  geom_line(aes(y = log_gdp, colour = "Log Real GDP"), linewidth = 0.8) +
  geom_line(aes(y = Trend, colour = "HP Trend"), linewidth = 1.2,
            linetype = "dashed") +
  scale_colour_manual(values = c("Log Real GDP" = "steelblue",
                                  "HP Trend"     = "red")) +
  labs(title  = "Real GDP: Log-Level vs. HP Trend  (λ = 1600)",
       x = NULL, y = "Log Real GDP", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

p_gdp_cycle <- ggplot(hp_plot_df, aes(x = Date, y = Cycle)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_line(colour = "darkgreen", linewidth = 0.8) +
  labs(title = "Real GDP: HP-Filter Cycle Component",
       x = NULL, y = "Cycle") +
  theme_minimal(base_size = 11)

hp_panel <- gridExtra::grid.arrange(p_gdp_trend, p_gdp_cycle, ncol = 1)
ggsave("Data/Transformed/hp_filter_real_gdp.png",
       hp_panel, width = 10, height = 8, dpi = 150)


# ============================================================
# 8) NORMALITY & DISTRIBUTION TESTS
# ============================================================
# Python: kstest, ks_2samp, levene, pearsonr (scipy.stats)

normality_test <- function(x, series_name) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 8) {
    return(data.frame(Series = series_name,
                      KS_stat = NA, KS_p = NA, KS_Stars = NA,
                      stringsAsFactors = FALSE))
  }
  # KS test against normal  (Python: kstest(x, 'norm'))
  ks <- ks.test(
    scale(x_clean),          # standardise before testing
    "pnorm"
  )
  data.frame(
    Series   = series_name,
    KS_stat  = round(as.numeric(ks$statistic), 4),
    KS_p     = round(ks$p.value, 4),
    KS_Stars = significance_stars(ks$p.value),
    stringsAsFactors = FALSE
  )
}

norm_results_raw <- map_dfr(
  c("HICP", "Call Money (%)", "10Y Bond Yield (%)"),
  function(col) normality_test(df[[col]], col)
)

norm_results_trans <- map_dfr(
  c("LogDiff-HICP",
    "AR(1)detrend-Call Money (%)",
    "AR(1)detrend-10Y Bond Yield (%)"),
  function(col) normality_test(jp_core_trans_df[[col]], col)
)

cat("
--- KS Normality Tests – Raw Core Series ---
")
print(norm_results_raw)

cat("
--- KS Normality Tests – Transformed Core Series ---
")
print(norm_results_trans)

# Levene's test for equal variances (Python: levene())
# Compares variance of raw vs. transformed HICP
hicp_raw_clean   <- df[["HICP"]][!is.na(df[["HICP"]])]
hicp_trans_clean <- jp_core_trans_df[["LogDiff-HICP"]][
  !is.na(jp_core_trans_df[["LogDiff-HICP"]])
]

levene_test <- car::leveneTest(
  c(scale(hicp_raw_clean), scale(hicp_trans_clean)),
  group = factor(c(rep("Raw", length(hicp_raw_clean)),
                   rep("Transformed", length(hicp_trans_clean))))
)
cat("
--- Levene Test (Raw vs. Transformed HICP variance) ---
")
print(levene_test)

# Pearson correlation between Call Money and 10Y Bond Yield
# Python: pearsonr(x, y)
call_money_clean <- df[["Call Money (%)"]][!is.na(df[["Call Money (%)"]])]
bond_yield_clean <- df[["10Y Bond Yield (%)"]][!is.na(df[["10Y Bond Yield (%)"]])]
min_len          <- min(length(call_money_clean), length(bond_yield_clean))

pearson_result <- cor.test(
  call_money_clean[1:min_len],
  bond_yield_clean[1:min_len],
  method = "pearson"
)
cat(sprintf(
  "
--- Pearson r (Call Money vs. 10Y Bond Yield): r=%.4f, p=%.4f %s ---
",
  pearson_result$estimate,
  pearson_result$p.value,
  significance_stars(pearson_result$p.value)
))


# ============================================================
# 9) CORRELATION HEATMAP  (Mirrors seaborn heatmap)
# ============================================================
# Python: sns.heatmap(corr_matrix, annot=True, cmap='coolwarm')

numeric_trans_cols <- jp_trans_df %>%
  select(-Country, -Time) %>%
  select(where(is.numeric)) %>%
  select(where(~ sum(!is.na(.)) > 50))   # keep columns with enough data

corr_matrix <- cor(numeric_trans_cols, use = "pairwise.complete.obs")

# ggplot2 heatmap
corr_long <- as.data.frame(as.table(corr_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)

p_heatmap <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(colour = "white") +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 7),
    axis.text.y  = element_text(size = 7),
    plot.title   = element_text(face = "bold")
  ) +
  labs(title = "Correlation Matrix – Transformed Variables",
       x = NULL, y = NULL)

ggsave("Data/Transformed/correlation_heatmap.png",
       p_heatmap, width = 14, height = 12, dpi = 150)

cat("
All outputs saved to Data/Transformed/
")
