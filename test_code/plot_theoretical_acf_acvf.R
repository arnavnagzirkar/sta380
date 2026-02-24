# Define placeholder models needed by the sourced computation file
sigma <- 1

arma <- list(
  p = 1,
  q = 1,
  ar_coefs = c(0.7),
  ma_coefs = c(0.4)
)

ar <- list(
  p = 1,
  q = 0,
  ar_coefs = c(0.7),
  ma_coefs = NULL
)

ma <- list(
  p = 0,
  q = 1,
  ar_coefs = NULL,
  ma_coefs = c(0.4)
)

# Load theoretical ACVF/ACF functions
source("Theoretical_ACVF_ACF_Computation.R")

# Choose the model and lag settings to plot
model <- list(
  p = 1,
  q = 1,
  ar_coefs = c(0.7),
  ma_coefs = c(0.4)
)

sigma <- 1
max_lag <- 30

# Compute theoretical ACVF and ACF values
acvf_vals <- get_theoretical_acvf(model, sigma, max_lag)
acf_vals <- get_theoretical_acf(model, sigma, max_lag)
lags <- 0:max_lag

# Set output filenames in the current directory
out_dir <- getwd()

file_acvf <- file.path(out_dir, "Theoretical_ACVF.png")
file_acf <- file.path(out_dir, "Theoretical_ACF.png")
file_both <- file.path(out_dir, "Theoretical_ACVF_and_ACF.png")

width_single <- 1200
height_single <- 800
width_both <- 1200
height_both <- 1000
resolution <- 150

# Save the ACVF plot as a PNG
png(
  filename = file_acvf,
  width = width_single,
  height = height_single,
  res = resolution
)

plot(
  x = lags,
  y = acvf_vals,
  type = "h",
  main = "Theoretical ACVF",
  xlab = "Lag",
  ylab = "ACVF"
)

dev.off()

# Save the ACF plot as a PNG
png(
  filename = file_acf,
  width = width_single,
  height = height_single,
  res = resolution
)

plot(
  x = lags,
  y = acf_vals,
  type = "h",
  main = "Theoretical ACF",
  xlab = "Lag",
  ylab = "ACF"
)

dev.off()

# Save the combined ACVF and ACF plot as a combined PNG
png(
  filename = file_both,
  width = width_both,
  height = height_both,
  res = resolution
)

par(mfrow = c(2, 1))

plot(
  x = lags,
  y = acvf_vals,
  type = "h",
  main = "Theoretical ACVF",
  xlab = "Lag",
  ylab = "ACVF"
)

plot(
  x = lags,
  y = acf_vals,
  type = "h",
  main = "Theoretical ACF",
  xlab = "Lag",
  ylab = "ACF"
)

par(mfrow = c(1, 1))

dev.off()