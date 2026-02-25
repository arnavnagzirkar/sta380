# ============================================================
# Tests for theoretical_acf.R
# Run with: testthat
# ============================================================
# Strategy: where possible, check our output against R's built-in
# ARMAacf(), which provides a trusted reference for ACF values.
# ACVF values are verified by hand-derivable closed forms (e.g.
# AR(1): gamma(0) = sigma_w^2 / (1 - a^2)).

# Environment Setup
library(testthat)

source("test_code/Theoretical_ACVF_ACF_Computation.R")
source("test_code/Model_Implementations.R")

# Constants and Parameters

# ============================================================
# GENERATOR Tests
# ============================================================

test_that("gen_arma model for single ar(p)", {

  # Define test parameters
  n <- 100
  k <- 2
  mean <- 0
  sigma <- 1

  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))
  wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))
  
  coefs <- eval(coefs_expr)
  wn <- eval(wn_expr)

  tol <- 1e-6

  # Call our function
  my_ar <- gen_arma(n = n, p = k, q = 0, ar_coefs = coefs,
                    ma_coefs = NULL, wn = wn)$data

  # In-built R call
  r_ar <- as.numeric(arima.sim(model = list(ar = coefs),
                               n = n, 
                               innov = wn, 
                               start.innov = rep(0,100)))
                     
  # Note that the start.innov is a fix for R starting with a longer
  # than expected series for stability of patterns.

  # Compare outputs
  expect_equal(my_ar, r_ar, tolerance = tol)

})

test_that("gen_arma model for single ma(q)", {
  
  # Define test parameters
  n <- 100
  k <- 2
  mean <- 0
  sigma <- 1
  
  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))
  wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))
  
  coefs <- eval(coefs_expr)
  wn <- eval(wn_expr)
  
  tol <- 1e-6
  
  # Call our function
  my_ma <- gen_arma(n = n, p = 0, q = k, ar_coefs = NULL,
                    ma_coefs = coefs, wn = wn)$data
  
  # In-built R call
  r_ma <- as.numeric(arima.sim(model = list(ma = coefs),
                               n = n, 
                               innov = wn, 
                               start.innov = rep(0,100)))
  
  # Note that the start.innov is a fix for R starting with a longer
  # than expected series for stability of patterns.
  
  # Compare outputs
  expect_equal(my_ma, r_ma, tolerance = tol)
  
})

test_that("gen_arma model for a whole arma(p,q)", {
  
  # Define test parameters
  n <- 100
  k <- 2
  mean <- 0
  sigma <- 1
  
  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))
  wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))
  
  coefs <- eval(coefs_expr)
  wn <- eval(wn_expr)
  
  tol <- 1e-6
  
  # Call our function
  my_arma <- gen_arma(n = n, p = k, q = k, ar_coefs = coefs,
                    ma_coefs = coefs, wn = wn)$data
  
  # In-built R call
  r_arma <- as.numeric(arima.sim(model = list(ar = coefs, ma = coefs),
                               n = n, 
                               innov = wn, 
                               start.innov = rep(0,100)))
  
  # Note that the start.innov is a fix for R starting with a longer
  # than expected series for stability of patterns.
  
  # Compare outputs
  expect_equal(my_arma, r_arma, tolerance = tol)
  
})

# ============================================================
# ARMA TRANSFORMATION tests
# ============================================================


test_that("arma_to_ma matches ARMAtoMA for ar case", {
  
  # Define test parameters
  p <- 2
  q <- 0
  max_lag <- 10
  
  coefs_expr <- quote(seq(from = 0.5/p, to = 0, length.out = p))
  coefs <- eval(coefs_expr)
  
  tol <- 1e-6
  
  model <- list(p = p, 
                q = q, 
                ar_coefs = coefs, 
                ma_coefs = NULL)
  
  # Call our function
  my_psi <- arma_to_ma(model, max_lag = max_lag)
  
  # In-built R call
  r_psi <- ARMAtoMA(ar = coefs, ma = numeric(0), lag.max = max_lag)
  
  # Compare outputs with an index offset
  expect_equal(my_psi[1], 1)
  expect_equal(as.numeric(my_psi[2:(max_lag + 1)]), as.numeric(r_psi), tolerance = tol)
  
})


test_that("arma_to_ma matches ARMAtoMA for pure ma case", {
  
  # Define test parameters
  p <- 0
  q <- 2
  k <- q
  max_lag <- 10
  
  coefs_expr <- quote(seq(from = 0.5/q, to = 0, length.out = q))
  coefs <- eval(coefs_expr)
  
  tol <- 1e-6
  
  model <- list(p = p, 
                q = q, 
                ar_coefs = NULL, 
                ma_coefs = coefs)
  
  # Call our function
  my_psi <- arma_to_ma(model, max_lag = max_lag)
  
  # In-built R call
  r_psi <- ARMAtoMA(ar = numeric(0), ma = coefs, lag.max = max_lag)
  
  # Compare outputs with an index offset
  expect_equal(my_psi[1], 1)
  expect_equal(as.numeric(my_psi[2:(max_lag + 1)]), as.numeric(r_psi), tolerance = tol)
  
})


test_that("arma_to_ma matches ARMAtoMA for arma case", {
  
  # Define test parameters
  p <- 1
  q <- 1
  max_lag <- 10
  
  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))
  wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))
  
  coefs <- eval(coefs_expr)
  wn <- eval(wn_expr)
  
  tol <- 1e-6
  
  model <- list(p = p, 
                q = q, 
                ar_coefs = coefs, 
                ma_coefs = coefs)
  
  # Call our function
  my_psi <- arma_to_ma(model, max_lag = max_lag)
  
  # In-built R call
  r_psi <- ARMAtoMA(ar = coefs, ma = coefs,lag.max = max_lag)
  
  # Compare outputs with an index offset
  expect_equal(my_psi[1], 1)
  expect_equal(as.numeric(my_psi[2:(max_lag + 1)]), as.numeric(r_psi))
  
})


# ============================================================
# CAUSALITY tests
# ============================================================

test_that("is_casual identifies causality for ar case", {
  
  # Note that if the roots lie outside the unit circle
  # we are stable
  
  # Stable case:
  model <- list(p = 1, 
                       q = 0, 
                       ar_coefs = 0.5, 
                       ma_coefs = NULL)
  expect_true(is_casual(model))
  
  # Unstable case
  model <- list(p = 1, 
                         q = 0, 
                         ar_coefs = 1.2, 
                         ma_coefs = NULL)
  expect_false(is_casual(model))

})


test_that("is_casual should alway s return true for ma case", {
  
  model <- list(p = 0, 
                   q = 1, 
                   ar_coefs = NULL, 
                   ma_coefs = 1.2)
  
  expect_true(is_casual(model))
  
})


test_that("is_casual works identifies causality for arma case)", {
  
  # Stable case
  model <- list(p = 1, 
                     q = 1, 
                     ar_coefs = 0.7, 
                     ma_coefs = 0.4)
  
  expect_true(is_casual(model))
  
  # Unstable case
  model <- list(p = 1, 
                              q = 1, 
                              ar_coefs = 1.1, 
                              ma_coefs = 0.4)
  
  expect_false(is_casual(model))
  
})

# ============================================================
# ACVF/ACF COMPUTATION tests
# ============================================================

test_that("ACF matches the manual computation", {
  
  # Recall the that ACF = ACVF * Var
  
  # Define test parameters
  p <- 1
  q <- 1
  sigma <- 1
  max_lag <- 10
  
  # Pick coefficients
  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))
  wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))
  
  coefs <- eval(coefs_expr)
  wn <- eval(wn_expr)
  
  tol <- 1e-6
  
  model <- list(p = p, 
                q = q, 
                ar_coefs = coefs, 
                ma_coefs = coefs)
  
  # Call our functions
  my_acf <- get_theoretical_acf(model = model, sigma = sigma, max_lag = max_lag)
  my_acvf <- get_theoretical_acvf(model = model, sigma = sigma, max_lag = max_lag)
  
  # In-built R call
  r_acf <- ARMAacf(ar = model$ar_coefs, ma = model$ma_coefs, lag.max = max_lag)
  
  # Compare ACF values
  expect_equal(my_acf[1], 1)
  expect_equal(as.numeric(my_acf), as.numeric(r_acf), tolerance = tol)
  
  # Compare ACVF values 
  expected_acvf <- as.numeric(r_acf) * my_acvf[1]
  expect_equal(as.numeric(my_acvf), expected_acvf, tolerance = tol)
})

test_that("ma case should not have acf after q lags", {
  
  p <- 0
  q <- 1
  max_lag <- 5
  ma_coefs <- 0.5
  sigma <- 1
  
  model <- list(p = p, 
                   q = q, 
                   ar_coefs = NULL, 
                   ma_coefs = ma_coefs)
  
  my_acf <- get_theoretical_acf(model = model, sigma = sigma, max_lag = max_lag)
  
  expect_true(abs(my_acf[2]) > 0)
  expect_equal(my_acf[3], 0)
  expect_equal(my_acf[4], 0)
  
})

