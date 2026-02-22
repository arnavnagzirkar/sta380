# ============================================================
# Tests for theoretical_acf.R
# Run with: testthat
# ============================================================
# Strategy: where possible, check our output against R's built-in
# ARMAacf(), which provides a trusted reference for ACF values.
# ACVF values are verified by hand-derivable closed forms (e.g.
# AR(1): gamma(0) = sigma_w^2 / (1 - a^2)).

library(testthat)
source("tseries_functions.R")

tol <- 1e-6   # numeric tolerance for all expect_equal() calls

# AI USE DISCLOSURE - ALL TESTS ARE AI GENERATED 

# ============================================================
# AR Tests
# ============================================================

test_that("AR(1): ACF matches ARMAacf reference", {
  a <- 0.7
  result <- theoretical_ar(ar_coefs = c(a), sigma_w = 1, max_lag = 10)
  ref    <- as.numeric(ARMAacf(ar = c(a), lag.max = 10))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("AR(1): ACVF(0) equals closed-form variance sigma_w^2 / (1 - a^2)", {
  a      <- 0.7
  sw     <- 2
  result <- theoretical_ar(ar_coefs = c(a), sigma_w = sw, max_lag = 5)
  
  expect_equal(result$acvf[1], sw^2 / (1 - a^2), tolerance = tol)
})

test_that("AR(1): ACF satisfies rho(tau) = a^tau", {
  a      <- 0.6
  result <- theoretical_ar(ar_coefs = c(a), sigma_w = 1, max_lag = 8)
  
  expected_acf <- a^(0:8)
  expect_equal(result$acf, expected_acf, tolerance = tol)
})

test_that("AR(2): ACF matches ARMAacf reference", {
  ar     <- c(0.5, 0.2)
  result <- theoretical_ar(ar_coefs = ar, sigma_w = 1, max_lag = 10)
  ref    <- as.numeric(ARMAacf(ar = ar, lag.max = 10))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("AR: rho(0) == 1 and |rho(tau)| <= 1 for all lags", {
  result <- theoretical_ar(ar_coefs = c(0.4, -0.3), sigma_w = 1, max_lag = 15)
  
  expect_equal(result$acf[1], 1, tolerance = tol)
  expect_true(all(abs(result$acf) <= 1 + tol))
})

test_that("AR: output vectors have length max_lag + 1", {
  result <- theoretical_ar(ar_coefs = c(0.5), sigma_w = 1, max_lag = 12)
  
  expect_length(result$acvf, 13)
  expect_length(result$acf,  13)
})

test_that("AR: sigma_w scales ACVF by sigma_w^2, ACF is unaffected", {
  ar  <- c(0.5)
  r1  <- theoretical_ar(ar_coefs = ar, sigma_w = 1, max_lag = 5)
  r2  <- theoretical_ar(ar_coefs = ar, sigma_w = 3, max_lag = 5)
  
  expect_equal(r2$acvf, r1$acvf * 9, tolerance = tol)
  expect_equal(r2$acf,  r1$acf,      tolerance = tol)
})


# ============================================================
# MA Tests
# ============================================================

test_that("MA(1): ACVF matches closed form", {
  b  <- 0.6
  sw <- 1
  # gamma(0) = sw^2 * (1 + b^2), gamma(1) = sw^2 * b, gamma(tau>1) = 0
  result <- theoretical_ma(ma_coefs = c(b), sigma_w = sw, max_lag = 5)
  
  expect_equal(result$acvf[1], sw^2 * (1 + b^2), tolerance = tol)
  expect_equal(result$acvf[2], sw^2 * b,          tolerance = tol)
  expect_equal(result$acvf[3], 0,                 tolerance = tol)
})

test_that("MA(1): ACF matches ARMAacf reference", {
  ma     <- c(0.6)
  result <- theoretical_ma(ma_coefs = ma, sigma_w = 1, max_lag = 8)
  ref    <- as.numeric(ARMAacf(ma = ma, lag.max = 8))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("MA(2): ACF matches ARMAacf reference", {
  ma     <- c(0.4, -0.3)
  result <- theoretical_ma(ma_coefs = ma, sigma_w = 1, max_lag = 8)
  ref    <- as.numeric(ARMAacf(ma = ma, lag.max = 8))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("MA: ACF is exactly 0 for lags > q", {
  result <- theoretical_ma(ma_coefs = c(0.5, 0.3), sigma_w = 1, max_lag = 10)
  
  # q = 2, so lags 3..10 (indices 4..11) must be 0
  expect_true(all(result$acf[4:11] == 0))
  expect_true(all(result$acvf[4:11] == 0))
})

test_that("MA: rho(0) == 1 and output length is max_lag + 1", {
  result <- theoretical_ma(ma_coefs = c(0.3, -0.2, 0.1), sigma_w = 2, max_lag = 10)
  
  expect_equal(result$acf[1], 1, tolerance = tol)
  expect_length(result$acvf, 11)
  expect_length(result$acf,  11)
})

test_that("MA: sigma_w scales ACVF by sigma_w^2, ACF is unaffected", {
  ma  <- c(0.5)
  r1  <- theoretical_ma(ma_coefs = ma, sigma_w = 1, max_lag = 5)
  r2  <- theoretical_ma(ma_coefs = ma, sigma_w = 2, max_lag = 5)
  
  expect_equal(r2$acvf, r1$acvf * 4, tolerance = tol)
  expect_equal(r2$acf,  r1$acf,      tolerance = tol)
})


# ============================================================
# ARMA Tests
# ============================================================

test_that("ARMA(1,1): ACF matches ARMAacf reference", {
  ar <- c(0.5); ma <- c(0.3)
  result <- theoretical_arma(ar_coefs = ar, ma_coefs = ma, sigma_w = 1, max_lag = 10)
  ref    <- as.numeric(ARMAacf(ar = ar, ma = ma, lag.max = 10))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("ARMA(2,1): ACF matches ARMAacf reference", {
  ar <- c(0.5, -0.2); ma <- c(0.4)
  result <- theoretical_arma(ar_coefs = ar, ma_coefs = ma, sigma_w = 1, max_lag = 10)
  ref    <- as.numeric(ARMAacf(ar = ar, ma = ma, lag.max = 10))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("ARMA(1,2): ACF matches ARMAacf reference", {
  ar <- c(0.6); ma <- c(0.3, -0.2)
  result <- theoretical_arma(ar_coefs = ar, ma_coefs = ma, sigma_w = 1, max_lag = 10)
  ref    <- as.numeric(ARMAacf(ar = ar, ma = ma, lag.max = 10))
  
  expect_equal(result$acf, ref, tolerance = tol)
})

test_that("ARMA: rho(0) == 1 and output length is max_lag + 1", {
  result <- theoretical_arma(ar_coefs = c(0.4), ma_coefs = c(0.2),
                             sigma_w = 1, max_lag = 15)
  
  expect_equal(result$acf[1], 1, tolerance = tol)
  expect_length(result$acvf, 16)
  expect_length(result$acf,  16)
})

test_that("ARMA: sigma_w scales ACVF by sigma_w^2, ACF is unaffected", {
  ar <- c(0.5); ma <- c(0.3)
  r1 <- theoretical_arma(ar_coefs = ar, ma_coefs = ma, sigma_w = 1, max_lag = 8)
  r2 <- theoretical_arma(ar_coefs = ar, ma_coefs = ma, sigma_w = 2, max_lag = 8)
  
  expect_equal(r2$acvf, r1$acvf * 4, tolerance = tol)
  expect_equal(r2$acf,  r1$acf,      tolerance = tol)
})

test_that("ARMA reduces to AR when ma_coefs is all zero", {
  # With no MA component, ARMA(1,1) with b=0 should equal AR(1)
  ar <- c(0.5)
  arma_result <- theoretical_arma(ar_coefs = ar, ma_coefs = c(0), sigma_w = 1, max_lag = 10)
  ar_result   <- theoretical_ar(ar_coefs = ar, sigma_w = 1, max_lag = 10)
  
  expect_equal(arma_result$acf, ar_result$acf, tolerance = tol)
})