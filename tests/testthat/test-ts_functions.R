# ============================================================
# Tests for ts_functions.R
# Run with: testthat
# ============================================================
# Strategy: where possible, check our output against R's built-in
# offerings, which provide a trusted reference.


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
  my_ar <- gen_arma(n = n, ar_coefs = coefs, ma_coefs = NULL, wn = wn)$data

  # In-built R call
  r_ar <- as.numeric(arima.sim(model = list(ar = coefs),
                               n = n,
                               innov = wn,
                               start.innov = rep(0,100)))

  # Note that the start.innov is a fix for R starting with a longer
  # than expected series for the series to stabilize.

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
  my_ma <- gen_arma(n = n, ar_coefs = NULL, ma_coefs = coefs, wn = wn)$data

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
  my_arma <- gen_arma(n = n, ar_coefs = coefs, ma_coefs = coefs, wn = wn)$data

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

  model <- list(p = p, q = q, ar_coefs = coefs, ma_coefs = NULL)

  # Call our function
  my_psi <- arma_to_ma(model, max_lag = max_lag)

  # In-built R call
  r_psi <- ARMAtoMA(ar = coefs, ma = numeric(0), lag.max = max_lag)

  # Compare outputs with an index offset
  expect_equal(my_psi[1], 1)
  expect_equal(as.numeric(my_psi[2:(max_lag + 1)]), as.numeric(r_psi), tolerance = tol)

})

test_that("arma_to_ma matches ARMAtoMA for ma case", {

  # Define test parameters
  p <- 0
  q <- 2
  k <- q
  max_lag <- 10

  coefs_expr <- quote(seq(from = 0.5/q, to = 0, length.out = q))
  coefs <- eval(coefs_expr)

  tol <- 1e-6

  model <- list(p = p, q = q, ar_coefs = NULL, ma_coefs = coefs)

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
  k <- 1
  max_lag <- 10

  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))

  coefs <- eval(coefs_expr)

  tol <- 1e-6

  model <- list(p = p, q = q, ar_coefs = coefs, ma_coefs = coefs)

  # Call our function
  my_psi <- arma_to_ma(model, max_lag = max_lag)

  # In-built R call
  r_psi <- ARMAtoMA(ar = coefs, ma = coefs, lag.max = max_lag)

  # Compare outputs with an index offset
  expect_equal(my_psi[1], 1)
  expect_equal(as.numeric(my_psi[2:(max_lag + 1)]), as.numeric(r_psi))

})


# ============================================================
# CAUSALITY tests
# ============================================================

test_that("is_causal identifies causality for ar case", {

  # Note that if the roots lie outside the unit circle
  # we are stable

  # Stable case:
  model <- list(p = 1, q = 0, ar_coefs = 0.5, ma_coefs = NULL)

  expect_true(is_causal(model))

  # Unstable case
  model <- list(p = 1, q = 0, ar_coefs = 1.2, ma_coefs = NULL)

  expect_false(is_causal(model))

})

test_that("is_causal should always return true for ma case", {

  model <- list(p = 0, q = 1, ar_coefs = NULL, ma_coefs = 1.2)

  expect_true(is_causal(model))

})

test_that("is_causal works identifies causality for arma case)", {

  # Stable case
  model <- list(p = 1, q = 1, ar_coefs = 0.7, ma_coefs = 0.4)

  expect_true(is_causal(model))

  # Unstable case
  model <- list(p = 1, q = 1, ar_coefs = 1.1, ma_coefs = 0.4)

  expect_false(is_causal(model))

})


# ============================================================
# ACVF/ACF COMPUTATION tests
# ============================================================


# ============= Theoretical Computation Sub-cases ============

test_that("ACF matches the manual computation", {

  # Define test parameters
  p <- 1
  q <- 1
  k <- q
  sigma <- 1
  max_lag <- 10
  n <- 100

  tol <- 1e-3

  # Pick coefficients
  coefs_expr <- quote(seq(from = 0.5/k, to = 0, length.out = k))

  coefs <- eval(coefs_expr)

  model <- list(p = p, q = q, ar_coefs = coefs, ma_coefs = coefs)

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

test_that("ma case should not have theoretical acf after q lags", {

  p <- 0
  q <- 1
  max_lag <- 5
  ma_coefs <- 0.5
  sigma <- 1

  model <- list(p = p, q = q, ar_coefs = NULL, ma_coefs = ma_coefs)

  my_acf <- get_theoretical_acf(model = model, sigma = sigma, max_lag = max_lag)

  expect_true(abs(my_acf[2]) > 0)
  expect_equal(my_acf[3], 0)
  expect_equal(my_acf[4], 0)

})

# ============= Sample Computation Sub-cases ============

test_that("Sample ACF matches the manual computation", {

  # Parameters
  p <- 1
  q <- 1
  k <- q
  sigma <- 1
  max_lag <- 10
  mean <- 0
  n <- 100

  tol <- 1e-6

  coefs <- eval(quote(seq(from = 0.5/k, to = 0, length.out = k)))
  wn <- eval(quote(rnorm(n, mean = mean, sd = sigma)))

  # Generate the model
  model <- gen_arma(n = n, wn = wn, ar_coefs = coefs, ma_coefs = coefs)

  # Call out sample functions
  sample_acvf <- get_sample_acvf(model = model, max_lag = max_lag)
  sample_acf <- get_sample_acf(model = model, max_lag = max_lag)

  # In-built R calls
  r_acvf_obj <- acf(model$data, lag.max = max_lag, plot = FALSE, type = "covariance")
  sample_acvf_r <- as.numeric(r_acvf_obj$acf)

  r_acf_obj <- acf(model$data, lag.max = max_lag, plot = FALSE, type = "correlation")
  sample_acf_r <- as.numeric(r_acf_obj$acf)

  # Compare outputs
  expect_equal(sample_acvf, sample_acvf_r, tolerance = tol)
  expect_equal(sample_acf, sample_acf_r, tolerance = tol)
})

