# Below is the master function for 
# computing the theoretical ACVF/ACF.
# There are several helper functions
# noted below which are called depending
# on the type of tseries selected 
# in the Shiny interface.

# ============================================================
# Theoretical ACVF & ACF for AR, MA, and ARMA Models
# ============================================================
# Assumes stationarity. Though may be augmented
# to include error messaging or stationary transformations
# for non stationary series if time permits. 
# Connects to a Shiny interface via
# compute_theoretical(), which dispatches based on MODEL_FLAG.

# AI USE DISCLOSURE - AI was consulted for the following:

# - A summary of relevant R functions for the purposes of vectorized operations.
# - Clean up of the aesthetics of the document as of 2026-02-21. 
# - AR and ARMA computations are largely based on AI suggested procedures as
# of 2026-02-21

library(tidyverse)

MODEL_FLAG <- NULL
AR   <- "AR"
MA   <- "MA"
ARMA <- "ARMA"

# ---- Dispatcher ----

compute_theoretical <- function(coefs, sigma_w, max_lag) {
  if (MODEL_FLAG == AR) {
    return(theoretical_ar(coefs, sigma_w, max_lag))
  } else if (MODEL_FLAG == MA) {
    return(theoretical_ma(coefs, sigma_w, max_lag))
  } else {
    return(theoretical_arma(coefs[["ar"]], coefs[["ma"]], sigma_w, max_lag))
  }
}

# ---- AR(p) ----

theoretical_ar <- function(ar_coefs, sigma_w = 1, max_lag = 20) {
  
  p <- length(ar_coefs)
  
  # Step 1: Build and solve the Yule-Walker system to find the initial 
  # p-1 gamma. Currently unbounded size but difficult to understand.
  
  M <- matrix(0, nrow = p, ncol = p)
  
  for (i in 1:p) {
    h <- i - 1
    for (j in 1:p) {
      target_lag <- j - 1
      val <- ifelse(i == j, 1, 0)
      for (k in 1:p) {
        
        if (abs(h - k) == target_lag) val <- val - ar_coefs[k]
      }
      M[i, j] <- val
    }
  }
  
  rhs <- c(sigma_w^2, rep(0, p - 1))
  anchor_gammas <- solve(M, rhs)
  
  # Step 2: Find the roots of the characteristic eq'n
  roots <- polyroot(c(1, -ar_coefs))
  
  # Step 3: Solve Vandermonde system for the M_i
  V <- outer(0:(p - 1), roots, "^")
  A_weights <- solve(V, anchor_gammas)
  
  # Step 4: Compute the output vector for all lags
  power_matrix <- outer(0:max_lag, roots, "^")
  gamma <- Re(power_matrix %*% A_weights)
  
  list(acvf = gamma, acf = gamma / gamma[1]) # Output as list repeated for all func.
} 

# ---- MA(q) ----

# Direct application of formula from Discord sheet

theoretical_ma <- function(ma_coefs, sigma_w = 1, max_lag = 20) {
  
  q <- length(ma_coefs)
  b <- c(1, ma_coefs)   
  
  gamma <- numeric(max_lag + 1)
  
  for (lag in 0:max_lag) {
    if (lag <= q) {
      
      slice_left  <- b[(lag + 1):(q + 1)]      
      slice_right <- b[1:(q + 1 - lag)]         
      gamma[lag + 1] <- (sigma_w^2) * sum(slice_left * slice_right)
    }
    
  }
  
  list(acvf = gamma, acf = gamma / gamma[1])
}

# ---- ARMA(p, q) ----

theoretical_arma <- function(ar_coefs, ma_coefs, sigma_w = 1, max_lag = 20) {
  
  p <- length(ar_coefs)
  q <- length(ma_coefs)
  r <- max(p, q)
  
  theta <- c(-1, ma_coefs)  
  a <- c(-1, ar_coefs)   
  
  # Step 1: Initialize MA coefficients with AR influence
  c_coef <- numeric(q + 1)
  c_coef[1] <- 1   
  
  for (tau in 1:q) {
    ar_sum <- 0
    if (p >= 1) {
      for (i in 1:min(p, tau)) {
        ar_sum <- ar_sum + ar_coefs[i] * c_coef[tau - i + 1]
      }
    }
    c_coef[tau + 1] <- -ma_coefs[tau] + ar_sum
  }
  
  # Step 2: Compute cross product terms 
  d_coef <- numeric(q + 1)
  for (tau in 0:q) {
    d_coef[tau + 1] <- sum(theta[(tau + 1):(q + 1)] * c_coef[1:(q + 1 - tau)])
  }
  
  # Helper: safe lookup with zero-padding for out-of-range indices
  get_c <- function(idx) ifelse(idx >= 1 & idx <= q + 1, c_coef[idx], 0)
  get_d <- function(idx) ifelse(idx >= 1 & idx <= q + 1, d_coef[idx], 0)
  
  # Step 3: Solve the system for the initial autocovariances
  A_mat <- matrix(0, p, p)
  y_vec <- numeric(p)
  
  for (i in 1:p) {
    for (j in 1:p) {
      A_mat[i, j] <- get_c(abs(i - j) + 1) + get_d(i + j)
    }
    y_vec[i] <- -get_d(i + 1)   
  }
  
  x <- solve(A_mat, y_vec) * sigma_w^2
  anchor_gammas <- x
  
  # Step 4: Compute the rest of the autocovariances using the AR recurrence
  all_gammas <- c(anchor_gammas, numeric(max(0, max_lag + 1 - p)))
  
  for (tau in p:max_lag) {
    all_gammas[tau + 1] <- sum(ar_coefs * all_gammas[abs(tau - 1:p) + 1])
  }
  
  gamma <- all_gammas[1:(max_lag + 1)]
  list(acvf = gamma, acf = gamma / gamma[1])
}
