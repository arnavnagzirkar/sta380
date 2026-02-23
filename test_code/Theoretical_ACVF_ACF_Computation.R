# Theoretical ACF/ACVF #####

# Algorithm from Introduction to Time Series Forecasting Brockwell & Davis 3rd ed.
arma_to_ma <- function(model, max_lag = 30) {
  p <- model$p
  q <- model$q
  
  theta <- model$ma_coefs
  phi <- model$ar_coefs
  
  psi <- numeric(max_lag)
  psi[1] <- 1
  
  for (j in 2:(max_lag + 1)) {
    theta_j <- ifelse(j - 1 > q, 0, theta[j - 1])
    
    psi_sum <- 0
    
    # For AR
    if (!is.null(phi)) {
      for (k in 1:min(p, j - 1)) {
        psi_sum <- psi_sum + phi[k] * psi[j - k]
      }
    }
    
    psi[j] <- theta_j + psi_sum
  }
  
  return(psi)
}

# Simple test
arma_to_ma(arma)
ARMAtoMA(ar = arma$ar_coefs, ma = arma$ma_coefs, lag.max = 30)

# Casualty check
is_casual <- function(model) {
  if(is.null(model$ar_coefs)) return(TRUE)
  
  poly = c(1, -model$ar_coefs)
  
  roots <- polyroot(poly)
  
  all(abs(roots) > 1) 
}

is_casual(arma)

get_theoretical_acvf <- function(model, sigma, max_lag = 30) {
  psi <- arma_to_ma(model, max_lag)
  
  sapply(0:max_lag, function(j) {
    sigma^2 * sum(psi[1:(length(psi) - j)] * psi[(1 + j):length(psi)])
  })
}

get_theoretical_acf <- function(model, sigma, max_lag = 30) {
  if(!is_casual(model)) {
    print("Returning theoretical PACF instead as AR part is not casual.")
  }
  
  acvf <- get_theoretical_acvf(model, sigma, max_lag)
  
  return(acvf / acvf[1])
}

# ACF Tests (also verifies ACVF)
get_theoretical_acf(arma, sigma)
ARMAacf(arma$ar_coefs, arma$ma_coefs)

get_theoretical_acf(ar, sigma)
ARMAacf(ar$ar_coefs, ar$ma_coefs)

get_theoretical_acf(ma, sigma)
ARMAacf(ma$ar_coefs, ma$ma_coefs)

# Derived theoretical MA ACF/ACVF #####

my_theoretical_ma_acvf <- function(model, sigma, max_lag = 30) {
  q <- model$q
  b <- c(1, model$ma_coefs)
  
  sapply(0:max_lag, function(tau) {
    if (tau > q) return(0)
    
    s <- tau:q
    
    sigma^2 * sum(b[s + 1] * b[s - tau + 1])
  })
}

my_theoretical_ma_acf <- function(model, sigma, max_lag = 30) {
  acvf <- my_theoretical_ma_acvf(model, sigma, max_lag)
  
  acvf / acvf[1]
}

my_theoretical_ma_acvf(ma, sigma)
get_theoretical_acvf(ma, sigma)

my_theoretical_ma_acf(ma, sigma)
get_theoretical_acf(ma, sigma)
