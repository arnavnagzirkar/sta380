# Model Parameters #####
n <- 100
k <- 2
mean <- 0
sigma <- 1

coefs_expr <- quote(seq(from = 1 - (1 / k), to = 0, by = -1 / k))
wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))

# Model Generating Functions #####

gen_ma <- function(n, p, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
  data <- numeric(n)
  
  for (i in 1:n) {
    lag <- min(p, i - 1)
    
    terms <- wn[i]
    
    if (lag > 0) {
      terms <- terms + sum(coefs[1:lag] * wn[(i - 1):(i - lag)])
    }
    
    data[i] <- terms
  }
  
  return(list(data = data,
              n = n,
              p = p,
              wn = wn,
              ma_coefs = coefs,
              model_type = "MA"
  ))
}

gen_ar <- function(n, q, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
  data <- numeric(n)
  
  for (i in 1:n) {
    lag <- min(q, i - 1)
    
    data[i] <- wn[i] + sum(coefs[1:lag] * data[(i - 1):(i - lag)])
  }
  
  return(list(data = data,
              n = n,
              q = q,
              wn = wn,
              ar_coefs = coefs,
              model_type = "AR"
  ))
}

gen_arma <- function(n, p, q, 
                     wn = eval(wn_expr), 
                     ar_coefs = eval(coefs_expr), 
                     ma_coefs = eval(coefs_expr)) {
  
  data <- numeric(n)
  
  for (i in 1:n) {
    
    # AR part
    ar_terms <- 0
    
    ar_lag <- min(p, i - 1)
    
    if (ar_lag > 0) {
      ar_terms <- sum(ar_coefs[1:ar_lag] * data[(i - 1):(i - ar_lag)])
    }
    
    # MA part
    ma_terms <- 0
    
    ma_lag <- min(q, i - 1)
    
    if (ma_lag > 0) {
      ma_terms <- sum(ma_coefs[1:ma_lag] * wn[(i - 1):(i - ma_lag)])
    }
    
    # ARMA part
    data[i] <- ar_terms + wn[i] + ma_terms
  }
  
  return(list(data = data,
              n = n,
              p = p,
              q = q,
              wn = wn,
              ar_coefs = ar_coefs,
              ma_coefs = ma_coefs,
              model_type = "ARMA"
  ))
}

# Simple Tests
ar <- gen_ar(n, k)
ma <- gen_ma(n, k, ar$wn, ar$ar_coefs)
arma <- gen_arma(n, k, k, ar$wn, ar$ar_coefs, ma$ma_coefs)

plot.ts(arma$data)
plot.ts(ar$data)
plot.ts(ma$data)

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
    for (k in 1:min(p, j - 1)) {
      psi_sum <- psi_sum + phi[k] * psi[j - k]
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
  poly = c(1, -model$ar_coefs)
  
  roots <- polyroot(poly)
  
  all(abs(roots) > 1) 
}

is_casual(arma)

get_theoretical_acvf <- function(model, sigma, max_lag = 30) {
  if (model$model_type == "MA") {
    # TO DO
  }
  
  psi <- arma_to_ma(model, max_lag)
  
  sapply(0:max_lag, function(j) {
    sigma^2 * sum(psi[1:(length(psi) - j)] * psi[(1 + j):length(psi)])
  })
}

acvf <- get_theoretical_acvf(arma, sigma^2)

get_theoretical_acf <- function(model, sigma, max_lag = 30) {
  if(!is_casual(model)) {
    print("AR part is not casual, thus we return the theoretical PACF instead.")
  }
  
  acvf <- get_theoretical_acvf(model, sigma, max_lag)
  
  return(acvf / acvf[1])
}

get_theoretical_acf(arma, sigma^2)
ARMAacf(arma$ar_coefs, arma$ma_coefs)

