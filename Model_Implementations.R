# Model Parameters #####
n <- 100
k <- 3
mean <- 0
sigma <- 1

coefs_expr <- quote(seq(from = 1 - (1 / k), to = 0, by = -1 / k))
wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))

# Model Generating Functions #####

gen_ma <- function(n, k, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
  data <- numeric(n)
  
  for (i in 1:n) {
    lag <- min(k, i - 1)
    
    terms <- wn[i]
    
    if (lag > 0) {
      terms <- terms + sum(coefs[1:lag] * wn[(i - 1):(i - lag)])
    }
    
    data[i] <- terms
  }
  
  return(list(data = data,
              n = n,
              k = k,
              wn = wn,
              coefs = coefs
  ))
}

gen_ar <- function(n, k, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
  data <- numeric(n)
  
  for (i in 1:n) {
    lag <- min(k, i - 1)
    
    data[i] <- wn[i] + sum(coefs[1:lag] * data[(i - 1):(i - lag)])
  }
  
  return(list(data = data,
              n = n,
              k = k,
              wn = wn,
              coefs = coefs
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
              ma_coefs = ma_coefs
  ))
}

# Simple Tests
ar <- gen_ar(n, k)
ma <- gen_ma(n, k, ar$wn, ar$coefs)
arma <- gen_arma(n, k, k, ar$wn, ar$coefs, ar$coefs)

plot.ts(arma$data)
plot.ts(ar$data)
plot.ts(ma$data)
