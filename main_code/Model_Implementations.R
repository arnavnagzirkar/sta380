# Model Parameters #####
n <- 100
k <- 2
mean <- 0
sigma <- 1

coefs_expr <- quote(seq(from = 1 - (1 / k), to = 0, by = -1 / k))
wn_expr <- quote(rnorm(n, mean = mean, sd = sigma))

# Model Generating Functions #####

# OUTDATED: Only use ARMA with p = 0 for MA, q = 0 for AR
# gen_ma <- function(n, p, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
#   data <- numeric(n)
# 
#   for (i in 1:n) {
#     lag <- min(p, i - 1)
# 
#     terms <- wn[i]
# 
#     if (lag > 0) {
#       terms <- terms + sum(coefs[1:lag] * wn[(i - 1):(i - lag)])
#     }
# 
#     data[i] <- terms
#   }
# 
#   return(list(data = data,
#               n = n,
#               p = p,
#               wn = wn,
#               ma_coefs = coefs,
#               model_type = "MA"
#   ))
# }
# 
# gen_ar <- function(n, q, wn = eval(wn_expr), coefs = eval(coefs_expr)) {
#   data <- numeric(n)
# 
#   for (i in 1:n) {
#     lag <- min(q, i - 1)
# 
#     data[i] <- wn[i] + sum(coefs[1:lag] * data[(i - 1):(i - lag)])
#   }
# 
#   return(list(data = data,
#               n = n,
#               q = q,
#               wn = wn,
#               ar_coefs = coefs,
#               model_type = "AR"
#   ))
# }

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
arma <- gen_arma(n, k, k)
ar <- gen_arma(n, k, 0, arma$wn, ar_coefs = arma$ar_coefs, ma_coefs = NULL)
ma <- gen_arma(n, 0, k, arma$wn, ar_coefs = NULL, ma_coefs = arma$ma_coefs)

plot.ts(arma$data)
plot.ts(ar$data)
plot.ts(ma$data)
