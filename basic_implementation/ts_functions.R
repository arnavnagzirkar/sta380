#' @description
#' This function procedurally generates univariate AR(p), MA(q), or
#' ARMA(p, q) time series by simulation.
#'
#' @param n Integer. Number of time steps to generate.
#' @param wn Numeric vector of length \code{n}. The white noise sequence,
#' typically i.i.d. from \code{rnorm()}.
#' @param ar_coefs Numeric vector of length \code{p}. The AR
#' coefficients \eqn{(\phi_1, \ldots, \phi_p)}.
#' @param ma_coefs Numeric vector of length \code{q}. The MA
#' coefficients \eqn{(\theta_1, \ldots, \theta_q)}.
#'
#' @return
#' A list containing model details:
#' \itemize{
#'   \item \code{data} – the generated ARMA time series of length \code{n}
#'   \item \code{n} – number of observations
#'   \item \code{p} – AR order
#'   \item \code{q} – MA order
#'   \item \code{wn} – the white noise vector
#'   \item \code{ar_coefs} – AR coefficients
#'   \item \code{ma_coefs} – MA coefficients
#' }
#'
#' @examples
#' set.seed(1)
#' n <- 100
#' phi <- c(0.5, 0.2)
#' theta <- -0.3
#' wn <- rnorm(n)
#' sim <- gen_arma(n, wn, ar_coefs = phi, ma_coefs = theta)
#' plot.ts(sim$data)
#'
#' @export
gen_arma <- function(n, wn, ar_coefs, ma_coefs) {

  p <- length(ar_coefs)
  q <- length(ma_coefs)

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

#' @description
#' Converts an ARMA(p, q) model into its infinite-order MA representation
#' by computing the first \code{max_lag} MA coefficients \eqn{\psi_j}.
#' The algorithm follows the method in
#' *Introduction to Time Series and Forecasting* by Brockwell & Davis (3rd ed.).
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#' @param max_lag Integer. Number of MA coefficients \eqn{\psi_j} to compute.
#' The output vector has length \code{max_lag + 1}, as \eqn{\psi_0 = 1}.
#'
#' @details
#' The function computes the coefficients in the infinite moving-average expansion
#' of an ARMA(p, q) model:
#' \deqn{
#'   X_t = \sum_{j=0}^{\infty} \psi_j W_{t-j},
#' }
#' where:
#' \deqn{
#'   \psi_j = \theta_j + \sum_{k=1}^{\min(p, j)} \phi_k \psi_{j-k},
#' }
#' and \eqn{\theta_j = 0} for \eqn{j > q}.
#'
#' @return
#' A numeric vector of length \code{max_lag + 1} containing the MA coefficients
#' \eqn{\psi_0,\psi_1,\ldots,\psi_{max\_lag}} of the infinite-order MA representation.
#'
#' @examples
#' n <- 100
#' phi <- c(0.5, 0.2)
#' theta <- -0.3
#' wn <- rnorm(n)
#' model <- gen_arma(n, wn, ar_coefs = phi, ma_coefs = theta)
#'
#' arma_to_ma(model, max_lag = 10)
#'
#' @export
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

#' @description
#' Checks whether an ARMA model is causal. This can be determined by
#' whether its AR polynomial has all roots strictly outside the unit circle.
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#'
#' @details
#' The AR characteristic polynomial is:
#' \deqn{
#'   1 - \phi_1 z - \cdots - \phi_p z^p = 0.
#' }
#' The process is causal if all roots satisfy \eqn{|z| > 1}.
#' If the model has no AR part (\code{ar_coefs = NULL}), the function returns TRUE.
#'
#' @return
#' Logical.
#' \code{TRUE} if the AR component is causal, \code{FALSE} otherwise.
#'
#' @examples
#' n <- 100
#' wn <- rnorm(n)
#' model1 <- gen_arma(n, wn, ar_coefs = 0.2, ma_coefs = NULL)
#' model2 <- gen_arma(n, wn, ar_coefs = 1.2, ma_coefs = NULL)
#'
#' is_casual(model1) # TRUE
#' is_casual(model2) # FALSE
#'
#' @export
is_causal <- function(model) {
  if(is.null(model$ar_coefs)) return(TRUE)

  poly = c(1, -model$ar_coefs)

  roots <- polyroot(poly)

  all(abs(roots) > 1)
}

#' @description
#' Computes the theoretical autocovariance function (ACVF) of an
#' ARMA(p, q) model using its infinite-order MA representation.
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#' @param sigma Numeric. Standard deviation of the white noise process.
#' @param max_lag Integer. Maximum lag for which to compute the ACVF.
#'
#' @details
#' The ACVF of an ARMA model can be written using its MA coefficients \eqn{\psi_j}:
#' \deqn{
#'   \gamma(j) = \sigma^2 \sum_{k=0}^{\infty} \psi_k \psi_{k+j}.
#' }
#' This function computes \eqn{\psi_j} up to \code{max_lag} using
#' \code{arma_to_ma()} and approximates the infinite sum accordingly.
#'
#' @return
#' A numeric vector of length \code{max_lag + 1} containing:
#' \deqn{\gamma(0), \gamma(1), \ldots, \gamma(\text{max\_lag}).}
#'
#' @examples
#' n <- 100
#' phi <- c(0.5, 0.2)
#' theta <- -0.3
#' wn <- rnorm(n)
#' model <- gen_arma(n, wn, ar_coefs = phi, ma_coefs = theta)
#'
#' get_theoretical_acvf(model, sigma = 1, max_lag = 10)
#'
#' @export
get_theoretical_acvf <- function(model, sigma, max_lag = 30) {
  psi <- arma_to_ma(model, max_lag)

  sapply(0:max_lag, function(j) {
    sigma^2 * sum(psi[1:(length(psi) - j)] * psi[(1 + j):length(psi)])
  })
}

#' @description
#' Computes the theoretical autocorrelation function (ACF) of an
#' ARMA(p, q) model.
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#' @param sigma Numeric. Standard deviation of the white noise process.
#' @param max_lag Integer. Maximum lag for which to compute the ACF.
#'
#' @details
#' The ACF is obtained by normalizing the ACVF:
#' \deqn{
#'   \rho(j) = \frac{\gamma(j)}{\gamma(0)}.
#' }
#'
#' Note: If the AR part is not causal, a warning is printed and the function
#' still returns the normalized partial ACF.
#'
#' @return
#' A numeric vector of length \code{max_lag + 1} containing:
#' \deqn{\rho(0), \rho(1), \ldots, \rho(\text{max\_lag}).}
#'
#' @examples
#' n <- 100
#' phi <- c(0.5, 0.2)
#' theta <- -0.3
#' wn <- rnorm(n)
#' model <- gen_arma(n, wn, ar_coefs = phi, ma_coefs = theta)
#'
#' get_theoretical_acf(model, sigma = 1, max_lag = 10)
#'
#' @export
get_theoretical_acf <- function(model, sigma, max_lag = 30) {
  if(!is_causal(model)) {
    print("AR part is not casual, so returned ACF may not be accurate.")
  }

  acvf <- get_theoretical_acvf(model, sigma, max_lag)

  return(acvf / acvf[1])
}

#' @description
#' Computes the sample autocovariance function (ACVF) of a time series
#' generated from an ARMA model. The sample ACVF is computed using the
#' standard unbiased-denominator formula:
#' \deqn{
#'   \hat{\gamma}(h) = \frac{1}{n} \sum_{t=1}^{n-h} (X_t - \bar{X})(X_{t+h} - \bar{X}).
#' }
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#' @param max_lag Integer. Maximum lag for which to compute the sample ACVF.
#' Defaults to \code{floor(n / 2)}.
#'
#' @return
#' A numeric vector of length \code{max_lag + 1} containing:
#' \deqn{\hat{\gamma}(0), \hat{\gamma}(1), \ldots, \hat{\gamma}(\text{max\_lag}).}
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' wn <- rnorm(n)
#' model <- gen_arma(n, wn, ar_coefs = 0.5, ma_coefs = NULL)
#'
#' get_sample_acvf(model, max_lag = 10)
#'
#' @export
get_sample_acvf <- function(model, max_lag = NULL) {
  x <- model$data
  n <- model$n

  if (is.null(max_lag)) {
    max_lag <- floor(n / 2)
  }

  x_bar <- mean(x)
  x_centered <- x - x_bar

  sapply(0:max_lag, function(h) {
    (1 / n) * sum(x_centered[1:(n - h)] * x_centered[(1 + h):n])
  })
}

#' @description
#' Computes the sample autocorrelation function (ACF) of a time series
#' generated from an ARMA model. Values are obtained by normalizing the
#' sample ACVF by its value at lag 0:
#' \deqn{
#'   \hat{\rho}(h) = \frac{\hat{\gamma}(h)}{\hat{\gamma}(0)}.
#' }
#'
#' @param model A list containing the output ARMA model from function \code{gen_arma()}
#' @param max_lag Integer. Maximum lag for which to compute the sample ACF.
#' Defaults to \code{floor(n / 2)}.
#'
#' @return
#' A numeric vector of length \code{max_lag + 1} containing:
#' \deqn{\hat{\rho}(0), \hat{\rho}(1), \ldots, \hat{\rho}(\text{max\_lag}).}
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' wn <- rnorm(n)
#' model <- gen_arma(n, wn, ar_coefs = 0.5, ma_coefs = NULL)
#'
#' get_sample_acf(model, max_lag = 10)
#'
#' @export
get_sample_acf <- function(model, max_lag = NULL) {
  acvf <- get_sample_acvf(model, max_lag)
  acvf / acvf[1]
}
