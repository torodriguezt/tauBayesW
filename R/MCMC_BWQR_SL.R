#' Bayesian Weighted Quantile Regression via Semi-Likelihood (SL)
#'
#' Implements a Metropolis–Hastings sampler for Bayesian quantile regression with observation weights
#' using the semi-likelihood (SL) approach proposed by Wang & He. This version is limited to models
#' with exactly 3 covariates.
#'
#' @param y Response vector (numeric).
#' @param X Design matrix with 3 columns (numeric matrix).
#' @param w Vector of observation weights (numeric).
#' @param tau Quantile level (default is 0.5 for median regression).
#' @param n_mcmc Total number of MCMC iterations.
#' @param burnin Number of burn-in iterations.
#' @param thin Thinning interval.
#' @param b0 Optional prior mean vector for \eqn{\beta}; defaults to zero vector.
#' @param B0 Optional prior covariance matrix; defaults to 1000 * I.
#'
#' @return A list with:
#' \describe{
#'   \item{beta}{Matrix of posterior samples of regression coefficients.}
#'   \item{accept_rate}{Metropolis–Hastings acceptance rate.}
#'   \item{call}{Name of the function called.}
#' }
#' @export
MCMC_BWQR_SL <- function(y, X, w, tau = 0.5,
                         n_mcmc = 10000,
                         burnin = 2000,
                         thin = 10,
                         b0 = NULL,
                         B0 = NULL) {
  .Call(`_tauBayesW_MCMC_BWQR_SL`, y, X, w, tau, n_mcmc, burnin, thin, b0, B0)
}
