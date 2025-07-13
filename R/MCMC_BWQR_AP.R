#' Bayesian Weighted Quantile Regression via Adaptive Proposal (AP)
#'
#' Implements an adaptive Metropolis–Hastings algorithm for Bayesian quantile regression with observation weights,
#' using a check-loss pseudo-likelihood (Wang & He, 2007). This implementation supports models with exactly 3 covariates.
#'
#' @param y Response vector (numeric).
#' @param X Design matrix with 3 columns (numeric matrix).
#' @param w Vector of observation weights (numeric).
#' @param tau Quantile level (default is 0.5 for median regression).
#' @param n_mcmc Total number of MCMC iterations.
#' @param burnin Number of burn-in iterations.
#' @param thin Thinning interval.
#' @param w_scale Scaling factor for the weights (default: 2.0).
#' @param b0 Optional prior mean vector for \eqn{\beta}; defaults to zero vector.
#' @param B0 Optional prior covariance matrix; defaults to 100 * I.
#'
#' @return A list with:
#' \describe{
#'   \item{beta}{Matrix of posterior samples of regression coefficients.}
#'   \item{accept_rate}{Metropolis–Hastings acceptance rate.}
#'   \item{call}{Name of the function called.}
#' }
#' @export
MCMC_BWQR_AP <- function(y, X, w, tau = 0.5,
                         n_mcmc = 20000,
                         burnin = 5000,
                         thin = 100,
                         w_scale = 2.0,
                         b0 = NULL,
                         B0 = NULL) {
  .Call(`_tauBayesW_MCMC_BWQR_AP`, y, X, w, tau, n_mcmc, burnin, thin, w_scale, b0, B0)
}
