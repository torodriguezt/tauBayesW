#' Bayesian Weighted Quantile Regression via Asymmetric Laplace (AL) model
#'
#' Runs a Gibbs sampler for Bayesian quantile regression using the asymmetric Laplace (AL) likelihood
#' and observation-specific weights. This implementation uses an efficient inverse-Gaussian sampler
#' for the latent variable updates and is robust to matrix singularities via adaptive regularization.
#'
#' @param y Vector of responses (numeric).
#' @param X Design matrix (numeric matrix).
#' @param w Vector of observation weights (numeric).
#' @param tau Quantile level (e.g., 0.5 for median). Must be in (0, 1).
#' @param n_mcmc Total number of MCMC iterations (integer).
#' @param burnin Number of burn-in iterations (integer).
#' @param thin Thinning interval for posterior samples (integer).
#'
#' @return A list with posterior samples:
#' \describe{
#'   \item{beta}{Matrix of posterior samples for regression coefficients.}
#'   \item{sigma}{Vector of posterior samples for the scale parameter.}
#' }
#'
#' @export
MCMC_BWQR_AL <- function(y, X, w, tau = 0.5, n_mcmc = 50000, burnin = 10000, thin = 10) {
  .Call(`_tauBayesW_MCMC_BWQR_AL`, y, X, w, tau, n_mcmc, burnin, thin)
}
