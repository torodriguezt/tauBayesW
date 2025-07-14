#' Sample v from Generalized Inverse Gaussian (GIG)
#'
#' Draws samples from the Generalized Inverse Gaussian distribution using the
#' \code{GIGrvg::rgig} function.
#'
#' @param y Vector of responses.
#' @param X Design matrix.
#' @param w Vector of observation weights.
#' @param beta Vector of regression coefficients.
#' @param delta2 Scalar parameter.
#' @param theta Scalar parameter.
#' @param sigma Scalar scale parameter.
#' @param N Number of observations.
#'
#' @return A vector of sampled latent variables \code{v}.
#' @export
atualizarV_GIG <- function(y, X, w, beta, delta2, theta, sigma, N) {
  .Call(`_tauBayesW_atualizarV_GIG`, y, X, w, beta, delta2, theta, sigma, N)
}

#' Update beta coefficients
#'
#' Samples \eqn{\beta} from its conditional posterior distribution given the
#' current values of the latent variables.
#'
#' @inheritParams atualizarV_GIG
#' @param b Mean vector of the prior.
#' @param B Covariance matrix of the prior.
#'
#' @return A vector sampled from the posterior of \eqn{\beta}.
#' @export
atualizarBETA <- function(b, B, X, w, sigma, delta2, theta, v, y) {
  .Call(`_tauBayesW_atualizarBETA`, b, B, X, w, sigma, delta2, theta, v, y)
}

#' Update sigma
#'
#' Samples \eqn{\sigma} from its conditional posterior distribution.
#'
#' @inheritParams atualizarV_GIG
#' @param beta Current value of the regression coefficients.
#' @param tau2 Scalar parameter.
#' @param c0,C0 Prior parameters for the inverse gamma distribution.
#' @param n Number of observations.
#'
#' @return A scalar sampled from the posterior of \eqn{\sigma}.
#' @export
atualizarSIGMA <- function(c0, C0, X, w, beta, tau2, theta, v, y, n) {
  .Call(`_tauBayesW_atualizarSIGMA`, c0, C0, X, w, beta, tau2, theta, v, y, n)
}

#' Gibbs Sampler for Non-Crossing BWQR with Asymmetric Laplace Errors
#'
#' Runs the full Gibbs sampler for Bayesian Weighted Quantile Regression under
#' asymmetric Laplace errors using the Non-Crossing model.
#'
#' @inheritParams atualizarV_GIG
#' @param tau Quantile level (e.g., 0.5 for median regression).
#' @param n_mcmc Total number of iterations.
#' @param burnin_mcmc Number of burn-in iterations.
#' @param thin_mcmc Thinning interval.
#'
#' @return A list with posterior samples of \code{beta} and \code{sigma}.
#' @export
#' @export
NonCrossingBWQR_AL <- function(y, X, w, tau = 0.5,
                               n_mcmc = 50000,
                               burnin_mcmc = 10000,
                               thin_mcmc = 10) {
  .Call(`_tauBayesW_NonCrossingBWQR_AL`, y, X, w, tau,
        n_mcmc, burnin_mcmc, thin_mcmc)
}
