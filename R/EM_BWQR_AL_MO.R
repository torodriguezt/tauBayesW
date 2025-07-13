#' EM Algorithm for Bayesian Weighted Quantile Regression (AL)
#'
#' Implements the Expectation-Maximization (EM) algorithm for Bayesian
#' quantile regression with asymmetric Laplace (AL) likelihood and observation
#' weights. This function solves the model using moment-based approximations.
#'
#' @param y Response matrix (n × q), where each column represents a direction.
#' @param x Design matrix (n × p).
#' @param w Observation weights vector of length n.
#' @param u Unit vector (length q) indicating the direction of interest.
#' @param gamma_u Orthogonal complement direction for residual adjustment.
#' @param tau Quantile level (between 0 and 1).
#' @param mu0 Prior mean vector for \eqn{\beta}.
#' @param sigma0 Prior covariance matrix for \eqn{\beta}.
#' @param a0 Shape parameter for the inverse gamma prior on \eqn{\sigma}.
#' @param b0 Rate parameter for the inverse gamma prior on \eqn{\sigma}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{beta}{Estimated posterior mode of regression coefficients.}
#'   \item{sigma}{Estimated posterior mode of the scale parameter.}
#' }
#' @export
bayesQR_weighted_EM_cpp <- function(y, x, w, u, gamma_u,
                                    tau, mu0, sigma0, a0, b0) {
  .Call(`_tauBayesW_bayesQR_weighted_EM_cpp`,
        y, x, w, u, gamma_u, tau, mu0, sigma0, a0, b0)
}
