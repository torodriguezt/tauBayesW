# mo_bqr_svy.R # # -----------------------------------------------------------------------------

#   * Prior is now passed as a weak covariance (diag 1e6) -> inverted internallytiple-Output Bayesian Quantile Regression for Complex Surveys (EM algorithm)
# Fixes:
#   * Prior is now passed as a weak covariance (diag 1e6) -> inverted internallytable version (August 2025)
# -----------------------------------------------------------------------------
# Multiple-Output Bayesian Quantile Regression for Complex Surveys (EM algorithm)
# Fixes:
#   * Prior is now passed as a weak covariance (diag 1e6) - inverted internally
#     in the back-end; avoids collapse of beta to 0.
#   * gamma_u = 0 in the univariate case (orthogonal to u).
# -----------------------------------------------------------------------------

#' Multiple-Output Bayesian Quantile Regression for Complex Surveys
#'
#' Fits a Bayesian quantile regression model under asymmetric Laplace
#' likelihood using an EM algorithm, accounting for survey weights.
#' Supports multiple quantiles simultaneously. - stable version (August 2025)
# -----------------------------------------------------------------------------
# Multiple‑Output Bayesian Quantile Regression for Complex Surveys (EM algorithm)
# Fixes:
#   * Prior is now passed as a weak covariance (diag 1e6) → inverted internally
#     in the back-end; avoids collapse of beta to 0.
#   * gamma_u = 0 in the univariate case (orthogonal to u).
# -----------------------------------------------------------------------------

#' Multiple-Output Bayesian Quantile Regression for Complex Surveys
#'
#' Fits a Bayesian quantile regression model under asymmetric Laplace
#' likelihood using an EM algorithm, accounting for survey weights.
#' Supports multiple quantiles simultaneously.
#'
#' @param formula  Model formula (`y ~ x1 + x2`)
#' @param weights  Survey weights (numeric) or `NULL`
#' @param data     Data frame with variables used in the formula
#' @param quantile Scalar or vector of quantiles (default 0.5)
#' @param algorithm Currently only `'em'`
#' @param prior    List with `beta_mean`, `beta_cov`, `sigma_shape`, `sigma_rate`
#' @param n_dir    Number of directions (reserved for future extensions)
#' @param epsilon  EM convergence tolerance
#' @param max_iter Maximum number of EM iterations
#' @param verbose  If `TRUE`, prints progress
#'
#' @return An object of class `mo.bqr.svy`
#' @export
#' @import stats
mo.bqr.svy <- function(formula,
                       weights  = NULL,
                       data,
                       quantile = 0.5,
                       algorithm = "em",
                       prior    = NULL,
                       n_dir    = 1,
                       epsilon  = 1e-6,
                       max_iter = 1000,
                       verbose  = FALSE) {
  
  # ---------------------------
  # 1. Input checks
  # ---------------------------
  if (algorithm != "em")
    stop("Only 'em' is implemented.")
  if (missing(data))
    stop("'data' must be provided.")
  
  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  X  <- model.matrix(attr(mf, "terms"), mf)
  n  <- length(y)
  
  # ---------------- Survey weights -------------------------------
  wts <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(wts) != n)  stop("'weights' must have length n.")
  if (any(!is.finite(wts)) || any(wts <= 0))
    stop("Invalid weights.")
  wts <- wts / mean(wts)  # normalize
  
  if (any(!is.finite(y))) stop("Response 'y' contains non-finite values.")
  
  p <- ncol(X)
  
  # ---------------- Prior (weak covariance) ----------------------
  if (is.null(prior)) prior <- list(
    beta_mean   = rep(0, p),
    beta_cov    = diag(1e6, p),
    sigma_shape = 0.001,
    sigma_rate  = 0.001)
  
  if (!all(c("beta_mean", "beta_cov", "sigma_shape", "sigma_rate") %in% names(prior)))
    stop("'prior' must contain beta_mean, beta_cov, sigma_shape, sigma_rate.")
  
  # ---------------- Output container -----------------------------
  results <- vector("list", length(quantile))
  names(results) <- paste0("q", quantile)
  
  # ---------------------------
  # 2. Loop over quantiles
  # ---------------------------
  for (qi in seq_along(quantile)) {
    q <- quantile[qi]
    if (q <= 0 || q >= 1) stop("'quantile' must be in (0,1).")
    
    # ----- Data for C++ backend ----------------------------------
    y_matrix <- matrix(y, ncol = 1)
    u        <- matrix(1.0, 1, 1)  # main direction
    gamma_u  <- matrix(0.0, 1, 1)  # orthogonal in 1-D
    
    # ----- Expanded prior ----------------------------------------
    m      <- p + 1
    mu0    <- c(prior$beta_mean, 0)
    sigma0 <- diag(1e6, m)
    sigma0[1:p, 1:p] <- prior$beta_cov
    
    if (verbose) message(sprintf("Fitting tau = %.3f", q))
    
    # ----- Call to C++ backend -----------------------------------
    cpp_result <- bayesQR_weighted_EM_cpp(
      y        = y_matrix,
      x        = X,
      w        = wts,
      u        = u,
      gamma_u  = gamma_u,
      tau      = q,
      mu0      = mu0,
      sigma0   = sigma0,
      a0       = prior$sigma_shape,
      b0       = prior$sigma_rate,
      eps      = epsilon,
      max_iter = max_iter,
      verbose  = verbose)
    
    beta_final  <- cpp_result$beta[1:p]
    sigma_final <- cpp_result$sigma
    
    results[[qi]] <- list(beta      = as.numeric(beta_final),
                          sigma     = as.numeric(sigma_final),
                          iter      = cpp_result$iter,
                          converged = cpp_result$converged)
  }
  
  structure(list(call      = match.call(),
                 quantile  = quantile,
                 algorithm = algorithm,
                 prior     = prior,
                 fit       = results,
                 n_dir     = n_dir),
            class = "mo.bqr.svy")
}

# -----------------------------------------------------------------# Utilities
rho_q <- function(u, q) u * (q - as.numeric(u < 0))

#' @export
print.mo.bqr.svy <- function(x, ...) {
  cat("Call:\n"); print(x$call)
  for (i in seq_along(x$quantile)) {
    cat(sprintf("\nQuantile: %.2f\n", x$quantile[i]))
    print(x$fit[[i]]$beta)
  }
  invisible(x)
}

#' Simulate Data for Multiple Output Bayesian Quantile Regression
#'
#' Generates simulated data suitable for testing multiple output Bayesian
#' quantile regression models with survey weights.
#'
#' @param n Integer. Number of observations to generate. Default is 200.
#' @param beta Numeric vector. True regression coefficients. Default is c(1, 2, -1).
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#' @param b Numeric. Scale parameter for the Laplace error distribution. Default is 1.
#'
#' @return A list containing:
#'   \item{data}{A data frame with response variable y and covariates x1, x2}
#'   \item{weights}{A numeric vector of survey weights}
#'
#' @details This function generates data following a linear model with Laplace
#' errors, which is suitable for quantile regression analysis. The covariates
#' x1 and x2 are generated from normal and uniform distributions respectively.
#' Survey weights are generated from a uniform distribution.
#'
#' @examples
#' \dontrun{
#' # Generate simulated data
#' sim_data <- simulate_mo_bqr_data(n = 100, seed = 123)
#' 
#' # Use with mo.bqr.svy
#' result <- mo.bqr.svy(y ~ x1 + x2, data = sim_data$data, 
#'                      weights = sim_data$weights, quantiles = c(0.25, 0.5, 0.75))
#' }
#'
#' @export
simulate_mo_bqr_data <- function(n = 200,
                                 beta = c(1, 2, -1),
                                 seed = NULL,
                                 b = 1) {
  if (!is.null(seed)) set.seed(seed)
  x1 <- rnorm(n); x2 <- runif(n)
  laplace <- function(n, mu = 0, b = 1) {
    u <- runif(n, -0.5, 0.5)
    mu - b * sign(u) * log(1 - 2 * abs(u))
  }
  eps <- laplace(n, b)
  y   <- beta[1] + beta[2]*x1 + beta[3]*x2 + eps
  w   <- runif(n, 0.5, 2)
  list(data = data.frame(y, x1, x2), weights = w)
}
