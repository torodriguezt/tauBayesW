# =====================================================
# mo.bqr.svy() - Multiple-Output Bayesian Quantile Regression for Complex Surveys (EM)
# =====================================================

#' Multiple-Output Bayesian Quantile Regression for Complex Surveys (EM)
#'
#' Fits a multiple-output Bayesian quantile regression model under complex survey
#' weights using the Expectation-Maximization (EM) algorithm implemented in C++.
#' Currently only the \code{"em"} algorithm is supported.
#'
#' @param formula An object of class \code{\link{formula}} specifying the model.
#' @param weights Optional survey weights. Can be a numeric vector of length equal
#'   to the number of observations.
#' @param data A \code{data.frame} containing the variables in the model.
#' @param quantile Numeric vector of quantiles in (0, 1) to estimate. Multiple
#'   quantiles can be passed.
#' @param algorithm Character string; currently only \code{"em"} is implemented.
#' @param prior A list containing prior parameters:
#'   \describe{
#'     \item{\code{beta_mean}}{Prior mean vector for regression coefficients.}
#'     \item{\code{beta_cov}}{Prior covariance matrix for regression coefficients.}
#'     \item{\code{sigma_shape}}{Shape parameter for inverse-gamma prior on sigma\eqn{^2}.}
#'     \item{\code{sigma_rate}}{Rate parameter for inverse-gamma prior on sigma\eqn{^2}.}
#'   }
#'   If \code{NULL}, default vague priors are used.
#' @param n_dir Integer; number of directional quantiles to estimate (default 1).
#' @param epsilon Convergence tolerance for EM.
#' @param max_iter Maximum number of EM iterations.
#' @param verbose Logical; if \code{TRUE}, prints progress messages.
#' @param niter,burnin,thin Ignored for EM, but included for compatibility with MCMC
#'   interfaces.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"mo.bqr.svy"}, which is a list containing:
#' \describe{
#'   \item{\code{call}}{Matched call.}
#'   \item{\code{formula}}{Model formula.}
#'   \item{\code{terms}}{Model terms object.}
#'   \item{\code{quantile}}{Quantile levels estimated.}
#'   \item{\code{algorithm}}{Algorithm used.}
#'   \item{\code{prior}}{Prior specification used.}
#'   \item{\code{fit}}{List of fitted results for each quantile.}
#'   \item{\code{coefficients}}{Vector of coefficients for the first quantile.}
#'   \item{\code{n_dir}}{Number of directional quantiles estimated.}
#' }
#'
#' @details
#' The EM algorithm iteratively maximizes the posterior mode under the specified
#' prior distribution. For each quantile \eqn{\tau}, the weighted check loss is
#' minimized under Bayesian regularization.
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{simulate_mo_bqr_data}}
#'
#' @examples
#' sim <- simulate_mo_bqr_data(n = 50, p = 2)
#' fit <- mo.bqr.svy(y ~ x1 + x2, weights = sim$weights, data = sim$data,
#'                   quantile = c(0.1, 0.5, 0.9), algorithm = "em", max_iter = 200)
#' print(fit)
#'
#' @export
#' @importFrom stats model.frame model.matrix model.response
mo.bqr.svy <- function(formula,
                       weights  = NULL,
                       data,
                       quantile = 0.5,
                       algorithm = "em",
                       prior    = NULL,
                       n_dir    = 1,
                       epsilon  = 1e-6,
                       max_iter = 1000,
                       verbose  = FALSE,
                       niter    = NULL,
                       burnin   = NULL,
                       thin     = NULL,
                       ...) {

  if (algorithm != "em")
    stop("Only 'em' is implemented.")
  if (missing(data))
    stop("'data' must be provided.")

  if (!is.null(niter)) {
    warning("'niter' ignored for EM; using as 'max_iter' instead.")
    max_iter <- niter
  }
  if (!is.null(burnin)) warning("'burnin' ignored for EM.")
  if (!is.null(thin))   warning("'thin' ignored for EM.")

  if (length(quantile) == 0)
    stop("'quantile' cannot be empty.")
  if (any(!is.finite(quantile)))
    stop("'quantile' must be numeric and finite.")
  if (any(quantile <= 0 | quantile >= 1))
    stop("'quantile' must be between 0 and 1 (exclusive).")

  quantile <- sort(quantile)

  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  X  <- model.matrix(attr(mf, "terms"), mf)
  n  <- length(y)
  wts <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(wts) != n)  stop("'weights' must have length n.")
  if (any(!is.finite(wts)) || any(wts <= 0)) stop("Invalid weights.")
  wts <- wts / mean(wts)
  if (any(!is.finite(y))) stop("Response 'y' contains non-finite values.")

  p <- ncol(X)

  if (is.null(prior)) prior <- list(
    beta_mean   = rep(0, p),
    beta_cov    = diag(1e6, p),
    sigma_shape = 0.001,
    sigma_rate  = 0.001)

  if (!all(c("beta_mean", "beta_cov", "sigma_shape", "sigma_rate") %in% names(prior)))
    stop("'prior' must contain beta_mean, beta_cov, sigma_shape, sigma_rate.")

  results <- vector("list", length(quantile))
  names(results) <- paste0("q", quantile)

  for (qi in seq_along(quantile)) {
    q <- quantile[qi]

    y_matrix <- matrix(y, ncol = 1)
    u        <- matrix(1.0, 1, 1)
    gamma_u  <- matrix(0.0, 1, 1)

    m      <- p + 1
    mu0    <- c(prior$beta_mean, 0)
    sigma0 <- diag(1e6, m)
    sigma0[1:p, 1:p] <- prior$beta_cov

    if (verbose) message(sprintf("Fitting tau = %.3f", q))

    cpp_result <- .bwqr_weighted_em_cpp(
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

  coefficients_all <- as.numeric(results[[1]]$beta)

  structure(list(
    call         = match.call(),
    formula      = formula,
    terms        = attr(mf, "terms"),
    quantile     = quantile,
    algorithm    = algorithm,
    prior        = prior,
    fit          = results,
    coefficients = coefficients_all,
    n_dir        = n_dir
  ), class = "mo.bqr.svy")
}

rho_q <- function(u, q) u * (q - as.numeric(u < 0))

#' Print method for mo.bqr.svy objects
#'
#' @param x An object of class \code{mo.bqr.svy}.
#' @param ... Further arguments passed to or from other methods.
#' @export
print.mo.bqr.svy <- function(x, ...) {
  cat("Call:\n"); print(x$call)
  for (i in seq_along(x$quantile)) {
    cat(sprintf("\nQuantile: %.2f\n", x$quantile[i]))
    print(x$fit[[i]]$beta)
  }
  invisible(x)
}

#' Simulate data for Multiple-Output Bayesian Quantile Regression
#'
#' Generates synthetic data suitable for \code{\link{mo.bqr.svy}}. Produces a response,
#' predictors, survey weights, and true coefficients.
#'
#' @param n Number of observations.
#' @param p Number of predictors (excluding intercept).
#' @param beta_true Optional numeric vector of length \code{p + 1} containing the
#'   intercept and slopes. If \code{NULL}, defaults to \code{c(1, 1, 2, ..., p)}.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{data}}{\code{data.frame} with response \code{y} and predictors.}
#'   \item{\code{weights}}{Numeric vector of survey weights.}
#'   \item{\code{true_betas}}{Matrix with the true coefficients.}
#'   \item{\code{quantiles}}{Vector of example quantiles.}
#' }
#'
#' @examples
#' sim <- simulate_mo_bqr_data(n = 50, p = 3)
#' head(sim$data)
#'
#' @export
simulate_mo_bqr_data <- function(n = 100, p = 2, beta_true = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("x", 1:p)

  if (is.null(beta_true)) {
    beta_true <- c(1, seq(1, p))
  } else {
    if (length(beta_true) != (p + 1)) {
      stop("La longitud de beta_true debe ser p + 1 (intercepto + coeficientes).")
    }
  }

  y <- beta_true[1] + X %*% beta_true[-1] + rnorm(n, 0, 1)

  list(
    data = data.frame(y = as.numeric(y), X),
    weights = runif(n, 0.5, 2),
    true_betas = matrix(beta_true, nrow = 1),
    quantiles = c(0.1, 0.5, 0.9)
  )
}
