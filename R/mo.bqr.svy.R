# =====================================================
#  Helpers
# =====================================================

if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a


# =====================================================
#  Prior (constructor, coercion, print) for mo.bqr.svy
# =====================================================

new_mo_bqr_prior <- function(beta_mean, beta_cov, sigma_shape, sigma_rate, names = NULL) {
  if (!is.null(names)) {
    names(beta_mean) <- names
    dimnames(beta_cov) <- list(names, names)
  }
  structure(
    list(
      beta_mean   = beta_mean,
      beta_cov    = beta_cov,
      sigma_shape = sigma_shape,
      sigma_rate  = sigma_rate
    ),
    class = "mo_bqr_prior"
  )
}

#' Default prior for Multiple-Output BQR (EM)
#'
#' Creates a prior object (class \code{"mo_bqr_prior"}) for \code{\link{mo.bqr.svy}}.
#' The regression coefficients have a multivariate normal prior, and the noise
#' variance has an inverse-gamma prior specified via shape/rate.
#'
#' @param p Number of regression coefficients (including the intercept).
#' @param beta_mean Numeric vector of length \code{p}. A scalar is expanded to length \code{p}.
#' @param beta_cov Prior covariance for coefficients. May be:
#'   \itemize{
#'     \item a \code{p x p} matrix,
#'     \item a scalar (expanded to \code{diag(scalar, p)}), or
#'     \item a length-\code{p} vector (expanded to \code{diag(vector)}).
#'   }
#' @param sigma_shape Positive scalar (shape of the inverse-gamma prior on \eqn{\sigma^2}).
#' @param sigma_rate Positive scalar (rate of the inverse-gamma prior on \eqn{\sigma^2}).
#' @param names Optional coefficient names to attach to \code{beta_mean} and \code{beta_cov}.
#'
#' @return An object of class \code{"mo_bqr_prior"} with fields \code{beta_mean},
#'   \code{beta_cov}, \code{sigma_shape}, and \code{sigma_rate}.
#'
#' @examples
#' pr <- mo_prior_default(p = 3, beta_mean = 0, beta_cov = 1e6)
#' pr
#' @export
mo_prior_default <- function(p,
                             beta_mean   = rep(0, p),
                             beta_cov    = diag(1e6, p),
                             sigma_shape = 0.001,
                             sigma_rate  = 0.001,
                             names       = NULL) {
  # Expand scalars/vectors
  if (length(beta_mean) == 1L) beta_mean <- rep(beta_mean, p)
  if (is.numeric(beta_cov) && length(beta_cov) == 1L) beta_cov <- diag(beta_cov, p)
  if (is.numeric(beta_cov) && is.null(dim(beta_cov)) && length(beta_cov) == p)
    beta_cov <- diag(beta_cov, p)

  # Validate
  if (length(beta_mean) != p)
    stop("length(beta_mean) must be ", p, ".", call. = FALSE)
  if (!is.matrix(beta_cov) || any(dim(beta_cov) != c(p, p)))
    stop("beta_cov must be a ", p, "x", p, " matrix.", call. = FALSE)
  if (!is.numeric(sigma_shape) || length(sigma_shape) != 1L || sigma_shape <= 0)
    stop("sigma_shape must be a positive scalar.", call. = FALSE)
  if (!is.numeric(sigma_rate) || length(sigma_rate) != 1L || sigma_rate <= 0)
    stop("sigma_rate must be a positive scalar.", call. = FALSE)

  new_mo_bqr_prior(
    beta_mean   = beta_mean,
    beta_cov    = beta_cov,
    sigma_shape = sigma_shape,
    sigma_rate  = sigma_rate,
    names       = names
  )
}

#' Coerce to a \code{mo_bqr_prior} object
#'
#' Allows passing legacy list priors to \code{\link{mo.bqr.svy}}. A valid list must
#' contain \code{beta_mean}, \code{beta_cov}, \code{sigma_shape}, and \code{sigma_rate}.
#' Scalars/vectors are expanded as in \code{\link{mo_prior_default}}.
#'
#' @param x A \code{mo_bqr_prior} or a list with components
#'   \code{beta_mean}, \code{beta_cov}, \code{sigma_shape}, \code{sigma_rate}.
#' @param p Number of coefficients.
#' @param names Optional coefficient names.
#'
#' @return A \code{mo_bqr_prior} object.
#' @export
as_mo_bqr_prior <- function(x, p, names = NULL) {
  if (inherits(x, "mo_bqr_prior")) {
    if (!is.null(names) && is.null(dimnames(x$beta_cov))) {
      x$beta_mean <- setNames(x$beta_mean, names)
      dimnames(x$beta_cov) <- list(names, names)
    }
    return(x)
  }

  if (!is.list(x) || !all(c("beta_mean", "beta_cov", "sigma_shape", "sigma_rate") %in% names(x)))
    stop("'prior' must be a 'mo_bqr_prior' object or a list with ",
         "beta_mean, beta_cov, sigma_shape, sigma_rate.", call. = FALSE)

  beta_mean   <- x$beta_mean
  beta_cov    <- x$beta_cov
  sigma_shape <- x$sigma_shape
  sigma_rate  <- x$sigma_rate

  # Expand
  if (length(beta_mean) == 1L) beta_mean <- rep(beta_mean, p)
  if (is.numeric(beta_cov) && length(beta_cov) == 1L) beta_cov <- diag(beta_cov, p)
  if (is.numeric(beta_cov) && is.null(dim(beta_cov)) && length(beta_cov) == p)
    beta_cov <- diag(beta_cov, p)

  # Validate
  if (length(beta_mean) != p)
    stop("length(beta_mean) must be ", p, ".", call. = FALSE)
  if (!is.matrix(beta_cov) || any(dim(beta_cov) != c(p, p)))
    stop("beta_cov must be a ", p, "x", p, " matrix.", call. = FALSE)
  if (!is.numeric(sigma_shape) || length(sigma_shape) != 1L || sigma_shape <= 0)
    stop("sigma_shape must be a positive scalar.", call. = FALSE)
  if (!is.numeric(sigma_rate) || length(sigma_rate) != 1L || sigma_rate <= 0)
    stop("sigma_rate must be a positive scalar.", call. = FALSE)

  new_mo_bqr_prior(
    beta_mean   = beta_mean,
    beta_cov    = beta_cov,
    sigma_shape = sigma_shape,
    sigma_rate  = sigma_rate,
    names       = names
  )
}

#' @export
print.mo_bqr_prior <- function(x, ...) {
  cat("mo_bqr_prior\n")
  cat("  beta_mean: length", length(x$beta_mean), "\n")
  cat("  beta_cov:", nrow(x$beta_cov), "x", ncol(x$beta_cov), "\n")
  cat("  sigma_shape:", x$sigma_shape, "\n")
  cat("  sigma_rate :", x$sigma_rate, "\n")
  invisible(x)
}


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
#' @param prior A \code{mo_bqr_prior} created by \code{\link{mo_prior_default}}
#'   (or a compatible list; it will be coerced via \code{\link{as_mo_bqr_prior}}).
#'   Contains:
#'   \describe{
#'     \item{\code{beta_mean}}{Prior mean vector for regression coefficients.}
#'     \item{\code{beta_cov}}{Prior covariance matrix for regression coefficients.}
#'     \item{\code{sigma_shape}}{Shape parameter for inverse-gamma prior on \eqn{\sigma^2}.}
#'     \item{\code{sigma_rate}}{Rate parameter for inverse-gamma prior on \eqn{\sigma^2}.}
#'   }
#'   If \code{NULL}, a default vague prior is used (\code{\link{mo_prior_default}}).
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
#' @seealso \code{\link{mo_prior_default}}, \code{\link{as_mo_bqr_prior}},
#'   \code{\link{bqr.svy}}, \code{\link{simulate_mo_bqr_data}}
#'
#' @examples
#' sim <- simulate_mo_bqr_data(n = 50, p = 2)
#' pr  <- mo_prior_default(p = 3)  # intercept + two slopes in the example below
#' fit <- mo.bqr.svy(y ~ x1 + x2, weights = sim$weights, data = sim$data,
#'                   quantile = c(0.1, 0.5, 0.9), algorithm = "em",
#'                   max_iter = 200, prior = pr)
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
  coef_names <- colnames(X)

  wts <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(wts) != n)  stop("'weights' must have length n.")
  if (any(!is.finite(wts)) || any(wts <= 0)) stop("Invalid weights.")
  wts <- wts / mean(wts)
  if (any(!is.finite(y))) stop("Response 'y' contains non-finite values.")

  p <- ncol(X)

  # Resolve prior: default constructor or coercion of user-supplied object/list
  prior <- if (is.null(prior)) {
    mo_prior_default(p, names = coef_names)  # 'mo_bqr_prior' object
  } else {
    as_mo_bqr_prior(prior, p = p, names = coef_names)
  }

  results <- vector("list", length(quantile))
  names(results) <- paste0("q", quantile)

  for (qi in seq_along(quantile)) {
    q <- quantile[qi]

    # These objects are 1x1 in the current implementation
    y_matrix <- matrix(y, ncol = 1)
    u        <- matrix(1.0, 1, 1)
    gamma_u  <- matrix(0.0, 1, 1)

    # Augment prior to match the C++ parameterization (p + 1)
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
      verbose  = verbose
    )

    beta_final  <- cpp_result$beta[1:p]
    sigma_final <- cpp_result$sigma

    results[[qi]] <- list(
      beta      = as.numeric(beta_final),
      sigma     = as.numeric(sigma_final),
      iter      = cpp_result$iter,
      converged = cpp_result$converged
    )
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
#' Generates synthetic data suitable for \code{\link{mo.bqr.svy}}.
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
    beta_true <- c(1, seq_len(p))
  } else {
    if (length(beta_true) != (p + 1)) {
      stop("Length of beta_true must be p + 1 (intercept + coefficients).")
    }
  }

  y <- beta_true[1] + X %*% beta_true[-1] + rnorm(n, 0, 1)

  list(
    data       = data.frame(y = as.numeric(y), X),
    weights    = runif(n, 0.5, 2),
    true_betas = matrix(beta_true, nrow = 1),
    quantiles  = c(0.1, 0.5, 0.9)
  )
}
