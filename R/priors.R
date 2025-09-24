# =============================================================================
# Unified Prior Interface for tauBayesW
# =============================================================================

if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Create prior for Bayesian quantile regression models for complex survey data
#'
#' \code{prior} creates prior distributions for both single (\code{bqr.svy}) and multiple-output 
#' (\code{mo.bqr.svy}) Bayesian quantile regression models for complex survey data. 
#'
#' @param beta_x_mean (p+1)-dimensional vector of prior means for the regression coefficients,
#'  where p is the number of covariates (default = \code{rep(0,(p+1)}}).   
#' @param beta_x_cov ((p+1)x(p+1))-dimensional prior covariance matrix for the regression coefficients.
#'  (default = \code{diag(1e6, (p+1))}).
#' @param sigma_shape shape parameter for inverse Gamma prior for \eqn{\sigma^2}.
#'  (default = 0.001).
#' @param sigma_rate rate parameter for inverse Gamma prior for \eqn{\sigma^2}.
#'  (default = 0.001).
#' @param beta_y_mean (d-1)-dimensional vector of prior means for the coefficients related to 
#'  the variables that emerge from the product between the orthogonal basis and the outputs.
#'  where d is the number of outputs (default = \code{rep(0,(d-1)}}).
#' @param beta_y_cov ((d-1)x(d-1))-dimensional prior covariance matrix for the coefficients related to 
#'  the variables that emerge from the product between the orthogonal basis and the outputs.
#'  (default = \code{diag(1e6, (d-1))}).
#'
#' @details
#' The function \code{prior} builds prior distributions for the three methods implemented in the function 
#' \code{bqr.svy} and for the multiple-output quantile regression implemented in the function \code{mo.bqr.svy}.
#' Every nonspecified prior parameter will get the default value.
#'   
#' \itemize{
#'   \item \code{method = "ald"} in function \code{bqr.svy} allow the specification of hyperparameters 
#'         \code{beta_x_mean}, \code{beta_x_cov}, \code{sigma_shape}, and \code{sigma_rate}.
#'   \item \code{method = "score"} in function \code{bqr.svy} allow the specification of hyperparameters 
#'         \code{beta_x_mean} and \code{beta_x_cov}.
#'   \item \code{method = "approximate"} in function \code{bqr.svy} allow the specification of hyperparameters 
#'         \code{beta_x_mean} and \code{beta_x_cov}.
#'   \item In function \code{mo.bqr.svy}, the specification of hyperparameters \code{beta_x_mean},\code{beta_x_cov},
#'         \code{sigma_shape}, \code{sigma_rate}, \code{beta_y_mean}, and \code{beta_y_cov} are allowed.
#' }
#'
#' @return An object of class \code{bqr_prior} for the single-output models and an object of class 
#'  \code{mo_bqr_prior} for the multiple-output model.
#'
#' @examples
#' # Create informative prior objects regarding the single-output methods
#' prior_ald <- prior(
#'   beta_x_mean = c(2, 1.5, -0.8),
#'   beta_x_cov = diag(c(0.25, 0.25, 0.25)),
#'   sigma_shape = 3,
#'   sigma_rate = 2
#' )
#'
#' prior_score <- prior(
#'   beta_x_mean = c(2, 1.5, -0.8),
#'   beta_x_cov = diag(c(0.25, 0.25, 0.25))
#' )
#'
#' prior_approximate <- prior(
#'   beta_x_mean = c(2, 1.5, -0.8),
#'   beta_x_cov = diag(c(0.25, 0.25, 0.25))
#' )
#'
#' # Estimate the model parameters with informative prior
#' fit_ald <- bqr.svy(y ~ x1 + x2, data = mydata, weights = w, prior = prior_ald)
#' fit_scr <- bqr.svy(y ~ x1 + x2, data = mydata, weights = w, method = "score", prior = prior_score)
#' fit_apx <- bqr.svy(y ~ x1 + x2, data = mydata, weights = w, method = "approximate", prior = prior_approximate)
#'    
#' # Create an informative prior object regarding the multiple-output method
#' prior_mo <- prior(
#'   beta_x_mean = c(2, 1.5, -0.8),
#'   beta_x_cov = diag(c(0.25, 0.25, 0.25)),
#'   sigma_shape = 3,
#'   sigma_rate = 2,
#'   beta_y_mean = 1,
#'   beta_y_cov = 0.25,
#' )
#'
#' # Estimate the model parameters with informative prior
#' fit_mo <- mo.bqr.svy(cbind(y1, y2) ~ x1 + x2, data = mydata, weights = w, prior = prior_mo, n_dir = 10)
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{mo.bqr.svy}},
#'   \code{\link{summary}}
#' @export
prior <- function(
                  beta_x_mean = rep(0, (p+1)),
                  beta_x_cov = diag(1e6, (p+1)),
                  sigma_shape = 0.001,
                  sigma_rate = 0.001,
                  beta_y_mean = rep(0, (d-1)),
                  beta_y_cov = diag(1e6, (d-1)) {

  # Default to univariate if not specified
  if (is.null(type)) {
    type <- "MCMC"
  }

  # Validate type
  type <- match.arg(type, choices = c("MCMC", "EM"))

  # Validate p
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p != as.integer(p)) {
    stop("'p' must be a positive integer.", call. = FALSE)
  }

  # Create appropriate prior based on type
  if (type == "MCMC") {
    # For bqr.svy models - use original prior_default logic
    prior_default(
      p = p,
      b0 = beta_mean,
      B0 = beta_cov,
      c0 = sigma_shape,
      C0 = sigma_rate,
      names = names
    )
  } else {
    # For mo.bqr.svy models - use mo_prior_default logic
    mo_prior_default(
      p = p,
      beta_mean = beta_mean,
      beta_cov = beta_cov,
      sigma_shape = sigma_shape,
      sigma_rate = sigma_rate,
      names = names
    )
  }
}

# =============================================================================
# Univariate Prior Functions (for bqr.svy)
# =============================================================================

new_bqr_prior <- function(b0, B0, c0 = NULL, C0 = NULL, names = NULL) {
  if (!is.null(names)) {
    names(b0) <- names
    dimnames(B0) <- list(names, names)
  }
  structure(list(b0 = b0, B0 = B0, c0 = c0, C0 = C0),
            class = "bqr_prior")
}

#' Default Prior for Bayesian Weighted Quantile Regression
#'
#' Creates a unified prior object (class \code{"bqr_prior"}) to be passed to
#' \code{\link{bqr.svy}}. It stores a multivariate normal
#' prior on the regression coefficients and, for the \code{"ald"} kernel, optional
#' Inverse-Gamma hyperparameters \code{c0}, \code{C0} for \eqn{\sigma^2}.
#' Methods that do not use some fields simply ignore them.
#'
#' @note For a simpler unified interface, consider using \code{\link{prior}}
#'   which automatically handles both univariate and multivariate models.
#'
#' @param p Number of regression coefficients (including the intercept).
#' @param b0 Numeric vector of prior means (length \code{p}). If a scalar is
#'   supplied, it is expanded to length \code{p}.
#' @param B0 Prior covariance. May be a \code{p x p} matrix, a scalar
#'   (expanded to \code{diag(scalar, p)}), or a length-\code{p} vector
#'   (expanded to \code{diag(vector)}).
#' @param c0 Shape parameter of the Inverse-Gamma prior for \eqn{\sigma^2} (ALD).
#' @param C0 Scale parameter of the Inverse-Gamma prior for \eqn{\sigma^2} (ALD).
#' @param names Optional coefficient names to attach to \code{b0} and \code{B0}.
#'
#' @return An object of class \code{"bqr_prior"} with components \code{b0},
#'   \code{B0}, and optionally \code{c0}, \code{C0}.
#' @seealso \code{\link{prior}} for a unified interface
#' @keywords internal
prior_default <- function(p,
                          b0    = rep(0, p),
                          B0    = diag(1e6, p),
                          c0    = 0.001,
                          C0    = 0.001,
                          names = NULL) {
  # Expand scalars/vectors to matrix shape where needed
  if (length(b0) == 1L) b0 <- rep(b0, p)
  if (is.numeric(B0) && length(B0) == 1L) B0 <- diag(B0, p)
  if (is.numeric(B0) && is.null(dim(B0)) && length(B0) == p) B0 <- diag(B0, p)

  # Validate
  if (length(b0) != p) stop("length(b0) must be ", p, ".", call. = FALSE)
  if (!is.matrix(B0) || any(dim(B0) != c(p, p)))
    stop("B0 must be a ", p, "x", p, " matrix.", call. = FALSE)
  if (!is.null(c0) && (!is.numeric(c0) || length(c0) != 1L || c0 <= 0))
    stop("'c0' must be a positive scalar.", call. = FALSE)
  if (!is.null(C0) && (!is.numeric(C0) || length(C0) != 1L || C0 <= 0))
    stop("'C0' must be a positive scalar.", call. = FALSE)

  new_bqr_prior(b0 = b0, B0 = B0, c0 = c0, C0 = C0, names = names)
}

#' Coerce to a \code{bqr_prior} object (unified)
#'
#' Allows passing legacy list priors to \code{\link{bqr.svy}}. A valid list must
#' have components \code{b0}, \code{B0}, and optionally \code{c0}, \code{C0}.
#'
#' @param x A list to be coerced.
#' @param ... Additional arguments (ignored).
#' @return A \code{bqr_prior} object.
#' @keywords internal
as_bqr_prior <- function(x, ...) {
  UseMethod("as_bqr_prior")
}

#' @keywords internal
as_bqr_prior.bqr_prior <- function(x, ...) x

#' @keywords internal
as_bqr_prior.list <- function(x, ...) {
  if (!all(c("b0", "B0") %in% names(x)))
    stop("List must have components 'b0' and 'B0'.", call. = FALSE)
  new_bqr_prior(b0 = x$b0, B0 = x$B0, c0 = x$c0, C0 = x$C0, names = NULL)
}

#' @keywords internal
print.bqr_prior <- function(x, digits = 3, ...) {
  cat("Bayesian Quantile Regression Prior (bqr_prior)\n")
  cat("Beta prior (normal):\n")
  cat("  Mean    :", round(x$b0, digits), "\n")
  cat("  Cov diag:", round(diag(x$B0), digits), "\n")
  if (!is.null(x$c0) && !is.null(x$C0)) {
    cat("Sigma prior (inverse-gamma):\n")
    cat("  Shape   :", round(x$c0, digits), "\n")
    cat("  Rate    :", round(x$C0, digits), "\n")
  }
  invisible(x)
}

# =============================================================================
# Multivariate Prior Functions (for mo.bqr.svy)
# =============================================================================

new_mo_bqr_prior <- function(beta_mean, beta_cov, sigma_shape, sigma_rate, names = NULL) {
  if (!is.null(names)) {
    if (length(names) != length(beta_mean))
      stop("'names' must have same length as 'beta_mean'.")
    if (!all(dim(beta_cov) == c(length(names), length(names))))
      stop("'beta_cov' must be a square matrix with dimension length(names).")
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
#' variance has an inverse-gamma prior specified via shape/rate. This function
#' can be used to create individual priors that can then be assigned to specific
#' quantiles, allowing for quantile-specific prior distributions.
#'
#' @note For a simpler unified interface, consider using \code{\link{prior}}
#'   with \code{type = "EM"} which automatically handles both
#'   univariate and multivariate models.
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
#' @details
#' When used with \code{\link{mo.bqr.svy}}, you can specify different priors for each quantile
#' by providing a list of \code{mo_bqr_prior} objects, a function that takes (tau, p, names)
#' as arguments, or use a single prior that will be recycled across all quantiles.
#'
#' @return An object of class \code{"mo_bqr_prior"} with fields \code{beta_mean},
#'   \code{beta_cov}, \code{sigma_shape}, and \code{sigma_rate}.
#'
#' @examples
#' # Create a single prior (will be recycled for all quantiles)
#' prior1 <- prior(p = 3, type = "EM", beta_mean = c(0, 1, -0.5))
#'
#' # Create quantile-specific priors using a list
#' priors_list <- list(
#'   q0.1 = prior(p = 3, type = "EM", beta_mean = c(0, 0.8, -0.3)),
#'   q0.5 = prior(p = 3, type = "EM", beta_mean = c(0, 1.0, -0.5)),
#'   q0.9 = prior(p = 3, type = "EM", beta_mean = c(0, 1.2, -0.7))
#' )
#'
#' # Create quantile-specific priors using a function
#' prior_fn <- function(tau, p, names) {
#'   # More informative priors for extreme quantiles
#'   variance <- ifelse(tau < 0.2 | tau > 0.8, 0.5, 1.0)
#'   prior(p = p, type = "EM", beta_cov = diag(variance, p), names = names)
#' }
#'
#' @seealso \code{\link{prior}} for a unified interface
#' @keywords internal
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
    stop("'sigma_shape' must be a positive scalar.", call. = FALSE)
  if (!is.numeric(sigma_rate) || length(sigma_rate) != 1L || sigma_rate <= 0)
    stop("'sigma_rate' must be a positive scalar.", call. = FALSE)

  new_mo_bqr_prior(beta_mean = beta_mean, beta_cov = beta_cov,
                   sigma_shape = sigma_shape, sigma_rate = sigma_rate,
                   names = names)
}

#' Coerce to a \code{mo_bqr_prior} object
#'
#' Allows conversion from lists or other objects to \code{mo_bqr_prior}.
#'
#' @param x Object to coerce.
#' @param ... Additional arguments (ignored).
#' @return A \code{mo_bqr_prior} object.
#' @keywords internal
as_mo_bqr_prior <- function(x, ...) {
  UseMethod("as_mo_bqr_prior")
}

#' @keywords internal
as_mo_bqr_prior.mo_bqr_prior <- function(x, ...) x

#' @keywords internal
as_mo_bqr_prior.list <- function(x, ...) {
  required_names <- c("beta_mean", "beta_cov", "sigma_shape", "sigma_rate")
  if (!all(required_names %in% names(x)))
    stop("List must have components: ", paste(required_names, collapse = ", "), ".", call. = FALSE)
  new_mo_bqr_prior(beta_mean = x$beta_mean, beta_cov = x$beta_cov,
                   sigma_shape = x$sigma_shape, sigma_rate = x$sigma_rate)
}

#' @keywords internal
print.mo_bqr_prior <- function(x, digits = 3, ...) {
  cat("Multiple-Output Bayesian Quantile Regression Prior (mo_bqr_prior)\n")
  cat("Beta prior (multivariate normal):\n")
  cat("  Mean    :", round(x$beta_mean, digits), "\n")
  cat("  Cov diag:", round(diag(x$beta_cov), digits), "\n")
  cat("Sigma prior (inverse-gamma):\n")
  cat("  Shape   :", round(x$sigma_shape, digits), "\n")
  cat("  Rate    :", round(x$sigma_rate, digits), "\n")
  invisible(x)
}
