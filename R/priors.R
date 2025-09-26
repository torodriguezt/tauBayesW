# =============================================================================
# Unified Prior Interface for tauBayesW
# =============================================================================

if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# Small helper: expand scalar/vector to diagonal p x p
.as_diag <- function(x, p, what) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1L) return(diag(x, p))
  if (is.numeric(x) && is.null(dim(x)) && length(x) == p) return(diag(x, p))
  if (is.matrix(x)) {
    if (all(dim(x) == c(p, p))) return(x)
    stop(what, " must be a ", p, "x", p, " matrix.", call. = FALSE)
  }
  stop(what, " must be scalar, length-", p, " vector, or ", p, "x", p, " matrix.", call. = FALSE)
}

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
#' fit_ald <- bqr.svy(y ~ x1 + x2, weights = w, data = mydata, prior = prior_ald)
#' fit_scr <- bqr.svy(y ~ x1 + x2, weights = w, data = mydata, method = "score", prior = prior_score)
#' fit_apx <- bqr.svy(y ~ x1 + x2, weights = w, data = mydata, method = "approximate", prior = prior_approximate)
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
#' fit_mo <- mo.bqr.svy(cbind(y1, y2) ~ x1 + x2, weights = w, data = mydata, prior = prior_mo, n_dir = 10)
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{mo.bqr.svy}},
#'   \code{\link{summary}}
#' @export
prior <- function(
  beta_x_mean = NULL,
  beta_x_cov  = NULL,
  sigma_shape = 0.001,
  sigma_rate  = 0.001,
  beta_y_mean = NULL,
  beta_y_cov  = NULL
) {
  if (!is.null(sigma_shape) && (!is.numeric(sigma_shape) || length(sigma_shape)!=1L || sigma_shape<=0))
    stop("'sigma_shape' must be a positive scalar.", call. = FALSE)
  if (!is.null(sigma_rate) && (!is.numeric(sigma_rate) || length(sigma_rate)!=1L || sigma_rate<=0))
    stop("'sigma_rate' must be a positive scalar.", call. = FALSE)

  structure(
    list(
      beta_x_mean = beta_x_mean,
      beta_x_cov  = beta_x_cov,
      sigma_shape = sigma_shape,
      sigma_rate  = sigma_rate,
      beta_y_mean = beta_y_mean,
      beta_y_cov  = beta_y_cov
    ),
    class = "prior"
  )
}

print.prior <- function(x, digits = 3, ...) {
  cat("Unified prior (class 'prior')\n")
  cat("  beta_x_mean: ", if (is.null(x$beta_x_mean)) "NULL" else paste(round(head(x$beta_x_mean), digits), collapse=" "), "\n", sep = "")
  cat("  beta_x_cov:  ", if (is.null(x$beta_x_cov))  "NULL" else "provided", "\n", sep = "")
  cat("  sigma IG:    ",
      if (is.null(x$sigma_shape) || is.null(x$sigma_rate)) "NULL"
      else paste0("shape=", round(x$sigma_shape, digits), ", rate=", round(x$sigma_rate, digits)),
      "\n", sep = "")
  cat("  beta_y_mean: ", if (is.null(x$beta_y_mean)) "NULL" else paste(round(head(x$beta_y_mean), digits), collapse=" "), "\n", sep = "")
  cat("  beta_y_cov:  ", if (is.null(x$beta_y_cov))  "NULL" else "provided", "\n", sep = "")
  invisible(x)
}