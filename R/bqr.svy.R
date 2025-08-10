if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a


# ==== PRIOR: constructor, coercion, and print =================================

new_bqr_prior <- function(b0, B0, w_scale = NULL, names = NULL) {
  if (!is.null(names)) {
    names(b0) <- names
    dimnames(B0) <- list(names, names)
  }
  structure(list(b0 = b0, B0 = B0, w_scale = w_scale), class = "bqr_prior")
}

#' Default Prior for Bayesian Weighted Quantile Regression
#'
#' Creates a prior object (class \code{"bqr_prior"}) ready to be passed to
#' \code{\link{bqr.svy}} (or \code{mo.bqr.svy}). The prior is a multivariate
#' normal prior on the regression coefficients and, for the
#' \code{method = "approximate"} kernel, it can also carry a weight-scale
#' parameter \code{w_scale}.
#'
#' @param p Number of regression coefficients (including the intercept).
#' @param b0 Numeric vector of prior means (length \code{p}). If a scalar is
#'   supplied, it is expanded to length \code{p}.
#' @param B0 Prior covariance. May be:
#'   \itemize{
#'     \item a \code{p x p} matrix,
#'     \item a scalar (expanded to \code{diag(scalar, p)}), or
#'     \item a length-\code{p} numeric vector (expanded to \code{diag(vector)}).
#'   }
#' @param w_scale Positive scalar weight-scale parameter (used by the
#'   \code{"approximate"} kernel; stored in the object regardless).
#' @param names Optional coefficient names to attach to \code{b0} and \code{B0}.
#'
#' @return An object of class \code{"bqr_prior"} with components \code{b0},
#'   \code{B0}, and optionally \code{w_scale}.
#'
#' @examples
#' pr <- prior_default(p = 3, b0 = 0, B0 = 1000)
#' pr
#'
#' @export
prior_default <- function(p,
                          b0      = rep(0, p),
                          B0      = diag(1e6, p),
                          w_scale = 2,
                          names   = NULL) {
  # Expand scalars/vectors to matrix shape where needed
  if (length(b0) == 1L) b0 <- rep(b0, p)
  if (is.numeric(B0) && length(B0) == 1L) B0 <- diag(B0, p)
  if (is.numeric(B0) && is.null(dim(B0)) && length(B0) == p) B0 <- diag(B0, p)

  # Validate dimensions and values
  if (length(b0) != p) stop("length(b0) must be ", p, ".", call. = FALSE)
  if (!is.matrix(B0) || any(dim(B0) != c(p, p)))
    stop("B0 must be a ", p, "x", p, " matrix.", call. = FALSE)
  if (!is.null(w_scale) && (!is.numeric(w_scale) || length(w_scale) != 1L || w_scale <= 0))
    stop("'w_scale' must be a positive scalar.", call. = FALSE)

  new_bqr_prior(b0 = b0, B0 = B0, w_scale = w_scale, names = names)
}

#' Coerce to a \code{bqr_prior} object
#'
#' Allows passing legacy list priors to \code{\link{bqr.svy}}. A valid list must
#' contain at least \code{b0} and \code{B0}. Scalars/vectors are expanded as in
#' \code{\link{prior_default}}. When \code{method = "approximate"} and
#' \code{w_scale} is missing, it defaults to 2; otherwise \code{w_scale} is set
#' to \code{NULL}.
#'
#' @param x A \code{bqr_prior} or a list with components \code{b0}, \code{B0},
#'   and optionally \code{w_scale}.
#' @param p Number of coefficients.
#' @param names Optional coefficient names.
#' @param method One of \code{"ald"}, \code{"score"}, \code{"approximate"}.
#'
#' @return A \code{bqr_prior} object.
#' @export
as_bqr_prior <- function(x, p, names = NULL, method = c("ald", "score", "approximate")) {
  method <- match.arg(method)
  if (inherits(x, "bqr_prior")) {
    # Attach names if not present
    if (!is.null(names) && is.null(dimnames(x$B0))) {
      x$b0 <- setNames(x$b0, names)
      dimnames(x$B0) <- list(names, names)
    }
    return(x)
  }
  if (!is.list(x) || !all(c("b0", "B0") %in% names(x)))
    stop("'prior' must be a 'bqr_prior' object or a list with b0 and B0.", call. = FALSE)

  b0 <- x$b0; B0 <- x$B0; w_scale <- x$w_scale
  # Expand
  if (length(b0) == 1L) b0 <- rep(b0, p)
  if (is.numeric(B0) && length(B0) == 1L) B0 <- diag(B0, p)
  if (is.numeric(B0) && is.null(dim(B0)) && length(B0) == p) B0 <- diag(B0, p)
  # Validate
  if (length(b0) != p) stop("length(b0) must be ", p, ".", call. = FALSE)
  if (!is.matrix(B0) || any(dim(B0) != c(p, p)))
    stop("B0 must be a ", p, "x", p, " matrix.", call. = FALSE)

  # Method-specific default for w_scale
  if (identical(method, "approximate")) w_scale <- (w_scale %||% 2) else w_scale <- NULL

  new_bqr_prior(b0 = b0, B0 = B0, w_scale = w_scale, names = names)
}

#' @export
print.bqr_prior <- function(x, ...) {
  cat("bqr_prior\n")
  cat("  b0: length", length(x$b0), "\n")
  cat("  B0:", nrow(x$B0), "x", ncol(x$B0), "\n")
  if (!is.null(x$w_scale)) cat("  w_scale:", x$w_scale, "\n")
  invisible(x)
}


# ==== MODEL FITTER ============================================================

#' Bayesian Weighted Quantile Regression (Survey Design)
#'
#' Fits a Bayesian quantile regression model with survey weights using one of
#' three MCMC kernels implemented in C++:
#' \itemize{
#'   \item \code{.MCMC_BWQR_AL} – Asymmetric Laplace Distribution
#'   \item \code{.MCMC_BWQR_SL} – Score likelihood
#'   \item \code{.MCMC_BWQR_AP} – Approximate likelihood
#' }
#' Only a single quantile can be estimated at a time.
#'
#' @param formula An object of class \code{\link{formula}} specifying the model.
#' @param weights Optional survey weights. Can be a numeric vector of length equal
#'   to the number of observations, or a one-sided formula referring to a variable
#'   in \code{data}.
#' @param data An optional \code{data.frame} containing the variables in the model.
#'   If not supplied, variables are taken from \code{\link{environment}(formula)}.
#' @param quantile Numeric scalar in (0, 1) giving the quantile level \eqn{\tau}.
#' @param method Character string; one of \code{"ald"}, \code{"score"}, or
#'   \code{"approximate"} indicating which MCMC kernel to use.
#' @param prior A \code{bqr_prior} object created by \code{\link{prior_default}}
#'   (or a compatible list; it will be coerced via \code{\link{as_bqr_prior}}).
#'   For \code{"approximate"}, \code{w_scale} can be stored in the prior.
#' @param niter Integer; total number of MCMC iterations.
#' @param burnin Integer; number of burn-in iterations to discard.
#' @param thin Integer; thinning interval for MCMC samples.
#' @param ... Additional arguments passed to internal functions (currently unused).
#'
#' @return An object of class \code{"bqr.svy"} and \code{"bwqr_fit"}, which is a
#'   list containing:
#'   \describe{
#'     \item{\code{beta}}{Posterior mean estimates of regression coefficients.}
#'     \item{\code{draws}}{Matrix of posterior draws.}
#'     \item{\code{accept_rate}}{Average acceptance rate (if available).}
#'     \item{\code{n_chains}}{Number of MCMC chains.}
#'     \item{\code{warmup}}{Number of warmup iterations.}
#'     \item{\code{thin}}{Thinning interval.}
#'     \item{\code{runtime}}{Elapsed runtime in seconds.}
#'     \item{\code{call}}{Matched call.}
#'     \item{\code{method}}{Method used.}
#'     \item{\code{quantile}}{Quantile level estimated.}
#'     \item{\code{prior}}{Prior specification used.}
#'     \item{\code{terms}}{Model terms object.}
#'     \item{\code{model}}{Model frame.}
#'     \item{\code{formula}}{Model formula.}
#'   }
#'
#' @details
#' This function is a high-level interface to compiled C++ MCMC routines for
#' Bayesian quantile regression under complex survey weights. The \code{"ald"} and
#' \code{"score"} kernels use normalized weights; the \code{"approximate"} kernel
#' uses raw weights and may rely on an additional \code{w_scale} parameter stored
#' in the prior object (if not provided, it defaults to 2 in the call).
#'
#' @seealso \code{\link{prior_default}}, \code{\link{as_bqr_prior}},
#'   \code{\link{mo.bqr.svy}} for multiple-output quantile regression,
#'   \code{\link{simulate_bqr_data}} for data simulation.
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_bqr_data(n = 50)
#' pr  <- prior_default(p = 3) # intercept + two slopes in the example below
#' fit <- bqr.svy(y ~ x1 + x2, weights = sim$weights, data = sim$data,
#'                quantile = 0.5, method = "ald", niter = 2000, burnin = 500,
#'                prior = pr)
#' summary(fit)
#'
#' @importFrom stats model.frame model.matrix model.response terms
#' @export
bqr.svy <- function(formula,
                    weights  = NULL,
                    data     = NULL,
                    quantile = 0.5,
                    method   = c("ald", "score", "approximate"),
                    prior    = NULL,
                    niter    = 50000,
                    burnin   = 10000,
                    thin     = 1,
                    ...) {

  tic    <- proc.time()[["elapsed"]]
  method <- match.arg(method)
  cl <- match.call()

  if (length(quantile) != 1 || quantile <= 0 || quantile >= 1)
    stop("'quantile' must be a scalar in (0,1).", call. = FALSE)
  if (niter <= 0 || burnin < 0 || thin <= 0)
    stop("'niter' and 'thin' must be > 0, and 'burnin' >= 0.", call. = FALSE)

  if (is.null(data)) data <- environment(formula)

  mf <- model.frame(formula, data, na.action = NULL)
  if (anyNA(mf))
    stop("Data contains missing values; please remove or impute them.", call. = FALSE)

  y  <- model.response(mf, "numeric")
  X  <- model.matrix(attr(mf, "terms"), mf)
  coef_names <- colnames(X)
  mt <- attr(mf, "terms")

  w <- if (is.null(weights)) {
    rep(1, length(y))
  } else if (is.numeric(weights)) {
    if (length(weights) != length(y))
      stop("Length of 'weights' != length of response.", call. = FALSE)
    weights
  } else if (inherits(weights, "formula")) {
    model.frame(weights, data)[[1L]]
  } else stop("'weights' must be numeric or formula.", call. = FALSE)

  p <- ncol(X)

  # Resolve prior: default constructor or coercion of user-supplied object/list
  prior <- if (is.null(prior)) {
    prior_default(p, names = coef_names)  # 'bqr_prior' object
  } else {
    as_bqr_prior(prior, p = p, names = coef_names, method = method)
  }

  w_norm <- w / mean(w)

  draws <- switch(method,
                  "ald" = .MCMC_BWQR_AL(y, X, w_norm, tau = quantile,
                                        n_mcmc = niter, burnin = burnin, thin = thin),
                  "score" = .MCMC_BWQR_SL(y, X, w_norm, tau = quantile,
                                          n_mcmc = niter, burnin = burnin, thin = thin,
                                          b0_ = prior$b0, B0_ = prior$B0),
                  "approximate" = .MCMC_BWQR_AP(y, X, w, n_mcmc = niter, burnin = burnin,
                                                thin = thin, tau = quantile,
                                                w_scale = prior$w_scale %||% 2,
                                                b0_ = prior$b0, B0_ = prior$B0)
  )

  if (is.list(draws) && !is.data.frame(draws)) {
    draws <- lapply(draws, function(x) if (is.numeric(x) || is.matrix(x)) x)
    draws <- draws[!vapply(draws, is.null, logical(1))]
    draws <- do.call(cbind, draws)
  }
  if (is.data.frame(draws))
    draws <- data.matrix(draws[vapply(draws, is.numeric, logical(1))])

  draws <- as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (anyNA(draws))
    stop("Backend returned non-numeric values; cannot summarize.", call. = FALSE)

  diag_cols <- c("accept_rate", "n_mcmc", "burnin", "thin", "n_samples")
  keep_idx  <- !colnames(draws) %in% diag_cols
  accept_rate <- if ("accept_rate" %in% colnames(draws))
    mean(draws[, "accept_rate"]) else NA_real_
  draws <- draws[, keep_idx, drop = FALSE]

  if (ncol(draws) >= p)
    colnames(draws)[1:p] <- coef_names
  idx_blank <- which(is.na(colnames(draws)) | colnames(draws) == "")
  if (length(idx_blank))
    colnames(draws)[idx_blank] <- sprintf("V%d", idx_blank)

  beta_hat <- if (ncol(draws) >= p) colMeans(draws[, seq_len(p), drop = FALSE]) else numeric(0)
  names(beta_hat) <- if (length(beta_hat) > 0) coef_names else character(0)

  runtime <- proc.time()[["elapsed"]] - tic

  out <- list(
    beta         = beta_hat,
    draws        = draws,
    accept_rate  = accept_rate,
    n_chains     = 1L,
    warmup       = burnin,
    thin         = thin,
    runtime      = runtime,
    call         = cl,
    method       = method,
    quantile     = quantile,
    prior        = prior,
    terms        = mt,
    model        = mf,
    formula      = formula
  )
  out$call$formula <- formula
  class(out) <- c("bwqr_fit", "bqr.svy")
  out
}


# ==== DATA SIMULATION ==========================================================

#' Simulate data for Bayesian Weighted Quantile Regression
#'
#' Generates synthetic data compatible with \code{\link{bqr.svy}}, allowing the user
#' to specify true regression coefficients, error scale, and optional survey weights.
#'
#' @param n Number of observations.
#' @param betas Numeric vector of true coefficients (first element is intercept).
#' @param sigma Standard deviation of the Gaussian error term.
#' @param weights Optional numeric vector of survey weights. If \code{NULL}, generated
#'   from Uniform(0.5, 2).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{data}}{\code{data.frame} with response \code{y} and predictors.}
#'   \item{\code{weights}}{Numeric vector of survey weights.}
#'   \item{\code{true_betas}}{The true coefficients used in data generation.}
#' }
#'
#' @examples
#' sim <- simulate_bqr_data(n = 50, betas = c(1, 2, -1), sigma = 0.5)
#' head(sim$data)
#'
#' @export
simulate_bqr_data <- function(n = 100,
                              betas = c(1, 2, -0.5),
                              sigma = 1,
                              weights = NULL,
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  p <- length(betas) - 1
  if (p < 1) stop("'betas' must include intercept and at least one slope.", call. = FALSE)

  X <- replicate(p, rnorm(n))
  colnames(X) <- paste0("x", seq_len(p))

  y <- betas[1] + X %*% betas[-1] + rnorm(n, 0, sigma)

  if (is.null(weights)) {
    weights <- runif(n, 0.5, 2)
  } else {
    if (length(weights) != n)
      stop("'weights' must be length n.", call. = FALSE)
  }

  data <- data.frame(y = as.numeric(y), X)

  list(
    data = data,
    weights = weights,
    true_betas = betas
  )
}
