if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a


# ==== PRIOR: constructor, coercion, and print =================================

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
#' \code{\link{bqr.svy}} (or \code{mo.bqr.svy}). It stores a multivariate normal
#' prior on the regression coefficients and, for the \code{"ald"} kernel, optional
#' Inverse-Gamma hyperparameters \code{c0}, \code{C0} for \eqn{\sigma^2}.
#' Methods that do not use some fields simply ignore them.
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
#' @export
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
#' contain at least \code{b0} and \code{B0}. Scalars/vectors are expanded as in
#' \code{\link{prior_default}}. Optional fields \code{c0}, \code{C0} are relevant
#' for \code{method = "ald"} and ignored otherwise.
#'
#' @param x A \code{bqr_prior} or a list with components \code{b0}, \code{B0},
#'   and optionally \code{c0}, \code{C0}.
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

  b0 <- x$b0
  B0 <- x$B0
  c0 <- x$c0
  C0 <- x$C0

  # Expand
  if (length(b0) == 1L) b0 <- rep(b0, p)
  if (is.numeric(B0) && length(B0) == 1L) B0 <- diag(B0, p)
  if (is.numeric(B0) && is.null(dim(B0)) && length(B0) == p) B0 <- diag(B0, p)

  # Validate
  if (length(b0) != p) stop("length(b0) must be ", p, ".", call. = FALSE)
  if (!is.matrix(B0) || any(dim(B0) != c(p, p)))
    stop("B0 must be a ", p, "x", p, " matrix.", call. = FALSE)

  # Method-specific sensible defaults (only for methods that use them)
  if (identical(method, "ald")) {
    c0 <- (c0 %||% 0.001)
    C0 <- (C0 %||% 0.001)
  }

  new_bqr_prior(b0 = b0, B0 = B0, c0 = c0, C0 = C0, names = names)
}

#' @export
print.bqr_prior <- function(x, ...) {
  cat("bqr_prior\n")
  cat("  b0: length", length(x$b0), "\n")
  cat("  B0:", nrow(x$B0), "x", ncol(x$B0), "\n")
  if (!is.null(x$c0)) cat("  c0:", x$c0, "\n")
  if (!is.null(x$C0)) cat("  C0:", x$C0, "\n")
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
#' @param formula A \code{\link{formula}} specifying the model.
#' @param weights Optional survey weights (numeric vector or one-sided formula).
#' @param data Optional \code{data.frame} for variables in the model.
#' @param quantile Numeric scalar in (0, 1): target quantile \eqn{\tau}.
#' @param method One of \code{"ald"}, \code{"score"}, \code{"approximate"}.
#' @param prior Unified \code{bqr_prior} (see \code{\link{prior_default}}). For
#'   \code{"ald"}: uses \code{b0}, \code{B0}, \code{c0}, \code{C0}. For \code{"score"}:
#'   uses \code{b0}, \code{B0}. For \code{"approximate"}: uses \code{b0}, \code{B0}.
#' @param niter, burnin, thin MCMC settings.
#' @return An object of class \code{"bqr.svy"} and \code{"bwqr_fit"}.
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

  # Unified prior
  prior <- if (is.null(prior)) {
    prior_default(p, names = coef_names)
  } else {
    as_bqr_prior(prior, p = p, names = coef_names, method = method)
  }

  # Pesos: ALD y Score usan w normalizado; Approximate usa w crudo
  w_norm <- w / mean(w)

  draws <- switch(method,
                  "ald" = .MCMC_BWQR_AL(
                    y, X, w_norm,
                    tau           = quantile,
                    n_mcmc        = niter,
                    burnin        = burnin,
                    thin          = thin,
                    b_prior_mean  = prior$b0,
                    B_prior_prec  = solve(prior$B0),
                    c0            = prior$c0 %||% 0.001,
                    C0            = prior$C0 %||% 0.001
                  ),
                  "score" = .MCMC_BWQR_SL(
                    y, X, w_norm,
                    tau           = quantile,
                    n_mcmc        = niter,
                    burnin        = burnin,
                    thin          = thin,
                    b_prior_mean  = prior$b0,
                    B_prior_prec  = solve(prior$B0)
                  ),
                  "approximate" = .MCMC_BWQR_AP(
                    y, X, w,
                    n_mcmc        = niter,
                    burnin        = burnin,
                    thin          = thin,
                    tau           = quantile,
                    b_prior_mean  = prior$b0,
                    B_prior_prec  = solve(prior$B0)
                  )
  )

  # Coerción y limpieza de draws
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
