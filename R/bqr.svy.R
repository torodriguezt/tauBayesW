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
#' One or more quantiles can be estimated, depending on the input.
#'
#' Survey weights are handled differently by each method:
#' \itemize{
#'   \item \code{"ald"} and \code{"score"}: weights are normalized (divided by their mean).
#'   \item \code{"approximate"}: weights are used as provided (raw weights).
#' }
#'
#' @param formula A \code{\link{formula}} specifying the model.
#' @param weights Optional survey weights (numeric vector or one-sided formula).
#'   Weights are passed directly to the underlying C++ algorithms without any
#'   preprocessing like scaling.
#' @param data Optional \code{data.frame} containing the variables used in the model.
#' @param quantile Numeric scalar or vector in (0, 1): target quantile(s) \eqn{\tau}.
#'   Duplicates are automatically removed.
#' @param method One of \code{"ald"}, \code{"score"}, \code{"approximate"}.
#' @param prior Prior specification. Can be:
#'   \itemize{
#'     \item A \code{bqr_prior} object from \code{\link{prior_default}}
#'     \item A list with components \code{b0}, \code{B0}, and optionally \code{c0}, \code{C0}
#'     \item \code{NULL} (uses default vague priors)
#'   }
#'   For \code{"ald"}: uses \code{b0}, \code{B0}, \code{c0}, \code{C0}.
#'   For \code{"score"} and \code{"approximate"}: uses \code{b0}, \code{B0} only.
#' @param niter Integer. Number of MCMC iterations.
#' @param burnin Integer. Number of burn-in iterations.
#' @param thin Integer. Thinning interval.
#' @param ... Additional arguments passed to underlying functions (reserved for future use).
#'
#' @details
#' \strong{Prior Specification:}
#'
#' The prior can be specified in several ways:
#' \enumerate{
#'   \item Using \code{\link{prior_default}} (recommended).
#'   \item As a list with \code{b0}, \code{B0}, and optionally \code{c0}, \code{C0}.
#'   \item As \code{NULL}, in which case vague priors are used.
#' }
#'
#' Multiple quantiles can be fitted in a single call. The returned object
#' adapts its class accordingly (\code{"bwqr_fit"} for one quantile,
#' \code{"bwqr_fit_multi"} for several).
#'
#' @return An object of class \code{"bqr.svy"}, containing:
#' \item{beta}{Posterior mean estimates of regression coefficients.}
#' \item{draws}{Posterior draws from the MCMC sampler.}
#' \item{accept_rate}{Average acceptance rate (if available).}
#' \item{quantile}{The quantile(s) fitted.}
#' \item{prior}{Prior specification used.}
#' \item{formula, terms, model}{Model specification details.}
#' \item{runtime}{Elapsed runtime in seconds.}
#'
#' @examples
#' # Simulate data
#' sim <- simulate_bqr_data(n = 100, betas = c(2, 1.5, -0.8))
#'
#' # Basic usage with default priors
#' fit1 <- bqr.svy(y ~ x1 + x2, data = sim$data, weights = sim$weights)
#'
#' # With informative priors
#' prior <- prior_default(
#'   p  = 3,
#'   b0 = c(2, 1.5, -0.8),
#'   B0 = diag(c(0.25, 0.25, 0.25)),
#'   c0 = 3, C0 = 2
#' )
#' fit2 <- bqr.svy(y ~ x1 + x2, data = sim$data, weights = sim$weights,
#'                 method = "ald", prior = prior)
#'
#' # Compare methods
#' fit_score <- bqr.svy(y ~ x1 + x2, data = sim$data, weights = sim$weights,
#'                      method = "score")
#' fit_approx <- bqr.svy(y ~ x1 + x2, data = sim$data, weights = sim$weights,
#'                       method = "approximate")
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

  # --- NEW: allow one or multiple taus ---
  if (!is.numeric(quantile) || any(!is.finite(quantile)))
    stop("'quantile' must be numeric and finite.", call. = FALSE)
  if (any(quantile <= 0 | quantile >= 1))
    stop("All elements of 'quantile' must be in (0,1).", call. = FALSE)
  taus <- sort(unique(as.numeric(quantile)))
  if (length(taus) < length(quantile))
    warning("Duplicated quantiles were provided; using unique sorted values.")

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

  # Weights
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

  # Prior
  prior <- if (is.null(prior)) {
    prior_default(p, names = coef_names)
  } else {
    as_bqr_prior(prior, p = p, names = coef_names, method = method)
  }

  # ALD/Score use normalized weights; Approximate uses raw weights
  w_norm <- w / mean(w)

  # --- helper to run backend for a single tau ---
  run_backend_one <- function(tau_i) {
    draws_i <- switch(method,
                      "ald" = .MCMC_BWQR_AL(
                        y, X, w_norm,
                        tau           = tau_i,
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
                        tau           = tau_i,
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
                        tau           = tau_i,
                        b_prior_mean  = prior$b0,
                        B_prior_prec  = solve(prior$B0)
                      )
    )

    # coerce/clean draws
    if (is.list(draws_i) && !is.data.frame(draws_i)) {
      draws_i <- lapply(draws_i, function(x) if (is.numeric(x) || is.matrix(x)) x)
      draws_i <- draws_i[!vapply(draws_i, is.null, logical(1))]
      draws_i <- do.call(cbind, draws_i)
    }
    if (is.data.frame(draws_i))
      draws_i <- data.matrix(draws_i[vapply(draws_i, is.numeric, logical(1))])

    draws_i <- as.matrix(draws_i)
    storage.mode(draws_i) <- "numeric"
    if (anyNA(draws_i))
      stop("Backend returned non-numeric values; cannot summarize.", call. = FALSE)

    diag_cols <- c("accept_rate", "n_mcmc", "burnin", "thin", "n_samples")
    keep_idx  <- !colnames(draws_i) %in% diag_cols
    accept_rate_i <- if ("accept_rate" %in% colnames(draws_i))
      mean(draws_i[, "accept_rate"]) else NA_real_
    draws_i <- draws_i[, keep_idx, drop = FALSE]

    if (ncol(draws_i) >= p)
      colnames(draws_i)[1:p] <- coef_names
    idx_blank <- which(is.na(colnames(draws_i)) | colnames(draws_i) == "")
    if (length(idx_blank))
      colnames(draws_i)[idx_blank] <- sprintf("V%d", idx_blank)

    beta_hat_i <- if (ncol(draws_i) >= p) colMeans(draws_i[, seq_len(p), drop = FALSE]) else numeric(0)
    names(beta_hat_i) <- if (length(beta_hat_i) > 0) coef_names else character(0)

    list(draws = draws_i, beta = beta_hat_i, accept_rate = accept_rate_i)
  }

  # run for all taus
  fits <- lapply(taus, run_backend_one)
  names(fits) <- paste0("tau=", formatC(taus, format = "f", digits = 3))

  runtime <- proc.time()[["elapsed"]] - tic

  # --- return single- or multi-tau object ---
  if (length(taus) == 1L) {
    out <- list(
      beta         = fits[[1]]$beta,
      draws        = fits[[1]]$draws,
      accept_rate  = fits[[1]]$accept_rate,
      n_chains     = 1L,
      warmup       = burnin,
      thin         = thin,
      runtime      = runtime,
      call         = cl,
      method       = method,
      quantile     = taus,
      prior        = prior,
      terms        = mt,
      model        = mf,
      formula      = formula
    )
    out$call$formula <- formula
    class(out) <- c("bwqr_fit", "bqr.svy")
    return(out)
  } else {
    beta_mat <- do.call(cbind, lapply(fits, `[[`, "beta"))    # p × K
    colnames(beta_mat) <- names(fits)
    draws_list <- lapply(fits, `[[`, "draws")                 # list of matrices
    acc_vec    <- vapply(fits, `[[`, numeric(1), "accept_rate")
    names(acc_vec) <- names(fits)

    out <- list(
      beta         = beta_mat,
      draws        = draws_list,
      accept_rate  = acc_vec,
      n_chains     = 1L,
      warmup       = burnin,
      thin         = thin,
      runtime      = runtime,
      call         = cl,
      method       = method,
      quantile     = taus,
      prior        = prior,
      terms        = mt,
      model        = mf,
      formula      = formula
    )
    out$call$formula <- formula
    class(out) <- c("bwqr_fit_multi", "bqr.svy")
    return(out)
  }
}


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

#' @export
plot.bqr.svy <- function(x, ..., datafile = NULL, response = "Y", x_var = NULL,
                         paintedArea = TRUE, band_choice = c("minmax","symmetric"),
                         show_data = !is.null(datafile)) {
  drawQuantile1D(
    fit         = x,
    datafile    = datafile,
    response    = response,
    x_var       = x_var,
    paintedArea = paintedArea,
    band_choice = band_choice,
    print_plot  = TRUE,
    show_data   = show_data
  )
}

