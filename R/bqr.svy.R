if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

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
#'   Default is \code{"ald"} (Asymmetric Laplace Distribution).
#' @param prior Prior specification. Can be:
#'   \itemize{
#'     \item A \code{bqr_prior} object from \code{\link{prior}}
#'     \item A list with components \code{b0}, \code{B0}, and optionally \code{c0}, \code{C0}
#'     \item \code{NULL} (uses default vague priors)
#'   }
#'   For \code{"ald"}: uses \code{b0}, \code{B0}, \code{c0}, \code{C0}.
#'   For \code{"score"} and \code{"approximate"}: uses \code{b0}, \code{B0} only.
#'   \strong{Tip:} Use \code{\link{prior}()} for a simpler unified interface.
#' @param niter Integer. Number of MCMC iterations.
#' @param burnin Integer. Number of burn-in iterations.
#' @param thin Integer. Thinning interval.
#' @param print_progress Integer. Print progress every \code{print_progress} iterations.
#'   Set to 0 to disable progress printing. Default is 1000.
#' @param ... Additional arguments passed to underlying functions (reserved for future use).
#'
#' @details
#' \strong{Prior Specification:}
#'
#' The prior can be specified in several ways:
#' \enumerate{
#'   \item Using \code{\link{prior}} (recommended).
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
#' # Generate example data
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- runif(n, -1, 1)
#' y <- 2 + 1.5*x1 - 0.8*x2 + rnorm(n)
#' weights <- runif(n, 0.5, 2)
#' data <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Basic usage with default priors
#' fit1 <- bqr.svy(y ~ x1 + x2, data = data, weights = weights)
#'
#' # With informative priors
#' prior <- prior(
#'   p = 3,
#'   type = "univariate",
#'   beta_mean = c(2, 1.5, -0.8),
#'   beta_cov = diag(c(0.25, 0.25, 0.25)),
#'   sigma_shape = 3, sigma_rate = 2
#' )
#' fit2 <- bqr.svy(y ~ x1 + x2, data = data, weights = weights,
#'                 method = "ald", prior = prior)
#'
#' # Compare methods
#' fit_score <- bqr.svy(y ~ x1 + x2, data = data, weights = weights,
#'                      method = "score")
#' fit_approx <- bqr.svy(y ~ x1 + x2, data = data, weights = weights,
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
                    print_progress = 1000,
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

  if (!is.numeric(print_progress) || length(print_progress) != 1 || print_progress < 0)
    stop("'print_progress' must be a non-negative integer.", call. = FALSE)
  print_progress <- as.integer(print_progress)

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

  # Check for unused sigma hyperparameters in non-ALD methods
  if (method %in% c("score", "approximate")) {
    if (!is.null(prior$c0) || !is.null(prior$C0)) {
      warning("Method '", method, "' does not estimate sigma, so hyperparameters 'c0' and 'C0' ",
              "specified in the prior will be ignored.", call. = FALSE)
    }
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
                        C0            = prior$C0 %||% 0.001,
                        print_progress = print_progress
                      ),
                      "score" = .MCMC_BWQR_SL(
                        y, X, w_norm,
                        tau           = tau_i,
                        n_mcmc        = niter,
                        burnin        = burnin,
                        thin          = thin,
                        b_prior_mean  = prior$b0,
                        B_prior_prec  = solve(prior$B0),
                        print_progress = print_progress
                      ),
                      "approximate" = .MCMC_BWQR_AP(
                        y, X, w,
                        n_mcmc        = niter,
                        burnin        = burnin,
                        thin          = thin,
                        tau           = tau_i,
                        b_prior_mean  = prior$b0,
                        B_prior_prec  = solve(prior$B0),
                        print_progress = print_progress
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


#' @keywords internal
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
