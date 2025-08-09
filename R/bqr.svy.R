if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' Bayesian weighted quantile regression (survey design)
#'
#' Fits a Bayesian quantile regression model with survey weights using one of 
#' three MCMC kernels implemented in C++: `MCMC_BWQR_AL`, `MCMC_BWQR_SL`, 
#' `MCMC_BWQR_AP`. **Only one quantile at a time.**
#'
#' @param formula A symbolic description of the model to be fitted
#' @param weights Survey weights (optional). Can be NULL, numeric vector, or formula
#' @param data An optional data frame containing the variables in the model
#' @param quantile Numeric scalar containing the quantile of interest (default=0.5)
#' @param method Character string specifying the method: 'ald', 'score', 'approximate'
#' @param prior An S3 object of class "prior". If omitted, a diffuse prior will be assumed
#' @param niter Number of MCMC draws
#' @param burnin Number of burn-in iterations to discard from the beginning of the Markov chain
#' @param thin Thinning parameter, i.e., keep every `thin`-th draw (default=1)
#' @param ... Additional arguments (currently unused)
#' @return An object of class `c("bwqr_fit", "bqr.svy")` containing:
#'   \item{draws}{Matrix of posterior draws}
#'   \item{accept_rate}{Acceptance rate for MCMC}
#'   \item{method}{Method used}
#'   \item{quantile}{Quantile fitted}
#'   \item{prior}{Prior specification used}
#'   \item{terms}{Terms object from the model frame}
#'   \item{model}{The model frame}
#'   \item{call}{The matched call}
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- runif(n)
#' y <- 1 + 2*x1 - 0.5*x2 + rnorm(n)
#' weights <- runif(n, 0.5, 2)
#' data <- data.frame(y, x1, x2)
#' 
#' fit <- bqr.svy(y ~ x1 + x2, weights = weights, data = data, 
#'                quantile = 0.5, method = "ald", niter = 1000)
#' summary(fit)
#' }
#' @export
#' @importFrom stats model.frame model.matrix model.response terms
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
  
  if (is.null(data)) data <- environment(formula)
  
  mf <- model.frame(formula, data)
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
  
  prior <- prior %||% prior_default(ncol(X))
  if (!is.list(prior) || !all(c("b0", "B0") %in% names(prior)))
    stop("'prior' must contain at least 'b0' and 'B0'.", call. = FALSE)
  
  w_norm <- w/mean(w)
  
  draws <- switch(method,
                  "ald" = MCMC_BWQR_AL(y, X, w_norm, tau = quantile,
                                       n_mcmc = niter, burnin = burnin, thin = thin),
                  "score" = MCMC_BWQR_SL(y, X, w_norm, tau = quantile,
                                         n_mcmc = niter, burnin = burnin, thin = thin,
                                         b0_ = prior$b0, B0_ = prior$B0),
                  "approximate" = MCMC_BWQR_AP(y, X, w, tau = quantile,
                                               n_mcmc = niter, burnin = burnin, thin = thin,
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
  
  p <- length(coef_names)
  if (ncol(draws) >= p)
    colnames(draws)[1:p] <- coef_names
  idx_blank <- which(is.na(colnames(draws)) | colnames(draws) == "")
  if (length(idx_blank))
    colnames(draws)[idx_blank] <- sprintf("V%d", idx_blank)
  
  runtime <- proc.time()[["elapsed"]] - tic
  
  out <- list(
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

#' @keywords internal
prior_default <- function(p,
                          b0      = rep(0, p),
                          B0      = diag(1e6, p),
                          w_scale = 2) {
  list(b0 = b0, B0 = B0, w_scale = w_scale)
}