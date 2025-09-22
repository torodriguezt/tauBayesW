# =====================================================
#  Helpers
# =====================================================

if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a)) b else a

# =====================================================
#  MODEL FITTER (multiple-output/multivariate)
# =====================================================
    
#' Multiple-Output Bayesian quantile regression for complex survey data
#'
#' mo.bqr.svy implements a Bayesian approach to multiple-output quantile regression 
#' for complex survey data analysis. The method builds a quantile region based on
#' a directional approach. To improve computational efficiency, an Expectation-Maximization (EM)
#' algorithm is implemented instead of the usual Markov Chain Monte Carlo (MCMC).
#'
#' @param formula a symbolic description of the model to be fit.
#' @param weights an optional numerical vector containing the survey weights. If \code{NULL}, equal weights are used.
#' @param data an optional data frame containing the variables in the model.
#' @param quantile numerical scalar or vector containing quantile(s) of interest (default=0.5).
#' @param prior a \code{bqr_prior} object of class "prior". If omitted, a vague prior is assumed (see \code{\link{prior}).
#' @param U an optional \eqn{d \times K}-matrix of directions, where \eqn{d} indicates the response variable dimension
#' and \eqn{K} indicates indicates the number of directions. 
#' @param gamma_U an optional list with length equal to \eqn{K} for which each element corresponds to
#' \eqn{d \times (d-1)}-matrix of ortoghonal basis for each row of \code{U}.
#' @param n_dir numerical scalar corresponding to the number of directions (if \code{U} and \code{gamma_U} are not supplied).
#' @param epsilon numerical scalar indicating the convergence tolerance for the EM algorithm (default = 1e-6).
#' @param max_iter numerical scalar indicating maximum number of EM iterations (default = 1000).
#  @param verbose logical flag indicating whether to print progress messages (default=TRUE).
#'
#' @details
#' The algorithm works by drawing or receiving as input a set of unit directions
#' \eqn{u_k \in \mathbb{R}^d}. For each direction, an orthonormal basis of its
#' orthogonal complement \eqn{\Gamma_k} is computed using \code{pracma::nullspace}.
#' The response \eqn{Y} is then projected into the pair \eqn{(u_k, \Gamma_k)}, and
#' a Bayesian quantile regression is fitted along that direction using the EM
#' algorithm. Results across all directions can be combined to approximate the
#' multivariate quantile region.
#'
#' Prior distributions can be specified globally or quantile-specific. When a list
#' of priors is provided, elements can be named using either \code{"q0.1"} format or
#' \code{"0.1"} format to match specific quantiles. When a function is provided, it
#' will be called with \code{(tau, p, names)} for each quantile level.
#'
#' @return An object of class \code{"mo.bqr.svy"} containing:
#'   \item{call}{The matched call}
#'   \item{formula}{The model formula}
#'   \item{terms}{The terms object}
#'   \item{quantile}{Vector of fitted quantiles}
#'   \item{algorithm}{Algorithm used}
#'   \item{prior}{List of priors used for each quantile}
#'   \item{fit}{List of fitted results for each quantile, each containing one sub-list per direction}
#'   \item{coefficients}{Coefficients from the first quantile}
#'   \item{n_dir}{Number of directions}
#'   \item{U}{Matrix of projection directions (\eqn{d \times K})}
#'   \item{Gamma_list}{List of orthogonal complement bases, one per direction}
#'   \item{n_obs}{Number of observations}
#'   \item{n_vars}{Number of covariates}
#'   \item{response_dim}{Dimension of the response \eqn{d}}
#'
#' @examples
#' \donttest{
#' # Datos simulados para el ejemplo
#' set.seed(1)
#' n  <- 150
#' x1 <- runif(n,-1,1)
#' x2 <- rnorm(n)
#' y  <- 1 + 2*x1 + 0.5*x2 + rnorm(n)
#' mydata <- data.frame(y, x1, x2)
#'
#' # Basic usage with default priors
#' fit1 <- mo.bqr.svy(y ~ x1 + x2, data = mydata,
#'                    quantile = c(0.1, 0.5, 0.9))
#'
#' # Using quantile-specific priors via function
#' prior_fn <- function(tau, p, names) {
#'   variance <- ifelse(tau < 0.2 | tau > 0.8, 0.1, 1.0)
#'   mo_prior_default(p = p, beta_cov = diag(variance, p), names = names)
#' }
#' fit2 <- mo.bqr.svy(y ~ x1 + x2, data = mydata,
#'                    quantile = c(0.1, 0.5, 0.9), prior = prior_fn)
#'
#' # Explicit control of directions
#' set.seed(1)
#' y1 <- 1 + 2*x1 + 0.5*x2 + rnorm(n)
#' y2 <- -1 + 1.5*x1 + rnorm(n)
#' y3 <- 0.5 - x2 + rnorm(n)
#' mydata <- data.frame(y1, y2, y3, x1, x2)
#' U <- matrix(rnorm(9), nrow = 3)  # d=3, K=2
#' U <- apply(U, 2, function(v) v / sqrt(sum(v^2))) # normalize
#' fit3 <- mo.bqr.svy(cbind(y1,y2,y3) ~ x1 + x2, data = mydata,
#'                    quantile = 0.5, U = U)
#' }
#' @export
#' @importFrom stats model.frame model.matrix model.response
#' @importFrom pracma nullspace
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
                       gamma_prior_var = 1e6,
                       ...) {

  if (algorithm != "em") stop("Only 'em' is implemented.")
  if (missing(data))     stop("'data' must be provided.")

  if (length(quantile) == 0) stop("'quantile' cannot be empty.")
  if (any(!is.finite(quantile))) stop("'quantile' must be numeric and finite.")
  if (any(quantile <= 0 | quantile >= 1)) stop("'quantile' must be in (0,1).")
  quantile <- sort(quantile)

  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  if (is.vector(y)) y <- matrix(y, ncol = 1)
  if (!is.matrix(y)) stop("'y' must be a numeric matrix or vector.")
  if (any(!is.finite(y))) stop("Response 'y' contains non-finite values.")
  n <- nrow(y); d <- ncol(y)

  X  <- model.matrix(attr(mf, "terms"), mf)
  if (nrow(X) != n) stop("nrow(X) must match nrow(y).")
  p  <- ncol(X)
  coef_names <- colnames(X)

  wts <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(wts) != n)  stop("'weights' must have length n.")
  if (any(!is.finite(wts)) || any(wts <= 0)) stop("Invalid weights.")
  wts <- wts / mean(wts)

  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("Package 'pracma' is required for nullspace calculation")
  }

  `%||%` <- function(a, b) if (is.null(a) || (is.atomic(a) && length(a) == 1L && is.na(a))) b else a
  dots <- list(...)
  U_user <- dots$U %||% NULL

  if (!is.null(U_user)) {
    U <- as.matrix(U_user)
    if (nrow(U) != d) stop("U must have d rows.")
    K <- ncol(U)

    if (d == 1) {
      Gamma_list <- replicate(K, matrix(numeric(0), 1, 0), simplify = FALSE)
    } else {
      Gamma_list <- lapply(seq_len(K), function(k) pracma::nullspace(t(U[, k])))
    }

  } else {
    if (d == 1) {
      U <- matrix(1.0, 1, 1)
      K <- 1
      Gamma_list <- list(matrix(numeric(0), 1, 0))
    } else {
      K <- max(1L, as.integer(n_dir))
      U <- matrix(NA_real_, d, K)
      Gamma_list <- vector("list", K)
      for (k in seq_len(K)) {
        u_k <- rnorm(d)
        u_k <- u_k / sqrt(sum(u_k^2))
        U[, k] <- u_k
        Gamma_list[[k]] <- pracma::nullspace(t(u_k))
      }
    }
  }


  # --- Priors helper ---
  as_prior_list_per_tau <- function(prior, p, names, taus) {
    if (is.null(prior)) {
      priors <- replicate(length(taus),
                          list(beta_mean = rep(0, p),
                               beta_cov  = diag(1e6, p),
                               sigma_shape = 1e-3,
                               sigma_rate  = 1e-3),
                          simplify = FALSE)
      names(priors) <- paste0("q", taus)
      return(priors)
    }
    if (is.function(prior)) {
      priors <- lapply(taus, function(tau) {
        pr <- prior(tau, p, names)
        if (is.list(pr) && all(c("beta_mean","beta_cov","sigma_shape","sigma_rate") %in% names(pr))) pr else
          stop("prior(tau,...) must return list with beta_mean,beta_cov,sigma_shape,sigma_rate")
      })
      names(priors) <- paste0("q", taus)
      return(priors)
    }
    if (is.list(prior) && !is.null(prior$beta_mean)) {
      priors <- replicate(length(taus), prior, simplify = FALSE)
      names(priors) <- paste0("q", taus)
      return(priors)
    }
    if (is.list(prior)) {
      if (!is.null(names(prior))) {
        priors <- vector("list", length(taus))
        for (i in seq_along(taus)) {
          key1 <- paste0("q", taus[i]); key2 <- as.character(taus[i])
          pr <- prior[[key1]] %||% prior[[key2]] %||% prior[[i]]
          if (is.null(pr)) stop("Missing prior for tau=", taus[i])
          priors[[i]] <- pr
        }
        names(priors) <- paste0("q", taus)
        return(priors)
      }
      if (length(prior) != length(taus)) stop("Length of prior list must match 'quantile'.")
      names(prior) <- paste0("q", taus)
      return(prior)
    }
    stop("Invalid 'prior'.")
  }

  prior_list <- as_prior_list_per_tau(prior, p = p, names = coef_names, taus = quantile)

  # --- Main fitting loop ---
  results <- vector("list", length(quantile))
  names(results) <- paste0("q", quantile)

  for (qi in seq_along(quantile)) {
    q  <- quantile[qi]
    pr <- prior_list[[qi]]

    if (verbose) message(sprintf("Fitting tau = %.3f (d=%d, K=%d)", q, d, K))

    direction_results <- vector("list", K)
    names(direction_results) <- paste0("dir_", seq_len(K))

    for (k in seq_len(K)) {
      u_k <- U[, k]
      gamma_uk <- Gamma_list[[k]]
      r_k <- if (d > 1) ncol(gamma_uk) else 0
      p_ext <- p + r_k

      mu0_ext <- c(pr$beta_mean, rep(0, r_k))
      sigma0_ext <- diag(1e6, p_ext)
      sigma0_ext[1:p, 1:p] <- pr$beta_cov
      if (r_k > 0) {
        sigma0_ext[(p+1):p_ext, (p+1):p_ext] <- diag(gamma_prior_var, r_k)
      }

      u_k_matrix <- matrix(u_k, ncol = 1)
      gamma_k_matrix <- if (d > 1) gamma_uk else matrix(numeric(0), d, 0)

      cpp_result <- .bwqr_weighted_em_cpp_sep(
        y        = y,
        x        = X,
        w        = wts,
        u        = u_k_matrix,
        gamma_u  = gamma_k_matrix,
        tau      = q,
        mu0      = mu0_ext,
        sigma0   = sigma0_ext,
        a0       = pr$sigma_shape,
        b0       = pr$sigma_rate,
        eps      = epsilon,
        max_iter = max_iter,
        verbose  = verbose
      )

      beta_k <- as.numeric(cpp_result$beta[1, ])
      sigma_k <- as.numeric(cpp_result$sigma[1])
      covariate_names <- c(coef_names,
                           if (r_k > 0) paste0("gamma_", seq_len(r_k)) else character(0))
      names(beta_k) <- covariate_names

      direction_results[[k]] <- list(
        beta        = beta_k,
        sigma       = sigma_k,
        iter        = cpp_result$iter,
        converged   = cpp_result$converged,
        u           = u_k,
        gamma_u     = gamma_uk
      )
    }

    results[[qi]] <- list(
      directions  = direction_results,
      prior       = pr,
      U           = U,
      Gamma_list  = Gamma_list,
      quantile    = q
    )
  }

  first_result <- results[[1]]
  coefficients_all <- unlist(lapply(seq_len(K), function(k) {
    beta_k <- first_result$directions[[k]]$beta
    names(beta_k) <- paste0(names(beta_k), "_dir", k)
    beta_k
  }))

  structure(list(
    call         = match.call(),
    formula      = formula,
    terms        = attr(mf, "terms"),
    quantile     = quantile,
    algorithm    = algorithm,
    prior        = prior_list,
    fit          = results,
    coefficients = coefficients_all,
    n_dir        = K,
    U            = U,
    Gamma_list   = Gamma_list,
    n_obs        = n,
    n_vars       = p,
    response_dim = d
  ), class = "mo.bqr.svy")
}


#' @keywords internal
plot.mo.bqr.svy <- function(x, ..., datafile = NULL, response = c("Y1","Y2"),
                            xValue = NULL, paintedArea = FALSE, comparison = FALSE,
                            show_data = !is.null(datafile)) {
  drawQuantileRegion(
    fit         = x,
    datafile    = datafile,
    response    = response,
    xValue      = xValue,
    paintedArea = paintedArea,
    comparison  = comparison,
    print_plot  = TRUE,
    show_data   = show_data
  )
}
