# =====================================================
#  Helpers
# =====================================================

if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a)) b else a


# =====================================================
#  Prior (constructor, coercion, print) for mo.bqr.svy
# =====================================================

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
#' prior1 <- mo_prior_default(p = 3, beta_mean = c(0, 1, -0.5))
#'
#' # Create quantile-specific priors using a list
#' priors_list <- list(
#'   q0.1 = mo_prior_default(p = 3, beta_mean = c(0, 0.8, -0.3)),
#'   q0.5 = mo_prior_default(p = 3, beta_mean = c(0, 1.0, -0.5)),
#'   q0.9 = mo_prior_default(p = 3, beta_mean = c(0, 1.2, -0.7))
#' )
#'
#' # Create quantile-specific priors using a function
#' prior_fn <- function(tau, p, names) {
#'   # More informative priors for extreme quantiles
#'   variance <- ifelse(tau < 0.2 | tau > 0.8, 0.5, 1.0)
#'   mo_prior_default(p = p, beta_cov = diag(variance, p), names = names)
#' }
#'
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
#' This function ensures that the input is converted to a valid
#' \code{mo_bqr_prior} object. It accepts an existing \code{mo_bqr_prior},
#' or a list with the necessary components, and performs validation
#' and expansion of scalar or vector inputs.
#'
#' @param x An object to coerce. Can be:
#'   \itemize{
#'     \item A valid \code{mo_bqr_prior} object
#'     \item A list with components \code{beta_mean}, \code{beta_cov},
#'           \code{sigma_shape}, and \code{sigma_rate}
#'   }
#' @param p Integer. Number of regression coefficients (including intercept).
#'   Used to validate and expand inputs.
#' @param names Optional character vector of coefficient names. If provided,
#'   they are attached to \code{beta_mean} and \code{beta_cov}.
#'
#' @return A \code{mo_bqr_prior} object with fields:
#'   \itemize{
#'     \item \code{beta_mean}: Numeric vector of prior means
#'     \item \code{beta_cov}: Prior covariance matrix
#'     \item \code{sigma_shape}: Shape parameter of inverse-gamma prior
#'     \item \code{sigma_rate}: Rate parameter of inverse-gamma prior
#'   }
#'
#' @examples
#' # From an existing mo_bqr_prior
#' prior1 <- mo_prior_default(p = 3)
#' as_mo_bqr_prior(prior1, p = 3)
#'
#' # From a list
#' prior_list <- list(
#'   beta_mean   = c(0, 0, 0),
#'   beta_cov    = diag(1e6, 3),
#'   sigma_shape = 0.001,
#'   sigma_rate  = 0.001
#' )
#' as_mo_bqr_prior(prior_list, p = 3, names = c("(Intercept)", "x1", "x2"))
#'
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


#' Print method for \code{mo_bqr_prior} objects
#'
#' This function defines the \code{print()} method for objects of class
#' \code{mo_bqr_prior}. It displays a summary of the prior structure,
#' including the length of \code{beta_mean}, the dimension of
#' \code{beta_cov}, and the values of \code{sigma_shape} and \code{sigma_rate}.
#'
#' @param x An object of class \code{mo_bqr_prior}.
#' @param ... Additional arguments passed to or from other methods
#'   (currently unused).
#'
#' @return The object \code{x}, invisibly.
#'
#' @examples
#' prior <- mo_prior_default(p = 3)
#' print(prior)
#'
#' @export
print.mo_bqr_prior <- function(x, ...) {
  cat("mo_bqr_prior\n")
  cat("  beta_mean: length", length(x$beta_mean), "\n")
  cat("  beta_cov:", nrow(x$beta_cov), "x", ncol(x$beta_cov), "\n")
  cat("  sigma_shape:", x$sigma_shape, "\n")
  cat("  sigma_rate :", x$sigma_rate, "\n")
  invisible(x)
}



as_prior_list_per_tau <- function(prior, p, names, taus) {
  # NULL case
  if (is.null(prior)) {
    priors <- replicate(length(taus), mo_prior_default(p, names = names), simplify = FALSE)
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # function case
  if (is.function(prior)) {
    priors <- lapply(taus, function(tau) {
      pr <- prior(tau, p, names)
      as_mo_bqr_prior(pr, p = p, names = names)
    })
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # single object or valid "legacy" list case
  if (inherits(prior, "mo_bqr_prior") ||
      (is.list(prior) && all(c("beta_mean","beta_cov","sigma_shape","sigma_rate") %in% names(prior)))) {
    pr0 <- as_mo_bqr_prior(prior, p = p, names = names)
    priors <- replicate(length(taus), pr0, simplify = FALSE)
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # list of priors (one per tau)
  if (is.list(prior)) {
    # named: try to match "q0.1" or "0.1"
    if (!is.null(names(prior))) {
      priors <- vector("list", length(taus))
      for (i in seq_along(taus)) {
        key1 <- paste0("q", taus[i])
        key2 <- as.character(taus[i])
        if (!is.null(prior[[key1]])) {
          priors[[i]] <- as_mo_bqr_prior(prior[[key1]], p = p, names = names)
        } else if (!is.null(prior[[key2]])) {
          priors[[i]] <- as_mo_bqr_prior(prior[[key2]], p = p, names = names)
        } else if (length(prior) == length(taus)) {
          priors[[i]] <- as_mo_bqr_prior(prior[[i]], p = p, names = names)
        } else {
          stop("No prior found for tau = ", taus[i],
               ". Name the elements as 'q", taus[i], "' or provide a list of exact length.")
        }
      }
      names(priors) <- paste0("q", taus)
      return(priors)
    }
    # unnamed: require correct length
    if (length(prior) != length(taus))
      stop("'prior' as a list must have the same length as 'quantile' or be named.")
    priors <- lapply(prior, function(pr) as_mo_bqr_prior(pr, p = p, names = names))
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  stop("Unrecognized 'prior' format. Use mo_bqr_prior, a valid list, a function, or NULL.")
}



#' Multiple-Output Bayesian Quantile Regression for Complex Surveys (Directional EM)
#'
#' Fits Bayesian quantile regression models for multivariate responses using the
#' EM algorithm and a directional approach. The method projects the response into
#' random unit vectors (directions) and their orthogonal complements, and then fits
#' univariate Bayesian quantile regression models along each projection. The
#' collection of fitted directions defines the multivariate quantile region.
#'
#' @param formula A formula object specifying the model.
#' @param weights Optional vector of sampling weights. If \code{NULL}, equal weights are used.
#' @param data A data frame containing the variables in the model.
#' @param quantile Numeric vector of quantile levels (between 0 and 1, exclusive).
#' @param algorithm Character string specifying the algorithm. Currently only \code{"em"} is supported.
#' @param prior Prior specification. Can be:
#'   \itemize{
#'     \item \code{NULL}: Default priors are used for all quantiles
#'     \item A single \code{mo_bqr_prior} object: Recycled for all quantiles
#'     \item A list of \code{mo_bqr_prior} objects: One prior per quantile
#'     \item A function \code{f(tau, p, names)}: Generates quantile-specific priors
#'   }
#' @param n_dir Integer. Number of projection directions (if directions \code{U} are not supplied).
#' @param epsilon Convergence tolerance for the EM algorithm.
#' @param max_iter Maximum number of EM iterations.
#' @param verbose Logical indicating whether to print progress messages.
#' @param gamma_prior_var Numeric. Prior variance for the gamma coefficients
#'   associated with orthogonal complements.
#' @param ... Additional arguments for direction specification:
#'   \describe{
#'     \item{U}{Optional user-specified matrix of directions (\eqn{d \times K}). If not provided,
#'       \code{n_dir} random unit vectors are generated automatically.}
#'   }
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


#' Print method for mo.bqr.svy objects
#'
#' Displays the fitted call, quantiles, and estimated coefficients per direction.
#'
#' @param x An object of class \code{"mo.bqr.svy"}.
#' @param ... Additional arguments (not used).
#' Print method for mo.bqr.svy objects
#' @method print mo.bqr.svy
#' @export
print.mo.bqr.svy <- function(x, ...) {
  cat("Call:\n"); print(x$call)

  cat("\nFitted quantiles:", paste(x$quantile, collapse = ", "), "\n")
  cat("Number of directions:", x$n_dir, "\n")
  cat("Response dimension:", x$response_dim, "\n")

  for (qi in seq_along(x$quantile)) {
    cat(sprintf("\n--- Quantile %.2f ---\n", x$quantile[qi]))
    dir_results <- x$fit[[qi]]$directions

    for (k in seq_along(dir_results)) {
      dr <- dir_results[[k]]
      cat(sprintf(" Direction %d:\n", k))
      print(round(dr$beta, 4))
      cat("  sigma:", round(dr$sigma, 4),
          " | converged:", dr$converged,
          " | iter:", dr$iter, "\n")
    }
  }

  invisible(x)
}



#' Simulate data for Multiple-Output Bayesian Quantile Regression
#'
#' Generates synthetic data suitable for testing and demonstrating
#' \code{\link{mo.bqr.svy}}. The function simulates predictors \code{X},
#' a response \code{y}, random weights, and stores the true regression
#' coefficients used for generation.
#'
#' @param n Integer. Number of observations to simulate.
#' @param p Integer. Number of predictors (excluding the intercept).
#' @param beta_true Numeric vector of length \code{p + 1}, giving the true
#'   regression coefficients (first element is the intercept, followed by slopes).
#'   If \code{NULL}, defaults to \code{c(1, 1, 2, ..., p)}.
#' @param seed Optional integer for reproducibility.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{data}}{A \code{data.frame} with response \code{y} and
#'   predictors \code{x1, ..., xp}.}
#'   \item{\code{weights}}{A numeric vector of length \code{n} with
#'   random survey weights drawn from \eqn{U(0.5, 2)}.}
#'   \item{\code{true_betas}}{Matrix with the true regression coefficients
#'   used in the simulation (1 row, \code{p+1} columns).}
#'   \item{\code{quantiles}}{Default quantile levels \code{c(0.1, 0.5, 0.9)}.}
#' }
#'
#' @examples
#' # Simulate data with 2 predictors and default coefficients
#' sim <- simulate_mo_bqr_data(n = 50, p = 2, seed = 123)
#' str(sim$data)
#'
#' # Use custom true coefficients
#' sim2 <- simulate_mo_bqr_data(n = 50, p = 3,
#'                              beta_true = c(1, 2, -1, 0.5),
#'                              seed = 456)
#' sim2$true_betas
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



#' @export
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

