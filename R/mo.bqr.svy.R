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


as_prior_list_per_tau <- function(prior, p, names, taus) {
  # caso NULL
  if (is.null(prior)) {
    priors <- replicate(length(taus), mo_prior_default(p, names = names), simplify = FALSE)
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # caso función
  if (is.function(prior)) {
    priors <- lapply(taus, function(tau) {
      pr <- prior(tau, p, names)
      as_mo_bqr_prior(pr, p = p, names = names)
    })
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # caso objeto único o lista "legacy" válida
  if (inherits(prior, "mo_bqr_prior") ||
      (is.list(prior) && all(c("beta_mean","beta_cov","sigma_shape","sigma_rate") %in% names(prior)))) {
    pr0 <- as_mo_bqr_prior(prior, p = p, names = names)
    priors <- replicate(length(taus), pr0, simplify = FALSE)
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  # caso lista de priors (uno por tau)
  if (is.list(prior)) {
    # nombrada: intentar emparejar "q0.1" o "0.1"
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
          stop("No se encontró prior para tau=", taus[i],
               ". Nombra los elementos como 'q", taus[i], "' o provee longitud exacta.")
        }
      }
      names(priors) <- paste0("q", taus)
      return(priors)
    }
    # no nombrada: exigir longitud correcta
    if (length(prior) != length(taus))
      stop("'prior' como lista debe tener la misma longitud que 'quantile' o venir nombrada.")
    priors <- lapply(prior, function(pr) as_mo_bqr_prior(pr, p = p, names = names))
    names(priors) <- paste0("q", taus)
    return(priors)
  }

  stop("Formato de 'prior' no reconocido. Usa mo_bqr_prior, lista válida, función, o NULL.")
}


# =====================================================
# mo.bqr.svy() - Multiple-Output Bayesian Quantile Regression for Complex Surveys (EM)
# =====================================================

#' Multiple-Output Bayesian Quantile Regression for Complex Surveys (EM)
#'
#' Fits Bayesian quantile regression models for multiple quantiles simultaneously
#' using the EM algorithm. Supports complex survey designs through sampling weights
#' and allows for quantile-specific prior distributions.
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
#' @param n_dir Number of search directions (currently not used).
#' @param epsilon Convergence tolerance for the EM algorithm.
#' @param max_iter Maximum number of EM iterations.
#' @param verbose Logical indicating whether to print progress messages.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' This function allows for flexible prior specification across quantiles. When a list
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
#'   \item{fit}{List of fitted results for each quantile}
#'   \item{coefficients}{Coefficients from the first quantile}
#'   \item{n_dir}{Number of directions}
#'
#' @examples
#' # Basic usage with default priors
#' fit1 <- mo.bqr.svy(y ~ x1 + x2, data = mydata, quantile = c(0.1, 0.5, 0.9))
#'
#' # Using quantile-specific priors via function
#' prior_fn <- function(tau, p, names) {
#'   # More concentrated priors for extreme quantiles
#'   variance <- ifelse(tau < 0.2 | tau > 0.8, 0.1, 1.0)
#'   mo_prior_default(p = p, beta_cov = diag(variance, p), names = names)
#' }
#' fit2 <- mo.bqr.svy(y ~ x1 + x2, data = mydata,
#'                    quantile = c(0.1, 0.5, 0.9), prior = prior_fn)
#'
#' # Using a list of quantile-specific priors
#' priors <- list(
#'   q0.1 = mo_prior_default(p = 3, beta_mean = c(0, 0.8, -0.3)),
#'   q0.5 = mo_prior_default(p = 3, beta_mean = c(0, 1.0, -0.5)),
#'   q0.9 = mo_prior_default(p = 3, beta_mean = c(0, 1.2, -0.7))
#' )
#' fit3 <- mo.bqr.svy(y ~ x1 + x2, data = mydata,
#'                    quantile = c(0.1, 0.5, 0.9), prior = priors)
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
                       # NUEVO:
                       em_mode  = c("joint","separable"),
                       gamma_prior_var = 1e6,
                       ...) {

  em_mode <- match.arg(em_mode)

  if (algorithm != "em") stop("Only 'em' is implemented.")
  if (missing(data))     stop("'data' must be provided.")

  if (length(quantile) == 0) stop("'quantile' cannot be empty.")
  if (any(!is.finite(quantile))) stop("'quantile' must be numeric and finite.")
  if (any(quantile <= 0 | quantile >= 1)) stop("'quantile' must be in (0,1).")
  quantile <- sort(quantile)

  mf <- model.frame(formula, data)
  y  <- model.response(mf)
  if (is.vector(y)) y <- matrix(y, ncol = 1)     # asegurar n x d
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

  # --- Direcciones: permitir pasar via ... o construir desde n_dir ---
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.atomic(a) && length(a) == 1L && is.na(a)) return(b)
    a
  }
  dots <- list(...)
  U_user     <- dots$U      %||% NULL
  Gamma_user <- dots$Gamma  %||% NULL
  r_user     <- dots$r      %||% NULL  # nº de columnas de Gamma por dirección (0..d-1)

  # helper robusto para completar una base ortonormal a partir de v (unitario)
  .orthobasis_from_v <- function(v, d, r_needed) {
    v <- v / sqrt(sum(v^2))
    B <- diag(d); B[, 1] <- v
    Q <- qr.Q(qr(B), complete = TRUE)
    # Fallback si por alguna razón faltaran columnas
    if (ncol(Q) < (1 + r_needed)) {
      Pperp <- diag(d) - v %*% t(v)
      sv <- svd(Pperp)
      Q <- cbind(v, sv$u[, 1:(d-1), drop = FALSE])
    }
    Q
  }

  .build_U_Gamma <- function(d, K, r = NULL) {
    if (d == 1) return(list(U = matrix(1, 1, 1), Gamma = matrix(numeric(0), 1, 0), r = 0))
    if (is.null(r)) r <- 1L
    r <- as.integer(max(0, min(r, d - 1)))

    if (d == 2) {
      angles <- (0:(K - 1)) * 2 * pi / K
      U      <- rbind(cos(angles),  sin(angles))
      Gamma  <- if (r == 0) matrix(numeric(0), 2, 0) else rbind(-sin(angles), cos(angles))
      return(list(U = U, Gamma = Gamma, r = r))
    }

    # d >= 3
    U <- matrix(NA_real_, d, K)
    Gamma <- if (r == 0) matrix(numeric(0), d, 0) else matrix(NA_real_, d, K * r)
    for (k in seq_len(K)) {
      v <- rnorm(d)
      Q <- .orthobasis_from_v(v, d = d, r_needed = r)
      U[, k] <- Q[, 1]
      if (r > 0) Gamma[, ((k - 1) * r + 1):(k * r)] <- Q[, 2:(1 + r), drop = FALSE]
    }
    list(U = U, Gamma = Gamma, r = r)
  }

  if (!is.null(U_user)) {
    U <- as.matrix(U_user)
    if (nrow(U) != d) stop("U must have d rows.")
    K <- ncol(U)
    if (!is.null(Gamma_user)) {
      Gamma <- as.matrix(Gamma_user)
      if (nrow(Gamma) != d) stop("Gamma must have d rows.")
      if (ncol(Gamma) %% K != 0) stop("ncol(Gamma) must be a multiple of ncol(U).")
      r <- ncol(Gamma) / K
    } else {
      tmp <- .build_U_Gamma(d, K, r = r_user)
      r <- tmp$r
      if (r == 0) {
        Gamma <- matrix(numeric(0), d, 0)
      } else {
        Gamma <- matrix(NA_real_, d, K * r)
        for (k in seq_len(K)) {
          v <- U[, k]
          Q <- .orthobasis_from_v(v, d = d, r_needed = r)
          Gamma[, ((k - 1) * r + 1):(k * r)] <- Q[, 2:(1 + r), drop = FALSE]
        }
      }
    }
  } else {
    K <- max(1L, as.integer(n_dir))
    built <- .build_U_Gamma(d, K, r = r_user)
    U     <- built$U
    Gamma <- built$Gamma
    r     <- built$r
  }

  G <- ncol(Gamma)  # puede ser 0

  # --- Priors por cuantil (siempre en términos de beta_X) ---
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
          key1 <- paste0("q", taus[i]); key2 <- as.character(taus[i])  # <- fix de 'laus'
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

  # Nombres coef conjunto (modo joint)
  if (G > 0) {
    stopifnot(G %% ncol(U) == 0)
    r_eff <- G / ncol(U)
    gamma_names_joint <- paste0("gamma_k", rep(seq_len(ncol(U)), each = r_eff),
                                "_c", sequence(rep(r_eff, ncol(U))))
  } else gamma_names_joint <- character(0)
  all_coef_names_joint <- c(coef_names, gamma_names_joint)

  # Nombres coef separable (modo separable: X por dirección)
  x_names_by_dir <- as.vector(t(outer(coef_names, paste0("_k", seq_len(ncol(U))), paste0)))
  gamma_names_sep <- if (r > 0) gamma_names_joint else character(0)
  all_coef_names_sep <- c(x_names_by_dir, gamma_names_sep)

  results <- vector("list", length(quantile))
  names(results) <- paste0("q", quantile)

  for (qi in seq_along(quantile)) {
    q  <- quantile[qi]
    pr <- prior_list[[qi]]

    if (verbose) message(sprintf("Fitting tau = %.3f (mode=%s, d=%d, K=%d, r=%d, G=%d)",
                                 q, em_mode, d, ncol(U), r, G))

    if (em_mode == "joint") {
      # ---- PRIOR conjunto: m = p + G
      m      <- p + G
      mu0    <- c(pr$beta_mean, rep(0, G))
      sigma0 <- diag(1e6, m)
      sigma0[1:p, 1:p] <- pr$beta_cov
      # llamar wrapper conjunta
      cpp_result <- .bwqr_weighted_em_cpp(
        y        = y,
        x        = X,
        w        = wts,
        u        = U,
        gamma_u  = Gamma,
        tau      = q,
        mu0      = mu0,
        sigma0   = sigma0,
        a0       = pr$sigma_shape,
        b0       = pr$sigma_rate,
        eps      = epsilon,
        max_iter = max_iter,
        verbose  = verbose
      )
      beta_final <- as.numeric(cpp_result$beta)  # length p+G
      names(beta_final) <- all_coef_names_joint

      results[[qi]] <- list(
        beta        = beta_final,                    # vector p+G
        sigma       = as.numeric(cpp_result$sigma),  # escalar
        iter        = cpp_result$iter,
        converged   = cpp_result$converged,
        prior       = pr,
        mode        = "joint",
        U           = U,
        Gamma       = Gamma
      )

    } else {
      # ---- PRIOR separable por bloque: m_blk = p + r (replicado en K direcciones)
      m_blk   <- p + r
      mu0_blk <- c(pr$beta_mean, rep(0, r))
      # matriz de covarianza prior por-bloque:
      sigma0_blk <- diag(1e-6, m_blk)   # valor "de relleno"; se sobreescribe abajo
      sigma0_blk[1:p, 1:p] <- pr$beta_cov
      if (r > 0) sigma0_blk[(p+1):(p+r), (p+1):(p+r)] <- diag(gamma_prior_var, r)

      cpp_result <- .bwqr_weighted_em_cpp_sep(
        y        = y,
        x        = X,
        w        = wts,
        u        = U,
        gamma_u  = Gamma,
        tau      = q,
        mu0      = mu0_blk,     # por-bloque (el C++ replica por K)
        sigma0   = sigma0_blk,  # por-bloque
        a0       = pr$sigma_shape,
        b0       = pr$sigma_rate,
        eps      = epsilon,
        max_iter = max_iter,
        verbose  = verbose
      )

      # cpp_result$beta: K x (p+r)
      beta_dir <- as.matrix(cpp_result$beta)
      colnames(beta_dir) <- c(coef_names, if (r>0) paste0("gamma_c", seq_len(r)))
      rownames(beta_dir) <- paste0("k", seq_len(ncol(U)))

      # armar un vector "flat" con nombres expandidos (X por dir + todas gammas por dir):
      beta_x_by_dir <- beta_dir[, 1:p, drop = FALSE]  # K x p
      beta_gamma_by_dir <- if (r>0) beta_dir[, (p+1):(p+r), drop = FALSE] else NULL

      beta_flat <- as.numeric(t(beta_x_by_dir))  # X por dir apilado por filas
      names(beta_flat) <- as.vector(t(outer(coef_names, paste0("_k", seq_len(ncol(U))), paste0)))

      if (r > 0) {
        # añadir gammas en el orden k1_c1, k1_c2, ... kK_c_r
        gam_vec <- as.numeric(t(beta_gamma_by_dir))
        names(gam_vec) <- paste0("gamma_k", rep(seq_len(ncol(U)), each = r),
                                 "_c", sequence(rep(r, ncol(U))))
        beta_flat <- c(beta_flat, gam_vec)
      }

      results[[qi]] <- list(
        beta_dir     = beta_dir,                     # K x (p+r)
        beta         = beta_flat,                    # vector con nombres expandidos
        sigma_by_dir = as.numeric(cpp_result$sigma), # K
        iter         = cpp_result$iter,
        converged    = cpp_result$converged,
        prior        = pr,
        mode         = "separable",
        U            = U,
        Gamma        = Gamma
      )
    }
  }

  # coefficients (para compatibilidad con métodos que esperan vector)
  coefficients_all <- as.numeric(results[[1]]$beta)
  names(coefficients_all) <- names(results[[1]]$beta)

  structure(list(
    call         = match.call(),
    formula      = formula,
    terms        = attr(mf, "terms"),
    quantile     = quantile,
    algorithm    = algorithm,
    prior        = prior_list,
    fit          = results,
    coefficients = coefficients_all,
    n_dir        = ncol(U),
    U            = U,
    Gamma        = Gamma,
    mode         = em_mode
  ), class = "mo.bqr.svy")
}



#' Print method for mo.bqr.svy objects
#'
#' @export
print.mo_bqr.svy <- function(x, ...) {
  cat("Call:\n"); print(x$call)
  for (i in seq_along(x$quantile)) {
    cat(sprintf("\nQuantile: %.2f\n", x$quantile[i]))
    print(x$fit[[i]]$beta)
  }
  invisible(x)
}

#' Simulate data for Multiple-Output Bayesian Quantile Regression
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
