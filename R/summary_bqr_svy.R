# =============================================================================
# Helpers internos
# =============================================================================

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.atomic(a) && length(a) == 1L) return(if (is.na(a)) b else a)
  a
}

.autocovariance_vehtari <- function(x, max_lag = NULL) {
  n <- length(x)
  if (is.null(max_lag)) max_lag <- n - 1
  max_lag <- min(max_lag, n - 1)
  x_centered <- x - mean(x)
  autocov <- numeric(max_lag + 1)
  for (lag in 0:max_lag) {
    if (lag == 0) autocov[lag + 1] <- mean(x_centered^2)
    else autocov[lag + 1] <- mean(x_centered[1:(n - lag)] * x_centered[(lag + 1):n])
  }
  autocov
}

.integrated_time_vehtari <- function(autocov) {
  if (length(autocov) < 2) return(1)
  autocov <- autocov / autocov[1]
  W <- 1
  for (W in 1:(length(autocov) - 1)) {
    tau_int_W <- 1 + 2 * sum(autocov[2:(W + 1)])
    if (W >= 6 * tau_int_W) break
  }
  tau_int <- 1 + 2 * sum(autocov[2:(W + 1)])
  max(1, tau_int)
}

.ess_vehtari_single <- function(x, split_chains = TRUE) {
  x <- as.numeric(x); n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0)  return(NA_real_)
  x_matrix <- if (isTRUE(split_chains)) {
    half <- floor(n / 2); matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  } else matrix(x, ncol = 1)
  x_ranked <- apply(x_matrix, 2, function(chain) (rank(chain) - 0.5) / length(chain))
  x_normal <- qnorm(pmax(pmin(x_ranked, 1 - 1e-15), 1e-15))
  autocov_chains <- apply(x_normal, 2, function(chain)
    .autocovariance_vehtari(chain, max_lag = min(length(chain) - 1, 200)))
  n_iter <- nrow(x_normal); n_chains <- ncol(x_normal)
  max_lag <- min(nrow(autocov_chains), 200)
  autocov_mean <- if (n_chains > 1) rowMeans(autocov_chains[1:max_lag, , drop = FALSE])
  else autocov_chains[1:max_lag]
  tau_int <- .integrated_time_vehtari(autocov_mean)
  max(1, n_chains * n_iter / (2 * tau_int + 1))
}

.ess_tail_vehtari_single <- function(x, prob = c(0.05, 0.95), split_chains = TRUE) {
  x <- as.numeric(x); n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0)  return(NA_real_)
  x_matrix <- if (isTRUE(split_chains)) {
    half <- floor(n / 2); matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  } else matrix(x, ncol = 1)
  n_iter <- nrow(x_matrix); n_chains <- ncol(x_matrix)
  pooled <- as.vector(x_matrix)
  ql <- stats::quantile(pooled, prob[1]); qu <- stats::quantile(pooled, prob[2])

  tail_ess <- function(ind) {
    xr <- apply(ind, 2, function(chain) (rank(chain) - 0.5) / length(chain))
    xn <- qnorm(pmax(pmin(xr, 1 - 1e-15), 1e-15))
    ac <- apply(xn, 2, function(chain)
      .autocovariance_vehtari(chain, max_lag = min(length(chain) - 1, 200)))
    max_lag <- min(nrow(ac), 200)
    acm <- if (n_chains > 1) rowMeans(ac[1:max_lag, , drop = FALSE]) else ac[1:max_lag]
    tau <- .integrated_time_vehtari(acm)
    max(1, n_chains * n_iter / (2 * tau + 1))
  }

  lower_ind <- apply(x_matrix, 2, function(chain) as.numeric(chain <= ql))
  upper_ind <- apply(x_matrix, 2, function(chain) as.numeric(chain >= qu))
  el <- tryCatch(tail_ess(lower_ind), error = function(e) NA_real_)
  eu <- tryCatch(tail_ess(upper_ind), error = function(e) NA_real_)
  if (is.na(el) && is.na(eu)) return(NA_real_)
  if (is.na(el)) return(eu)
  if (is.na(eu)) return(el)
  min(el, eu)
}

.rhat_rank <- function(x, split_chains = TRUE) {
  x <- as.numeric(x); n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0)  return(NA_real_)
  if (!split_chains) return(NA_real_)
  half <- floor(n / 2)
  xm <- matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  xr <- apply(xm, 2, function(chain) (rank(chain) - 0.5) / length(chain))
  xn <- qnorm(xr)
  cm <- colMeans(xn)
  B <- nrow(xn) * stats::var(cm)
  W <- mean(apply(xn, 2, stats::var))
  var_plus <- ((nrow(xn) - 1) / nrow(xn)) * W + B / nrow(xn)
  sqrt(var_plus / W)
}

.check_convergence_and_warn <- function(stats, n_samples, rhat_threshold = 1.01, ess_ratio_threshold = 0.30) {
  if (is.null(stats) || nrow(stats) == 0) return(invisible(NULL))
  rhat_issues <- stats$rhat > rhat_threshold | is.na(stats$rhat)
  ess_threshold <- n_samples * ess_ratio_threshold
  ess_issues <- stats$ess_bulk < ess_threshold | is.na(stats$ess_bulk)
  !(any(rhat_issues) || any(ess_issues))
}

# Resumen y diagnC3sticos a partir de una matriz de draws
summarise_draws_custom <- function(draws, max_lag = 200) {
  if (!is.matrix(draws)) draws <- as.matrix(draws)
  var_names <- colnames(draws); if (is.null(var_names)) var_names <- paste0("V", seq_len(ncol(draws)))
  out <- data.frame(
    variable = var_names,
    mean     = apply(draws, 2, mean,   na.rm = TRUE),
    median   = apply(draws, 2, median, na.rm = TRUE),
    sd       = apply(draws, 2, sd,     na.rm = TRUE),
    q2.5     = apply(draws, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
    q97.5    = apply(draws, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  out$rhat      <- apply(draws, 2, function(x) .rhat_rank(x, split_chains = TRUE))
  out$ess_bulk  <- apply(draws, 2, function(x) .ess_vehtari_single(x, split_chains = TRUE))
  out$ess_tail  <- apply(draws, 2, function(x) .ess_tail_vehtari_single(x, prob = c(0.05, 0.95), split_chains = TRUE))
  out
}

# =============================================================================
# SUMMARY (usar gen??rico base: NO redefinir summary())
# =============================================================================

#' Resumen para objetos bqr.svy (media posterior, IC, Rhat, ESS)
#'
#' Genera un resumen con media a posterior, intervalos de credibilidad,
#' \eqn{\hat{R}} y tama??o muestral efectivo por par??metro.
#'
#' @param object Objeto \code{bqr.svy} (puede contener uno o varios cuantiles).
#' @param probs Probabilidades para IC cre??bles. Default \code{c(0.025, 0.975)}.
#' @param digits Decimales al imprimir (se redondea solo en print).
#' @param ... Ignorado.
#' @return Objeto de clase \code{summary.bqr.svy} con un bloque por cada \eqn{\tau}.
#' @exportS3Method summary bqr.svy
summary.bqr.svy <- function(object, probs = c(0.025, 0.975), digits = 3, ...) {
  stopifnot(inherits(object, "bqr.svy"))

  # --- Metadatos comunes ---
  meta <- list(
    n_chains    = object$n_chains %||% 1L,
    warmup      = object$warmup   %||% 0L,
    thin        = object$thin     %||% 1L,
    accept_rate = object$accept_rate %||% NA_real_,
    runtime     = object$runtime  %||% NA_real_
  )

  # --- Helper: IC por nombre, tolerante a columnas faltantes ---
  ic_by_name <- function(D, vars, probs) {
    lower <- upper <- rep(NA_real_, length(vars))
    cn <- colnames(D)
    for (i in seq_along(vars)) {
      j <- match(vars[i], cn)
      if (!is.na(j)) {
        vcol <- D[, j]
        lower[i] <- stats::quantile(vcol, probs = probs[1], na.rm = TRUE)
        upper[i] <- stats::quantile(vcol, probs = probs[2], na.rm = TRUE)
      }
    }
    list(lower = lower, upper = upper)
  }

  # --- Helper: construir bloque por tau ---
  make_block <- function(D, tau) {
    D <- if (is.data.frame(D)) data.matrix(D) else as.matrix(D)
    storage.mode(D) <- "numeric"
    if (!nrow(D) || !ncol(D)) stop("Matriz de draws vac??a.", call. = FALSE)

    stats <- summarise_draws_custom(D)  # debe retornar al menos columnas: variable, mean, sd, etc.

    # Tabla principal: excluir 'sigma'
    coef_idx   <- stats$variable != "sigma"
    coef_stats <- stats[coef_idx, , drop = FALSE]

    # IC alineadas por nombre en el orden de coef_stats
    vars <- coef_stats$variable
    ic   <- ic_by_name(D, vars, probs)
    coef_stats$lower_ci <- ic$lower
    coef_stats$upper_ci <- ic$upper

    list(
      tau          = tau,
      coef_summary = coef_stats,
      full_summary = stats,
      n_draws      = nrow(D),
      meta         = meta,
      probs        = probs,
      digits       = digits
    )
  }

  # --- Soporte uno o varios cuantiles ---
  if (is.list(object$draws)) {
    per_tau <- Map(make_block, object$draws, object$quantile)
    names(per_tau) <- paste0("tau=", formatC(object$quantile, format = "f", digits = 3))
  } else {
    per_tau <- list(make_block(object$draws, object$quantile))
    names(per_tau) <- paste0("tau=", formatC(object$quantile, format = "f", digits = 3))
  }

  res <- list(
    call      = object$call %||% NULL,
    method    = object$method %||% object$algorithm %||% NA_character_,
    quantiles = object$quantile,
    per_tau   = per_tau
  )
  class(res) <- "summary.bqr.svy"
  res
}

#' Impresi??n del resumen para objetos summary.bqr.svy
#' @exportS3Method print summary.bqr.svy
print.summary.bqr.svy <- function(x, ...) {
  stopifnot(inherits(x, "summary.bqr.svy"))
  cat("\nMethod: ", x$method %||% "NA", "\n", sep = "")
  cat("Quantiles: ",
      paste(formatC(x$quantiles, format = "f", digits = 3), collapse = ", "),
      "\n\n", sep = "")

  # Promedio global de acceptance rate (por si es vector)
  acc_global <- tryCatch({
    vals <- vapply(x$per_tau, function(b) {
      a <- b$meta$accept_rate
      if (length(a) == 0) NA_real_ else mean(a, na.rm = TRUE)
    }, numeric(1))
    if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
  }, error = function(e) NA_real_)

  for (nm in names(x$per_tau)) {
    blk <- x$per_tau[[nm]]
    cat("== ", nm, " ==\n", sep = "")

    # Acceptance rate: usa por-bloque si es escalar, si no cae al global
    acc_here <- blk$meta$accept_rate
    acc_here <- if (length(acc_here) <= 1L) as.numeric(acc_here) else
      suppressWarnings(mean(acc_here, na.rm = TRUE))

    if (is.finite(acc_here)) {
      cat("  Acceptance rate (avg): ", round(acc_here, 3), "\n", sep = "")
    } else if (is.finite(acc_global)) {
      cat("  Acceptance rate (avg): ", round(acc_global, 3), "\n", sep = "")
    }

    cat("  Draws: ", blk$n_draws,
        " | Warmup: ", blk$meta$warmup,
        " | Thin: ", blk$meta$thin, "\n\n", sep = "")

    # Selecci??n robusta de columnas a mostrar
    cs <- blk$coef_summary
    show_cols <- c("variable","mean","sd","rhat","ess_bulk","ess_tail","lower_ci","upper_ci")
    show_cols <- intersect(show_cols, colnames(cs))
    if (!length(show_cols)) {
      print(cs)
    } else {
      df <- cs[, show_cols, drop = FALSE]
      num_cols <- setdiff(show_cols, "variable")
      for (cl in num_cols) {
        df[[cl]] <- ifelse(is.finite(df[[cl]]), round(df[[cl]], blk$digits), df[[cl]])
      }
      print(df, row.names = FALSE)
    }
    cat("\n")
  }
  invisible(x)
}


#' Resumen para objetos bwqr_fit (un cuantil)
#'
#' @param object Objeto \code{bwqr_fit}.
#' @param probs Probabilidades para IC.
#' @param digits Decimales para impresi??n.
#' @param max_lag M??ximo rezago para autocorrelaci??n (si lo usa summarise_draws_custom).
#' @param ... Ignorado.
#' @return Objeto de clase \code{summary.bwqr_fit}.
#' @exportS3Method summary bwqr_fit
summary.bwqr_fit <- function(object, probs = c(0.025, 0.975), digits = 3, max_lag = 200, ...) {
  draws <- object$draws
  draws <- if (is.data.frame(draws)) data.matrix(draws) else as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (nrow(draws) == 0L || ncol(draws) == 0L)
    stop("'object$draws' vac??o o no num??rico.", call. = FALSE)

  stats <- summarise_draws_custom(draws, max_lag = max_lag)

  # IC alineadas por nombre en el orden de 'stats'
  vars <- stats$variable
  ic <- (function(D, vars, probs) {
    lower <- upper <- rep(NA_real_, length(vars))
    cn <- colnames(D)
    for (i in seq_along(vars)) {
      j <- match(vars[i], cn)
      if (!is.na(j)) {
        vcol <- D[, j]
        lower[i] <- stats::quantile(vcol, probs = probs[1], na.rm = TRUE)
        upper[i] <- stats::quantile(vcol, probs = probs[2], na.rm = TRUE)
      }
    }
    list(lower = lower, upper = upper)
  })(draws, vars, probs)
  stats$lower_ci <- ic$lower
  stats$upper_ci <- ic$upper

  coef_df <- stats[stats$variable != "sigma",
                   intersect(c("variable","mean","sd","rhat","ess_bulk","ess_tail","lower_ci","upper_ci"),
                             colnames(stats)), drop = FALSE]

  summary_obj <- list(
    call              = object$call %||% NULL,
    method            = object$method %||% NA_character_,
    quantile          = object$quantile %||% NA_real_,
    n_draws_total     = nrow(draws),
    n_chains          = object$n_chains %||% 1L,
    warmup            = object$warmup   %||% 0L,
    thin              = object$thin     %||% 1L,
    accept_rate       = object$accept_rate %||% NA_real_,
    runtime           = object$runtime  %||% NA_real_,
    posterior_summary = stats,
    coefficients      = coef_df,
    probs             = probs,
    digits            = digits
  )
  class(summary_obj) <- "summary.bwqr_fit"
  summary_obj
}

#' Impresi??n del resumen para objetos summary.bwqr_fit
#' @exportS3Method print summary.bwqr_fit
print.summary.bwqr_fit <- function(x, ...) {
  stopifnot(inherits(x, "summary.bwqr_fit"))
  cat("\nMethod: ", x$method %||% "NA", "\n", sep = "")
  cat("Quantile: ", formatC(x$quantile, format = "f", digits = 3), "\n", sep = "")
  cat("Draws: ", x$n_draws_total, " | Warmup: ", x$warmup, " | Thin: ", x$thin, "\n\n", sep = "")

  cs <- x$coefficients
  if (!nrow(cs)) {
    cat("(No coefficient table available)\n")
    return(invisible(x))
  }
  num_cols <- setdiff(colnames(cs), "variable")
  for (cl in num_cols) {
    cs[[cl]] <- ifelse(is.finite(cs[[cl]]), round(cs[[cl]], x$digits), cs[[cl]])
  }
  print(cs, row.names = FALSE)
  invisible(x)
}
