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

# Resumen y diagnósticos a partir de una matriz de draws
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
# SUMMARY (usar genérico de base: NO redefinir summary())
# =============================================================================

#' Resumen para objetos bqr.svy (media posterior, IC, Rhat, ESS)
#'
#' Genera un resumen con media a posterior, intervalos de credibilidad,
#' \eqn{\hat{R}} y tamaño muestral efectivo por parámetro.
#'
#' @param object Objeto \code{bqr.svy}.
#' @param probs Probabilidades para IC creíbles. Default \code{c(0.025, 0.975)}.
#' @param digits Decimales al imprimir (almacenamos sin redondear; se redondea en print).
#' @param ... Ignorado.
#' @return Objeto de clase \code{summary.bqr.svy}.
#' @exportS3Method summary bqr.svy
summary.bqr.svy <- function(object, probs = c(0.025, 0.975), digits = 3, ...) {
  stopifnot(inherits(object, "bqr.svy"))
  meta <- list(
    n_chains    = object$n_chains %||% 1L,
    warmup      = object$warmup   %||% 0L,
    thin        = object$thin     %||% 1L,
    accept_rate = object$accept_rate %||% NA_real_,
    runtime     = object$runtime  %||% NA_real_
  )

  make_block <- function(D, tau) {
    D <- as.matrix(D)
    stats <- summarise_draws_custom(D)

    # tabla principal (excluye sigma) + IC pedidas
    coef_idx <- stats$variable != "sigma"
    coef_stats <- stats[coef_idx, , drop = FALSE]
    coef_stats$lower_ci <- apply(D[, coef_idx, drop = FALSE], 2, stats::quantile, probs = probs[1], na.rm = TRUE)
    coef_stats$upper_ci <- apply(D[, coef_idx, drop = FALSE], 2, stats::quantile, probs = probs[2], na.rm = TRUE)

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

  per_tau <- if (is.list(object$draws)) {
    Map(make_block, object$draws, object$quantile)
  } else list(make_block(object$draws, object$quantile))

  res <- list(
    call      = object$call %||% NULL,
    method    = object$method %||% object$algorithm %||% NA_character_,
    quantiles = object$quantile,
    per_tau   = per_tau
  )
  class(res) <- "summary.bqr.svy"
  res
}

#' Resumen para objetos bwqr_fit (con Rhat y ESS)
#'
#' @param object Objeto \code{bwqr_fit}.
#' @param probs Probabilidades para IC.
#' @param digits Decimales para impresión.
#' @param max_lag Máximo rezago autocorrelación.
#' @param ... Ignorado.
#' @return Objeto de clase \code{summary.bwqr_fit}.
#' @exportS3Method summary bwqr_fit
summary.bwqr_fit <- function(object, probs = c(0.025, 0.975), digits = 3, max_lag = 200, ...) {
  draws <- object$draws
  if (is.data.frame(draws)) draws <- data.matrix(draws)
  draws <- as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (nrow(draws) == 0L || ncol(draws) == 0L)
    stop("'object$draws' empty or not numeric.", call. = FALSE)

  stats <- summarise_draws_custom(draws, max_lag = max_lag)
  stats$lower_ci <- apply(draws, 2, stats::quantile, probs = probs[1], na.rm = TRUE)
  stats$upper_ci <- apply(draws, 2, stats::quantile, probs = probs[2], na.rm = TRUE)

  coef_df <- stats[stats$variable != "sigma",
                   c("variable","mean","sd","lower_ci","upper_ci")]

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

#' Resumen para objetos mo.bqr.svy (EM)
#' @param object Objeto \code{mo.bqr.svy}.
#' @param digits Decimales.
#' @param ... Ignorado.
#' @return Objeto \code{summary.mo_bqr.svy} (data.frame con atributos).
#' @exportS3Method summary mo.bqr.svy
summary.mo.bqr.svy <- function(object, digits = 3, ...) {
  stopifnot(inherits(object, "mo.bqr.svy"))
  taus <- object$quantile; fit_list <- object$fit

  qsum <- lapply(seq_along(taus), function(i) {
    q  <- taus[i]; fi <- fit_list[[i]]; drs <- fi$directions
    df <- do.call(rbind, lapply(seq_along(drs), function(k) {
      dr <- drs[[k]]
      data.frame(
        quantile   = q,
        direction  = k,
        t(as.matrix(round(dr$beta, digits))),
        sigma      = round(dr$sigma, digits),
        iter       = dr$iter,
        converged  = dr$converged,
        row.names  = NULL,
        check.names = FALSE
      )
    }))
    attr(df, "conv_global")  <- all(df$converged)
    attr(df, "iter_summary") <- mean(df$iter, na.rm = TRUE)
    df
  })

  out <- do.call(rbind, qsum); rownames(out) <- NULL
  class(out) <- c("summary.mo_bqr.svy", "data.frame")
  out
}

# =============================================================================
# PRINT (usar genérico de base: NO redefinir print())
# =============================================================================
# =============================================================================
# PRINT (usar genérico base; definir métodos S3 minimalistas)
# =============================================================================

#' @exportS3Method print bqr.svy
print.bqr.svy <- function(x, digits = 3, ...) {
  stopifnot(inherits(x, "bqr.svy"))

  # Preferir beta; si no existe, calcular medias de los draws (excluyendo "sigma")
  if (!is.null(x$beta)) {
    beta <- x$beta
  } else {
    D <- x$draws
    if (is.list(D)) {
      beta <- sapply(D, function(m) {
        m <- as.matrix(m)
        keep <- setdiff(colnames(m), "sigma")
        colMeans(m[, keep, drop = FALSE])
      })
      if (is.null(dim(beta))) beta <- matrix(beta, ncol = 1)
      # Fijar nombres si es posible
      base_cols <- colnames(as.matrix(D[[1]]))
      rn <- setdiff(base_cols, "sigma")
      if (!is.null(rn)) rownames(beta) <- rn
      colnames(beta) <- paste0("tau=", formatC(x$quantile, format = "f", digits = 3))
    } else {
      D <- as.matrix(D)
      keep <- setdiff(colnames(D), "sigma")
      beta <- colMeans(D[, keep, drop = FALSE])
      names(beta) <- keep
    }
  }

  cat("\nCoeficientes (medias posteriores)\n")
  if (is.matrix(beta)) {
    print(round(beta, digits))
  } else {
    out <- data.frame(Estimate = round(beta, digits),
                      row.names = names(beta), check.names = FALSE)
    print(out)
  }
  invisible(x)
}

# Mantener la misma salida minimalista para clases alias
#' @exportS3Method print bwqr_fit
print.bwqr_fit <- function(x, digits = 3, ...) {
  print.bqr.svy(x, digits = digits, ...)
}

#' @exportS3Method print bwqr_fit_multi
print.bwqr_fit_multi <- function(x, digits = 3, ...) {
  print.bqr.svy(x, digits = digits, ...)
}

# (Opcional) extractor conveniente
#' @exportS3Method coef bqr.svy
coef.bqr.svy <- function(object, ...) object$beta


#' @exportS3Method print mo.bqr.svy
print.mo.bqr.svy <- function(x, ...) {
  cat("\nMultiple-Output Bayesian Quantile Regression (EM Algorithm)\n")
  cat(rep("=", 60), "\n", sep = "")

  if (!is.null(x$call)) {
    cat("Call: "); print(x$call)
  }

  cat("Quantiles estimated : ", paste(sprintf("τ=%.3f", x$quantile), collapse = ", "), "\n")
  cat("EM Algorithm        : Multiple-output approach\n")
  cat("Response dimension  : ", x$response_dim, "\n")
  cat("Number of directions: ", x$n_dir, "\n")

  for (qi in seq_along(x$quantile)) {
    cat(sprintf("\n%s τ = %.3f %s\n", rep("-", 20), x$quantile[qi], rep("-", 20)), sep = "")
    dir_results <- x$fit[[qi]]$directions

    for (k in seq_along(dir_results)) {
      dr <- dir_results[[k]]
      cat(sprintf("\nDirection %d:\n", k))

      # Format coefficients nicely
      if (is.numeric(dr$beta) && length(dr$beta) > 0) {
        coef_df <- data.frame(
          Coefficient = if(!is.null(names(dr$beta))) names(dr$beta) else paste0("β", seq_along(dr$beta)),
          Estimate = round(dr$beta, 4),
          stringsAsFactors = FALSE
        )
        print(coef_df, row.names = FALSE)
      }

      # Convergence info
      status_symbol <- if(dr$converged) "✓" else "✗"
      cat(sprintf("  %s Converged: %s | Sigma: %.4f | Iterations: %d\n",
                  status_symbol, dr$converged, dr$sigma, dr$iter))
    }
  }

  cat("\nUse summary() for detailed convergence diagnostics\n")
  invisible(x)
}

#' @exportS3Method print summary.bqr.svy
print.summary.bqr.svy <- function(x, ...) {
  # Header
  cat("\nBayesian Weighted Quantile Regression - MCMC Summary\n")
  cat(rep("=", 55), "\n", sep = "")

  # Model information
  qs <- x$quantiles
  method <- x$method %||% "<unknown>"

  if (length(qs) > 1L) {
    cat("Quantiles estimated: ", paste(sprintf("τ=%.3f", qs), collapse = ", "), "\n")
  } else {
    cat("Quantile estimated : τ=", sprintf("%.3f", qs), "\n")
  }
  cat("MCMC method        : ", method, "\n")

  b1 <- x$per_tau[[1]]
  cat("Credible intervals : ", sprintf("[%.1f%%, %.1f%%]\n", 100*b1$probs[1], 100*b1$probs[2]))
  cat("Burn-in draws      : ", b1$meta$warmup, "\n")
  cat("Retained draws     : ", b1$n_draws, "\n")

  if (!is.na(b1$meta$accept_rate)) {
    cat("Acceptance rate    : ", sprintf("%.3f", b1$meta$accept_rate), "\n")
  }

  # Coefficient tables for each quantile
  for (blk in x$per_tau) {
    if (length(qs) > 1L) {
      cat(sprintf("\n%s τ = %.3f %s\n", rep("-", 20), blk$tau, rep("-", 20)), sep = "")
    } else {
      cat("\nPosterior Summary:\n")
      cat(rep("-", 35), "\n", sep = "")
    }

    cs <- blk$coef_summary
    fnum <- function(v, d = blk$digits) ifelse(is.na(v), "---", sprintf(paste0("%.", d, "f"), as.numeric(v)))

    tab <- data.frame(
      Parameter        = cs$variable,
      `Posterior Mean` = fnum(cs$mean),
      `Lower CI`       = fnum(cs$lower_ci),
      `Upper CI`       = fnum(cs$upper_ci),
      `R-hat`          = ifelse(is.na(cs$rhat),     "---", sprintf("%.3f", cs$rhat)),
      `ESS`            = ifelse(is.na(cs$ess_bulk), "---", sprintf("%.0f", cs$ess_bulk)),
      check.names = FALSE
    )
    print(tab, row.names = FALSE, right = TRUE)

    # Diagnostic warnings
    prob_rhat <- cs$variable[!is.na(cs$rhat)     & cs$rhat     > 1.1]
    prob_ess  <- cs$variable[!is.na(cs$ess_bulk) & cs$ess_bulk < 0.1 * blk$n_draws]

    if (length(prob_rhat) || length(prob_ess)) {
      cat("\nDiagnostic Warnings:\n")
      if (length(prob_rhat)) {
        cat(" ⚠ High R-hat (>1.1): ", paste(prob_rhat, collapse = ", "), "\n")
      }
      if (length(prob_ess)) {
        cat(" ⚠ Low ESS (<10%):    ", paste(prob_ess, collapse = ", "), "\n")
      }
    } else {
      cat("\n✓ All diagnostics look good!\n")
    }
  }

  cat("\nDiagnostic Notes:\n")
  cat(rep("-", 20), "\n", sep = "")
  cat(" • R-hat ≈ 1.0 indicates good chain mixing\n")
  cat(" • ESS measures effective sample size (higher is better)\n")
  cat(" • Use posterior package for detailed chain diagnostics\n")
  cat(" • 'sigma' parameter omitted from coefficient table\n")

  invisible(x)
}

#' @exportS3Method print summary.bwqr_fit
print.summary.bwqr_fit <- function(x, ...) {
  cat("\nBayesian Weighted Quantile Regression - MCMC Summary\n")
  cat(rep("=", 55), "\n", sep = "")

  # Model information
  cat("Model Information:\n")
  cat("  Method             :", x$method, "\n")
  cat("  Quantile (τ)       :", sprintf("%.3f", x$quantile), "\n")

  # Call information
  if (!is.null(x$call)) {
    cat("  Call               :"); print(x$call)
  }

  # MCMC details
  n_ch <- if (is.null(x$n_chains) || is.na(x$n_chains)) 1L else as.integer(x$n_chains)
  per_chain <- if (x$n_draws_total %% n_ch == 0L) x$n_draws_total / n_ch else NA

  cat("\nMCMC Configuration:\n")
  cat("  Chains             :", n_ch, "\n")
  cat("  Post-warmup draws  :", if (is.na(per_chain)) x$n_draws_total else paste0(per_chain, " per chain, ", x$n_draws_total, " total"), "\n")
  cat("  Warmup draws       :", x$warmup %||% 0L, "\n")
  cat("  Thinning interval  :", x$thin   %||% 1L, "\n")

  if (!is.na(x$accept_rate)) {
    cat("  Acceptance rate    :", sprintf("%.3f", x$accept_rate), "\n")
  }
  if (!is.na(x$runtime)) {
    cat("  Runtime            :", sprintf("%.2f", x$runtime), "seconds\n")
  }

  # Posterior summary table
  cat("\nPosterior Summary:\n")
  cat(rep("-", 80), "\n", sep = "")

  ps <- as.data.frame(x$posterior_summary, stringsAsFactors = FALSE)
  need <- c("variable","mean","sd","lower_ci","upper_ci","rhat","ess_bulk","ess_tail")
  for (m in setdiff(need, names(ps))) ps[[m]] <- NA_real_

  prob_lower <- sprintf("%.1f%%", x$probs[1] * 100)
  prob_upper <- sprintf("%.1f%%", x$probs[2] * 100)
  fmt <- function(v, d) ifelse(is.na(v), "---", sprintf(paste0("%.", d, "f"), as.numeric(v)))

  # Split between coefficients and sigma
  coef_idx <- ps$variable != "sigma"
  sigma_idx <- ps$variable == "sigma"

  # Coefficients table
  if (any(coef_idx)) {
    ps_coef <- ps[coef_idx, , drop = FALSE]

    cat("Coefficients:\n")
    disp_coef <- data.frame(
      Parameter = ps_coef$variable,
      Mean      = fmt(ps_coef$mean, x$digits),
      SD        = fmt(ps_coef$sd, x$digits),
      CI_Lower  = fmt(ps_coef$lower_ci, x$digits),
      CI_Upper  = fmt(ps_coef$upper_ci, x$digits),
      Rhat      = ifelse(is.na(ps_coef$rhat), "---", sprintf("%.3f", ps_coef$rhat)),
      ESS_bulk  = ifelse(is.na(ps_coef$ess_bulk), "---", sprintf("%.0f", ps_coef$ess_bulk)),
      ESS_tail  = ifelse(is.na(ps_coef$ess_tail), "---", sprintf("%.0f", ps_coef$ess_tail)),
      check.names = FALSE
    )
    names(disp_coef)[4:5] <- c(paste0(prob_lower, " CI"), paste0(prob_upper, " CI"))
    print(disp_coef, row.names = FALSE, right = FALSE)
  }

  # Scale parameter table
  if (any(sigma_idx)) {
    ps_sigma <- ps[sigma_idx, , drop = FALSE]

    cat("\nScale Parameter:\n")
    disp_sigma <- data.frame(
      Parameter = "σ (sigma)",
      Mean      = fmt(ps_sigma$mean, x$digits),
      SD        = fmt(ps_sigma$sd, x$digits),
      CI_Lower  = fmt(ps_sigma$lower_ci, x$digits),
      CI_Upper  = fmt(ps_sigma$upper_ci, x$digits),
      Rhat      = ifelse(is.na(ps_sigma$rhat), "---", sprintf("%.3f", ps_sigma$rhat)),
      ESS_bulk  = ifelse(is.na(ps_sigma$ess_bulk), "---", sprintf("%.0f", ps_sigma$ess_bulk)),
      ESS_tail  = ifelse(is.na(ps_sigma$ess_tail), "---", sprintf("%.0f", ps_sigma$ess_tail)),
      check.names = FALSE
    )
    names(disp_sigma)[4:5] <- c(paste0(prob_lower, " CI"), paste0(prob_upper, " CI"))
    print(disp_sigma, row.names = FALSE, right = FALSE)
  }

  # Convergence diagnostics
  cat("\nConvergence Diagnostics:\n")
  cat(rep("-", 30), "\n", sep = "")

  # Check for problematic parameters
  prob_rhat <- ps$variable[!is.na(ps$rhat) & ps$rhat > 1.1]
  prob_ess_bulk <- ps$variable[!is.na(ps$ess_bulk) & ps$ess_bulk < 0.1 * x$n_draws_total]
  prob_ess_tail <- ps$variable[!is.na(ps$ess_tail) & ps$ess_tail < 0.1 * x$n_draws_total]

  if (length(prob_rhat) || length(prob_ess_bulk) || length(prob_ess_tail)) {
    if (length(prob_rhat)) {
      cat(" ⚠ High R-hat (>1.1)  :", paste(prob_rhat, collapse = ", "), "\n")
    }
    if (length(prob_ess_bulk)) {
      cat(" ⚠ Low ESS bulk (<10%):", paste(prob_ess_bulk, collapse = ", "), "\n")
    }
    if (length(prob_ess_tail)) {
      cat(" ⚠ Low ESS tail (<10%):", paste(prob_ess_tail, collapse = ", "), "\n")
    }

    cat("\n", rep("!", 60), "\n", sep = "")
    cat("CONVERGENCE WARNING: Some parameters show poor convergence.\n")
    cat("Consider:\n")
    cat("  - Running more iterations\n")
    cat("  - Checking model specification\n")
    cat("  - Using different priors or starting values\n")
    cat(rep("!", 60), "\n", sep = "")
  } else {
    cat(" ✓ All parameters show good convergence (R-hat ≈ 1.0)\n")
    cat(" ✓ Effective sample sizes are adequate (ESS > 10%)\n")
  }

  cat("\nDiagnostic Notes:\n")
  cat(rep("-", 20), "\n", sep = "")
  cat(" • R-hat measures chain mixing (should be ≈ 1.0)\n")
  cat(" • ESS_bulk: effective sample size for central estimates\n")
  cat(" • ESS_tail: effective sample size for tail quantiles\n")
  cat(" • Use traceplots to visualize chain behavior\n")

  invisible(x)
}

#' @exportS3Method print summary.mo_bqr.svy
print.summary.mo_bqr.svy <- function(x, ...) {
  cat("\nMultiple-Output Bayesian Quantile Regression Summary\n")
  cat(rep("=", 60), "\n", sep = "")

  if (!is.data.frame(x) || !"quantile" %in% names(x)) {
    cat("(empty summary)\n");
    return(invisible(x))
  }

  taus <- unique(x$quantile)
  cat("Quantiles estimated :", paste(sprintf("τ=%.3f", taus), collapse = ", "), "\n")
  cat("EM Algorithm        : Multiple-output approach\n")

  # Overall convergence assessment
  all_converged <- TRUE
  for (q in taus) {
    df_q <- x[x$quantile == q, , drop = FALSE]
    if ("converged" %in% names(df_q)) {
      if (!all(df_q$converged, na.rm = TRUE)) {
        all_converged <- FALSE
        break
      }
    }
  }

  cat("Overall convergence :", ifelse(all_converged, "✓ CONVERGED", "⚠ NOT CONVERGED"), "\n")

  for (q in taus) {
    cat(sprintf("\n%s τ = %.3f %s\n", rep("=", 20), q, rep("=", 20)), sep = "")

    df_q <- x[x$quantile == q, , drop = FALSE]
    has_conv <- "converged" %in% names(df_q)
    has_iter <- "iter" %in% names(df_q)

    # Quantile-level convergence
    conv_global <- if (has_conv) all(df_q$converged, na.rm = TRUE) else NA
    iter_summary <- if (has_iter) mean(df_q$iter, na.rm = TRUE) else NA_real_

    status_symbol <- if (isTRUE(conv_global)) "✓" else if (identical(conv_global, FALSE)) "✗" else "?"
    conv_txt <- if (isTRUE(conv_global)) "All directions converged" else if (identical(conv_global, FALSE)) "Some directions failed" else "Unknown"

    cat(sprintf("Status: %s %s\n", status_symbol, conv_txt))
    if (is.finite(iter_summary)) {
      cat(sprintf("Average iterations: %.1f\n", iter_summary))
    }

    cat("\nPer-direction Details:\n")
    cat(rep("-", 40), "\n", sep = "")

    # Display results table with better formatting
    cols <- c("direction", setdiff(names(df_q), c("quantile", "direction")))
    df_display <- df_q[, cols, drop = FALSE]

    # Format numeric columns
    for (col in names(df_display)) {
      if (is.numeric(df_display[[col]]) && !col %in% c("direction", "converged", "iter")) {
        df_display[[col]] <- round(df_display[[col]], 4)
      }
    }

    print(df_display, row.names = FALSE)

    # Warnings for non-converged directions
    if (has_conv && !isTRUE(conv_global)) {
      failed_dirs <- df_q[!df_q$converged, "direction", drop = TRUE]
      if (length(failed_dirs) > 0) {
        cat(sprintf("\n⚠ Warning: Direction(s) %s did not converge\n",
                    paste(failed_dirs, collapse = ", ")))
      }
    }
    cat("\n")
  }

  if (!all_converged) {
    cat(rep("!", 50), "\n", sep = "")
    cat("⚠ CONVERGENCE WARNING:\n")
    cat("Some quantiles/directions did not converge.\n")
    cat("Consider:\n")
    cat("  - Increasing maximum iterations\n")
    cat("  - Checking data quality and model specification\n")
    cat("  - Trying different starting values or tolerances\n")
    cat(rep("!", 50), "\n", sep = "")
  }

  cat("Note: Use plot() to visualize quantile estimates\n")
  invisible(x)
}

# =============================================================================
# Convergence check (genérico propio)
# =============================================================================

#' Check convergence diagnostics for tauBayesW model objects
#'
#' @param object \code{bqr.svy}, \code{mo.bqr.svy}, \code{bwqr_fit} o matriz/data.frame de draws.
#' @param rhat_threshold Umbral R-hat.
#' @param ess_ratio_threshold Umbral ESS relativo a draws.
#' @param verbose Mostrar resumen.
#' @param ... Ignorado.
#' @export
convergence_check <- function(object,
                              rhat_threshold = 1.1,
                              ess_ratio_threshold = 0.1,
                              verbose = TRUE,
                              ...) {
  UseMethod("convergence_check")
}

#' @exportS3Method convergence_check bqr.svy
convergence_check.bqr.svy <- function(object,
                                      rhat_threshold = 1.1,
                                      ess_ratio_threshold = 0.1,
                                      verbose = TRUE) {
  stopifnot(inherits(object, "bqr.svy"))
  diag_one <- function(D) {
    D <- as.matrix(D); n <- nrow(D)
    st <- summarise_draws_custom(D)
    ess_ratio <- st$ess_bulk / n
    list(
      rhat = stats::setNames(st$rhat, st$variable),
      neff = stats::setNames(st$ess_bulk, st$variable),
      ess_ratio = stats::setNames(ess_ratio, st$variable),
      not_converged = st$variable[!is.na(st$rhat) & st$rhat > rhat_threshold],
      low_ess = st$variable[ess_ratio < ess_ratio_threshold]
    )
  }
  if (is.list(object$draws)) {
    taus <- object$quantile
    out <- setNames(vector("list", length(taus)),
                    paste0("tau=", formatC(taus, format = "f", digits = 3)))
    for (i in seq_along(taus)) {
      res <- diag_one(object$draws[[i]])
      if (isTRUE(verbose)) {
        cat("=== Convergence (bqr.svy) tau=", formatC(taus[i], format = "f", digits = 3), " ===\n", sep = "")
        if (length(res$not_converged)) cat("R-hat >", rhat_threshold, ": ", paste(res$not_converged, collapse = ", "), "\n", sep = "") else cat("All parameters meet R-hat threshold.\n")
        if (length(res$low_ess))      cat("ESS ratio <", ess_ratio_threshold, ": ", paste(res$low_ess, collapse = ", "), "\n", sep = "") else cat("All parameters meet ESS ratio threshold.\n")
      }
      out[[i]] <- res
    }
    return(out)
  } else {
    res <- diag_one(object$draws)
    if (isTRUE(verbose)) {
      cat("=== Convergence (bqr.svy) ===\n")
      if (length(res$not_converged)) cat("R-hat >", rhat_threshold, ": ", paste(res$not_converged, collapse = ", "), "\n", sep = "") else cat("All parameters meet R-hat threshold.\n")
      if (length(res$low_ess))      cat("ESS ratio <", ess_ratio_threshold, ": ", paste(res$low_ess, collapse = ", "), "\n", sep = "") else cat("All parameters meet ESS ratio threshold.\n")
    }
    return(res)
  }
}

#' @exportS3Method convergence_check mo.bqr.svy
convergence_check.mo.bqr.svy <- function(object,
                                         rhat_threshold = 1.1,
                                         ess_ratio_threshold = 0.1,
                                         verbose = TRUE,
                                         ...) {
  if (is.null(object$fit) || length(object$fit) == 0)
    stop("No fit results found in object.")
  iter_counts <- sapply(object$fit, function(f) f$iter)
  converged_flags <- sapply(object$fit, function(f) isTRUE(f$converged))
  if (verbose) {
    cat("=== Convergence diagnostics (mo.bqr.svy, EM) ===\n")
    for (i in seq_along(object$quantile)) {
      cat(sprintf("Quantile %.3f: iterations = %d, converged = %s\n",
                  object$quantile[i], iter_counts[i], converged_flags[i]))
    }
  }
  list(iterations = iter_counts, converged = converged_flags)
}

#' @exportS3Method convergence_check default
convergence_check.default <- function(object,
                                      rhat_threshold = 1.01,
                                      ess_ratio_threshold = 0.10,
                                      ...) {
  if (is.data.frame(object)) object <- data.matrix(object)
  if (!is.matrix(object) || !is.numeric(object))
    stop("Default method for convergence_check expects a numeric matrix or data.frame.")
  stats <- summarise_draws_custom(object)
  n_samples <- nrow(object)
  converged_flags <- !(stats$rhat > rhat_threshold |
                         (stats$ess_bulk / n_samples) < ess_ratio_threshold)
  list(rhat = stats$rhat, neff = stats$ess_bulk, converged = converged_flags)
}
