# =============================================================================
# Helpers
# =============================================================================

# Safe scalar "or" operator:
# - Does not attempt is.na() on 'language' (calls) objects or non-scalar vectors.
# - If 'a' is NULL -> returns 'b'
# - If 'a' is atomic and scalar and NA -> returns 'b'
# - In any other case -> returns 'a'
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.atomic(a) && length(a) == 1L) {
    return(if (is.na(a)) b else a)
  }
  a
}

#' Autocovariance calculation for Vehtari method
#' @keywords internal
.autocovariance_vehtari <- function(x, max_lag = NULL) {
  n <- length(x)
  if (is.null(max_lag)) max_lag <- n - 1
  max_lag <- min(max_lag, n - 1)
  x_centered <- x - mean(x)
  autocov <- numeric(max_lag + 1)
  for (lag in 0:max_lag) {
    if (lag == 0) {
      autocov[lag + 1] <- mean(x_centered^2)
    } else {
      autocov[lag + 1] <- mean(x_centered[1:(n - lag)] * x_centered[(lag + 1):n])
    }
  }
  autocov
}

#' Integrated autocorrelation time for Vehtari method
#' @keywords internal
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

#' @keywords internal
.ess_vehtari_single <- function(x, split_chains = TRUE) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0) return(NA_real_)
  if (split_chains) {
    half <- floor(n / 2)
    x_matrix <- matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  } else {
    x_matrix <- matrix(x, ncol = 1)
  }
  x_ranked <- apply(x_matrix, 2, function(chain) {
    (rank(chain, ties.method = "average") - 0.5) / length(chain)
  })
  x_normal <- qnorm(x_ranked)
  autocov_chains <- apply(x_normal, 2, function(chain) {
    .autocovariance_vehtari(chain, max_lag = min(length(chain) - 1, 200))
  })
  n_iter <- nrow(x_normal)
  n_chains <- ncol(x_normal)
  max_lag <- min(nrow(autocov_chains), 200)
  autocov_mean <- if (n_chains > 1) {
    rowMeans(autocov_chains[1:max_lag, , drop = FALSE])
  } else {
    autocov_chains[1:max_lag]
  }
  tau_int <- .integrated_time_vehtari(autocov_mean)
  ess <- n_chains * n_iter / (2 * tau_int + 1)
  max(1, ess)
}

#' @keywords internal
.ess_tail_vehtari_single <- function(x, prob = c(0.05, 0.95), split_chains = TRUE) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0) return(NA_real_)
  if (split_chains) {
    half <- floor(n / 2)
    x_matrix <- matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  } else {
    x_matrix <- matrix(x, ncol = 1)
  }
  n_iter <- nrow(x_matrix)
  n_chains <- ncol(x_matrix)
  pooled_data <- as.vector(x_matrix)
  q_lower <- quantile(pooled_data, prob[1])
  q_upper <- quantile(pooled_data, prob[2])

  x_lower_tail <- apply(x_matrix, 2, function(chain) as.numeric(chain <= q_lower))
  x_upper_tail <- apply(x_matrix, 2, function(chain) as.numeric(chain >= q_upper))

  ess_lower <- tryCatch({
    x_ranked_lower <- apply(x_lower_tail, 2, function(chain) {
      (rank(chain, ties.method = "average") - 0.5) / length(chain)
    })
    x_normal_lower <- qnorm(pmax(pmin(x_ranked_lower, 1 - 1e-15), 1e-15))
    autocov_chains_lower <- apply(x_normal_lower, 2, function(chain) {
      .autocovariance_vehtari(chain, max_lag = min(length(chain) - 1, 200))
    })
    max_lag <- min(nrow(autocov_chains_lower), 200)
    autocov_mean_lower <- if (n_chains > 1) {
      rowMeans(autocov_chains_lower[1:max_lag, , drop = FALSE])
    } else {
      autocov_chains_lower[1:max_lag]
    }
    tau_int_lower <- .integrated_time_vehtari(autocov_mean_lower)
    max(1, n_chains * n_iter / (2 * tau_int_lower + 1))
  }, error = function(e) NA_real_)

  ess_upper <- tryCatch({
    x_ranked_upper <- apply(x_upper_tail, 2, function(chain) {
      (rank(chain, ties.method = "average") - 0.5) / length(chain)
    })
    x_normal_upper <- qnorm(pmax(pmin(x_ranked_upper, 1 - 1e-15), 1e-15))
    autocov_chains_upper <- apply(x_normal_upper, 2, function(chain) {
      .autocovariance_vehtari(chain, max_lag = min(length(chain) - 1, 200))
    })
    max_lag <- min(nrow(autocov_chains_upper), 200)
    autocov_mean_upper <- if (n_chains > 1) {
      rowMeans(autocov_chains_upper[1:max_lag, , drop = FALSE])
    } else {
      autocov_chains_upper[1:max_lag]
    }
    tau_int_upper <- .integrated_time_vehtari(autocov_mean_upper)
    max(1, n_chains * n_iter / (2 * tau_int_upper + 1))
  }, error = function(e) NA_real_)

  if (is.na(ess_lower) && is.na(ess_upper)) return(NA_real_)
  if (is.na(ess_lower)) return(ess_upper)
  if (is.na(ess_upper)) return(ess_lower)
  min(ess_lower, ess_upper)
}

#' Rank-normalized R-hat (Vehtari et al., 2021)
#' @keywords internal
.rhat_rank <- function(x, split_chains = TRUE) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 4 || all(is.na(x))) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0) return(NA_real_)
  if (!split_chains) return(NA_real_)
  half <- floor(n / 2)
  x_matrix <- matrix(c(x[1:half], x[(n - half + 1):n]), ncol = 2)
  x_ranked <- apply(x_matrix, 2, function(chain) {
    (rank(chain, ties.method = "average") - 0.5) / length(chain)
  })
  x_normal <- qnorm(x_ranked)
  chain_means <- colMeans(x_normal)
  B <- nrow(x_normal) * var(chain_means)
  W <- mean(apply(x_normal, 2, var))
  var_plus <- ((nrow(x_normal) - 1) / nrow(x_normal)) * W + B / nrow(x_normal)
  sqrt(var_plus / W)
}

#' Convergence check helper (silent)
#' @keywords internal
.check_convergence_and_warn <- function(stats, n_samples, rhat_threshold = 1.01, ess_ratio_threshold = 0.30) {
  if (is.null(stats) || nrow(stats) == 0) return(invisible(NULL))
  rhat_issues <- stats$rhat > rhat_threshold | is.na(stats$rhat)
  ess_threshold <- n_samples * ess_ratio_threshold
  ess_issues <- stats$ess_bulk < ess_threshold | is.na(stats$ess_bulk)
  !(any(rhat_issues) || any(ess_issues))
}

#' Summaries and diagnostics for MCMC draws (Vehtari et al., 2021)
#' @keywords internal
summarise_draws_custom <- function(draws, max_lag = 200) {
  if (!is.matrix(draws)) draws <- as.matrix(draws)
  var_names <- colnames(draws)
  if (is.null(var_names)) var_names <- paste0("V", seq_len(ncol(draws)))
  results <- data.frame(
    variable = var_names,
    mean  = round(apply(draws, 2, mean,   na.rm = TRUE), 3),
    median= round(apply(draws, 2, median, na.rm = TRUE), 3),
    sd    = round(apply(draws, 2, sd,     na.rm = TRUE), 3),
    mad   = round(apply(draws, 2, mad,    na.rm = TRUE), 3),
    q2.5    = round(apply(draws, 2, quantile, probs = 0.025, na.rm = TRUE), 3),
    q97.5   = round(apply(draws, 2, quantile, probs = 0.975, na.rm = TRUE), 3),
    stringsAsFactors = FALSE
  )
  results$rhat <- round(apply(draws, 2, function(x) .rhat_rank(x, split_chains = TRUE)), 3)
  results$ess_bulk <- round(apply(draws, 2, function(x) .ess_vehtari_single(x, split_chains = TRUE)), 0)
  results$ess_tail <- round(apply(draws, 2, function(x) .ess_tail_vehtari_single(x, prob = c(0.05, 0.95), split_chains = TRUE)), 0)
  results
}

#' Print method for bwqr_fit objects
#'
#' @param x An object of class \code{"bwqr_fit"}.
#' @param digits Integer, number of significant digits to print.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @export
print.bwqr_fit <- function(x, digits = 3, ...) {
  cat("\nBayesian Quantile Regression fit (class 'bwqr_fit')\n")
  cat("Method    :", x$method, "\n")
  cat("Quantile  :", x$quantile, "\n")
  cat("Draws     :", nrow(x$draws), "\n")
  if (!is.null(x$accept_rate) && !is.na(x$accept_rate)) {
    cat("Accept rate:", sprintf("%.3f", x$accept_rate), "\n")
  }
  cat("\nPosterior summary:\n")
  summary_stats <- summarise_draws_custom(x$draws)
  print(summary_stats[, c("variable", "mean", "sd", "q2.5", "q97.5")],
        row.names = FALSE, digits = digits)
  invisible(x)
}

#' Print method for bqr.svy objects
#'
#' @param x An object of class \code{"bqr.svy"}.
#' @param digits Integer, number of significant digits to print.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @export
print.bqr.svy <- function(x, digits = 3, ...) {
  cat("\nBayesian Quantile Regression (class 'bqr.svy')\n")
  cat("Method    :", x$method, "\n")

  # --- Caso multi-tau: draws es lista ---
  if (is.list(x$draws)) {
    taus <- x$quantile
    nm   <- names(x$draws)
    if (is.null(nm)) nm <- paste0("tau=", formatC(taus, format = "f", digits = 3))
    cat("Quantiles :", paste(taus, collapse = ", "), "\n")
    cat("Draws     :\n")
    for (i in seq_along(x$draws)) {
      n_i <- tryCatch(nrow(x$draws[[i]]), error = function(e) NA_integer_)
      cat("  ", nm[i], ":", n_i, "draws")
      if (!is.null(x$accept_rate) && length(x$accept_rate) >= i && !is.na(x$accept_rate[i])) {
        cat(", accept rate:", sprintf("%.3f", x$accept_rate[i]))
      }
      cat("\n")
    }
    if (!is.null(x$beta) && is.matrix(x$beta)) {
      cat("\nPosterior means (by tau):\n")
      print(round(x$beta, digits))
    } else {
      cat("\n(Use summary() for detailed output.)\n")
    }
    return(invisible(x))
  }

  # --- Caso single-tau (comportamiento previo) ---
  cat("Quantile  :", x$quantile, "\n")
  cat("Draws     :", nrow(x$draws), "\n")
  if (!is.null(x$accept_rate) && !is.na(x$accept_rate)) {
    cat("Accept rate:", sprintf("%.3f", x$accept_rate), "\n")
  }
  cat("\nPosterior means:\n")
  means <- round(apply(x$draws, 2, mean, na.rm = TRUE), digits)
  print(means)
  cat("\nUse summary() for detailed posterior statistics and diagnostics.\n")
  invisible(x)
}

#' Summary method for bwqr_fit objects
#'
#' @param object An object of class \code{"bwqr_fit"}.
#' @param probs Numeric vector of probabilities for summary statistics.
#' @param digits Integer, number of significant digits to display.
#' @param max_lag Integer, maximum lag for autocorrelation diagnostics.
#' @param ... Additional arguments passed to other methods.
#' @export
summary.bwqr_fit <- function(object,
                             probs   = c(0.025, 0.975),
                             digits  = 3,
                             max_lag = 200,
                             ...) {
  draws <- object$draws
  if (is.data.frame(draws)) draws <- data.matrix(draws)
  draws <- as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (nrow(draws) == 0L || ncol(draws) == 0L)
    stop("'object$draws' empty or not numeric.", call. = FALSE)

  # Base stats + diagnostics (usa tu summarise_draws_custom existente)
  stats <- summarise_draws_custom(draws, max_lag = max_lag)

  # Credible intervals
  stats$lower_ci <- round(apply(draws, 2, quantile, probs = probs[1], na.rm = TRUE), digits)
  stats$upper_ci <- round(apply(draws, 2, quantile, probs = probs[2], na.rm = TRUE), digits)

  # Back-compat: tabla de coeficientes sin sigma
  coef_df <- stats[stats$variable != "sigma",
                   c("variable","mean","sd","lower_ci","upper_ci")]
  names(coef_df) <- c("Variable","Mean","SD","Lower","Upper")

  summary_obj <- list(
    call              = object$call %||% NULL,
    method            = object$method %||% NA_character_,
    quantile          = object$quantile %||% NA_real_,
    n_draws           = nrow(draws),
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

  class(summary_obj) <- c("summary.bwqr_fit", "summary.bqr.svy")
  summary_obj
}

#' Print method for summary.bwqr_fit objects
#'
#' @param x An object of class \code{"summary.bwqr_fit"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @export
print.summary.bwqr_fit <- function(x, ...) {
  cat("\nBayesian Quantile Regression Summary\n")
  cat("====================================\n\n")
  cat("Model Information:\n")
  cat("  Method           :", x$method, "\n")
  cat("  Quantile (tau)   :", x$quantile, "\n")

  n_chains_val <- if (is.null(x$n_chains) || is.na(x$n_chains)) 1L else as.integer(x$n_chains)
  warmup_val   <- if (is.null(x$warmup)   || is.na(x$warmup))   0L else as.integer(x$warmup)
  thin_val     <- if (is.null(x$thin)     || is.na(x$thin))     1L else as.integer(x$thin)

  cat("  Chains           :", n_chains_val, "\n")
  cat("  Post-warmup draws:", x$n_draws, "per chain\n")
  cat("  Warmup           :", warmup_val, "draws\n")
  cat("  Thinning         :", thin_val, "\n")
  if (!is.null(x$accept_rate) && !is.na(x$accept_rate))
    cat("  Accept rate      :", sprintf("%.3f", x$accept_rate), "\n")
  if (!is.null(x$runtime) && !is.na(x$runtime))
    cat("  Runtime          :", sprintf("%.2f", x$runtime), "seconds\n")

  ps <- as.data.frame(x$posterior_summary, stringsAsFactors = FALSE)
  needed <- c("variable","mean","sd","lower_ci","upper_ci","rhat","ess_bulk","ess_tail")
  missing_cols <- setdiff(needed, names(ps))
  if (length(missing_cols)) for (m in missing_cols) ps[[m]] <- rep(NA_real_, nrow(ps))

  cat("\nPosterior Estimates:\n")
  prob_lower <- sprintf("%.1f%%", x$probs[1] * 100)
  prob_upper <- sprintf("%.1f%%", x$probs[2] * 100)
  fmt_num <- function(v, d) ifelse(is.na(v), "---", sprintf(paste0("%.", d, "f"), as.numeric(v)))

  display_table <- data.frame(
    Variable = ps$variable,
    Mean     = fmt_num(ps$mean,     x$digits),
    SD       = fmt_num(ps$sd,       x$digits),
    CI_Lower = fmt_num(ps$lower_ci, x$digits),
    CI_Upper = fmt_num(ps$upper_ci, x$digits),
    Rhat     = ifelse(is.na(ps$rhat),     "---", sprintf("%.3f", ps$rhat)),
    ESS_bulk = ifelse(is.na(ps$ess_bulk), "---", sprintf("%.0f", ps$ess_bulk)),
    ESS_tail = ifelse(is.na(ps$ess_tail), "---", sprintf("%.0f", ps$ess_tail)),
    check.names = FALSE
  )
  names(display_table) <- c("Variable", "Mean", "SD",
                            paste0(prob_lower, " CI"), paste0(prob_upper, " CI"),
                            "Rhat", "ESS_bulk", "ESS_tail")
  print(display_table, row.names = FALSE, right = FALSE)

  .check_convergence_and_warn(ps, x$n_draws)

  cat("Methods: Rank-normalization R-hat and ESS (Vehtari et al., 2021).\n")
  cat("Note: ESS_bulk measures center of distribution, ESS_tail measures tails.\n")
  cat("      Convergence diagnostics computed for all parameters including sigma.\n")
  invisible(x)
}



#' Summary method for bqr.svy objects
#'
#' @param object An object of class \code{"bqr.svy"}.
#' @param probs Numeric vector of probabilities for summary statistics.
#' @param digits Integer, number of significant digits to display.
#' @param ... Additional arguments passed to other methods.
#' @export
summary.bqr.svy <- function(object,
                            probs = c(0.025, 0.975),
                            digits = 3,
                            ...) {
  if (!inherits(object, "bqr.svy"))
    stop("Object must be of class 'bqr.svy'")

  make_block <- function(draws_mat, tau, meta) {
    d <- if (is.data.frame(draws_mat)) data.matrix(draws_mat) else as.matrix(draws_mat)
    storage.mode(d) <- "numeric"
    if (nrow(d) == 0L || ncol(d) == 0L)
      stop("Empty or non-numeric draws matrix for tau=", tau)

    stats <- summarise_draws_custom(d)
    stats$lower_ci <- round(apply(d, 2, quantile, probs = probs[1], na.rm = TRUE), digits)
    stats$upper_ci <- round(apply(d, 2, quantile, probs = probs[2], na.rm = TRUE), digits)

    list(
      tau               = tau,
      posterior_summary = stats,
      n_draws           = nrow(d),
      n_chains          = meta$n_chains %||% 1L,
      warmup            = meta$warmup   %||% 0L,
      thin              = meta$thin     %||% 1L,
      accept_rate       = meta$accept_rate %||% NA_real_,
      runtime           = meta$runtime  %||% NA_real_,
      probs             = probs,
      digits            = digits
    )
  }

  meta <- list(
    n_chains    = object$n_chains %||% 1L,
    warmup      = object$warmup   %||% 0L,
    thin        = object$thin     %||% 1L,
    accept_rate = object$accept_rate,
    runtime     = object$runtime
  )

  if (is.list(object$draws)) {
    taus   <- object$quantile
    dl     <- object$draws

    per_tau <- vector("list", length(dl))
    for (i in seq_along(dl)) {
      meta_i <- meta
      if (!is.null(meta$accept_rate) && length(meta$accept_rate) >= i) meta_i$accept_rate <- meta$accept_rate[i]
      if (!is.null(meta$runtime)     && length(meta$runtime)     >= i) meta_i$runtime     <- meta$runtime[i]
      per_tau[[i]] <- make_block(dl[[i]], tau = taus[i], meta = meta_i)
    }
    names(per_tau) <- paste0("tau=", formatC(taus, format="f", digits=3))

    out <- list(
      call      = object$call %||% NULL,
      method    = object$method %||% object$algorithm %||% NA_character_,
      quantiles = taus,
      per_tau   = per_tau
    )
    class(out) <- "summary.bqr.svy"
    return(out)
  }

  tau1  <- as.numeric(object$quantile %||% NA_real_)
  block <- make_block(object$draws, tau = tau1, meta = meta)

  out <- list(
    call      = object$call %||% NULL,
    method    = object$method %||% object$algorithm %||% NA_character_,
    quantiles = tau1,
    per_tau   = list(block)
  )
  names(out$per_tau) <- paste0("tau=", formatC(tau1, format="f", digits=3))
  class(out) <- "summary.bqr.svy"
  out
}




#' Print method for summary.bqr.svy objects
#'
#' @param x An object of class \code{"summary.bqr.svy"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @export
print.summary.bqr.svy <- function(x, ...) {
  cat("\nBayesian Quantile Regression Summary\n")
  cat("====================================\n\n")
  cat("Model Information:\n")
  cat("  Method           :", x$method, "\n")
  if (length(x$quantiles) > 1L) {
    cat("  Quantiles (tau)  :", paste(x$quantiles, collapse = ", "), "\n\n")
  } else {
    cat("  Quantile (tau)   :", x$quantiles, "\n\n")
  }

  fmt_num <- function(v, d) ifelse(is.na(v), "---", sprintf(paste0("%.", d, "f"), as.numeric(v)))

  # One block per tau (identical format for single/multi)
  for (i in seq_along(x$per_tau)) {
    b <- x$per_tau[[i]]
    cat(sprintf("----- Quantile tau = %.3f -----\n", b$tau))
    cat("  Chains           :", b$n_chains, "\n")
    cat("  Post-warmup draws:", b$n_draws, "per chain\n")
    cat("  Warmup           :", b$warmup, "draws\n")
    cat("  Thinning         :", b$thin, "\n")
    if (!is.null(b$accept_rate) && !is.na(b$accept_rate))
      cat("  Accept rate      :", sprintf("%.3f", b$accept_rate), "\n")
    if (!is.null(b$runtime) && !is.na(b$runtime))
      cat("  Runtime          :", sprintf("%.2f", b$runtime), "seconds\n")

    ps <- as.data.frame(b$posterior_summary, stringsAsFactors = FALSE)
    needed <- c("variable","mean","sd","lower_ci","upper_ci","rhat","ess_bulk","ess_tail")
    miss <- setdiff(needed, names(ps)); if (length(miss)) for (m in miss) ps[[m]] <- NA_real_

    cat("\n  Posterior Estimates:\n")
    prob_lower <- sprintf("%.1f%%", b$probs[1] * 100)
    prob_upper <- sprintf("%.1f%%", b$probs[2] * 100)

    display <- data.frame(
      Variable = ps$variable,
      Mean     = fmt_num(ps$mean,     b$digits),
      SD       = fmt_num(ps$sd,       b$digits),
      CI_Lower = fmt_num(ps$lower_ci, b$digits),
      CI_Upper = fmt_num(ps$upper_ci, b$digits),
      Rhat     = ifelse(is.na(ps$rhat),     "---", sprintf("%.3f", ps$rhat)),
      ESS_bulk = ifelse(is.na(ps$ess_bulk), "---", sprintf("%.0f", ps$ess_bulk)),
      ESS_tail = ifelse(is.na(ps$ess_tail), "---", sprintf("%.0f", ps$ess_tail)),
      check.names = FALSE
    )
    names(display)[which(names(display) %in% c("CI_Lower","CI_Upper"))] <-
      c(paste0(prob_lower, " CI"), paste0(prob_upper, " CI"))
    print(display, row.names = FALSE, right = FALSE)

    .check_convergence_and_warn(ps, b$n_draws)
    cat("\n")
  }
  cat("Methods: Rank-normalization R-hat and ESS (Vehtari et al., 2021).\n")
  cat("Note: ESS_bulk measures center, ESS_tail measures tails; diagnostics include sigma.\n")
  invisible(x)
}



#' Summary method for Multiple-Output Bayesian Quantile Regression
#'
#' Provides a tabular summary of fitted Bayesian quantile regression models
#' estimated with \code{mo.bqr.svy}. The output contains, for each quantile
#' and direction, the estimated coefficients, scale parameter, iteration count,
#' and convergence status. In addition, summary attributes at the quantile level
#' (average iterations, global convergence) are stored for use by the print method.
#'
#' @param object An object of class \code{"mo.bqr.svy"}, typically returned by
#'   \code{\link{mo.bqr.svy}}.
#' @param digits Integer, number of decimal places to use for rounding numeric
#'   results. Defaults to \code{3}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A \code{summary.mo_bqr.svy} object, which is a data frame containing:
#'   \itemize{
#'     \item \code{quantile}: quantile level \eqn{\tau}.
#'     \item \code{direction}: index of the direction used in the EM algorithm.
#'     \item coefficient columns: estimated regression parameters for each covariate.
#'     \item \code{sigma}: estimated scale parameter for that direction.
#'     \item \code{iter}: number of EM iterations taken for that direction.
#'     \item \code{converged}: logical indicating convergence for that direction.
#'   }
#'
#'   Additionally, each block of rows corresponding to a quantile
#'   has two attributes:
#'   \itemize{
#'     \item \code{"conv_global"}: TRUE if all directions converged, FALSE otherwise.
#'     \item \code{"iter_summary"}: average number of iterations across directions.
#'   }
#'
#' The object has class \code{c("summary.mo_bqr.svy", "data.frame")}, and its
#' default print method displays both per-quantile summaries and per-direction
#' details.
#'
#' @seealso \code{\link{mo.bqr.svy}}, \code{\link{print.summary.mo_bqr.svy}}
#' Summary method for Multiple-Output Bayesian Quantile Regression
#' (doc igual que ya tienes)
#' @method summary mo.bqr.svy
#' @export
summary.mo.bqr.svy <- function(object, digits = 3, ...) {
  if (!inherits(object, "mo.bqr.svy"))
    stop("Object must be of class 'mo.bqr.svy'")

  taus <- object$quantile
  fit_list <- object$fit

  quantile_summaries <- lapply(seq_along(taus), function(i) {
    q  <- taus[i]
    fi <- fit_list[[i]]
    dir_results <- fi$directions

    df <- do.call(rbind, lapply(seq_along(dir_results), function(k) {
      dr <- dir_results[[k]]
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

  out <- do.call(rbind, quantile_summaries)
  rownames(out) <- NULL
  class(out) <- c("summary.mo_bqr.svy", "data.frame")

  return(out)
}



#' Print method for summary.mo_bqr.svy objects
#'
#' @param x An object of class \code{"summary.mo_bqr.svy"}.
#' @param ... Additional arguments (currently unused).
#' @export
print.summary.mo_bqr.svy <- function(x, ...) {
  cat("\nMultiple-Output Bayesian Quantile Regression Summary\n")
  cat("===================================================\n\n")

  if (!is.data.frame(x) || !"quantile" %in% names(x)) {
    cat("(empty summary)\n")
    return(invisible(x))
  }

  taus <- unique(x$quantile)

  for (q in taus) {
    df_q <- x[x$quantile == q, , drop = FALSE]

    has_conv <- "converged" %in% names(df_q)
    has_iter <- "iter" %in% names(df_q)
    conv_global  <- if (has_conv) all(df_q$converged, na.rm = TRUE) else NA
    iter_summary <- if (has_iter) mean(df_q$iter, na.rm = TRUE) else NA_real_

    conv_txt <- if (isTRUE(conv_global)) "Yes" else if (identical(conv_global, FALSE)) "No" else "-"

    cat(sprintf("Quantile tau = %.3f:\n", q))
    cat("  Converged (all directions):", conv_txt, "\n")
    cat("  Avg iterations:", if (is.finite(iter_summary)) round(iter_summary, 1) else "-", "\n\n")

    cat("  Per-direction results:\n")
    df_show <- df_q
    cols <- c("direction", setdiff(names(df_show), c("quantile", "direction")))
    df_show <- df_show[, cols, drop = FALSE]
    class(df_show) <- "data.frame"
    print.data.frame(df_show, row.names = FALSE)
    cat("\n")
  }

  invisible(x)
}



#' Generic summary function for Bayesian Quantile Regression objects
#'
#' @param object An object containing model fit results.
#' @param ... Additional arguments passed to other methods.
#' @export
summary_bqr <- function(object, ...) {
  UseMethod("summary_bqr")
}

#' @export
summary_bqr.bqr.svy <- function(object, ...) summary.bqr.svy(object, ...)
#' @export
summary_bqr.mo.bqr.svy <- function(object, ...) summary.mo.bqr.svy(object, ...)
#' @export
summary_bqr.default <- function(object, ...) summary(object, ...)

#' Check convergence diagnostics for tauBayesW model objects
#'
#' This is a generic function that dispatches to specific methods depending on
#' the class of the object (e.g., \code{bqr.svy}, \code{mo.bqr.svy}).
#'
#' @param object An object of class \code{"bqr.svy"} or \code{"mo.bqr.svy"}.
#' @param rhat_threshold Numeric scalar. The threshold for the Gelman-Rubin \eqn{\hat{R}} statistic
#'   above which a parameter is considered not converged. Default is \code{1.1}.
#' @param ess_ratio_threshold Numeric scalar. The threshold for the ratio of Effective Sample Size (ESS)
#'   to total draws below which a parameter is considered to have low sampling efficiency. Default is \code{0.1}.
#' @param verbose Logical; if \code{TRUE}, prints a summary of convergence diagnostics to the console.
#'   Default is \code{TRUE}.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{rhat}}{Named numeric vector of \eqn{\hat{R}} values for each parameter (if available).}
#'   \item{\code{ess_ratio}}{Named numeric vector of ESS ratios for each parameter (if available).}
#'   \item{\code{not_converged}}{Character vector of parameter names failing \eqn{\hat{R}} threshold.}
#'   \item{\code{low_ess}}{Character vector of parameter names failing ESS ratio threshold.}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- bqr.svy(y ~ x, data = mydata, quantile = 0.5, method = "ald")
#' convergence_check(fit)
#' }
#' @export
convergence_check <- function(object,
                              rhat_threshold = 1.1,
                              ess_ratio_threshold = 0.1,
                              verbose = TRUE,
                              ...) {
  UseMethod("convergence_check")
}

#' Convergence diagnostics for `bqr.svy` objects
#'
#' Computes the rank-normalized \eqn{\hat{R}} (Gelman Rubin) and Effective Sample Size (ESS)
#' for parameters from MCMC output produced by \code{\link{bqr.svy}}.
#' Works with both single-quantile fits (one \eqn{\tau}) and multi-quantile fits.
#'
#' @inheritParams convergence_check
#'
#' @details
#' For single-quantile fits, \code{object$draws} must be a numeric matrix or data frame with
#' rows = iterations and columns = parameters.
#' For multi-quantile fits, \code{object$draws} must be a list of such matrices (one per \eqn{\tau}).
#' Diagnostics are computed via \code{\link{summarise_draws_custom}} following Vehtari et al. (2021).
#'
#' @return
#' If a \emph{single} \eqn{\tau} is present: a list with components \code{rhat}, \code{neff},
#' \code{ess_ratio}, \code{not_converged}, \code{low_ess}, and \code{converged}.
#' If \emph{multiple} \eqn{\tau} values are present: a named list where each element corresponds
#' to a \eqn{\tau} and has the same structure as above.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat <- data.frame(y = rnorm(50), x = rnorm(50))
#' fit1 <- bqr.svy(y ~ x, data = dat, quantile = 0.5, method = "ald")
#' convergence_check(fit1)
#'
#' fitk <- bqr.svy(y ~ x, data = dat, quantile = c(0.25, 0.5, 0.75), method = "ald")
#' convergence_check(fitk)
#' }
#'
#' @export
convergence_check.bqr.svy <- function(object,
                                      rhat_threshold = 1.1,
                                      ess_ratio_threshold = 0.1,
                                      verbose = TRUE,
                                      ...) {
  if (is.null(object$draws))
    stop("No MCMC draws found in object.")

  # --- Helper: diagnostics for a draws matrix ---
  .diag_one <- function(d) {
    if (is.data.frame(d)) d <- data.matrix(d)
    d <- as.matrix(d)
    storage.mode(d) <- "numeric"
    n_samples <- nrow(d)
    if (!is.finite(n_samples) || n_samples <= 1)
      stop("Not enough samples to compute convergence diagnostics.")

    stats <- summarise_draws_custom(d)  # usa R-hat rank-normalized y ESS (Vehtari)
    # Asegurar nombres
    var_names <- stats$variable
    rhat_vals <- stats$rhat;     names(rhat_vals) <- var_names
    neff_vals <- stats$ess_bulk; names(neff_vals) <- var_names

    ess_ratio <- neff_vals / n_samples
    not_conv  <- names(rhat_vals)[which(!is.na(rhat_vals) & rhat_vals > rhat_threshold)]
    low_ess   <- names(ess_ratio)[which(ess_ratio < ess_ratio_threshold)]
    conv_vec  <- !(names(rhat_vals) %in% union(not_conv, low_ess))
    names(conv_vec) <- names(rhat_vals)

    list(
      rhat          = rhat_vals,
      neff          = neff_vals,          # nombre conservado por compatibilidad
      ess_ratio     = ess_ratio,
      not_converged = not_conv,
      low_ess       = low_ess,
      converged     = conv_vec
    )
  }

  # --- Multi-tau: lista por tau ---
  if (is.list(object$draws)) {
    taus <- object$quantile
    dl   <- object$draws
    nm   <- names(dl)
    if (is.null(nm)) nm <- paste0("tau=", formatC(taus, format = "f", digits = 3))

    out_list <- vector("list", length(dl))
    names(out_list) <- nm

    for (i in seq_along(dl)) {
      res_i <- .diag_one(dl[[i]])
      out_list[[i]] <- res_i

      if (isTRUE(verbose)) {
        cat("=== Convergence diagnostics (bqr.svy) ", nm[i], " ===\n", sep = "")
        if (length(res_i$not_converged)) {
          cat("Parameters with R-hat >", rhat_threshold, ":\n",
              paste(res_i$not_converged, collapse = ", "), "\n")
        } else {
          cat("All parameters meet R-hat threshold.\n")
        }
        if (length(res_i$low_ess)) {
          cat("Parameters with ESS ratio <", ess_ratio_threshold, ":\n",
              paste(res_i$low_ess, collapse = ", "), "\n")
        } else {
          cat("All parameters meet ESS ratio threshold.\n")
        }
      }
    }
    return(out_list)
  }

  # --- Single tau: previous behavior (but using Vehtari) ---
  res <- .diag_one(object$draws)
  if (isTRUE(verbose)) {
    cat("=== Convergence diagnostics (bqr.svy) ===\n")
    if (length(res$not_converged)) {
      cat("Parameters with R-hat >", rhat_threshold, ":\n",
          paste(res$not_converged, collapse = ", "), "\n")
    } else {
      cat("All parameters meet R-hat threshold.\n")
    }
    if (length(res$low_ess)) {
      cat("Parameters with ESS ratio <", ess_ratio_threshold, ":\n",
          paste(res$low_ess, collapse = ", "), "\n")
    } else {
      cat("All parameters meet ESS ratio threshold.\n")
    }
  }
  res
}


#' Convergence diagnostics for mo.bqr.svy objects
#'
#' Checks convergence for EM algorithm fits produced by \code{\link{mo.bqr.svy}}.
#' Since EM is deterministic, this method reports the number of iterations and
#' whether convergence was achieved for each quantile.
#'
#' @inheritParams convergence_check
#'
#' @details For EM-based \code{mo.bqr.svy} fits, no MCMC draws are available, so
#'   this function reports iteration counts and convergence flags instead of
#'   R-hat/ESS statistics.
#'
#' @export
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

  list(
    iterations = iter_counts,
    converged = converged_flags
  )
}

#' Default convergence check
#'
#' Allows \code{convergence_check()} to work on generic matrices or data frames
#' containing MCMC draws (each column = parameter, each row = draw).
#'
#' @param object A numeric matrix or data frame of draws.
#' @param rhat_threshold Numeric, threshold for R-hat diagnostic (default 1.01).
#' @param ess_ratio_threshold Numeric, threshold for effective sample size ratio (default 0.10).
#' @param ... Not used.
#'
#' @return A list with components \code{rhat}, \code{neff}, and \code{converged}.
#' @export
convergence_check.default <- function(object,
                                      rhat_threshold = 1.01,
                                      ess_ratio_threshold = 0.10,
                                      ...) {
  # Convert to matrix if needed
  if (is.data.frame(object)) {
    object <- data.matrix(object)
  }
  if (!is.matrix(object) || !is.numeric(object)) {
    stop("Default method for convergence_check expects a numeric matrix or data.frame.")
  }

  stats <- summarise_draws_custom(object)
  n_samples <- nrow(object)

  converged_flags <- !(
    stats$rhat > rhat_threshold |
      (stats$ess_bulk / n_samples) < ess_ratio_threshold
  )

  list(
    rhat      = stats$rhat,
    neff      = stats$ess_bulk,
    converged = converged_flags
  )
}



