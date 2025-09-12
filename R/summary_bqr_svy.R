# ======================================================================
# ANCHOR DOC: SUMMARY (single manual page "summary.tauBayesW")
# ======================================================================

#' Summary methods for tauBayesW objects
#'
#' This page groups all S3 `summary()` methods provided by the
#' \pkg{tauBayesW} package for classes \code{bqr.svy}, \code{bwqr_fit},
#' and \code{mo.bqr.svy}. Keeping them under one help page makes the
#' manual concise while regular S3 dispatch still works as usual.
#'
#' @name summary.tauBayesW
#' @aliases summary
NULL


# ======================================================================
# ANCHOR DOC: PRINT (single manual page "print.tauBayesW")
# ======================================================================

#' Print methods for tauBayesW objects
#'
#' This page groups all S3 `print()` methods for the summary objects
#' returned by \pkg{tauBayesW}: \code{summary.bqr.svy},
#' \code{summary.bwqr_fit}, and \code{summary.mo.bqr.svy}.
#'
#' @name print.tauBayesW
#' @aliases print
NULL


# ======================================================================
# SUMMARY METHODS
# ======================================================================

#' @rdname summary.tauBayesW
#'
#' @description
#' Posterior summary (means, credible intervals, R-hat, bulk/tail ESS) for
#' objects of class \code{bqr.svy}. Supports one or multiple quantiles.
#'
#' @param object An object of class \code{bqr.svy}.
#' @param probs Credible interval probabilities. Default \code{c(0.025, 0.975)}.
#' @param digits Number of decimals to use for printing.
#' @param ... Unused.
#' @return A \code{summary.bqr.svy} object containing one block per \eqn{\tau}.
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

  make_block <- function(D, tau) {
    D <- if (is.data.frame(D)) data.matrix(D) else as.matrix(D)
    storage.mode(D) <- "numeric"
    if (!nrow(D) || !ncol(D)) stop("Empty draws matrix.", call. = FALSE)

    stats <- summarise_draws_custom(D)

    coef_idx   <- stats$variable != "sigma"
    coef_stats <- stats[coef_idx, , drop = FALSE]

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


#' @rdname summary.tauBayesW
#'
#' @description
#' Posterior summary for \code{bwqr_fit} (single quantile). Computes
#' means, SDs, R-hat, bulk/tail ESS, and credible intervals.
#'
#' @param object An object of class \code{bwqr_fit}.
#' @param probs Credible interval probabilities.
#' @param digits Number of decimals to use for printing.
#' @param max_lag Max lag for autocorrelation diagnostics.
#' @param ... Unused.
#' @return A \code{summary.bwqr_fit} object.
#' @exportS3Method summary bwqr_fit
summary.bwqr_fit <- function(object, probs = c(0.025, 0.975), digits = 3, max_lag = 200, ...) {
  draws <- object$draws
  draws <- if (is.data.frame(draws)) data.matrix(draws) else as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (nrow(draws) == 0L || ncol(draws) == 0L)
    stop("'object$draws' is empty or non-numeric.", call. = FALSE)

  stats <- summarise_draws_custom(draws, max_lag = max_lag)

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


#' @rdname summary.tauBayesW
#'
#' @description
#' MAP-only summary per quantile and direction for \code{mo.bqr.svy}
#' (no SD/CI).
#'
#' @param object An object of class \code{mo.bqr.svy}.
#' @param digits Number of decimals to print. Default 4.
#' @param ... Unused.
#' @return A \code{summary.mo.bqr.svy} object.
#' @exportS3Method summary mo.bqr.svy
summary.mo.bqr.svy <- function(object, digits = 4, ...) {
  stopifnot(inherits(object, "mo.bqr.svy"))
  `%||%` <- function(a, b) if (is.null(a)) b else a

  one_dir <- function(dir_res, tau, k) {
    beta <- dir_res$beta
    coef_tab <- data.frame(
      parameter  = names(beta),
      MAP        = as.numeric(beta),
      check.names = FALSE
    )
    list(
      tau       = tau,
      dir_id    = k,
      coef_tab  = coef_tab,
      sigma     = as.numeric(dir_res$sigma),
      iter      = dir_res$iter %||% NA_integer_,
      converged = isTRUE(dir_res$converged)
    )
  }

  blocks <- list()
  for (qi in seq_along(object$quantile)) {
    tau  <- object$quantile[qi]
    dirs <- object$fit[[qi]]$directions
    for (k in seq_along(dirs)) {
      blocks[[length(blocks) + 1]] <- one_dir(dirs[[k]], tau, k)
    }
  }

  out <- list(
    call         = object$call %||% NULL,
    algorithm    = object$algorithm %||% "em",
    tolerance    = object$tolerance %||% object$epsilon %||% NA_real_,
    quantiles    = object$quantile,
    n_dir        = object$n_dir,
    n_obs        = object$n_obs,
    n_vars       = object$n_vars,
    response_dim = object$response_dim,
    digits       = digits,
    blocks       = blocks
  )
  class(out) <- "summary.mo.bqr.svy"
  out
}


# ======================================================================
# PRINT METHODS
# ======================================================================

#' @rdname print.tauBayesW
#' @exportS3Method print summary.bqr.svy
print.summary.bqr.svy <- function(x, ...) {
  stopifnot(inherits(x, "summary.bqr.svy"))
  cat("\nMethod: ", x$method %||% "NA", "\n", sep = "")
  cat("Quantiles: ",
      paste(formatC(x$quantiles, format = "f", digits = 3), collapse = ", "),
      "\n\n", sep = "")

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


#' @rdname print.tauBayesW
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


#' @rdname print.tauBayesW
#' @exportS3Method print summary.mo.bqr.svy
print.summary.mo.bqr.svy <- function(x, ...) {
  stopifnot(inherits(x, "summary.mo.bqr.svy"))
  dig <- x$digits

  cat("Bayesian Quantile Regression (EM algorithm)\n")
  cat("--------------------------------------------\n")
  qs <- paste(formatC(x$quantiles, format = "f", digits = 3), collapse = ", ")
  cat("Quantile", if (length(x$quantiles) > 1) "s" else "", ": tau = ", qs, "\n", sep = "")
  cat("Posterior mode estimates (MAP)\n")
  cat("Algorithm: EM\n\n", sep = "")

  ord <- order(vapply(x$blocks, `[[`, numeric(1), "tau"),
               vapply(x$blocks, `[[`, numeric(1), "dir_id"))
  blocks <- x$blocks[ord]

  for (b in blocks) {
    cat("Quantile: tau = ", formatC(b$tau, format = "f", digits = 3),
        "   |   Direction ", b$dir_id, "\n", sep = "")
    has_tol <- is.finite(x$tolerance)
    if (has_tol && !is.na(b$iter)) {
      cat("EM status: ",
          if (isTRUE(b$converged)) "converged" else "not converged",
          " in ", b$iter, " iterations, tol=",
          formatC(x$tolerance, format = "g", digits = 3),
          "\n", sep = "")
    } else if (!is.na(b$iter)) {
      cat("EM status: ",
          if (isTRUE(b$converged)) "converged" else "not converged",
          " in ", b$iter, " iterations\n", sep = "")
    } else {
      cat("EM status: ",
          if (isTRUE(b$converged)) "converged" else "not converged",
          "\n", sep = "")
    }

    cat("\nCoefficients (posterior mode / MAP):\n")
    tab <- b$coef_tab
    tab$MAP <- ifelse(is.finite(tab$MAP), round(tab$MAP, dig), tab$MAP)
    print(tab, row.names = FALSE)

    cat("\nScale sigma: ", round(b$sigma, dig), " (posterior mode)\n\n", sep = "")
  }

  invisible(x)
}
