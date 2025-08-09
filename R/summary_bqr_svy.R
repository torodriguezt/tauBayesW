if (!exists("%||%"))
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a
#' Compute summary statistics for MCMC draws
#' @param draws Matrix of MCMC draws (samples x parameters)
#' @param max_lag Maximum lag for effective sample size calculation
#' @keywords internal
summarise_draws_custom <- function(draws, max_lag = 1000) {
  if (!is.matrix(draws)) {
    draws <- as.matrix(draws)
  }
  n_samples <- nrow(draws)
  n_vars <- ncol(draws)
  var_names <- colnames(draws)
  if (is.null(var_names)) {
    var_names <- paste0("V", seq_len(n_vars))
  }
  results <- data.frame(
    variable = var_names,
    mean = apply(draws, 2, mean, na.rm = TRUE),
    median = apply(draws, 2, median, na.rm = TRUE),
    sd = apply(draws, 2, sd, na.rm = TRUE),
    mad = apply(draws, 2, mad, na.rm = TRUE),
    q5 = apply(draws, 2, quantile, probs = 0.05, na.rm = TRUE),
    q95 = apply(draws, 2, quantile, probs = 0.95, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  # Calculate Rhat and ESS
  results$rhat <- apply(draws, 2, function(x) {
    if (all(is.na(x)) || sd(x, na.rm = TRUE) == 0) return(NA_real_)
    # Simple Rhat approximation for single chain
    n <- length(x)
    if (n < 4) return(NA_real_)
    # Split chain into two halves
    half <- floor(n/2)
    first_half <- x[1:half]
    second_half <- x[(n-half+1):n]
    # Calculate between and within chain variance
    chain_means <- c(mean(first_half, na.rm = TRUE), mean(second_half, na.rm = TRUE))
    overall_mean <- mean(x, na.rm = TRUE)
    B <- half * var(chain_means, na.rm = TRUE)
    W <- (var(first_half, na.rm = TRUE) + var(second_half, na.rm = TRUE)) / 2
    if (W <= 0) return(NA_real_)
    var_plus <- ((half - 1) / half) * W + B / half
    sqrt(var_plus / W)
  })
  # Calculate effective sample size (bulk)
  results$ess_bulk <- apply(draws, 2, function(x) {
    if (all(is.na(x)) || sd(x, na.rm = TRUE) == 0) return(NA_real_)
    # Simple ESS calculation using autocorrelation
    n <- length(x)
    if (n < 4) return(n)
    # Calculate autocorrelation function
    acf_vals <- tryCatch({
      acf_result <- acf(x, lag.max = min(max_lag, n-1), plot = FALSE)
      acf_result$acf[,,1]
    }, error = function(e) rep(0, min(max_lag + 1, n)))
    # Find first negative autocorrelation
    first_neg <- which(acf_vals[-1] <= 0)[1]
    if (is.na(first_neg)) first_neg <- length(acf_vals) - 1
    # Sum of autocorrelations
    sum_acf <- 1 + 2 * sum(acf_vals[2:(first_neg + 1)])
    if (sum_acf <= 0) return(n)
    n / sum_acf
  })
  # Calculate effective sample size (tail)
  results$ess_tail <- results$ess_bulk  # Simplified for now
  return(results)
}
## (section 2) print.bwqr_fit()
#' Print method for Bayesian Quantile Regression fits
#'
#' Provides a brief summary of a fitted Bayesian quantile regression model
#'
#' @param x An object of class 'bwqr_fit'
#' @param digits Number of decimal places for numeric output (default: 3)
#' @param ... Additional arguments passed to other methods
#' @return Invisibly returns the input object
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
  print(summary_stats[, c("variable", "mean", "sd", "q5", "q95")], 
        row.names = FALSE, digits = digits)
  invisible(x)
}
# 3) summary.bwqr_fit() - cleaner header
#' Summary method for Bayesian Quantile Regression fits (bwqr_fit class)
#'
#' Provides detailed summary statistics and MCMC diagnostics for fitted
#' Bayesian quantile regression models
#'
#' @param object An object of class 'bwqr_fit'
#' @param probs Quantiles for credible intervals (default: c(0.025, 0.975))
#' @param digits Number of decimal places for output (default: 3)
#' @param max_lag Maximum lag for effective sample size calculation (default: 1000)
#' @param ... Additional arguments passed to other methods
#' @return A summary object with posterior statistics and diagnostics
#' @export
summary.bwqr_fit <- function(object,
                             probs   = c(0.025, 0.975),
                             digits  = 3,
                             max_lag = 1000,
                             ...) {
  draws <- object$draws
  if (is.data.frame(draws))
    draws <- data.matrix(draws)
  draws <- as.matrix(draws)
  storage.mode(draws) <- "numeric"
  if (nrow(draws) == 0L || ncol(draws) == 0L)
    stop("'object$draws' empty or not numeric.", call. = FALSE)
  stats <- summarise_draws_custom(draws, max_lag = max_lag)
  ## -- separate diagnostic rows --------------------------------------
  diag_base <- c("accept_rate", "n_mcmc", "burnin", "thin", "n_samples")
  diag_rows <- grepl(paste0("^(", paste(diag_base, collapse = "|"), ")"), stats$variable)
  diag_info <- stats[diag_rows, , drop = FALSE]
  stats     <- stats[!diag_rows, , drop = FALSE]
  ## (only use the mean of accept_rate)
  acc_rate <- diag_info$mean[grepl("^accept_rate", diag_info$variable)][1]
  ## -- header -------------------------------------------------------
  cat("\nBayesian Quantile Regression fit (class 'bwqr_fit')\n")
  cat("Method        :", object$method, "\n")
  cat("Quantile      :", object$quantile, "\n")
  cat("Chains        :", object$n_chains %||% 1L, "\n")
  cat("Draws         :", nrow(draws), "post-warmup per chain\n")
  if (!is.na(acc_rate))
    cat("Accept rate   :", sprintf("%.3f", acc_rate), "\n")
  cat("\n")
  ## -- main table --------------------------------------------------
  print(stats, row.names = FALSE, digits = digits)
  ## -- warnings (only coefficients) ---------------------------------
  warn_rhat <- stats$rhat > 1.1 | is.na(stats$rhat)
  warn_ess  <- stats$ess_bulk < 100 | is.na(stats$ess_bulk)
  if (any(warn_rhat | warn_ess)) {
    cat("\nWarnings:\n")
    if (any(warn_rhat))
      cat("  * R-hat > 1.1 (or NA) for:",
          paste(stats$variable[warn_rhat], collapse = ", "), "\n")
    if (any(warn_ess))
      cat("  * ESS_bulk < 100 for   :",
          paste(stats$variable[warn_ess],   collapse = ", "), "\n")
  }
  invisible(stats)
}
# 4) summary.bqr.svy() - Improved summary for single quantile BQR
#' Summary method for Bayesian Quantile Regression (single quantile)
#' 
#' Provides a comprehensive summary of Bayesian quantile regression results
#' including posterior estimates, credible intervals, and MCMC diagnostics
#' following Stan-like output format.
#' 
#' @param object An object of class 'bqr.svy'
#' @param probs Quantiles for credible intervals (default: c(0.025, 0.975))
#' @param digits Number of decimal places for output (default: 3)
#' @param ... Additional arguments passed to other methods
#' @return A summary object with posterior statistics and diagnostics
#' @export
summary.bqr.svy <- function(object, 
                            probs = c(0.025, 0.975),
                            digits = 3, 
                            ...) {
  if (!inherits(object, "bqr.svy"))
    stop("Object must be of class 'bqr.svy'")
  # Get posterior summary using existing function
  stats <- summarise_draws_custom(object$draws)
  # Calculate credible intervals at specified probabilities
  lower_prob <- probs[1]
  upper_prob <- probs[2]
  stats$lower_ci <- apply(object$draws, 2, quantile, probs = lower_prob, na.rm = TRUE)
  stats$upper_ci <- apply(object$draws, 2, quantile, probs = upper_prob, na.rm = TRUE)
  # Create summary object
  summary_obj <- list(
    call = object$call,
    method = object$method,
    quantile = object$quantile,
    n_draws = nrow(object$draws),
    n_chains = object$n_chains %||% 1L,
    warmup = object$warmup %||% 0,
    thin = object$thin %||% 1,
    accept_rate = object$accept_rate,
    runtime = object$runtime,
    posterior_summary = stats,
    probs = probs,
    digits = digits
  )
  class(summary_obj) <- "summary.bqr.svy"
  return(summary_obj)
}
#' Print method for summary.bqr.svy
#' @param x A summary.bqr.svy object
#' @param ... Additional arguments (unused)
#' @export
print.summary.bqr.svy <- function(x, ...) {
  cat("\nBayesian Quantile Regression Summary\n")
  cat("====================================\n\n")
  # Model information
  cat("Model Information:\n")
  cat("  Method         :", x$method, "\n")
  cat("  Quantile (tau)   :", x$quantile, "\n")
  cat("  Chains         :", x$n_chains, "\n")
  cat("  Post-warmup    :", x$n_draws, "draws per chain\n")
  cat("  Warmup         :", x$warmup, "draws\n")
  cat("  Thinning       :", x$thin, "\n")
  if (!is.na(x$accept_rate))
    cat("  Accept rate    :", sprintf("%.3f", x$accept_rate), "\n")
  if (!is.null(x$runtime))
    cat("  Runtime        :", sprintf("%.2f", x$runtime), "seconds\n")
  cat("\n")
  # Posterior summary table
  cat("Posterior Estimates:\n")
  prob_lower <- sprintf("%.1f%%", x$probs[1] * 100)
  prob_upper <- sprintf("%.1f%%", x$probs[2] * 100)
  # Format table for display
  display_table <- data.frame(
    Variable = x$posterior_summary$variable,
    Mean = sprintf(paste0("%.", x$digits, "f"), x$posterior_summary$mean),
    SD = sprintf(paste0("%.", x$digits, "f"), x$posterior_summary$sd),
    CI_Lower = sprintf(paste0("%.", x$digits, "f"), x$posterior_summary$lower_ci),
    CI_Upper = sprintf(paste0("%.", x$digits, "f"), x$posterior_summary$upper_ci),
    Rhat = sprintf("%.3f", x$posterior_summary$rhat),
    ESS_bulk = sprintf("%.0f", x$posterior_summary$ess_bulk)
  )
  names(display_table) <- c("Variable", "Mean", "SD", 
                           paste0(prob_lower, " CI"), paste0(prob_upper, " CI"),
                           "Rhat", "ESS_bulk")
  print(display_table, row.names = FALSE, right = FALSE)
  # Diagnostics warnings
  warn_rhat <- x$posterior_summary$rhat > 1.1 | is.na(x$posterior_summary$rhat)
  warn_ess <- x$posterior_summary$ess_bulk < 100 | is.na(x$posterior_summary$ess_bulk)
  if (any(warn_rhat | warn_ess)) {
    cat("\nDiagnostic Warnings:\n")
    if (any(warn_rhat))
      cat("  * R-hat > 1.1 (convergence issues) for:",
          paste(x$posterior_summary$variable[warn_rhat], collapse = ", "), "\n")
    if (any(warn_ess))
      cat("  * ESS_bulk < 100 (low effective sample size) for:",
          paste(x$posterior_summary$variable[warn_ess], collapse = ", "), "\n")
  }
  invisible(x)
}
# 5) summary.mo.bqr.svy() - Improved summary for multiple quantile BQR
#' Summary method for Multiple-Output Bayesian Quantile Regression
#' 
#' Provides a comprehensive summary of multiple-output Bayesian quantile 
#' regression results including posterior estimates for each quantile,
#' convergence information, and iteration counts.
#' 
#' @param object An object of class 'mo.bqr.svy'
#' @param digits Number of decimal places for output (default: 3)
#' @param ... Additional arguments (unused)
#' @return A summary object with results for all quantiles
#' @export
summary.mo.bqr.svy <- function(object, digits = 3, ...) {
  if (!inherits(object, "mo.bqr.svy"))
    stop("Object must be of class 'mo.bqr.svy'")
  # Extract information for each quantile
  quantile_summaries <- lapply(seq_along(object$quantile), function(i) {
    q <- object$quantile[i]
    fit_q <- object$fit[[i]]
    if (is.null(fit_q)) {
      return(list(
        quantile = q,
        converged = FALSE,
        iterations = NA,
        coefficients = NULL,
        sigma = NA
      ))
    }
    # Create coefficient data frame with names
    n_coef <- length(fit_q$beta)
    coef_names <- if (n_coef == 1) {
      "(Intercept)"
    } else {
      c("(Intercept)", paste0("X", 1:(n_coef-1)))
    }
    coef_df <- data.frame(
      Coefficient = coef_names,
      Estimate = fit_q$beta,
      stringsAsFactors = FALSE
    )
    list(
      quantile = q,
      converged = fit_q$converged,
      iterations = fit_q$iter,
      coefficients = coef_df,
      sigma = fit_q$sigma
    )
  })
  names(quantile_summaries) <- paste0("q", object$quantile)
  # Create summary object
  summary_obj <- list(
    call = object$call,
    algorithm = object$algorithm,
    quantiles = object$quantile,
    n_directions = object$n_dir,
    prior = object$prior,
    quantile_results = quantile_summaries,
    digits = digits
  )
  class(summary_obj) <- "summary.mo.bqr.svy"
  return(summary_obj)
}
#' Print method for summary.mo.bqr.svy
#' @param x A summary.mo.bqr.svy object
#' @param ... Additional arguments (unused)
#' @export
print.summary.mo.bqr.svy <- function(x, ...) {
  cat("\nMultiple-Output Bayesian Quantile Regression Summary\n")
  cat("===================================================\n\n")
  # Model information
  cat("Model Information:\n")
  cat("  Algorithm      :", x$algorithm, "\n")
  cat("  Quantiles (tau)  :", paste(x$quantiles, collapse = ", "), "\n")
  cat("  Directions     :", x$n_directions, "\n")
  cat("\n")
  # Convergence summary
  converged_status <- sapply(x$quantile_results, function(qr) qr$converged)
  total_iter <- sapply(x$quantile_results, function(qr) qr$iterations)
  cat("Convergence Summary:\n")
  cat("  Converged quantiles:", sum(converged_status, na.rm = TRUE), 
      "out of", length(x$quantiles), "\n")
  if (any(!is.na(total_iter))) {
    cat("  Average iterations :", sprintf("%.1f", mean(total_iter, na.rm = TRUE)), "\n")
    cat("  Max iterations     :", max(total_iter, na.rm = TRUE), "\n")
  }
  cat("\n")
  # Results for each quantile
  for (i in seq_along(x$quantile_results)) {
    qr <- x$quantile_results[[i]]
    q_name <- names(x$quantile_results)[i]
    cat(sprintf("Quantile tau = %.3f:\n", qr$quantile))
    cat("  Converged  :", ifelse(qr$converged, "Yes", "No"))
    if (!is.na(qr$iterations)) {
      cat(" (", qr$iterations, "iterations)")
    }
    cat("\n")
    if (!is.null(qr$coefficients)) {
      cat("  Coefficients:\n")
      coef_display <- qr$coefficients
      coef_display$Estimate <- sprintf(paste0("%.", x$digits, "f"), 
                                      coef_display$Estimate)
      print(coef_display, row.names = FALSE, right = FALSE)
      if (!is.na(qr$sigma)) {
        cat("  sigma^2 =", sprintf(paste0("%.", x$digits, "f"), qr$sigma), "\n")
      }
    } else {
      cat("  No results available\n")
    }
    cat("\n")
  }
  # Warning for non-converged quantiles
  non_converged <- which(!converged_status)
  if (length(non_converged) > 0) {
    cat("Warnings:\n")
    cat("  * Non-converged quantiles:", 
        paste(x$quantiles[non_converged], collapse = ", "), "\n")
    cat("  * Consider increasing max_iter or checking model specification\n")
  }
  invisible(x)
}
# 6) Enhanced print methods for S3 classes
#' Print method for bqr.svy objects
#' @param x An object of class 'bqr.svy'
#' @param digits Number of decimal places for output (default: 3)
#' @param ... Additional arguments (unused)
#' @export
print.bqr.svy <- function(x, digits = 3, ...) {
  cat("\nBayesian Quantile Regression (class 'bqr.svy')\n")
  cat("Method    :", x$method, "\n")
  cat("Quantile  :", x$quantile, "\n")
  cat("Draws     :", nrow(x$draws), "\n")
  if (!is.null(x$accept_rate) && !is.na(x$accept_rate)) {
    cat("Accept rate:", sprintf("%.3f", x$accept_rate), "\n")
  }
  cat("\nPosterior means:\n")
  means <- apply(x$draws, 2, mean, na.rm = TRUE)
  print(round(means, digits))
  cat("\nUse summary() for detailed posterior statistics and diagnostics.\n")
  invisible(x)
}
#' Enhanced print method for bwqr_fit objects
#' @param x An object of class 'bwqr_fit'
#' @param digits Number of decimal places for output (default: 3)
#' @param ... Additional arguments (unused)
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
  print(summary_stats[, c("variable", "mean", "sd", "q5", "q95")], 
        row.names = FALSE, digits = digits)
  invisible(x)
}
#' Enhanced print method for mo.bqr.svy objects
#' @param x An object of class 'mo.bqr.svy'
#' @param digits Number of decimal places for output (default: 3)
#' @param ... Additional arguments (unused)
#' @export
print.mo.bqr.svy <- function(x, digits = 3, ...) {
  cat("\nMultiple-Output Bayesian Quantile Regression (class 'mo.bqr.svy')\n")
  cat("Algorithm :", x$algorithm, "\n")
  cat("Quantiles :", paste(x$quantile, collapse = ", "), "\n")
  cat("Directions:", x$n_dir, "\n")
  # Show brief results for each quantile
  cat("\nBrief Results:\n")
  for (i in seq_along(x$quantile)) {
    q <- x$quantile[i]
    fit_q <- x$fit[[i]]
    if (!is.null(fit_q)) {
      cat(sprintf("  tau = %.3f: %s (%d iter)\n", 
                  q, 
                  ifelse(fit_q$converged, "Converged", "Not converged"),
                  fit_q$iter))
    } else {
      cat(sprintf("  tau = %.3f: No results\n", q))
    }
  }
  cat("\nUse summary() for detailed results.\n")
  invisible(x)
}
# 7) Standard plot methods for S3 classes
#' Plot method for bqr.svy objects
#' @param x An object of class 'bqr.svy'
#' @param type Type of plot: "trace", "density", "intervals" (default: "trace")
#' @param pars Parameters to plot (default: all)
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.bqr.svy <- function(x, type = c("trace", "density", "intervals"), 
                         pars = NULL, ...) {
  type <- match.arg(type)
  
  # Set up plotting parameters for better aesthetics
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  if (is.null(pars)) {
    pars <- colnames(x$draws)
  } else {
    pars <- intersect(pars, colnames(x$draws))
    if (length(pars) == 0) {
      stop("No valid parameters found in 'pars'")
    }
  }
  
  draws_subset <- x$draws[, pars, drop = FALSE]
  n_pars <- ncol(draws_subset)
  
  # Create proper parameter names with mathematical notation
  param_names <- names(draws_subset)
  param_expressions <- lapply(param_names, function(name) {
    if (grepl("beta", name, ignore.case = TRUE)) {
      # Extract number if present
      num_match <- regmatches(name, regexpr("[0-9]+", name))
      if (length(num_match) > 0) {
        substitute(beta[i], list(i = as.numeric(num_match)))
      } else {
        expression(beta)
      }
    } else if (grepl("sigma", name, ignore.case = TRUE)) {
      expression(sigma)
    } else if (grepl("tau", name, ignore.case = TRUE)) {
      expression(tau)
    } else {
      name
    }
  })
  
  switch(type,
    "trace" = {
      # Calculate grid layout
      n_cols <- ceiling(sqrt(n_pars))
      n_rows <- ceiling(n_pars / n_cols)
      par(mfrow = c(n_rows, n_cols), 
          mar = c(4, 4, 3, 1),
          oma = c(0, 0, 2, 0))
      
      # Color palette for different parameters
      colors <- rainbow(n_pars, start = 0, end = 0.8, alpha = 0.8)
      
      for (i in 1:n_pars) {
        plot(draws_subset[, i], 
             type = "l", 
             col = colors[i],
             lwd = 1.5,
             main = param_expressions[[i]],
             xlab = "Iteration", 
             ylab = "Value",
             cex.main = 1.2,
             cex.lab = 1.0,
             las = 1,
             panel.first = grid(col = "lightgray", lty = "dotted"))
        
        # Add horizontal line at mean
        abline(h = mean(draws_subset[, i]), col = "red", lty = "dashed", lwd = 1.5)
      }
      
      mtext("MCMC Trace Plots", outer = TRUE, cex = 1.3, font = 2)
    },
    "density" = {
      # Calculate grid layout
      n_cols <- ceiling(sqrt(n_pars))
      n_rows <- ceiling(n_pars / n_cols)
      par(mfrow = c(n_rows, n_cols), 
          mar = c(4, 4, 3, 1),
          oma = c(0, 0, 2, 0))
      
      # Color palette
      colors <- rainbow(n_pars, start = 0, end = 0.8, alpha = 0.8)
      
      for (i in 1:n_pars) {
        dens <- density(draws_subset[, i])
        plot(dens, 
             main = param_expressions[[i]],
             xlab = "Value",
             ylab = "Density",
             col = colors[i],
             lwd = 2,
             cex.main = 1.2,
             cex.lab = 1.0,
             las = 1)
        
        # Fill under the curve
        polygon(dens, col = adjustcolor(colors[i], alpha = 0.3), border = NA)
        
        # Add vertical lines for quantiles
        quants <- quantile(draws_subset[, i], c(0.025, 0.5, 0.975))
        abline(v = quants[2], col = "red", lwd = 2, lty = "solid") # median
        abline(v = quants[c(1,3)], col = "red", lwd = 1, lty = "dashed") # 95% CI
        
        grid(col = "lightgray", lty = "dotted")
      }
      
      mtext("Posterior Density Plots", outer = TRUE, cex = 1.3, font = 2)
    },
    "intervals" = {
      par(mar = c(6, 4, 3, 2))
      
      means <- apply(draws_subset, 2, mean)
      q025 <- apply(draws_subset, 2, quantile, 0.025)
      q975 <- apply(draws_subset, 2, quantile, 0.975)
      
      # Enhanced interval plot
      plot(1:length(means), means, 
           ylim = range(c(q025, q975)),
           xlab = "",
           ylab = "Parameter Value",
           main = "95% Credible Intervals",
           xaxt = "n", 
           pch = 19, 
           cex = 1.3,
           col = "darkblue",
           cex.main = 1.3,
           cex.lab = 1.1,
           las = 1,
           panel.first = {
             grid(col = "lightgray", lty = "dotted")
             abline(h = 0, col = "gray50", lty = "dashed")
           })
      
      # Enhanced error bars
      segments(1:length(means), q025, 1:length(means), q975, 
               col = "darkblue", lwd = 2)
      segments(1:length(means) - 0.15, q025, 1:length(means) + 0.15, q025,
               col = "darkblue", lwd = 2)
      segments(1:length(means) - 0.15, q975, 1:length(means) + 0.15, q975,
               col = "darkblue", lwd = 2)
      
      # Add parameter names with mathematical notation
      axis(1, at = 1:length(means), labels = rep("", length(means)))
      for (i in 1:length(means)) {
        if (is.expression(param_expressions[[i]])) {
          mtext(param_expressions[[i]], side = 1, line = 1, at = i, cex = 1.0)
        } else {
          mtext(param_expressions[[i]], side = 1, line = 1, at = i, cex = 1.0)
        }
      }
    }
  )
  invisible(x)
}
#' Plot method for mo.bqr.svy objects
#' @param x An object of class 'mo.bqr.svy'
#' @param type Type of plot: "quantiles", "convergence" (default: "quantiles")
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.mo.bqr.svy <- function(x, type = c("quantiles", "convergence"), ...) {
  type <- match.arg(type)
  
  # Set up plotting parameters for better aesthetics
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  switch(type,
    "quantiles" = {
      # Plot coefficient estimates across quantiles with enhanced aesthetics
      n_coef <- length(x$fit[[1]]$beta)
      
      # Create a nicer grid layout
      n_cols <- ceiling(sqrt(n_coef))
      n_rows <- ceiling(n_coef / n_cols)
      par(mfrow = c(n_rows, n_cols), 
          mar = c(4.5, 4.5, 3, 1.5),
          oma = c(0, 0, 2, 0))
      
      # Define a nice color palette
      colors <- rainbow(n_coef, start = 0, end = 0.8, alpha = 0.8)
      
      for (j in 1:n_coef) {
        estimates <- sapply(x$fit, function(fit) fit$beta[j])
        
        # Create mathematical expression for coefficient name
        if (j == 1) {
          coef_expr <- expression(beta[0])  # Intercept
        } else {
          coef_expr <- substitute(beta[i], list(i = j-1))
        }
        
        # Enhanced plot with better aesthetics
        plot(x$quantile, estimates, 
             type = "o", 
             col = colors[j],
             pch = 16,
             cex = 1.2,
             lwd = 2,
             main = coef_expr,
             xlab = expression(tau),  # Greek tau
             ylab = "Coefficient Estimate",
             cex.main = 1.3,
             cex.lab = 1.1,
             cex.axis = 1.0,
             las = 1,
             panel.first = {
               grid(col = "lightgray", lty = "dotted", lwd = 1)
               abline(h = 0, col = "gray50", lty = "dashed", lwd = 1)
             })
        
        # Add confidence region if available
        if (!is.null(x$fit[[1]]$se) || !is.null(x$fit[[1]]$ci)) {
          # Add shaded confidence intervals if available
          se_vals <- sapply(x$fit, function(fit) {
            if (!is.null(fit$se)) fit$se[j] else NA
          })
          
          if (!all(is.na(se_vals))) {
            upper <- estimates + 1.96 * se_vals
            lower <- estimates - 1.96 * se_vals
            polygon(c(x$quantile, rev(x$quantile)), 
                    c(upper, rev(lower)),
                    col = adjustcolor(colors[j], alpha = 0.2),
                    border = NA)
          }
        }
      }
      
      # Add overall title
      mtext("Coefficient Estimates Across Quantiles", 
            outer = TRUE, cex = 1.4, font = 2)
    },
    "convergence" = {
      # Enhanced convergence plot
      par(mar = c(4.5, 4.5, 3, 1.5))
      
      iterations <- sapply(x$fit, function(fit) fit$iter)
      converged <- sapply(x$fit, function(fit) fit$converged)
      
      # Create the plot with enhanced aesthetics
      plot(x$quantile, iterations, 
           type = "o",
           col = ifelse(converged, "#2E8B57", "#DC143C"),  # SeaGreen and Crimson
           pch = ifelse(converged, 16, 17),  # Circles and triangles
           cex = 1.3,
           lwd = 2,
           main = expression(paste("Convergence by Quantile ", tau)),
           xlab = expression(paste("Quantile (", tau, ")")),
           ylab = "Number of Iterations",
           cex.main = 1.3,
           cex.lab = 1.1,
           cex.axis = 1.0,
           las = 1,
           panel.first = {
             grid(col = "lightgray", lty = "dotted", lwd = 1)
           })
      
      # Enhanced legend
      legend("topright", 
             legend = c("Converged", "Not Converged"),
             col = c("#2E8B57", "#DC143C"), 
             pch = c(16, 17),
             cex = 1.1,
             bg = "white",
             box.col = "gray",
             pt.cex = 1.2)
      
      # Add summary text
      n_converged <- sum(converged)
      n_total <- length(converged)
      mtext(paste0("Convergence Rate: ", n_converged, "/", n_total, 
                   " (", round(100 * n_converged / n_total, 1), "%)"),
            side = 1, line = 3, cex = 0.9, col = "gray40")
    }
  )
  invisible(x)
}
# 8) Compatibility and helper functions
# 8) Compatibility and helper functions
#' Helper function for user convenience - unified summary interface
#' @param object A fitted model object (bqr.svy or mo.bqr.svy)
#' @param ... Additional arguments passed to specific summary methods
#' @export
summary_bqr <- function(object, ...) {
  UseMethod("summary_bqr")
}
#' @export
summary_bqr.bqr.svy <- function(object, ...) {
  summary.bqr.svy(object, ...)
}
#' @export
summary_bqr.mo.bqr.svy <- function(object, ...) {
  summary.mo.bqr.svy(object, ...)
}
#' @export
summary_bqr.default <- function(object, ...) {
  summary(object, ...)
}
# 9) Utility functions for extracting model components
#' Extract coefficients from fitted models
#' @param object A fitted model object
#' @param ... Additional arguments
#' @export
coef.bqr.svy <- function(object, ...) {
  apply(object$draws, 2, mean, na.rm = TRUE)
}
#' @export
coef.mo.bqr.svy <- function(object, ...) {
  result <- lapply(seq_along(object$quantile), function(i) {
    q <- object$quantile[i]
    fit_q <- object$fit[[i]]
    if (!is.null(fit_q)) {
      setNames(fit_q$beta, paste0("beta", seq_along(fit_q$beta) - 1))
    } else {
      NULL
    }
  })
  names(result) <- paste0("q", object$quantile)
  result
}
#' Extract fitted values (not implemented for these models)
#' @param object A fitted model object
#' @param ... Additional arguments
#' @export
fitted.bqr.svy <- function(object, ...) {
  stop("fitted() method not implemented for bqr.svy objects. ",
       "Original data needed for prediction.")
}
#' @export
fitted.mo.bqr.svy <- function(object, ...) {
  stop("fitted() method not implemented for mo.bqr.svy objects. ",
       "Original data needed for prediction.")
}
#' Extract residuals (not implemented for these models)
#' @param object A fitted model object
#' @param ... Additional arguments
#' @export
residuals.bqr.svy <- function(object, ...) {
  stop("residuals() method not implemented for bqr.svy objects. ",
       "Original data needed for residual calculation.")
}
#' @export
residuals.mo.bqr.svy <- function(object, ...) {
  stop("residuals() method not implemented for mo.bqr.svy objects. ",
       "Original data needed for residual calculation.")
}
# 10) Model comparison and diagnostics utilities
#' Extract log-likelihood (placeholder - not implemented)
#' @param object A fitted model object
#' @param ... Additional arguments
#' @export
logLik.bqr.svy <- function(object, ...) {
  stop("logLik() method not implemented for bqr.svy objects.")
}
#' @export
logLik.mo.bqr.svy <- function(object, ...) {
  stop("logLik() method not implemented for mo.bqr.svy objects.")
}
#' Check MCMC convergence diagnostics
#' @param object A fitted model object
#' @param ... Additional arguments
#' @export
convergence_check <- function(object, ...) {
  UseMethod("convergence_check")
}
#' @export
convergence_check.bqr.svy <- function(object, rhat_threshold = 1.1, 
                                      ess_threshold = 100, ...) {
  stats <- summarise_draws_custom(object$draws)
  issues <- list(
    high_rhat = stats$variable[stats$rhat > rhat_threshold | is.na(stats$rhat)],
    low_ess = stats$variable[stats$ess_bulk < ess_threshold | is.na(stats$ess_bulk)]
  )
  # Remove empty issues
  issues <- issues[lengths(issues) > 0]
  if (length(issues) == 0) {
    cat("All convergence diagnostics look good!\n")
    return(invisible(TRUE))
  } else {
    cat("Convergence issues detected:\n")
    if (length(issues$high_rhat) > 0) {
      cat("  * High R-hat values:", paste(issues$high_rhat, collapse = ", "), "\n")
    }
    if (length(issues$low_ess) > 0) {
      cat("  * Low effective sample size:", paste(issues$low_ess, collapse = ", "), "\n")
    }
    return(invisible(FALSE))
  }
}
#' @export
convergence_check.mo.bqr.svy <- function(object, ...) {
  converged_quantiles <- sapply(object$fit, function(fit) fit$converged)
  non_converged <- object$quantile[!converged_quantiles]
  if (length(non_converged) == 0) {
    cat("All quantiles converged successfully!\n")
    return(invisible(TRUE))
  } else {
    cat("Convergence issues detected:\n")
    cat("  * Non-converged quantiles:", paste(non_converged, collapse = ", "), "\n")
    return(invisible(FALSE))
  }
}
