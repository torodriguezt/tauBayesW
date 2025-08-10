# =====================================================
# Plot utilities for tauBayesW
# =====================================================
#' @title Plot utilities for tauBayesW
#' @description Internal plotting helpers for \code{bqr.svy} and \code{mo.bqr.svy}.
#' These functions support plotting predicted quantile curves, observed points,
#' convergence diagnostics, and parameter traces.
#' @name tauBayesW-plot-utils
#' @keywords internal
#' @importFrom grDevices adjustcolor
#' @importFrom graphics plot points lines axis grid legend arrows mtext
#' @importFrom stats model.matrix terms delete.response median quantile
NULL

# =====================================================
# bqr.svy plotting
# =====================================================

#' Plot predicted quantile regression curve for bqr.svy objects
#'
#' This function plots the predicted quantile regression curve from a fitted
#' \code{bqr.svy} model. It can handle both numeric and categorical predictors
#' and optionally overlays the curve on an existing plot.
#'
#' @param object An object of class \code{bqr.svy}, typically the result of a call
#'   to \code{\link{bqr.svy}}.
#' @param data A \code{data.frame} containing the variables used in the model. Must
#'   include the predictor specified in \code{predictor} and any covariates in the
#'   fitted model.
#' @param predictor A character string giving the name of the predictor variable
#'   to plot on the x-axis.
#' @param grid_length Integer; number of grid points to generate for continuous
#'   predictors. Ignored for categorical predictors.
#' @param fixed_values Optional named list giving fixed values for covariates other
#'   than \code{predictor}. If not supplied, numeric covariates are fixed at their
#'   median and factors at their most frequent level.
#' @param add Logical; if \code{TRUE}, adds the curve to an existing plot instead of
#'   creating a new one.
#' @param line_col Color for the regression line.
#' @param line_lwd Line width for the regression line.
#' @param line_type Line type for the regression line.
#' @param point_pch Plotting symbol to use for points (categorical predictors).
#' @param point_cex Size of the plotting symbols for points (categorical predictors).
#' @param prefer_predict Logical; if \code{TRUE}, attempts to use the
#'   \code{predict()} method for the fitted object first.
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{points}}, or \code{\link[graphics]{lines}}.
#'
#' @return Invisibly returns a \code{data.frame} with:
#'   \item{predictor}{The sequence or factor levels of the predictor.}
#'   \item{predicted}{The predicted quantile values.}
#'   \item{quantile}{The tau value used.}
#'
#' @examples
#' \dontrun{
#' fit <- bqr.svy(y ~ x, data = mydata, quantile = 0.5)
#' plot_quantile.bqr.svy(fit, data = mydata, predictor = "x")
#' }
#'
#' @export
plot_quantile.bqr.svy <- function(object, data, predictor,
                                  grid_length = 100,
                                  fixed_values = NULL,
                                  add = FALSE,
                                  line_col = "red",
                                  line_lwd = 2,
                                  line_type = 1,
                                  point_pch = 19,
                                  point_cex = 1.2,
                                  prefer_predict = FALSE,
                                  main = NULL,
                                  ...) {
  if (!inherits(object, "bqr.svy"))
    stop("Object must be of class 'bqr.svy'.")
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")
  if (!predictor %in% names(data))
    stop("Predictor not found in data.")

  formula_obj <- if (!is.null(object$formula)) object$formula else object$call$formula
  all_vars <- all.vars(formula_obj)

  if (is.null(main)) {
    main <- paste("Quantile Regression (tau =", object$quantile, ") vs", predictor)
  }

  # Predictor values
  pred_is_cat <- is.factor(data[[predictor]]) || is.character(data[[predictor]])
  if (pred_is_cat) {
    pred_values <- if (is.factor(data[[predictor]]))
      levels(data[[predictor]]) else unique(as.character(data[[predictor]]))
  } else {
    rng <- range(data[[predictor]], na.rm = TRUE)
    pred_values <- seq(rng[1], rng[2], length.out = grid_length)
  }

  # Build newdata
  newdata <- data.frame(row.names = seq_along(pred_values))
  newdata[[predictor]] <- if (pred_is_cat)
    factor(pred_values, levels = levels(data[[predictor]])) else pred_values

  # Fill other variables
  for (v in setdiff(all_vars[-1], predictor)) {
    if (!v %in% names(data)) next
    colv <- data[[v]]
    if (!is.null(fixed_values) && v %in% names(fixed_values)) {
      newdata[[v]] <- rep(fixed_values[[v]], nrow(newdata))
    } else if (is.numeric(colv)) {
      newdata[[v]] <- rep(median(colv, na.rm = TRUE), nrow(newdata))
    } else {
      most_freq <- names(sort(table(colv), decreasing = TRUE))[1]
      newdata[[v]] <- if (is.factor(colv))
        factor(rep(most_freq, nrow(newdata)), levels = levels(colv))
      else rep(most_freq, nrow(newdata))
    }
  }

  # Prediction
  if (prefer_predict) {
    y_try <- try(predict(object, newdata = newdata), silent = TRUE)
    if (!inherits(y_try, "try-error")) {
      y_pred <- as.numeric(y_try)
    }
  }
  if (!exists("y_pred")) {
    # Get coefficients
    if (!is.null(object$coefficients)) {
      beta_hat <- as.numeric(object$coefficients)
      names(beta_hat) <- names(object$coefficients)
    } else if (!is.null(object$draws)) {
      beta_hat <- colMeans(object$draws, na.rm = TRUE)
      # drop potential non-beta columns by name if present (e.g., "sigma")
      if (!is.null(names(beta_hat))) {
        beta_hat <- beta_hat[!grepl("^sigma", names(beta_hat), ignore.case = TRUE)]
      }
    } else {
      stop("No coefficients or draws found for prediction.")
    }

    terms_obj <- if (!is.null(object$terms)) object$terms else terms(formula_obj)
    X_new <- model.matrix(delete.response(terms_obj), data = newdata)

    # Align beta_hat with X_new columns
    if (!is.null(names(beta_hat)) && all(colnames(X_new) %in% names(beta_hat))) {
      beta_hat <- beta_hat[colnames(X_new)]
    } else if (length(beta_hat) != ncol(X_new)) {
      if (length(beta_hat) > ncol(X_new)) {
        beta_hat <- beta_hat[seq_len(ncol(X_new))]
      } else {
        beta_hat <- c(beta_hat, rep(0, ncol(X_new) - length(beta_hat)))
      }
    }

    y_pred <- as.vector(X_new %*% beta_hat)
  }

  # Plot
  if (!add) {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      plot(xx, y_pred, type = "p", pch = point_pch, cex = point_cex, col = line_col,
           xlab = predictor, ylab = bquote(tau == .(object$quantile)),
           xaxt = "n", main = main, ...)
      axis(1, at = xx, labels = pred_values)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      plot(pred_values, y_pred, type = "l", col = line_col, lwd = line_lwd, lty = line_type,
           xlab = predictor, ylab = bquote(tau == .(object$quantile)),
           main = main, ...)
    }
    grid()
  } else {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      points(xx, y_pred, col = line_col, pch = point_pch, cex = point_cex)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      lines(pred_values, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    }
  }

  invisible(data.frame(predictor = pred_values, predicted = y_pred,
                       quantile = object$quantile))
}

#' Plot observed points and predicted quantile regression curve for \code{bqr.svy}
#'
#' @param object An object of class \code{bqr.svy}.
#' @param data A data frame containing the variables used in the model.
#' @param predictor Character string with the name of the predictor variable.
#' @param main Main title for the plot.
#' @param ... Additional arguments passed to \code{plot_quantile.bqr.svy}.
#' @export
plot_quantile_with_points.bqr.svy <- function(object, data, predictor,
                                              main = NULL, ...) {
  if (!inherits(object, "bqr.svy"))
    stop("Object must be of class 'bqr.svy'.")
  response_var <- all.vars(object$formula)[1]

  if (is.null(main)) {
    main <- paste("Quantile Regression (tau =", object$quantile, ") vs", predictor)
  }

  plot(data[[predictor]], data[[response_var]],
       col = adjustcolor("steelblue", 0.4),
       pch = 16, xlab = predictor, ylab = response_var,
       main = main)
  grid()
  plot_quantile.bqr.svy(object, data, predictor, add = TRUE, ...)
}

#' Plot method for \code{bqr.svy} objects
#'
#' @param x An object of class \code{bqr.svy}.
#' @param type Type of plot: \code{"trace"}, \code{"intervals"}, or \code{"quantiles"}.
#' @param ... Additional plotting arguments.
#' @export
plot.bqr.svy <- function(x, type = c("trace", "intervals", "quantiles"), ...) {
  type <- match.arg(type)

  if (type == "trace") {
    if (is.null(x$draws))
      stop("No MCMC draws available for trace plot.")
    op <- par(no.readonly = TRUE); on.exit(par(op))
    n_par <- ncol(x$draws); par(mfrow = c(ceiling(sqrt(n_par)), ceiling(n_par / ceiling(sqrt(n_par)))))
    for (j in seq_len(n_par)) {
      plot(seq_len(nrow(x$draws)), x$draws[, j], type = "l",
           xlab = "Iteration", ylab = paste0("Param[", j, "]"),
           main = paste("Trace:", colnames(x$draws)[j]), ...)
      grid()
    }
    par(op)

  } else if (type == "intervals") {
    if (is.null(x$draws))
      stop("No MCMC draws available for interval plot.")
    means <- apply(x$draws, 2, mean)
    lower <- apply(x$draws, 2, quantile, probs = 0.025)
    upper <- apply(x$draws, 2, quantile, probs = 0.975)

    plot(seq_along(means), means, ylim = range(c(lower, upper)), pch = 19,
         xaxt = "n", xlab = "Parameter", ylab = "Value",
         main = "Posterior means with 95% CI", ...)
    axis(1, at = seq_along(means), labels = if (!is.null(colnames(x$draws))) colnames(x$draws) else paste0("Param", seq_along(means)))
    arrows(seq_along(means), lower, seq_along(means), upper, angle = 90, code = 3, length = 0.05)
    grid()

  } else if (type == "quantiles") {
    stop("Quantile plot not implemented for single-quantile objects. Use plot_quantile.bqr.svy().")
  }
}

# =====================================================
# mo.bqr.svy plotting
# =====================================================

#' Plot predicted quantile regression curve for mo.bqr.svy objects
#'
#' This function plots the predicted quantile regression curve from a fitted
#' \code{mo.bqr.svy} model. It can handle both numeric and categorical predictors
#' and optionally overlays the curve on an existing plot. For multiple quantiles,
#' the first quantile is plotted by default.
#'
#' @param object An object of class \code{mo.bqr.svy}, typically the result of a
#'   call to \code{\link{mo.bqr.svy}}.
#' @param data A \code{data.frame} containing the variables used in the model.
#'   Must include the predictor specified in \code{predictor} and any covariates
#'   in the fitted model.
#' @param predictor A character string giving the name of the predictor variable
#'   to plot on the x-axis.
#' @param grid_length Integer; number of grid points to generate for continuous
#'   predictors. Ignored for categorical predictors.
#' @param fixed_values Optional named list giving fixed values for covariates other
#'   than \code{predictor}. If not supplied, numeric covariates are fixed at their
#'   median and factors at their most frequent level.
#' @param add Logical; if \code{TRUE}, adds the curve to an existing plot instead of
#'   creating a new one.
#' @param line_col Color for the regression line.
#' @param line_lwd Line width for the regression line.
#' @param line_type Line type for the regression line.
#' @param point_pch Plotting symbol to use for points (categorical predictors).
#' @param point_cex Size of the plotting symbols for points (categorical predictors).
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{points}}, or \code{\link[graphics]{lines}}.
#'
#' @return Invisibly returns a \code{data.frame} with:
#'   \item{predictor}{The sequence or factor levels of the predictor.}
#'   \item{predicted}{The predicted quantile values.}
#'   \item{quantile}{The tau value used (first element of \code{object$quantile}).}
#'
#' @examples
#' \dontrun{
#' fit <- mo.bqr.svy(y ~ x, data = mydata, quantile = c(0.25, 0.5, 0.75))
#' plot_quantile.mo.bqr.svy(fit, data = mydata, predictor = "x")
#' }
#'
#' @export

plot_quantile.mo.bqr.svy <- function(object, data, predictor,
                                     grid_length = 100,
                                     fixed_values = NULL,
                                     add = FALSE,
                                     line_col = "red",
                                     line_lwd = 2,
                                     line_type = 1,
                                     point_pch = 19,
                                     point_cex = 1.2,
                                     main = NULL,
                                     ...) {
  if (!inherits(object, "mo.bqr.svy"))
    stop("Object must be of class 'mo.bqr.svy'.")
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")
  if (!predictor %in% names(data))
    stop("Predictor not found in data.")

  formula_obj <- if (!is.null(object$formula)) object$formula else object$call$formula
  all_vars <- all.vars(formula_obj)

  if (is.null(main)) {
    main <- paste("Multiple-Output Quantile Regression (tau =", object$quantile, ") vs", predictor)
  }

  # Predictor values
  pred_is_cat <- is.factor(data[[predictor]]) || is.character(data[[predictor]])
  if (pred_is_cat) {
    pred_values <- if (is.factor(data[[predictor]]))
      levels(data[[predictor]]) else unique(as.character(data[[predictor]]))
  } else {
    rng <- range(data[[predictor]], na.rm = TRUE)
    pred_values <- seq(rng[1], rng[2], length.out = grid_length)
  }

  # newdata
  newdata <- data.frame(row.names = seq_along(pred_values))
  newdata[[predictor]] <- if (pred_is_cat)
    factor(pred_values, levels = levels(data[[predictor]])) else pred_values

  # Fill other variables
  for (v in setdiff(all_vars[-1], predictor)) {
    if (!v %in% names(data)) next
    colv <- data[[v]]
    if (!is.null(fixed_values) && v %in% names(fixed_values)) {
      newdata[[v]] <- rep(fixed_values[[v]], nrow(newdata))
    } else if (is.numeric(colv)) {
      newdata[[v]] <- rep(median(colv, na.rm = TRUE), nrow(newdata))
    } else {
      most_freq <- names(sort(table(colv), decreasing = TRUE))[1]
      newdata[[v]] <- if (is.factor(colv))
        factor(rep(most_freq, nrow(newdata)), levels = levels(colv))
      else rep(most_freq, nrow(newdata))
    }
  }

  # Get beta_hat (use first quantile by default)
  if (!is.null(object$coefficients)) {
    beta_hat <- as.numeric(object$coefficients)
    names(beta_hat) <- names(object$coefficients)
  } else if (!is.null(object$fit) && length(object$fit) >= 1 &&
             !is.null(object$fit[[1]]$beta)) {
    beta_hat <- as.numeric(object$fit[[1]]$beta)
  } else {
    stop("No coefficients found in 'mo.bqr.svy' object.")
  }

  terms_obj <- tryCatch(terms(formula_obj), error = function(e) NULL)
  if (is.null(terms_obj)) stop("No terms found to build model matrix.")

  X_new <- model.matrix(delete.response(terms_obj), data = newdata)

  # Align beta_hat with X_new columns
  if (!is.null(names(beta_hat)) && all(colnames(X_new) %in% names(beta_hat))) {
    beta_hat <- beta_hat[colnames(X_new)]
  } else if (length(beta_hat) != ncol(X_new)) {
    if (length(beta_hat) > ncol(X_new)) {
      beta_hat <- beta_hat[seq_len(ncol(X_new))]
    } else {
      beta_hat <- c(beta_hat, rep(0, ncol(X_new) - length(beta_hat)))
    }
  }

  y_pred <- as.vector(X_new %*% beta_hat)

  # Plot
  if (!add) {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      plot(xx, y_pred, type = "p", pch = point_pch, cex = point_cex, col = line_col,
           xlab = predictor, ylab = bquote(tau == .(object$quantile)),
           xaxt = "n", main = main, ...)
      axis(1, at = xx, labels = pred_values)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      plot(pred_values, y_pred, type = "l", col = line_col, lwd = line_lwd, lty = line_type,
           xlab = predictor, ylab = bquote(tau == .(object$quantile)),
           main = main, ...)
    }
    grid()
  } else {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      points(xx, y_pred, col = line_col, pch = point_pch, cex = point_cex)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      lines(pred_values, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    }
  }

  invisible(data.frame(predictor = pred_values, predicted = y_pred,
                       quantile = object$quantile[1]))
}

#' Plot observed points and predicted quantile regression curve for \code{mo.bqr.svy}
#'
#' @param object An object of class \code{mo.bqr.svy}.
#' @param data A data frame containing the variables used in the model.
#' @param predictor Character string with the name of the predictor variable.
#' @param main Main title for the plot.
#' @param ... Additional arguments passed to \code{plot_quantile.mo.bqr.svy}.
#' @export
plot_quantile_with_points.mo.bqr.svy <- function(object, data, predictor,
                                                 main = NULL, ...) {
  if (!inherits(object, "mo.bqr.svy"))
    stop("Object must be of class 'mo.bqr.svy'.")
  response_var <- all.vars(object$formula)[1]

  if (is.null(main)) {
    main <- paste("Multiple-Output Quantile Regression (tau =", object$quantile[1], ") vs", predictor)
  }

  plot(data[[predictor]], data[[response_var]],
       col = adjustcolor("steelblue", 0.4),
       pch = 16, xlab = predictor, ylab = response_var,
       main = main)
  grid()
  plot_quantile.mo.bqr.svy(object, data, predictor, add = TRUE, ...)
}

#' Plot method for \code{mo.bqr.svy} objects
#'
#' @param x An object of class \code{mo.bqr.svy}.
#' @param type Type of plot: \code{"quantiles"} or \code{"convergence"}.
#' @param ... Additional plotting arguments.
#' @export
plot.mo.bqr.svy <- function(x, type = c("quantiles", "convergence"), ...) {
  type <- match.arg(type)

  if (type == "quantiles") {
    # Robust: do NOT require x$data or x$formula.
    if (is.null(x$fit) || length(x$fit) == 0L)
      stop("No fitted quantiles found in object.")

    # number of coefficients
    n_coef <- length(x$fit[[1]]$beta)
    if (is.null(n_coef) || n_coef < 1L)
      stop("No coefficient vectors found in 'fit' elements.")

    op <- par(no.readonly = TRUE); on.exit(par(op))
    n_cols <- ceiling(sqrt(n_coef))
    n_rows <- ceiling(n_coef / n_cols)
    par(mfrow = c(n_rows, n_cols), mar = c(4.5, 4.5, 3, 1.5), oma = c(0, 0, 2, 0))

    for (j in seq_len(n_coef)) {
      estimates <- sapply(x$fit, function(fit) fit$beta[j])
      coef_expr <- if (j == 1) expression(beta[0]) else substitute(beta[i], list(i = j - 1))
      plot(x$quantile, estimates, type = "o", pch = 16,
           xlab = expression(tau), ylab = "Coefficient",
           main = coef_expr, ...)
      grid()
    }
    mtext("Coefficient estimates across quantiles", outer = TRUE, cex = 1.2, font = 2)

  } else if (type == "convergence") {
    if (is.null(x$fit) || length(x$fit) == 0L)
      stop("No fit data for convergence plot.")
    iterations <- sapply(x$fit, function(fit) fit$iter)
    converged  <- sapply(x$fit, function(fit) isTRUE(fit$converged))

    plot(x$quantile, iterations, type = "o",
         col = ifelse(converged, "#2E8B57", "#DC143C"),
         pch = ifelse(converged, 16, 17), lwd = 2,
         xlab = expression(paste("Quantile (", tau, ")")),
         ylab = "Number of iterations",
         main = "EM convergence by quantile", ...)
    grid()
    legend("topright",
           legend = c("Converged", "Not converged"),
           col = c("#2E8B57", "#DC143C"),
           pch = c(16, 17), bty = "n")
  }
}

