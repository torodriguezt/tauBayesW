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

#' 3D quantile body for \code{mo.bqr.svy} (d = 3)
#'
#' Draws the directional quantile body in \eqn{\mathbb{R}^3} for a fitted
#' \code{mo.bqr.svy} model at a fixed covariate configuration \eqn{X_0}.
#' For each direction \eqn{U_{\cdot k}} on the unit sphere the function computes
#' the directional quantile \eqn{r_k = X_0^\top \hat\beta_{\tau,k}} (or a common
#' \eqn{\hat\beta_\tau} in \code{em_mode="joint"}) and plots the points
#' \eqn{r_k U_{\cdot k}}. The convex hull of those points is rendered as a mesh.
#'
#' @param object A fitted object of class \code{mo.bqr.svy} with \code{nrow(object$U) == 3}.
#' @param tau Numeric scalar. Quantile level to display. Defaults to the first
#'   element in \code{object$quantile}.
#' @param data Optional \code{data.frame} from which sensible defaults for
#'   \code{fixed_values} (medians/modes) are derived to build \eqn{X_0}.
#' @param fixed_values Optional named \code{list} giving values for covariates
#'   (matching the model formula) to build \eqn{X_0}. Overrides values inferred
#'   from \code{data}. Example: \code{list(x1 = 0, x2 = 1)}.
#' @param dirs Optional integer vector with a subset of directions \eqn{k} to plot.
#'   By default all directions are used.
#' @param engine Character, \code{"plotly"} (default) for interactive plot or
#'   \code{"rgl"} for an OpenGL window.
#' @param col Color for the mesh (single color). Default \code{"#D1495B"}.
#' @param opacity Mesh opacity in \code{[0,1]}. Default \code{0.55}.
#' @param show_points Logical; if \code{TRUE}, also plots the vertex cloud.
#' @param z_by_dir Optional list or matrix with direction-specific “gamma inputs”
#'   when using \code{em_mode="joint"} and your specification includes directional
#'   \eqn{\gamma_{k,c}} terms. If provided, it adds \eqn{\sum_c \gamma_{k,c} z_{k,c}}
#'   to each radius \eqn{r_k}. Leave \code{NULL} (default) if you used \code{r = 0}.
#'
#' @return A \code{plotly} object (when \code{engine="plotly"}) or \code{NULL}
#'   (when \code{engine="rgl"}) invisibly.
#'
#' @details
#' The function assumes a linear predictor in the X-part. In
#' \code{em_mode="separable"}, the X-coefficients are direction-specific and taken
#' from \code{fit[[qi]]$beta_dir}. In \code{em_mode="joint"}, a single X-coefficient
#' vector is used for all directions. If your fit used \code{r = 0} (no \eqn{\Gamma}),
#' you can ignore \code{z_by_dir}.
#'
#' Column-name alignment is attempted between the model matrix columns and the
#' stored coefficients; unnamed coefficients fall back to column order.
#'
#' @section Dependencies:
#' Requires \pkg{plotly} for interactive rendering and \pkg{geometry} for the
#' convex hull (\code{convhulln}). With \code{engine="rgl"}, requires \pkg{rgl}.
#'
#' @examples
#' \dontrun{
#' # fit3d <- mo.bqr.svy(cbind(y1,y2,y3) ~ x1 + x2, data=mydata,
#' #                     quantile=0.5, algorithm="em", n_dir=60, r=0)
#' plot_quantile_body3d.mo.bqr.svy(fit3d,
#'   tau = 0.5, data = mydata, fixed_values = list(x1 = 0, x2 = 0),
#'   engine = "plotly", opacity = 0.6, show_points = TRUE)
#' }
#'
#' @importFrom stats terms model.matrix delete.response median
#' @export
plot_quantile_body3d.mo.bqr.svy <- function(object,
                                            tau = NULL,
                                            data = NULL,
                                            fixed_values = NULL,
                                            dirs = NULL,
                                            engine = c("plotly","rgl"),
                                            col = "#D1495B",
                                            opacity = 0.55,
                                            show_points = TRUE,
                                            z_by_dir = NULL) {
  engine <- match.arg(engine)
  if (!inherits(object, "mo.bqr.svy"))
    stop("object must be class 'mo.bqr.svy'.")
  U <- object$U
  if (is.null(U) || !is.matrix(U) || nrow(U) != 3)
    stop("This plot requires d = 3 (nrow(object$U) == 3).")
  K <- ncol(U)

  # Normaliza columnas de U (||u_k|| = 1)
  U <- apply(U, 2, function(v) { s <- sqrt(sum(v^2)); if (s == 0) v else v/s })

  # ---- seleccionar cuantil ----
  taus <- object$quantile
  if (is.null(taus) || !length(taus)) stop("No quantile levels in object.")
  if (is.null(tau)) tau <- taus[1]
  qi <- which.min(abs(taus - tau))
  tau <- taus[qi]

  # ---- construir X0 (una fila) ----
  coef_names <- names(object$coefficients)
  form <- if (!is.null(object$formula)) object$formula else object$call$formula

  X0 <- NULL; xnames <- NULL
  # Intento 1: armar a partir de formula + data (si hay) + fixed_values
  try({
    trm  <- stats::terms(form)
    newdata <- list()
    if (!is.null(data)) {
      for (v in all.vars(form)[-1]) {
        if (!v %in% names(data)) next
        colv <- data[[v]]
        if (!is.null(fixed_values) && v %in% names(fixed_values)) {
          newdata[[v]] <- fixed_values[[v]]
        } else if (is.numeric(colv)) {
          newdata[[v]] <- stats::median(colv, na.rm = TRUE)
        } else {
          mf <- names(sort(table(colv), decreasing = TRUE))[1]
          newdata[[v]] <- if (is.factor(colv)) factor(mf, levels = levels(colv)) else mf
        }
      }
    } else if (!is.null(fixed_values)) {
      newdata <- fixed_values
    }
    newdata <- as.data.frame(newdata, optional = TRUE)
    X0 <- stats::model.matrix(stats::delete.response(trm), data = newdata)
    xnames <- colnames(X0)
  }, silent = TRUE)

  # Intento 2 (fallback): si no se pudo, usar nombres de coeficientes:
  if (is.null(X0) || is.null(xnames) || ncol(X0) == 0) {
    if (is.null(coef_names) || !length(coef_names))
      stop("Couldn't build X0; provide 'fixed_values' or fit should expose coefficient names.")
    xnames <- coef_names
    X0 <- matrix(0, nrow = 1, ncol = length(xnames))
    colnames(X0) <- xnames
    if ("(Intercept)" %in% xnames) X0[, "(Intercept)"] <- 1
    if (!is.null(fixed_values)) {
      for (nm in names(fixed_values)) if (nm %in% xnames) X0[, nm] <- fixed_values[[nm]]
    }
  }

  # ---- direcciones a usar ----
  if (is.null(dirs)) dirs <- seq_len(K)
  dirs <- as.integer(dirs[dirs >= 1 & dirs <= K])
  if (!length(dirs)) stop("No valid directions selected.")
  Usel <- U[, dirs, drop = FALSE]

  # ---- radios r_k ----
  r_vec <- numeric(length(dirs))
  if (identical(object$mode, "separable")) {
    beta_dir <- object$fit[[qi]]$beta_dir
    if (is.null(beta_dir))
      stop("No 'beta_dir' found. Was the model fitted with em_mode='separable'?")
    # alineación por nombres si existen
    if (!is.null(colnames(beta_dir))) {
      bx <- matrix(0, nrow = nrow(beta_dir), ncol = ncol(X0))
      colnames(bx) <- colnames(X0)
      common <- intersect(colnames(beta_dir), colnames(X0))
      if (length(common)) bx[, common] <- beta_dir[, common, drop = FALSE]
    } else {
      pX <- min(ncol(beta_dir), ncol(X0))
      bx <- matrix(0, nrow = nrow(beta_dir), ncol = ncol(X0))
      colnames(bx) <- colnames(X0)
      bx[, seq_len(pX)] <- beta_dir[, seq_len(pX), drop = FALSE]
    }
    bx <- bx[dirs, , drop = FALSE]
    for (i in seq_along(dirs)) {
      r_vec[i] <- as.numeric(X0 %*% matrix(bx[i, , drop = FALSE], ncol = 1))
    }
  } else { # joint
    b <- object$fit[[qi]]$beta
    if (is.null(b)) stop("No 'beta' found. Was the model fitted with em_mode='joint'?")
    if (!is.null(names(b))) {
      bx <- numeric(ncol(X0)); names(bx) <- colnames(X0)
      common <- intersect(names(b), colnames(X0))
      if (length(common)) bx[common] <- b[common]
    } else {
      bx <- b[seq_len(ncol(X0))]
      names(bx) <- colnames(X0)
    }
    rk <- as.numeric(X0 %*% matrix(bx, ncol = 1))
    r_vec[] <- rk

    # offset por gammas si el usuario lo provee (con nombres tipo gamma_k*_c*)
    if (!is.null(z_by_dir) && !is.null(names(b))) {
      for (ii in seq_along(dirs)) {
        k <- dirs[ii]
        pat <- paste0("^gamma_k", k, "_c")
        gk  <- b[grep(pat, names(b), perl = TRUE)]
        zk  <- if (is.list(z_by_dir)) z_by_dir[[k]] else
          if (is.matrix(z_by_dir)) z_by_dir[k, seq_along(gk), drop = TRUE] else NULL
        if (!is.null(zk) && length(zk) == length(gk))
          r_vec[ii] <- r_vec[ii] + sum(as.numeric(gk) * as.numeric(zk))
      }
    }
  }

  # ---- puntos cartesianos: r_k * U_k ----
  pts <- sweep(Usel, 2, r_vec, `*`)  # 3 x |dirs|
  Xv <- as.numeric(pts[1,]); Yv <- as.numeric(pts[2,]); Zv <- as.numeric(pts[3,])

  # ====== Hull (robusto) ======
  if (engine == "plotly" && !requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required.", call. = FALSE)
  if (!requireNamespace("geometry", quietly = TRUE))
    stop("Package 'geometry' is required.", call. = FALSE)

  pts_mat <- unique(cbind(Xv, Yv, Zv))
  need_only_points <- nrow(pts_mat) < 4

  # si casi degenerado, jitter minúsculo
  if (!need_only_points) {
    s <- svd(scale(pts_mat, scale = FALSE))
    rank_est <- sum(s$d > (max(s$d) * .Machine$double.eps * 50))
    if (rank_est < 3) {
      rng <- apply(pts_mat, 2, function(v) diff(range(v)))
      eps <- pmax(rng, 1) * 1e-8
      set.seed(1)
      pts_mat <- pts_mat + sweep(matrix(rnorm(length(pts_mat), 0, 1), ncol = 3), 2, eps, `*`)
    }
  }

  hull_ok <- FALSE; tri <- NULL
  if (!need_only_points) {
    hull_try <- try(geometry::convhulln(pts_mat, options = "Qt QJ"), silent = TRUE)
    if (!inherits(hull_try, "try-error")) {
      tri <- t(hull_try)
      hull_ok <- TRUE
    }
  }

  # ---- nombres de respuestas (para pintar datos si 'data' no es NULL) ----
  resp_names <- NULL
  if (!is.null(data)) {
    # extrae LHS de la fórmula; maneja cbind(y1,y2,y3)
    lhs <- tryCatch(form[[2]], error = function(e) NULL)
    if (!is.null(lhs)) {
      vars_lhs <- setdiff(all.vars(lhs), "cbind")
      if (length(vars_lhs) == 3 && all(vars_lhs %in% names(data))) {
        resp_names <- vars_lhs
      }
    }
  }

  # ====== Render ======
  if (engine == "plotly") {
    plt <- plotly::plot_ly()
    if (hull_ok) {
      plt <- plotly::add_mesh(
        plt, x = pts_mat[,1], y = pts_mat[,2], z = pts_mat[,3],
        i = tri[1,]-1, j = tri[2,]-1, k = tri[3,]-1,
        color = I(col), opacity = opacity, name = paste0("tau=", tau)
      )
    }
    if (isTRUE(show_points) || !hull_ok) {
      plt <- plotly::add_markers(
        plt, x = pts_mat[,1], y = pts_mat[,2], z = pts_mat[,3],
        marker = list(size = 3, opacity = 0.9, color = if (hull_ok) "#333333" else col),
        name = if (hull_ok) "vertices" else paste0("tau=", tau, " (points)")
      )
    }
    # --- pintar datos observados si 'data' fue provista ---
    if (!is.null(resp_names)) {
      Yobs <- data[, resp_names]
      plt <- plotly::add_markers(
        plt,
        x = Yobs[[1]], y = Yobs[[2]], z = Yobs[[3]],
        marker = list(size = 3, color = "steelblue", opacity = 0.55),
        name   = "observed data"
      )
    }

    plt <- plotly::layout(
      plt,
      scene = list(xaxis = list(title = if (!is.null(resp_names)) resp_names[1] else "y1"),
                   yaxis = list(title = if (!is.null(resp_names)) resp_names[2] else "y2"),
                   zaxis = list(title = if (!is.null(resp_names)) resp_names[3] else "y3")),
      title = paste0("Quantile body (d=3), tau = ", tau)
    )
    return(plt)

  } else { # rgl
    if (!requireNamespace("rgl", quietly = TRUE))
      stop("Package 'rgl' is required for engine = 'rgl'.", call. = FALSE)
    if (hull_ok) {
      rgl::tmesh3d(vertices = t(pts_mat),
                   indices  = t(tri),
                   homogeneous = FALSE) |>
        rgl::shade3d(color = col, alpha = opacity)
    }
    if (isTRUE(show_points) || !hull_ok) {
      rgl::points3d(pts_mat, col = if (hull_ok) 1 else col, size = 5)
    }
    if (!is.null(resp_names)) {
      Yobs <- data[, resp_names]
      rgl::points3d(as.matrix(Yobs), col = "steelblue", size = 4)
    }
    invisible(NULL)
  }
}
