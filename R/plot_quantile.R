#' Plot Quantile Regression for Survey Data
#'
#' Draws the quantile regression curve for a given predictor from a
#' Bayesian quantile regression model fitted to survey data.
#'
#' @param object An object of class 'bqr.svy' containing the fitted model.
#' @param data A data frame with the original data used to fit the model.
#' @param predictor Character string with the name of the predictor to plot.
#' @param quantile_select Deprecated (kept for backward compatibility).
#' @param grid_length Integer, number of points in the prediction grid (default: 100).
#' @param fixed_values Named list with fixed values for other predictors.
#' @param add Logical; if TRUE, add the curve to an existing plot (default: FALSE).
#' @param line_col Color for the quantile curve (default: "red").
#' @param line_lwd Line width for the quantile curve (default: 2).
#' @param line_type Line type for the quantile curve (default: 1).
#' @param point_pch Point symbol for categorical predictors when creating a new plot (default: 19).
#' @param point_cex Point size for categorical predictors when creating a new plot (default: 1.2).
#' @param prefer_predict Logical; if TRUE, try using predict(object, newdata) first (default: FALSE).
#' @param ... Additional graphic arguments passed to base plotting functions.
#'
#' @return (Invisibly) a data.frame with predictor values and predicted quantiles.
#' @export
plot_quantile.bqr.svy <- function(object, data, predictor,
                                  quantile_select = NULL,
                                  grid_length = 100,
                                  fixed_values = NULL,
                                  add = FALSE,
                                  line_col = "red",
                                  line_lwd = 2,
                                  line_type = 1,
                                  point_pch = 19,
                                  point_cex = 1.2,
                                  prefer_predict = FALSE,
                                  ...) {
  if (!inherits(object, "bqr.svy")) stop("Object must be of class 'bqr.svy'.")
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!predictor %in% names(data)) stop("Predictor '", predictor, "' not found in 'data'.")
  
  formula_obj <- NULL
  
  if (!is.null(object$formula)) {
    formula_obj <- object$formula
  }
  else if (!is.null(object$call) && !is.null(object$call$formula)) {
    formula_obj <- tryCatch({
      f <- object$call$formula
      if (inherits(f, "formula")) f else as.formula(f)
    }, error = function(e) {
      eval(object$call$formula, envir = parent.frame())
    })
  }
  
  if (is.null(formula_obj) || !inherits(formula_obj, "formula"))
    stop("Could not obtain a valid formula from the model object.")
  
  all_vars <- all.vars(formula_obj)
  response_var <- all_vars[1]
  if (!response_var %in% names(data)) {
    stop("Response variable '", response_var, "' not found in 'data'.")
  }
  
  pred_is_cat <- is.factor(data[[predictor]]) || is.character(data[[predictor]])
  if (pred_is_cat) {
    if (is.factor(data[[predictor]])) {
      pred_values <- levels(data[[predictor]])
    } else {
      pred_values <- unique(as.character(data[[predictor]]))
    }
  } else {
    rng <- range(data[[predictor]], na.rm = TRUE)
    pred_values <- seq(rng[1], rng[2], length.out = grid_length)
  }
  
  newdata <- data.frame(row.names = seq_along(pred_values))
  if (pred_is_cat) {
    if (is.factor(data[[predictor]])) {
      newdata[[predictor]] <- factor(pred_values, levels = levels(data[[predictor]]))
    } else {
      newdata[[predictor]] <- pred_values
    }
  } else {
    newdata[[predictor]] <- pred_values
  }
  
  predictor_vars <- setdiff(all_vars[-1], predictor)
  for (v in predictor_vars) {
    if (!v %in% names(data)) next
    if (!is.null(fixed_values) && v %in% names(fixed_values)) {
      newdata[[v]] <- rep(fixed_values[[v]], nrow(newdata))
    } else {
      colv <- data[[v]]
      if (is.numeric(colv)) {
        newdata[[v]] <- rep(stats::median(colv, na.rm = TRUE), nrow(newdata))
      } else {
        tab <- sort(table(colv), decreasing = TRUE)
        most_freq <- names(tab)[1]
        if (is.factor(colv)) {
          newdata[[v]] <- factor(rep(most_freq, nrow(newdata)), levels = levels(colv))
        } else {
          newdata[[v]] <- rep(most_freq, nrow(newdata))
        }
      }
    }
  }
  
  y_pred <- NULL
  used_predict <- FALSE
  if (isTRUE(prefer_predict) && exists("predict.bqr.svy")) {
    y_try <- try(as.vector(predict(object, newdata = newdata)), silent = TRUE)
    if (!inherits(y_try, "try-error")) {
      y_pred <- y_try
      used_predict <- TRUE
    }
  }
  
  if (!used_predict) {
    if (is.null(object$draws)) {
      stop("No working 'predict' method and 'object$draws' not found: cannot compute predictions.")
    }
    
    X_new <- NULL
    
    if (!is.null(object$terms)) {
      terms_no_y <- delete.response(object$terms)
      X_new <- stats::model.matrix(terms_no_y, data = newdata)
    }
    else if (!is.null(object$model)) {
      terms_obj <- stats::terms(object$model)
      terms_no_y <- delete.response(terms_obj)
      X_new <- stats::model.matrix(terms_no_y, data = newdata)
    }
    else {
      X_new <- tryCatch({
        stats::model.matrix(formula_obj, data = newdata)
      }, error = function(e) {
        formula_str <- deparse(formula_obj)
        rhs <- sub(".*~\\s*", "", formula_str)
        formula_rhs <- as.formula(paste("~", rhs))
        stats::model.matrix(formula_rhs, data = newdata)
      })
    }
    
    n_coef_needed <- ncol(X_new)
    n_cols_draws <- ncol(object$draws)
    
    coef_names_needed <- colnames(X_new)
    
    if (!is.null(colnames(object$draws))) {
      coef_indices <- match(coef_names_needed, colnames(object$draws))
      
      if (any(is.na(coef_indices))) {
        coef_indices <- 1:n_coef_needed
      }
    } else {
      coef_indices <- 1:n_coef_needed
    }
    
    if (max(coef_indices) > n_cols_draws) {
      stop("Cannot identify regression coefficients in draws matrix. ",
           "Expected ", n_coef_needed, " coefficients but draws has ", 
           n_cols_draws, " columns.")
    }
    
    beta_draws <- object$draws[, coef_indices, drop = FALSE]
    beta_hat <- apply(beta_draws, 2, mean, na.rm = TRUE)
    
    if (length(beta_hat) != ncol(X_new)) {
      stop("Dimension mismatch after extraction: design matrix has ", ncol(X_new),
           " columns, but extracted ", length(beta_hat), " coefficients.")
    }
    
    y_pred <- as.vector(X_new %*% beta_hat)
  }
  
  if (!add) {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      plot(xx, y_pred,
           type = "p", pch = point_pch, cex = point_cex, col = line_col,
           xlab = predictor, ylab = bquote(paste("Quantile (", tau, " = ", .(object$quantile), ")")),
           xaxt = "n", ...)
      axis(1, at = xx, labels = pred_values)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      plot(pred_values, y_pred,
           type = "l", col = line_col, lwd = line_lwd, lty = line_type,
           xlab = predictor, ylab = bquote(paste("Quantile (", tau, " = ", .(object$quantile), ")")),
           ...)
    }
    grid(col = "lightgray", lty = "dotted")
  } else {
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      points(xx, y_pred, col = line_col, pch = point_pch, cex = point_cex)
      lines(xx, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      lines(pred_values, y_pred, col = line_col, lwd = line_lwd, lty = line_type)
    }
  }
  
  invisible(data.frame(
    predictor = if (pred_is_cat) seq_along(pred_values) else pred_values,
    predictor_labels = pred_values,
    predicted = y_pred,
    quantile = object$quantile,
    is_categorical = pred_is_cat
  ))
}

#' Plot Quantile Regression with Data Points
#'
#' Creates a scatter plot of the data and overlays the quantile regression curve.
#'
#' @inheritParams plot_quantile.bqr.svy
#' @param point_color Color for data points (default: "steelblue").
#' @param point_alpha Alpha for points, in [0, 1] (default: 0.4).
#' @param point_size Point size (default: 0.8).
#' @param line_color Color for the quantile curve (default: "red").
#' @param line_width Line width for the quantile curve (default: 3).
#' @param line_type Line type for the quantile curve (default: 1).
#' @param add_legend Logical; whether to add a legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "topleft").
#' @param grid_on Logical; whether to add grid lines (default: TRUE).
#' @param jitter_factor Jitter factor for categorical predictors (default: 0.3).
#' @param main Optional main title for the base scatter plot (default: NULL).
#'
#' @return (Invisibly) the data.frame returned by \code{plot_quantile.bqr.svy}.
#' @export
plot_quantile_with_points <- function(object, data, predictor,
                                      point_color = "steelblue",
                                      point_alpha = 0.4,
                                      point_size = 0.8,
                                      line_color = "red",
                                      line_width = 3,
                                      line_type = 1,
                                      add_legend = TRUE,
                                      legend_pos = "topleft",
                                      grid_on = TRUE,
                                      jitter_factor = 0.3,
                                      fixed_values = NULL,
                                      main = NULL,
                                      prefer_predict = FALSE,
                                      ...) {
  if (!inherits(object, "bqr.svy")) stop("Object must be of class 'bqr.svy'.")
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!predictor %in% names(data)) stop("Predictor '", predictor, "' not found in 'data'.")
  
  formula_obj <- NULL
  if (!is.null(object$formula)) {
    formula_obj <- object$formula
  } else if (!is.null(object$call) && !is.null(object$call$formula)) {
    formula_obj <- tryCatch({
      f <- object$call$formula
      if (inherits(f, "formula")) f else as.formula(f)
    }, error = function(e) {
      eval(object$call$formula, envir = parent.frame())
    })
  }
  
  if (is.null(formula_obj) || !inherits(formula_obj, "formula"))
    stop("Could not obtain a valid formula from the model object.")
  
  response_var <- all.vars(formula_obj)[1]
  if (!response_var %in% names(data)) {
    stop("Response variable '", response_var, "' not found in 'data'.")
  }
  
  is_categorical <- is.factor(data[[predictor]]) || is.character(data[[predictor]])
  if (is_categorical) {
    labs <- if (is.factor(data[[predictor]])) levels(data[[predictor]])
    else unique(as.character(data[[predictor]]))
    x_numeric <- match(as.character(data[[predictor]]), labs)
    plot(jitter(x_numeric, factor = jitter_factor), data[[response_var]],
         col = adjustcolor(point_color, alpha.f = point_alpha),
         pch = 16, cex = point_size,
         xlab = predictor, ylab = response_var,
         main = main,
         xaxt = "n", ...)
    axis(1, at = seq_along(labs), labels = labs)
  } else {
    plot(data[[predictor]], data[[response_var]],
         col = adjustcolor(point_color, alpha.f = point_alpha),
         pch = 16, cex = point_size,
         xlab = predictor, ylab = response_var,
         main = main,
         ...)
  }
  
  if (grid_on) grid(col = "lightgray", lty = "dotted")
  
  res <- plot_quantile.bqr.svy(
    object = object,
    data = data,
    predictor = predictor,
    fixed_values = fixed_values,
    add = TRUE,
    line_col = line_color,
    line_lwd = line_width,
    line_type = line_type,
    prefer_predict = prefer_predict
  )
  
  if (add_legend) {
    legend(legend_pos,
           legend = c("Observed data", bquote(paste("Quantile ", tau, " = ", .(object$quantile)))),
           col = c(adjustcolor(point_color, alpha.f = point_alpha), line_color),
           pch = c(16, NA),
           lty = c(NA, line_type),
           lwd = c(NA, line_width),
           pt.cex = c(point_size, NA),
           bty = "n", bg = "white", cex = 0.9)
  }
  
  invisible(res)
}