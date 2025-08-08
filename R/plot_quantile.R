# =============================================
# PLOT FUNCTIONS FOR QUANTILE REGRESSION
# =============================================

#' Plot Quantile Regression for Survey Data
#'
#' This function creates a plot showing the quantile regression curve for a 
#' specified predictor variable from a Bayesian quantile regression model 
#' fitted to survey data.
#'
#' @param object An object of class 'bqr.svy' containing the fitted quantile regression model
#' @param data A data frame containing the original data used to fit the model
#' @param predictor A character string specifying the name of the predictor variable to plot
#' @param quantile_select Deprecated parameter (for backward compatibility)
#' @param grid_length Integer specifying the number of points for the prediction grid (default: 100)
#' @param fixed_values A named list specifying fixed values for other predictors in the model
#' @param add Logical indicating whether to add the curve to an existing plot (default: FALSE)
#' @param line_col Color for the quantile regression line (default: "red")
#' @param line_lwd Line width for the quantile regression line (default: 2)
#' @param line_type Line type for the quantile regression line (default: 1)
#' @param point_pch Point character for categorical predictors (default: 19)
#' @param point_cex Point size for categorical predictors (default: 1.2)
#' @param ... Additional arguments passed to the plot function
#'
#' @return Invisibly returns a data frame with predictor values and predicted quantiles
#'
#' @details
#' The function creates predictions for the specified quantile across the range of 
#' the predictor variable. For other variables in the model, it uses either:
#' \itemize{
#'   \item Values specified in \code{fixed_values}
#'   \item Median for numeric variables
#'   \item Mode (most frequent value) for categorical variables
#' }
#'
#' @examples
#' \dontrun{
#' # Simulate survey data
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
#'                     data = apistrat, fpc = ~fpc)
#' 
#' # Fit quantile regression
#' model <- bqr.svy(api00 ~ ell + meals, design = dstrat, quantile = 0.5)
#' 
#' # Basic usage
#' plot_quantile.bqr.svy(model, data = apistrat, predictor = "ell")
#' 
#' # With fixed values for other predictors
#' plot_quantile.bqr.svy(model, data = apistrat, predictor = "ell", 
#'                       fixed_values = list(meals = 50))
#' 
#' # Add to existing plot
#' plot(apistrat$ell, apistrat$api00, col = "gray")
#' plot_quantile.bqr.svy(model, data = apistrat, predictor = "ell", add = TRUE)
#' }
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{mo.bqr.svy}}
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
                                  ...) {
  
  # Input validation
  if (!inherits(object, "bqr.svy")) {
    stop("Object must be of class 'bqr.svy'")
  }
  if (!predictor %in% names(data)) {
    stop("Predictor '", predictor, "' not found in data")
  }

  # Extract posterior mean coefficients
  coefs <- apply(object$draws, 2, mean, na.rm = TRUE)
  formula_obj <- object$call$formula
  var_names <- all.vars(formula_obj)
  predictor_vars <- var_names[-1]

  # Create prediction grid for main predictor
  if (is.factor(data[[predictor]]) || is.character(data[[predictor]])) {
    pred_values <- unique(data[[predictor]])
    is_categorical <- TRUE
  } else {
    pred_range <- range(data[[predictor]], na.rm = TRUE)
    pred_values <- seq(pred_range[1], pred_range[2], length.out = grid_length)
    is_categorical <- FALSE
  }

  # Create new data frame for predictions
  newdata <- data.frame(row.names = seq_along(pred_values))
  newdata[[predictor]] <- pred_values

  # Set values for other predictors
  for (var in predictor_vars) {
    if (var != predictor) {
      if (!is.null(fixed_values) && var %in% names(fixed_values)) {
        # Use user-specified fixed value
        newdata[[var]] <- rep(fixed_values[[var]], nrow(newdata))
      } else if (is.numeric(data[[var]])) {
        # Use median for numeric variables
        newdata[[var]] <- rep(median(data[[var]], na.rm = TRUE), nrow(newdata))
      } else {
        # Use mode (most frequent value) for categorical variables
        tab <- table(data[[var]])
        most_frequent <- names(tab)[which.max(tab)]
        
        if (is.factor(data[[var]])) {
          newdata[[var]] <- factor(rep(most_frequent, nrow(newdata)),
                                   levels = levels(data[[var]]))
        } else {
          newdata[[var]] <- rep(most_frequent, nrow(newdata))
        }
      }
    }
  }

  # Create design matrix (without response variable)
  terms_no_y <- delete.response(terms(formula_obj))
  X_new <- model.matrix(terms_no_y, data = newdata)

  # Verify dimensions match
  if (ncol(X_new) != length(coefs)) {
    stop("Mismatch between design matrix (", ncol(X_new), 
         ") and coefficients (", length(coefs), ")")
  }

  # Calculate predictions
  y_pred <- as.vector(X_new %*% coefs)

  # Create plot or add to existing plot
  if (!add) {
    # Create new plot
    if (is_categorical) {
      plot(seq_along(pred_values), y_pred, 
           type = "p", pch = point_pch, cex = point_cex, col = line_col,
           xlab = predictor, 
           ylab = paste("Quantile", object$quantile),
           main = paste("Quantile Regression Curve (tau =", object$quantile, ")"),
           xaxt = "n", ...)
      axis(1, at = seq_along(pred_values), labels = pred_values)
      
      # Connect points with lines
      lines(seq_along(pred_values), y_pred, 
            col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      plot(pred_values, y_pred, 
           type = "l", lwd = line_lwd, col = line_col, lty = line_type,
           xlab = predictor, 
           ylab = paste("Quantile", object$quantile),
           main = paste("Quantile Regression Curve (tau =", object$quantile, ")"), ...)
    }
    grid(col = "lightgray", lty = "dotted")
    
  } else {
    # Add to existing plot
    if (is_categorical) {
      points(seq_along(pred_values), y_pred, 
             col = line_col, pch = point_pch, cex = point_cex)
      lines(seq_along(pred_values), y_pred, 
            col = line_col, lwd = line_lwd, lty = line_type)
    } else {
      lines(pred_values, y_pred, 
            col = line_col, lwd = line_lwd, lty = line_type)
    }
  }

  # Return prediction data invisibly
  invisible(data.frame(
    predictor = if (is_categorical) seq_along(pred_values) else pred_values,
    predictor_labels = pred_values,
    predicted = y_pred,
    quantile = object$quantile,
    is_categorical = is_categorical
  ))
}

#' Plot Quantile Regression with Data Points
#'
#' This function creates a combined plot showing both the data points (scatter plot)
#' and the quantile regression curve on the same graph.
#'
#' @param object An object of class 'bqr.svy' containing the fitted quantile regression model
#' @param data A data frame containing the original data used to fit the model
#' @param predictor A character string specifying the name of the predictor variable to plot
#' @param point_color Color for the data points (default: "steelblue")
#' @param point_alpha Transparency level for data points, between 0 and 1 (default: 0.4)
#' @param point_size Size of the data points (default: 0.8)
#' @param line_color Color for the quantile regression line (default: "red")
#' @param line_width Width of the quantile regression line (default: 3)
#' @param line_type Type of the quantile regression line (default: 1)
#' @param add_legend Logical indicating whether to add a legend (default: TRUE)
#' @param legend_pos Position of the legend (default: "topleft")
#' @param grid_on Logical indicating whether to add grid lines (default: TRUE)
#' @param jitter_factor Jittering factor for categorical variables (default: 0.3)
#' @param fixed_values A named list specifying fixed values for other predictors
#' @param ... Additional arguments passed to the plot function
#'
#' @return Invisibly returns the result from plot_quantile.bqr.svy
#'
#' @details
#' This function first creates a scatter plot of the data points, then overlays
#' the quantile regression curve using the plot_quantile.bqr.svy function with add=TRUE.
#' For categorical predictors, points are jittered horizontally for better visualization.
#'
#' @examples
#' \dontrun{
#' # Simulate survey data
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
#'                     data = apistrat, fpc = ~fpc)
#' 
#' # Fit quantile regression
#' model <- bqr.svy(api00 ~ ell + meals, design = dstrat, quantile = 0.5)
#' 
#' # Basic usage
#' plot_quantile_with_points(model, data = apistrat, predictor = "ell")
#' 
#' # Customize appearance
#' plot_quantile_with_points(model, data = apistrat, predictor = "ell",
#'                          point_color = "navy", line_color = "red",
#'                          point_alpha = 0.6, line_width = 4)
#' }
#'
#' @seealso \code{\link{plot_quantile.bqr.svy}}, \code{\link{bqr.svy}}
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
                                     ...) {
  
  # Input validation
  if (!inherits(object, "bqr.svy")) {
    stop("Object must be of class 'bqr.svy'")
  }
  if (!predictor %in% names(data)) {
    stop("Predictor '", predictor, "' not found in data")
  }
  
  # Get response variable name from formula
  formula_obj <- object$call$formula
  response_var <- all.vars(formula_obj)[1]
  
  if (!response_var %in% names(data)) {
    stop("Response variable '", response_var, "' not found in data")
  }
  
  # Check if predictor is categorical
  is_categorical <- is.factor(data[[predictor]]) || is.character(data[[predictor]])
  
  # STEP 1: Create base plot with data points
  if (is_categorical) {
    x_numeric <- as.numeric(as.factor(data[[predictor]]))
    plot(jitter(x_numeric, factor = jitter_factor), data[[response_var]], 
         col = adjustcolor(point_color, alpha.f = point_alpha),
         pch = 16, cex = point_size,
         xlab = predictor, ylab = response_var,
         main = paste0("Quantile Regression (tau = ", object$quantile, ")"),
         xaxt = "n", ...)
    axis(1, at = seq_along(unique(data[[predictor]])), 
         labels = unique(data[[predictor]]))
  } else {
    plot(data[[predictor]], data[[response_var]], 
         col = adjustcolor(point_color, alpha.f = point_alpha),
         pch = 16, cex = point_size,
         xlab = predictor, ylab = response_var,
         main = paste0("Quantile Regression (tau = ", object$quantile, ")"),
         ...)
  }
  
  # Add grid if requested
  if (grid_on) {
    grid(col = "lightgray", lty = "dotted")
  }
  
  # STEP 2: Add quantile regression curve to existing plot
  result <- plot_quantile.bqr.svy(
    object = object,
    data = data,
    predictor = predictor,
    fixed_values = fixed_values,
    add = TRUE,  # KEY: add to existing plot
    line_col = line_color,
    line_lwd = line_width,
    line_type = line_type
  )
  
  # STEP 3: Add legend if requested
  if (add_legend) {
    legend(legend_pos, 
           legend = c("Observed Data", 
                     paste0("Quantile tau = ", object$quantile)),
           col = c(adjustcolor(point_color, alpha.f = point_alpha), line_color),
           pch = c(16, if(result$is_categorical) 19 else NA),
           lty = c(NA, line_type),
           lwd = c(NA, line_width),
           bg = "white",
           cex = 0.9)
  }
  
  # Return result invisibly
  invisible(result)
}
