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

# =============================================================================
# Unified Plot Interface for tauBayesW
# =============================================================================

#' Unified Plot Method for tauBayesW Objects
#'
#' This is a unified interface for plotting objects from the tauBayesW package.
#' It automatically detects the type of object and its dimensionality, then applies 
#' the appropriate plotting method.
#'
#' @param x An object to plot. Can be of class \code{"bqr.svy"} or \code{"mo.bqr.svy"}.
#' @param data Data frame containing the variables for plotting (required for most plots).
#' @param predictor Character string specifying the predictor variable name (for 1D plots).
#' @param response Character vector specifying response variable name(s).
#'   For 1D: single variable name. For 2D: vector of 2 names. For 3D: vector of 3 names.
#' @param ... Additional arguments passed to specific plotting methods.
#'
#' @return Invisibly returns the plot object or NULL, depending on the plotting method used.
#'
#' @details
#' This function automatically chooses the appropriate plotting method based on:
#' \itemize{
#'   \item \strong{Object type}: \code{bqr.svy} vs \code{mo.bqr.svy}
#'   \item \strong{Response dimensionality}:
#'     \itemize{
#'       \item \strong{1D}: Uses \code{plot_quantile_1D()} for both object types
#'       \item \strong{2D}: Uses \code{drawQuantileRegion()} for \code{mo.bqr.svy}
#'       \item \strong{3D}: Uses \code{drawQuantileRegion_3D()} for \code{mo.bqr.svy}
#'     }
#' }
#'
#' For \code{bqr.svy} objects (always 1D response):
#' \itemize{
#'   \item Always uses \code{plot_quantile_1D()}
#'   \item Requires \code{data} and \code{predictor} arguments
#' }
#'
#' For \code{mo.bqr.svy} objects:
#' \itemize{
#'   \item \strong{1D response}: Uses \code{plot_quantile_1D()}
#'   \item \strong{2D response}: Uses \code{drawQuantileRegion()}
#'   \item \strong{3D response}: Uses \code{drawQuantileRegion_3D()}
#'   \item Dimensionality is detected from \code{x$response_dim} or \code{length(response)}
#' }
#'
#' @examples
#' \dontrun{
#' # 1D examples (works for both bqr.svy and mo.bqr.svy)
#' fit1 <- bqr.svy(y ~ x, data = mydata, quantile = 0.5)
#' plot(fit1, data = mydata, predictor = "x")
#'
#' fit2 <- mo.bqr.svy(y ~ x, data = mydata, quantile = c(0.25, 0.75))  # 1D response
#' plot(fit2, data = mydata, predictor = "x", response = "y")
#'
#' # 2D example (mo.bqr.svy only)
#' fit3 <- mo.bqr.svy(cbind(y1, y2) ~ x, data = mydata, quantile = 0.5)
#' plot(fit3, data = mydata, response = c("y1", "y2"))
#'
#' # 3D example (mo.bqr.svy only)  
#' fit4 <- mo.bqr.svy(cbind(y1, y2, y3) ~ x, data = mydata, quantile = 0.5)
#' plot(fit4, data = mydata, response = c("y1", "y2", "y3"))
#' }
#'
#' @export
plot <- function(x, ...) {
  UseMethod("plot")
}

#' @rdname plot
#' @keywords internal
plot.bqr.svy <- function(x, data = NULL, datafile = NULL, predictor = NULL, 
                         response = NULL, x_var = NULL, ...) {
  # bqr.svy objects are always 1D, so use plot_quantile_1D
  
  # Handle backwards compatibility for argument names
  data <- data %||% datafile
  predictor <- predictor %||% x_var
  
  if (is.null(data)) {
    stop("'data' or 'datafile' argument is required for plotting bqr.svy objects.")
  }
  if (is.null(predictor)) {
    stop("'predictor' or 'x_var' argument is required for plotting bqr.svy objects.")
  }
  
  # Extract response name from formula if not provided
  if (is.null(response)) {
    # Try to extract from the object
    if (!is.null(x$terms)) {
      response_vars <- all.vars(x$terms)[1]  # First variable is usually the response
      response <- response_vars
    } else if (!is.null(x$formula)) {
      response_vars <- all.vars(x$formula)[1]
      response <- response_vars
    }
  }
  
  plot_quantile_1D(
    object = x,
    data = data,
    predictor = predictor,
    response = response,
    ...
  )
}

#' @rdname plot
#' @keywords internal
plot.mo.bqr.svy <- function(x, data = NULL, datafile = NULL, 
                            response = NULL, predictor = NULL, ...) {
  
  # Helper operator
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Determine data source (prioritize 'data' over 'datafile' for consistency)
  plot_data <- data %||% datafile
  if (is.null(plot_data)) {
    stop("Either 'data' or 'datafile' argument is required for plotting mo.bqr.svy objects.")
  }
  
  # Determine response dimensionality
  response_dim <- x$response_dim %||% length(response) %||% 1
  
  # Default response names if not provided
  if (is.null(response)) {
    if (response_dim == 1) {
      response <- "Y"  # Default for 1D
    } else if (response_dim == 2) {
      response <- c("Y1", "Y2")  # Default for 2D
    } else if (response_dim == 3) {
      response <- c("Y1", "Y2", "Y3")  # Default for 3D
    } else {
      stop("Cannot determine appropriate response variables for dimension ", response_dim, 
           ". Please specify 'response' argument.")
    }
  }
  
  # Validate response dimension consistency
  if (length(response) != response_dim) {
    warning("Length of 'response' (", length(response), 
            ") doesn't match object's response_dim (", response_dim, 
            "). Using response_dim from object.")
    response_dim <- x$response_dim
  }
  
  # Route to appropriate plotting function based on dimensionality
  if (response_dim == 1) {
    # Use 1D plotting
    if (is.null(predictor)) {
      stop("'predictor' argument is required for 1D plots.")
    }
    plot_quantile_1D(
      object = x,
      data = plot_data,
      predictor = predictor,
      response = response[1],  # Use first response name
      ...
    )
  } else if (response_dim == 2) {
    # Use 2D plotting
    drawQuantileRegion(
      fit = x,
      datafile = plot_data,
      response = response[1:2],  # Use first two response names
      print_plot = TRUE,
      ...
    )
  } else if (response_dim == 3) {
    # Use 3D plotting
    drawQuantileRegion_3D(
      fit = x,
      datafile = plot_data,
      response = response[1:3],  # Use first three response names
      print_plot = TRUE,
      ...
    )
  } else {
    stop("Plotting not supported for response dimension ", response_dim, 
         ". Supported dimensions: 1, 2, 3.")
  }
}

# =====================================================
# UNIFIED 1D QUANTILE PLOTTING
# =====================================================

#' Unified 1D Quantile Plotting for Bayesian Quantile Regression
#'
#' A unified interface for plotting univariate quantile regression curves from
#' fitted \code{bqr.svy} or \code{mo.bqr.svy} objects (with response_dim = 1).
#' This function automatically detects the model type and uses the appropriate
#' plotting method, providing a consistent interface for both model types.
#'
#' @param object A fitted quantile regression object of class \code{bqr.svy} or
#'   \code{mo.bqr.svy} (with response_dim = 1).
#' @param data A \code{data.frame} containing the variables used in the model.
#'   Must include the predictor specified in \code{predictor} and any covariates
#'   in the fitted model.
#' @param predictor A character string giving the name of the predictor variable
#'   to plot on the x-axis.
#' @param show_data Logical; if \code{TRUE}, shows the observed data points as a 
#'   scatter plot underneath the quantile curves.
#' @param response Character string giving the name of the response variable.
#'   For \code{bqr.svy} objects, this is extracted automatically from the formula.
#'   For \code{mo.bqr.svy} objects, specify the response column name.
#' @param grid_length Integer; number of grid points to generate for continuous
#'   predictors. Ignored for categorical predictors.
#' @param fixed_values Optional named list giving fixed values for covariates other
#'   than \code{predictor}. If not supplied, numeric covariates are fixed at their
#'   median and factors at their most frequent level.
#' @param use_ggplot Logical; if \code{TRUE}, uses ggplot2 for plotting,
#'   otherwise uses base R graphics. If \code{NULL} (default), automatically
#'   selects ggplot2 for multiple quantiles and base R for single quantiles.
#' @param paintedArea Logical; if \code{TRUE} and multiple quantiles are present,
#'   fills the area between extreme quantiles (only when \code{use_ggplot = TRUE}).
#' @param band_choice Character; for painted areas, either \code{"minmax"}
#'   (between min and max quantiles) or \code{"symmetric"} (between quantiles
#'   around 0.5). Only used when \code{use_ggplot = TRUE}.
#' @param line_col Color for the regression line(s). For multiple quantiles, can be
#'   a vector of colors or a single color (recycled).
#' @param line_lwd Line width for the regression line.
#' @param line_type Line type for the regression line.
#' @param data_col Color for the observed data points (when \code{show_data = TRUE}).
#' @param data_alpha Transparency for the observed data points (0-1, when \code{show_data = TRUE}).
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @param add Logical; if \code{TRUE}, adds the curve to an existing plot instead of
#'   creating a new one (only for base R graphics).
#' @param ... Additional graphical parameters passed to the underlying plotting functions.
#'
#' @return For ggplot2: a ggplot object. For base R: invisibly returns a data.frame
#'   with prediction results.
#'
#' @details
#' This function provides a unified interface that works with both:
#' \itemize{
#'   \item \code{bqr.svy} objects (univariate Bayesian quantile regression)
#'   \item \code{mo.bqr.svy} objects with \code{response_dim = 1} (multivariate setup with 1D response)
#' }
#'
#' For \code{bqr.svy} objects, it uses the optimized \code{plot_quantile.bqr.svy} method.
#' For \code{mo.bqr.svy} objects with 1D response, it uses \code{drawQuantile1D}.
#'
#' The function automatically detects:
#' \itemize{
#'   \item Model type (bqr.svy vs mo.bqr.svy)
#'   \item Response variable name (for bqr.svy)
#'   \item Whether to use ggplot2 or base R graphics
#' }
#'
#' @examples
#' \dontrun{
#' # Works with bqr.svy objects
#' fit1 <- bqr.svy(y ~ x, data = mydata, quantile = 0.5)
#' plot_quantile_1D(fit1, data = mydata, predictor = "x")
#' plot_quantile_1D(fit1, data = mydata, predictor = "x", show_data = TRUE)
#'
#' # Works with mo.bqr.svy objects (1D response)
#' fit2 <- mo.bqr.svy(y ~ x, data = mydata, quantile = c(0.1, 0.5, 0.9))
#' plot_quantile_1D(fit2, data = mydata, predictor = "x", response = "y")
#' plot_quantile_1D(fit2, data = mydata, predictor = "x", response = "y", 
#'                  show_data = TRUE, use_ggplot = TRUE)
#' }
#'
#' @keywords internal
plot_quantile_1D <- function(object, data, predictor,
                             show_data = FALSE,
                             response = NULL,
                             grid_length = 100,
                             fixed_values = NULL,
                             use_ggplot = NULL,
                             paintedArea = FALSE,
                             band_choice = c("minmax", "symmetric"),
                             line_col = NULL,
                             line_lwd = 2,
                             line_type = 1,
                             data_col = "steelblue",
                             data_alpha = 0.4,
                             main = NULL,
                             add = FALSE,
                             ...) {
  
  # Validate inputs
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.", call. = FALSE)
  if (!predictor %in% names(data))
    stop("Predictor '", predictor, "' not found in data.", call. = FALSE)
  
  # Detect model type and validate
  if (inherits(object, "bqr.svy")) {
    # For bqr.svy: use the existing optimized function
    if (!is.null(response)) {
      warning("'response' parameter is ignored for bqr.svy objects (extracted from formula).",
              call. = FALSE)
    }
    
    return(plot_quantile.bqr.svy(
      object = object,
      data = data,
      predictor = predictor,
      show_data = show_data,
      grid_length = grid_length,
      fixed_values = fixed_values,
      add = add,
      line_col = line_col,
      line_lwd = line_lwd,
      line_type = line_type,
      data_col = data_col,
      data_alpha = data_alpha,
      main = main,
      use_ggplot = use_ggplot,
      paintedArea = paintedArea,
      band_choice = band_choice,
      ...
    ))
    
  } else if (inherits(object, "mo.bqr.svy")) {
    # For mo.bqr.svy: check if it's 1D and use drawQuantile1D
    if (is.null(object$response_dim) || object$response_dim != 1) {
      stop("plot_quantile_1D only supports mo.bqr.svy objects with response_dim = 1. ",
           "For higher dimensions, use drawQuantileRegion() or drawQuantileRegion_3D().",
           call. = FALSE)
    }
    
    if (is.null(response)) {
      stop("For mo.bqr.svy objects, 'response' parameter must specify the response column name.",
           call. = FALSE)
    }
    
    if (!response %in% names(data)) {
      stop("Response variable '", response, "' not found in data.", call. = FALSE)
    }
    
    # Set defaults for mo.bqr.svy
    if (is.null(use_ggplot)) {
      use_ggplot <- length(object$quantile) > 1
    }
    
    # Build xValue for fixed_values if provided
    xValue <- if (!is.null(fixed_values)) {
      as.data.frame(fixed_values[!names(fixed_values) %in% predictor])
    } else {
      NULL
    }
    
    return(drawQuantile1D(
      fit = object,
      datafile = if (show_data) data else NULL,
      response = response,
      x_var = predictor,
      x_grid = if (is.factor(data[[predictor]]) || is.character(data[[predictor]])) {
        NULL  # Let drawQuantile1D handle categorical predictors
      } else {
        rng <- range(data[[predictor]], na.rm = TRUE)
        seq(rng[1], rng[2], length.out = grid_length)
      },
      xValue = xValue,
      paintedArea = paintedArea,
      band_choice = band_choice,
      print_plot = TRUE,
      show_data = show_data,
      main = main
    ))
    
  } else {
    stop("Object must be of class 'bqr.svy' or 'mo.bqr.svy'.", call. = FALSE)
  }
}


# =====================================================
# bqr.svy plotting
# =====================================================

#' Plot predicted quantile regression curve for bqr.svy objects
#'
#' This function plots the predicted quantile regression curve from a fitted
#' \code{bqr.svy} model. It can handle both numeric and categorical predictors,
#' optionally overlays the curve on an existing plot, and can show observed data points.
#' When the object contains multiple quantiles, all curves are plotted with different 
#' colors and a legend is added automatically.
#'
#' @param object An object of class \code{bqr.svy}, typically the result of a call
#'   to \code{\link{bqr.svy}}.
#' @param data A \code{data.frame} containing the variables used in the model. Must
#'   include the predictor specified in \code{predictor} and any covariates in the
#'   fitted model.
#' @param predictor A character string giving the name of the predictor variable
#'   to plot on the x-axis.
#' @param show_data Logical; if \code{TRUE}, shows the observed data points as a 
#'   scatter plot underneath the quantile curves.
#' @param grid_length Integer; number of grid points to generate for continuous
#'   predictors. Ignored for categorical predictors.
#' @param fixed_values Optional named list giving fixed values for covariates other
#'   than \code{predictor}. If not supplied, numeric covariates are fixed at their
#'   median and factors at their most frequent level.
#' @param add Logical; if \code{TRUE}, adds the curve to an existing plot instead of
#'   creating a new one.
#' @param line_col Color for the regression line(s). For multiple quantiles, can be
#'   a vector of colors or a single color (recycled).
#' @param line_lwd Line width for the regression line.
#' @param line_type Line type for the regression line.
#' @param point_pch Plotting symbol to use for points (categorical predictors).
#' @param point_cex Size of the plotting symbols for points (categorical predictors).
#' @param data_col Color for the observed data points (when \code{show_data = TRUE}).
#' @param data_alpha Transparency for the observed data points (0-1, when \code{show_data = TRUE}).
#' @param data_pch Plotting symbol for the observed data points.
#' @param data_cex Size of the observed data points.
#' @param prefer_predict Logical; if \code{TRUE}, attempts to use the
#'   \code{predict()} method for the fitted object first.
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @param use_ggplot Logical; if \code{TRUE}, uses \code{drawQuantile1D} for multiple
#'   quantiles (requires ggplot2), otherwise uses base R graphics.
#' @param paintedArea Logical; if \code{TRUE} and multiple quantiles are present,
#'   fills the area between extreme quantiles (only when \code{use_ggplot = TRUE}).
#' @param band_choice Character; for painted areas, either \code{"minmax"}
#'   (between min and max quantiles) or \code{"symmetric"} (between quantiles
#'   around 0.5). Only used when \code{use_ggplot = TRUE}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{points}}, or \code{\link[graphics]{lines}}.
#'
#' @return Invisibly returns a \code{data.frame} with:
#'   \item{predictor}{The sequence or factor levels of the predictor.}
#'   \item{predicted}{The predicted quantile values.}
#'   \item{quantile}{The tau value(s) used.}
#'
#' @examples
#' \dontrun{
#' # Single quantile without data points
#' fit <- bqr.svy(y ~ x, data = mydata, quantile = 0.5)
#' plot_quantile.bqr.svy(fit, data = mydata, predictor = "x")
#' 
#' # Single quantile with data points
#' plot_quantile.bqr.svy(fit, data = mydata, predictor = "x", show_data = TRUE)
#' 
#' # Multiple quantiles with data points
#' fit_multi <- bqr.svy(y ~ x, data = mydata, quantile = c(0.1, 0.5, 0.9))
#' plot_quantile_1D(fit_multi, data = mydata, predictor = "x", show_data = TRUE)
#' }
#' @keywords internal
plot_quantile.bqr.svy <- function(object, data, predictor,
                                  show_data = FALSE,
                                  grid_length = 100,
                                  fixed_values = NULL,
                                  add = FALSE,
                                  line_col = NULL,
                                  line_lwd = 2,
                                  line_type = 1,
                                  point_pch = 19,
                                  point_cex = 1.2,
                                  data_col = "steelblue",
                                  data_alpha = 0.4,
                                  data_pch = 16,
                                  data_cex = 1,
                                  prefer_predict = FALSE,
                                  main = NULL,
                                  use_ggplot = NULL,
                                  paintedArea = FALSE,
                                  band_choice = c("minmax", "symmetric"),
                                  ...) {
  
  if (!inherits(object, "bqr.svy"))
    stop("Object must be of class 'bqr.svy'.")
  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")
  if (!predictor %in% names(data))
    stop("Predictor not found in data.")

  response_var <- all.vars(object$formula)[1]
  taus <- object$quantile
  
  # Decide automáticamente si usar ggplot para múltiples cuantiles
  if (is.null(use_ggplot)) {
    use_ggplot <- length(taus) > 1
  }

  if (length(taus) > 1 && !use_ggplot) {
    message("Multiple quantiles detected. Consider setting use_ggplot=TRUE for better visualization.")
  }

  # Para múltiples cuantiles, usar drawQuantile1D si está disponible
  if (use_ggplot && length(taus) > 1) {
    if (is.null(main)) {
      main <- paste("Quantile Regression (tau =",
                    paste(taus, collapse = ", "), ") vs", predictor)
    }

    band_choice <- match.arg(band_choice)

    return(drawQuantile1D(
      fit = object,
      datafile = if (show_data) data else NULL,
      response = response_var,
      x_var = predictor,
      paintedArea = paintedArea,
      band_choice = band_choice,
      print_plot = TRUE,
      show_data = show_data
    ))
  }

  # Código para el caso base R (un cuantil o múltiples cuantiles sin ggplot)
  formula_obj <- if (!is.null(object$formula)) object$formula else object$call$formula
  all_vars <- all.vars(formula_obj)

  if (is.null(main)) {
    if (length(taus) == 1) {
      main <- paste("Quantile Regression (tau =", taus, ") vs", predictor)
    } else {
      main <- paste("Quantile Regression (tau =",
                    paste(taus, collapse = ", "), ") vs", predictor)
    }
  }

  # Predictor values para las curvas
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

  # Función helper para hacer predicciones para un cuantil específico
  predict_quantile <- function(obj, tau_idx = 1) {
    # Prediction
    if (prefer_predict) {
      y_try <- try(predict(obj, newdata = newdata), silent = TRUE)
      if (!inherits(y_try, "try-error")) {
        return(as.numeric(y_try))
      }
    }
    
    # Get coefficients
    if (!is.null(obj$coefficients)) {
      beta_hat <- as.numeric(obj$coefficients)
      names(beta_hat) <- names(obj$coefficients)
    } else if (!is.null(obj$beta) && is.matrix(obj$beta)) {
      beta_hat <- obj$beta[, tau_idx]
      names(beta_hat) <- rownames(obj$beta)
    } else if (!is.null(obj$draws)) {
      if (is.list(obj$draws) && length(obj$draws) >= tau_idx) {
        beta_hat <- colMeans(obj$draws[[tau_idx]], na.rm = TRUE)
      } else {
        beta_hat <- colMeans(obj$draws, na.rm = TRUE)
      }
      # drop potential non-beta columns by name if present (e.g., "sigma")
      if (!is.null(names(beta_hat))) {
        beta_hat <- beta_hat[!grepl("^sigma", names(beta_hat), ignore.case = TRUE)]
      }
    } else {
      stop("No coefficients or draws found for prediction.")
    }

    terms_obj <- if (!is.null(obj$terms)) obj$terms else terms(formula_obj)
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

    return(as.vector(X_new %*% beta_hat))
  }

  # Configurar colores por defecto
  if (is.null(line_col)) {
    if (length(taus) == 1) {
      line_col <- "red"
    } else {
      line_col <- if (requireNamespace("grDevices", quietly = TRUE)) {
        grDevices::rainbow(length(taus))
      } else {
        rep(c("red", "blue", "green", "orange", "purple"), length.out = length(taus))
      }
    }
  } else if (length(line_col) < length(taus)) {
    line_col <- rep(line_col, length.out = length(taus))
  }

  # Plot inicial con datos observados si se requiere
  if (!add) {
    if (show_data && response_var %in% names(data)) {
      # Scatter plot de los datos observados
      plot(data[[predictor]], data[[response_var]],
           col = adjustcolor(data_col, data_alpha),
           pch = data_pch, cex = data_cex,
           xlab = predictor, ylab = response_var,
           main = main, ...)
      grid()
    } else {
      # Plot vacío para las curvas - necesitamos determinar los rangos
      if (length(taus) == 1) {
        y_pred_temp <- predict_quantile(object, 1)
      } else {
        # Para múltiples cuantiles, calcular rango de todas las predicciones
        all_preds <- sapply(seq_along(taus), function(i) predict_quantile(object, i))
        y_pred_temp <- as.vector(all_preds)
      }
      
      if (pred_is_cat) {
        xx <- seq_along(pred_values)
        plot(xx, rep(mean(y_pred_temp), length(xx)), type = "n",
             ylim = range(y_pred_temp, na.rm = TRUE),
             xlab = predictor, ylab = response_var,
             xaxt = "n", main = main, ...)
        axis(1, at = xx, labels = pred_values)
      } else {
        plot(pred_values, rep(mean(y_pred_temp), length(pred_values)), type = "n",
             ylim = range(y_pred_temp, na.rm = TRUE),
             xlab = predictor, ylab = response_var,
             main = main, ...)
      }
      grid()
    }
  }

  # Graficar cuantiles
  if (length(taus) == 1) {
    # Caso de un solo cuantil
    y_pred <- predict_quantile(object, 1)
    
    if (pred_is_cat) {
      xx <- seq_along(pred_values)
      if (!add) {
        points(xx, y_pred, col = line_col[1], pch = point_pch, cex = point_cex)
      }
      lines(xx, y_pred, col = line_col[1], lwd = line_lwd, lty = line_type)
    } else {
      lines(pred_values, y_pred, col = line_col[1], lwd = line_lwd, lty = line_type)
    }
    
    return_df <- data.frame(predictor = pred_values, predicted = y_pred, quantile = taus[1])
    
  } else {
    # Caso de múltiples cuantiles
    return_list <- list()
    
    for (i in seq_along(taus)) {
      y_pred <- predict_quantile(object, i)
      
      if (pred_is_cat) {
        xx <- seq_along(pred_values)
        if (!add && i == 1) {
          points(xx, y_pred, col = line_col[i], pch = point_pch, cex = point_cex)
        }
        lines(xx, y_pred, col = line_col[i], lwd = line_lwd, lty = line_type)
      } else {
        lines(pred_values, y_pred, col = line_col[i], lwd = line_lwd, lty = line_type)
      }
      
      return_list[[i]] <- data.frame(predictor = pred_values, predicted = y_pred, quantile = taus[i])
    }
    
    # Agregar leyenda para múltiples cuantiles
    if (!add) {
      legend("topright",
             legend = paste("τ =", formatC(taus, format = "f", digits = 2)),
             col = line_col, lty = line_type, lwd = line_lwd, cex = 0.8, bg = "white")
    }
    
    return_df <- do.call(rbind, return_list)
  }

  invisible(return_df)
}

#' Diagnostic plot method for \code{bqr.svy} objects
#'
#' @param x An object of class \code{bqr.svy}.
#' @param type Type of plot: \code{"trace"}, \code{"intervals"}, or \code{"quantiles"}.
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @param ... Additional plotting arguments.
#' @keywords internal
plot_diagnostics.bqr.svy <- function(x, type = c("trace", "intervals", "quantiles"), main = NULL, ...) {
  type <- match.arg(type)

  if (type == "trace") {
    if (is.null(x$draws))
      stop("No MCMC draws available for trace plot.")
    if (is.null(main)) main <- "MCMC Trace Plots"
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
    if (is.null(main)) main <- "Posterior means with 95% CI"
    means <- apply(x$draws, 2, mean)
    lower <- apply(x$draws, 2, quantile, probs = 0.025)
    upper <- apply(x$draws, 2, quantile, probs = 0.975)

    plot(seq_along(means), means, ylim = range(c(lower, upper)), pch = 19,
         xaxt = "n", xlab = "Parameter", ylab = "Value",
         main = main, ...)
    axis(1, at = seq_along(means), labels = if (!is.null(colnames(x$draws))) colnames(x$draws) else paste0("Param", seq_along(means)))
    arrows(seq_along(means), lower, seq_along(means), upper, angle = 90, code = 3, length = 0.05)
    grid()

  } else if (type == "quantiles") {
    stop("Quantile plot not implemented for single-quantile objects. Use plot_quantile.bqr.svy().")
  }
}

#' @keywords internal
.build_xvec <- function(fit, newdata) {
  stopifnot(is.data.frame(newdata), nrow(newdata) == 1L)
  tt <- tryCatch(stats::delete.response(fit$terms), error = function(e) fit$terms)
  mf <- stats::model.frame(tt, newdata)
  mm <- stats::model.matrix(tt, mf)
  xvec <- as.numeric(mm[1, ])
  names(xvec) <- colnames(mm)
  xvec
}

#' @keywords internal
.collect_points_for_tau <- function(fit, tau, x0_vec) {
  taus <- fit$quantile
  if (length(taus) == 0) stop("The object has no quantiles in 'fit'.")
  idx_tau <- which.min(abs(taus - tau))
  fi <- fit$fit[[idx_tau]]
  if (is.null(fi)) stop("No results found for tau = ", tau)

  U <- fit$U
  Gamma_list <- fi$Gamma_list %||% fit$Gamma_list
  dirs <- fi$directions
  K <- length(dirs)
  d <- nrow(U)

  pts <- matrix(NA_real_, nrow = K, ncol = d)
  for (k in seq_len(K)) {
    u_k      <- U[, k, drop = FALSE]
    gamma_uk <- Gamma_list[[k]]
    beta_k   <- dirs[[k]]$beta
    pts[k, ] <- .reconstruct_point_dir(u = u_k, Gamma = gamma_uk,
                                       beta_dir = beta_k, x0_vec = x0_vec)
  }
  colnames(pts) <- paste0("Y", seq_len(d))
  pts
}


#' @keywords internal
.reconstruct_point_dir <- function(u, Gamma, beta_dir, x0_vec) {
  bx <- beta_dir[names(x0_vec)]
  tval <- sum(bx * x0_vec, na.rm = TRUE)
  as.numeric(u) * tval
}

# operador %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b



#' Draw univariate quantile curves/bands for mo.bqr.svy (d = 1)
#'
#' @note For a unified interface that works with both \code{bqr.svy} and 
#'   \code{mo.bqr.svy} objects, consider using \code{\link{plot_quantile_1D}}.
#'
#' @param fit mo.bqr.svy object with response_dim = 1
#' @param datafile optional data.frame to overlay observed points
#' @param response name of Y column in datafile (character, length 1)
#' @param x_var which predictor to map to the horizontal axis (character, length 1).
#'              If NULL, uses the first term label in fit$terms.
#' @param x_grid numeric vector of x values for x_var; if NULL, deduced from datafile or set to seq(-2,2,len=100)
#' @param xValue data.frame or list with fixed values for other predictors (one or more rows).
#'               If multiple rows are provided, curves are colored by row (comparison).
#' @param paintedArea if TRUE and there are at least 2 taus, fills ribbon between two taus (see band_choice)
#' @param band_choice character: "minmax" (default) or "symmetric".
#'        - "minmax": banda entre min(taus) y max(taus)
#'        - "symmetric": toma el mayor tau < 0.5 y el menor tau > 0.5 (si existen)
#' @param print_plot if TRUE returns ggplot; if FALSE returns a data.frame with predictions
#' @param show_data if TRUE and datafile/response/x_var available, shows observed points
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#' @return ggplot object or data.frame with columns: xid, x, tau, yhat
#' @keywords internal
drawQuantile1D <- function(fit, datafile = NULL, response = "Y",
                           x_var = NULL, x_grid = NULL, xValue = NULL,
                           paintedArea = TRUE, band_choice = c("minmax","symmetric"),
                           print_plot = TRUE, show_data = !is.null(datafile), main = NULL) {
  band_choice <- match.arg(band_choice)
  taus <- as.numeric(fit$quantile)
  if (length(taus) == 0L) stop("The object has no levels in 'fit$quantile'.")

  get_vars <- function(fit) {
    if (!is.null(fit$terms))      return(attr(stats::terms(fit$terms), "term.labels"))
    if (!is.null(fit$formula))    return(attr(stats::terms(fit$formula), "term.labels"))
    if (!is.null(fit$call) && !is.null(fit$call$formula))
      return(attr(stats::terms(stats::as.formula(fit$call$formula)), "term.labels"))
    stop("Could not find predictors (missing fit$terms or formula).")
  }
  vars <- get_vars(fit)
  if (is.null(x_var)) {
    if (length(vars) == 0L) stop("No predictors in the model; specify x_var.")
    x_var <- vars[1L]
  }
  if (!x_var %in% vars) stop(sprintf("x_var='%s' is not among the model predictors.", x_var))

  to_list_newdata <- function(xValue) {
    if (is.null(xValue)) {
      if (length(vars) == 0L) list(data.frame(row=1)[,FALSE])
      else list(as.data.frame(as.list(stats::setNames(rep(0, length(vars)), vars))))
    } else if (is.data.frame(xValue)) {
      split(xValue, seq_len(nrow(xValue)))
    } else if (is.list(xValue) && !is.data.frame(xValue)) {
      lapply(xValue, function(el) if (is.data.frame(el)) el[1,,drop=FALSE] else as.data.frame(el))
    } else stop("xValue must be a data.frame (1+ rows) or a list of named rows.")
  }
  base_list <- to_list_newdata(xValue)

  if (is.null(x_grid)) {
    if (!is.null(datafile) && x_var %in% names(datafile)) {
      rng <- range(datafile[[x_var]], na.rm = TRUE)
      x_grid <- if (is.finite(rng[1]) && is.finite(rng[2]) && diff(rng) > 0)
        seq(rng[1], rng[2], length.out = 100) else seq(-2, 2, length.out = 100)
    } else x_grid <- seq(-2, 2, length.out = 100)
  }

  extract_beta_from_node <- function(node) {
    cands <- list(
      tryCatch(node$directions[[1]]$beta, error = function(e) NULL),
      node$beta, node$coef, node$coefficients
    )
    for (el in cands) if (is.numeric(el)) return(el)
    NULL
  }
  get_beta_for_tau <- function(fit, tau, qi) {
    b_try <- try(stats::coef(fit, tau = tau), silent = TRUE)
    if (!inherits(b_try, "try-error") && is.numeric(b_try)) return(b_try)
    b_try <- try(coef(fit, tau = tau), silent = TRUE)
    if (!inherits(b_try, "try-error") && is.numeric(b_try)) return(b_try)

    cf <- NULL
    c1 <- try(stats::coef(fit), silent = TRUE)
    if (!inherits(c1, "try-error")) cf <- c1
    if (is.null(cf)) cf <- if (!is.null(fit$coef)) fit$coef else fit$coefficients
    if (!is.null(cf)) {
      if (is.matrix(cf)) {
        j <- qi
        if (!is.null(colnames(cf))) {
          numcn <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+","", colnames(cf))))
          if (any(is.finite(numcn))) { jj <- which.min(abs(numcn - tau)); if (length(jj)==1) j <- jj }
        }
        b <- cf[, j, drop = TRUE]; if (!is.null(rownames(cf))) names(b) <- rownames(cf); return(b)
      }
      if (is.list(cf)) {
        if (length(cf) >= qi && is.numeric(cf[[qi]])) return(cf[[qi]])
        nm <- names(cf); if (!is.null(nm)) { num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+","", nm)))
        jj <- which.min(abs(num - tau)); if (length(jj) == 1 && is.numeric(cf[[jj]])) return(cf[[jj]]) }
      }
      if (is.numeric(cf)) return(cf)
    }
    if (!is.null(fit$beta)) {
      b0 <- fit$beta
      if (is.numeric(b0) && is.null(dim(b0))) return(b0)
      if (is.matrix(b0)) {
        j <- qi
        if (!is.null(colnames(b0))) {
          numcn <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+","", colnames(b0))))
          if (any(is.finite(numcn))) { jj <- which.min(abs(numcn - tau)); if (length(jj)==1) j <- jj }
        }
        b <- b0[, j, drop = TRUE]; if (!is.null(rownames(b0))) names(b) <- rownames(b0); return(b)
      }
      if (is.list(b0)) {
        if (length(b0) >= qi && is.numeric(b0[[qi]])) return(b0[[qi]])
        nm <- names(b0); if (!is.null(nm)) { num <- suppressWarnings(as.numeric(gsub("[^0-9\\.]+","", nm)))
        jj <- which.min(abs(num - tau)); if (length(jj)==1 && is.numeric(b0[[jj]])) return(b0[[jj]]) }
      }
    }
    if (!is.null(fit$fit) && length(fit$fit) >= qi) {
      b <- extract_beta_from_node(fit$fit[[qi]]); if (is.numeric(b)) return(b)
    }
    stop("Could not find coefficients in 'fit' for tau = ", tau)
  }
  .safe_build_xvec <- function(fit, nd) {
    if (exists(".build_xvec", mode = "function")) {
      out <- try(.build_xvec(fit, nd), silent = TRUE)
      if (!inherits(out, "try-error")) return(out)
    }
    mm <- if (!is.null(fit$terms)) model.matrix(fit$terms, nd) else model.matrix(fit$formula, nd)
    v <- as.numeric(mm[1, ]); names(v) <- colnames(mm); v
  }
  dot_aligned <- function(beta, x0) {
    if (!is.null(names(beta))) {
      common <- intersect(names(beta), names(x0)); if (length(common)) return(sum(beta[common] * x0[common]))
    }
    k <- min(length(beta), length(x0)); sum(beta[seq_len(k)] * x0[seq_len(k)])
  }

  pred_list <- vector("list", length(base_list) * length(taus) * length(x_grid))
  idx <- 1L
  for (j in seq_along(base_list)) {
    base_row <- base_list[[j]]; if (!x_var %in% names(base_row)) base_row[[x_var]] <- NA_real_
    for (qi in seq_along(taus)) {
      t <- taus[qi]; beta_t <- get_beta_for_tau(fit, t, qi)
      for (xv in x_grid) {
        nd <- base_row; nd[[x_var]] <- xv
        x0 <- .safe_build_xvec(fit, nd)
        yhat <- dot_aligned(beta_t, x0)
        pred_list[[idx]] <- data.frame(xid = j, x = xv, tau = t, yhat = yhat)
        idx <- idx + 1L
      }
    }
  }
  df_pred <- do.call(rbind, pred_list)
  if (!print_plot) return(df_pred)

  g <- ggplot2::ggplot()
  if (isTRUE(show_data) && !is.null(datafile) && response %in% names(datafile) && x_var %in% names(datafile)) {
    g <- g + ggplot2::geom_point(
      data = datafile,
      aes(x = .data[[x_var]], y = .data[[response]]),
      alpha = 0.35, size = 1
    )
  }

  if (paintedArea && length(taus) >= 2L) {
    lower <- if (band_choice == "symmetric" && any(taus<0.5) && any(taus>0.5)) max(taus[taus<0.5]) else min(taus)
    upper <- if (band_choice == "symmetric" && any(taus<0.5) && any(taus>0.5)) min(taus[taus>0.5]) else max(taus)
    df_low <- subset(df_pred, abs(tau - lower) < 1e-12)
    df_up  <- subset(df_pred,  abs(tau - upper) < 1e-12)
    df_rib <- merge(df_low, df_up, by = c("x","xid"), suffixes = c("_lo","_up"))
    g <- g + ggplot2::geom_ribbon(
      data = df_rib,
      ggplot2::aes(x = .data$x, ymin = .data$yhat_lo, ymax = .data$yhat_up,
                   group = .data$xid, fill = factor(.data$xid)),
      alpha = 0.25, colour = NA
    )
  }

  g <- g + ggplot2::geom_line(
    data = df_pred,
    ggplot2::aes(x = .data$x, y = .data$yhat,
                 color = factor(.data$tau),
                 linetype = factor(.data$xid),
                 group = interaction(.data$xid, .data$tau)),
    linewidth = 0.9
  ) +
    ggplot2::scale_color_discrete(name = expression(tau)) +
    ggplot2::scale_linetype_discrete(name = "xValue") +
    ggplot2::labs(x = x_var, y = response, fill = "xValue", 
                  title = if (!is.null(main)) main else paste("Quantile Curves for", x_var)) +
    ggplot2::theme_bw()

  g
}



#' Draw quantile regions (2D) for mo.bqr.svy using convex hulls
#'
#' This function visualizes bivariate quantile regions from a fitted
#' \code{mo.bqr.svy} object. For each requested quantile level, a convex hull is
#' computed from the directional points and drawn as a closed polygon. The
#' function can overlay observed data, compare multiple predictor values, and
#' facet the plot so each quantile level is displayed in its own panel.
#'
#' @param fit \code{mo.bqr.svy} object with \code{response_dim = 2}.
#' @param datafile Optional \code{data.frame} with observed responses to overlay.
#' @param response Character vector of length 2 naming the Y columns in
#'   \code{datafile}, e.g. \code{c("Y1","Y2")}.
#' @param xValue Predictor values at which to evaluate the region. Can be:
#'   \itemize{
#'     \item \code{NULL}: uses the mean design vector (all zeros).
#'     \item \code{data.frame}: one or more rows specifying predictor settings.
#'     \item \code{list} of data.frames: each element is one row of predictors.
#'   }
#' @param paintedArea Logical; if \code{TRUE}, fills the polygon; if \code{FALSE},
#'   only draws the outline.
#' @param comparison Logical; if \code{TRUE} and multiple \code{xValue}s are provided,
#'   colors polygons by each \code{xValue}.
#' @param print_plot Logical; if \code{TRUE} returns a \pkg{ggplot} object;
#'   if \code{FALSE} returns a \code{data.frame} with polygon coordinates.
#' @param show_data Logical; if \code{TRUE} and valid \code{datafile}/\code{response}
#'   are provided, overlays observed points.
#' @param facet_by_tau Logical; if \code{TRUE}, facets the plot by quantile level,
#'   creating one panel per \eqn{\tau}.
#' @param facet_nrow Number of rows in the facet grid.
#' @param facet_ncol Number of columns in the facet grid (if \code{NULL}, computed automatically).
#' @param facet_scales Scales option passed to \code{facet_wrap} (\code{"fixed"},
#'   \code{"free"}, etc.).
#' @param round_digits Integer, number of digits to round projected points before
#'   computing convex hulls (helps remove duplicates).
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#'
#' @return Either a \pkg{ggplot2} object (if \code{print_plot = TRUE}) showing
#'   the quantile regions, or a \code{data.frame} with polygon coordinates
#'   (if \code{print_plot = FALSE}).
#'
#' @details
#' Internally, the function computes directional projections of the fitted model
#' via \code{.collect_points_for_tau}, applies a convex hull with
#' \code{grDevices::chull}, and then arranges the vertices to form closed polygons.
#' This ensures smooth and convex quantile regions. Multiple quantiles can be
#' visualized simultaneously using faceting.
#'
#' @examples
#' \dontrun{
#' # Fit a bivariate model
#' fit2 <- mo.bqr.svy(cbind(Y1,Y2) ~ x, data=df2,
#'                    weights=df2$w, quantile=c(0.5,0.8))
#'
#' # Draw quantile regions with observed points
#' drawQuantileRegion(fit2, datafile=df2, response=c("Y1","Y2"),
#'                    xValue=data.frame(x=c(-1,0,1)),
#'                    paintedArea=FALSE, comparison=TRUE,
#'                    facet_by_tau=TRUE)
#' }
#' @importFrom ggplot2 ggplot geom_point aes geom_polygon geom_path facet_wrap labs coord_equal theme_bw
#' @keywords internal
drawQuantileRegion <- function(fit, datafile = NULL, response = c("Y1","Y2"),
                               xValue = NULL, paintedArea = FALSE,
                               comparison = FALSE, print_plot = TRUE,
                               show_data = !is.null(datafile),
                               facet_by_tau = TRUE, facet_nrow = 1, facet_ncol = NULL,
                               facet_scales = "fixed",
                               round_digits = 10, main = NULL) {

  if (is.null(fit$response_dim) || fit$response_dim != 2L)
    stop("drawQuantileRegion: a 'fit' with response_dim = 2 is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("'ggplot2' is missing. Please install it with install.packages('ggplot2').")

  to_list_newdata <- function(fit, xValue) {
    if (is.null(xValue)) {
      vars <- attr(stats::terms(fit$terms), "term.labels")
      if (length(vars) == 0L) list(data.frame(row=1)[,FALSE])
      else list(as.data.frame(as.list(setNames(rep(0, length(vars)), vars))))
    } else if (is.data.frame(xValue)) {
      split(xValue, seq_len(nrow(xValue)))
    } else if (is.list(xValue) && !is.data.frame(xValue)) {
      lapply(xValue, function(el) if (is.data.frame(el)) el[1,,drop=FALSE] else as.data.frame(el))
    } else stop("xValue must be a data.frame (with 1 or more rows) or a list of named rows.")
  }

  newdata_list <- to_list_newdata(fit, xValue)
  taus <- fit$quantile
  if (length(taus) == 0L) stop("The object has no levels in 'fit$quantile'.")

  poly_list <- list(); idx <- 1L
  for (t in taus) for (j in seq_along(newdata_list)) {
    nd <- newdata_list[[j]]
    x0 <- .build_xvec(fit, nd)
    pts <- .collect_points_for_tau(fit, tau = t, x0_vec = x0)

    pts <- unique(round(pts, round_digits))
    pts <- pts[stats::complete.cases(pts), , drop = FALSE]

    if (nrow(pts) < 2L) {
      P <- rbind(pts, pts)
    } else if (nrow(pts) == 2L) {
      P <- rbind(pts, pts[1,,drop=FALSE])
    } else {
      h <- grDevices::chull(pts)
      P <- rbind(pts[h,,drop=FALSE], pts[h[1],,drop=FALSE])  # cerrar
    }

    poly_list[[idx]] <- data.frame(
      y1 = P[,1], y2 = P[,2],
      tau = rep(t, nrow(P)), xid = rep(j, nrow(P))
    )
    idx <- idx + 1L
  }
  df_poly <- do.call(rbind, poly_list)
  if (!print_plot) return(df_poly)

  tau_levels <- sort(unique(df_poly$tau))
  df_poly$tau_f <- factor(df_poly$tau, levels = tau_levels,
                          labels = paste0("\u03C4 = ", tau_levels))

  g <- ggplot()
  if (isTRUE(show_data) && !is.null(datafile) && all(response %in% names(datafile))) {
    g <- g + geom_point(data = datafile,
                        aes(x = .data[[response[1]]], y = .data[[response[2]]]),
                        alpha = 0.35, size = 1)
  }

  if (paintedArea) {
    if (comparison || length(newdata_list) > 1L) {
      g <- g + geom_polygon(data = df_poly,
                            aes(x = y1, y = y2,
                                group = interaction(tau_f, xid),
                                fill  = factor(xid)),
                            alpha = 0.35, colour = NA)
    } else {
      g <- g + geom_polygon(data = df_poly,
                            aes(x = y1, y = y2, group = tau_f),
                            alpha = 0.35, fill = "lightblue", colour = NA)
    }
  } else {
    if (comparison || length(newdata_list) > 1L) {
      g <- g + geom_path(data = df_poly,
                         aes(x = y1, y = y2,
                             group  = interaction(tau_f, xid),
                             colour = factor(xid)),
                         linewidth = 0.9)
    } else {
      g <- g + geom_path(data = df_poly,
                         aes(x = y1, y = y2, group = tau_f),
                         linewidth = 0.9)
    }
  }

  if (facet_by_tau) {
    g <- g + facet_wrap(~tau_f, nrow = facet_nrow, ncol = facet_ncol, scales = facet_scales)
  }

  g + labs(x = "Y1", y = "Y2", colour = "xValue", fill = "xValue",
            title = if (!is.null(main)) main else "Quantile Regions") +
    coord_equal() + theme_bw()
}




#' Draw quantile regions (3D) for mo.bqr.svy using convex hulls
#'
#' This function visualizes trivariate quantile regions from a fitted
#' \code{mo.bqr.svy} object using \pkg{plotly}. For each requested quantile
#' level, a convex hull is computed from the directional points and plotted as
#' a 3D mesh. When multiple quantiles are present, each one is displayed in its
#' own subplot (grid of 3D panels).
#'
#' @param fit \code{mo.bqr.svy} object with \code{response_dim = 3}.
#' @param xValue Predictor values at which to evaluate the region. Can be:
#'   \itemize{
#'     \item \code{NULL}: uses the mean design vector (all zeros).
#'     \item \code{data.frame}: one or more rows specifying predictor settings.
#'     \item \code{list} of data.frames: each element is one row of predictors.
#'   }
#'   Works in the same way as in \code{drawQuantileRegion} (2D).
#' @param opacity Opacity, between 0 and 1, of each quantile body.
#' @param datafile Optional \code{data.frame} with observed responses to overlay.
#' @param response Character vector of length 3 naming the Y columns in
#'   \code{datafile}, e.g. \code{c("Y1","Y2","Y3")}.
#' @param show_points Logical; if \code{TRUE} and valid \code{datafile}/
#'   \code{response} are provided, overlays observed points as a 3D scatter.
#' @param point_opacity Opacity of observed points.
#' @param point_size Size of observed points.
#' @param nrows Number of rows in the grid of subplots (when multiple quantiles).
#' @param ncols Number of columns in the grid of subplots. If \code{NULL},
#'   computed automatically from the number of quantiles and \code{nrows}.
#' @param show_titles Logical; if \code{TRUE}, adds facet titles (e.g. tau = 0.5)
#'   above each subplot.
#' @param round_digits Integer, number of digits to round projected points
#'   before computing convex hulls (helps remove duplicates).
#' @param main Main title for the plot. If \code{NULL}, a default is constructed.
#'
#' @return A \pkg{plotly} object with one 3D scene per quantile, each containing
#'   a convex-hull mesh for every \code{xValue}, plus (optionally) the observed
#'   data points.
#'
#' @details
#' Internally, the function computes directional projections of the fitted model
#' via \code{.collect_points_for_tau}, applies a convex hull with
#' \code{geometry::convhulln}, and then maps triangular faces to a mesh object.
#' The resulting quantile bodies are plotted in interactive 3D. When multiple
#' quantiles are present, they are arranged in a grid of subplots so each level
#' can be inspected separately.
#'
#' @examples
#' \dontrun{
#' # Fit a trivariate model
#' fit3 <- mo.bqr.svy(cbind(Y1,Y2,Y3) ~ x, data=df3,
#'                    weights=df3$w, quantile=c(0.5,0.8), U=U)
#'
#' # Visualize quantile bodies at x = -1, 0, 1
#' drawQuantileRegion_3D(fit3,
#'                       xValue=data.frame(x=c(-1,0,1)),
#'                       datafile=df3, response=c("Y1","Y2","Y3"),
#'                       show_points=TRUE)
#' }
#'
#' @keywords internal
drawQuantileRegion_3D <- function(fit, xValue = NULL, opacity = 0.5,
                                  datafile = NULL, response = c("Y1","Y2","Y3"),
                                  show_points = FALSE, point_opacity = 0.25,
                                  point_size = 2,
                                  nrows = 1, ncols = NULL, show_titles = TRUE,
                                  round_digits = 10, main = NULL) {
  if (is.null(fit$response_dim) || fit$response_dim != 3L)
    stop("drawQuantileRegion_3D: a 'fit' with response_dim = 3 is required.")
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("'plotly' is missing. Please install it with install.packages('plotly').")
  if (!requireNamespace("geometry", quietly = TRUE))
    stop("'geometry' is missing. Please install it with install.packages('geometry').")

  to_list_newdata <- function(fit, xValue) {
    if (is.null(xValue)) {
      vars <- attr(stats::terms(fit$terms), "term.labels")
      if (length(vars) == 0L) list(data.frame(row=1)[,FALSE])
      else list(as.data.frame(as.list(stats::setNames(rep(0, length(vars)), vars))))
    } else if (is.data.frame(xValue)) {
      split(xValue, seq_len(nrow(xValue)))
    } else if (is.list(xValue) && !is.data.frame(xValue)) {
      lapply(xValue, function(el) if (is.data.frame(el)) el[1,,drop=FALSE] else as.data.frame(el))
    } else stop("xValue must be a data.frame (with 1+ rows) or a list of named rows.")
  }
  label_nd <- function(nd) {
    if (ncol(nd)==0) return("xValue#1")
    paste(paste(names(nd), signif(unlist(nd[1,]), 3), sep="="), collapse=", ")
  }

  newdata_list <- to_list_newdata(fit, xValue)
  taus <- sort(fit$quantile)
  if (length(taus) == 0L) stop("The object has no levels in 'fit$quantile'.")

  if (is.null(ncols)) ncols <- ceiling(length(taus) / nrows)
  n_panels <- length(taus)

  n_x <- max(1L, length(newdata_list))
  pal <- try(grDevices::hcl.colors(n_x, "Dark 3"), silent = TRUE)
  if (inherits(pal, "try-error")) pal <- grDevices::rainbow(n_x)

  pts_obs <- NULL
  if (isTRUE(show_points) && !is.null(datafile) && all(response %in% names(datafile))) {
    pts_obs <- stats::na.omit(datafile[, response])
  }

  plt <- plotly::plot_ly()

  dx <- 1 / ncols
  dy <- 1 / nrows
  pad <- 0.02

  ann_list <- list()

  for (i in seq_along(taus)) {
    tau <- taus[i]
    row_i <- ceiling(i / ncols)
    col_i <- i - (row_i - 1L) * ncols

    x0 <- (col_i - 1) * dx + pad
    x1 <- col_i * dx - pad
    inv_row <- nrows - row_i
    y0 <- inv_row * dy + pad
    y1 <- (inv_row + 1) * dy - pad

    scene_id <- if (i == 1) "scene" else paste0("scene", i)

    for (j in seq_along(newdata_list)) {
      nd <- newdata_list[[j]]
      x0_vec <- .build_xvec(fit, nd)
      pts <- .collect_points_for_tau(fit, tau = tau, x0_vec = x0_vec)
      pts <- unique(round(pts, round_digits))
      pts <- pts[stats::complete.cases(pts), , drop = FALSE]
      colnames(pts) <- c("Y1","Y2","Y3")

      if (nrow(pts) >= 4) {
        ch <- geometry::convhulln(pts, options = "Qt")
        tri <- if (ncol(ch) == 4) {
          do.call(rbind, lapply(seq_len(nrow(ch)), function(ii) {
            v <- ch[ii,]
            rbind(c(v[1],v[2],v[3]), c(v[1],v[3],v[4]),
                  c(v[1],v[2],v[4]), c(v[2],v[3],v[4]))
          }))
        } else ch
        ii <- tri[,1]-1L; jj <- tri[,2]-1L; kk <- tri[,3]-1L

        plt <- plt |>
          plotly::add_mesh(
            x = pts[,1], y = pts[,2], z = pts[,3],
            i = ii, j = jj, k = kk,
            name = paste0(label_nd(nd), " | \u03C4=", tau),
            opacity = opacity, color = pal[j],
            legendgroup = paste0("xid", j),
            showlegend = (i == 1),
            scene = scene_id
          )
      }
    }

    if (!is.null(pts_obs)) {
      plt <- plt |>
        plotly::add_markers(
          x = pts_obs[[response[1]]],
          y = pts_obs[[response[2]]],
          z = pts_obs[[response[3]]],
          name = "Observed Y",
          opacity = point_opacity,
          marker = list(size = point_size),
          inherit = FALSE,
          legendgroup = "observed",
          showlegend = (i == 1),
          scene = scene_id
        )
    }

    scene_cfg <- list(
      domain = list(x = c(x0, x1), y = c(y0, y1)),
      xaxis  = list(title = "Y1"),
      yaxis  = list(title = "Y2"),
      zaxis  = list(title = "Y3"),
      aspectmode = "cube"
    )

    plt <- do.call(plotly::layout, c(list(plt), setNames(list(scene_cfg), scene_id)))

    if (isTRUE(show_titles)) {
      ann_list[[length(ann_list)+1]] <- list(
        text = paste0("\u03C4 = ", tau),
        x = (x0 + x1)/2, y = y1 + 0.01,
        xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "center", yanchor = "bottom",
        font = list(size = 12)
      )
    }
  }

  if (isTRUE(show_titles) && length(ann_list)) {
    plt <- plotly::layout(plt, annotations = ann_list)
  }
  
  # Add main title if provided
  if (!is.null(main)) {
    plt <- plotly::layout(plt, title = main)
  }

  plt
}



