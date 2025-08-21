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
  if (length(taus) == 0) stop("El objeto no tiene cuantiles en 'fit'.")
  idx_tau <- which.min(abs(taus - tau))
  fi <- fit$fit[[idx_tau]]
  if (is.null(fi)) stop("No se encontró resultados para tau = ", tau)

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



#' Draw quantile regions (2D) for mo.bqr.svy
#'
#' @param fit objeto \code{mo.bqr.svy} con d = 2
#' @param datafile (opcional) data.frame para superponer puntos observados
#' @param response (opcional) nombres de columnas Y en datafile, p.ej. c("Y1","Y2")
#' @param xValue valores de predictores donde evaluar la región (una o varias filas)
#' @param paintedArea si TRUE rellena el polígono; si FALSE solo contorno
#' @param comparison si TRUE y hay múltiples xValue, colorea por cada uno
#' @param print_plot si TRUE devuelve ggplot; si FALSE devuelve data.frame de polígonos
#' @param show_data si TRUE y hay datafile/response válidos, dibuja los puntos observados
#' @return ggplot o data.frame con coordenadas
#' @export
drawQuantileRegion <- function(fit, datafile = NULL, response = c("Y1","Y2"),
                               xValue = NULL, paintedArea = FALSE,
                               comparison = FALSE, print_plot = TRUE,
                               show_data = !is.null(datafile)) {
  if (is.null(fit$response_dim) || fit$response_dim != 2L)
    stop("drawQuantileRegion: se requiere un 'fit' con response_dim = 2.")

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Falta 'ggplot2'. Instálalo con install.packages('ggplot2').")

  # --- construir lista de newdata (una por cada xValue solicitado) ---
  to_list_newdata <- function(fit, xValue) {
    if (is.null(xValue)) {
      vars <- attr(stats::terms(fit$terms), "term.labels")
      if (length(vars) == 0L) list(data.frame(row=1)[,FALSE])
      else list(as.data.frame(as.list(setNames(rep(0, length(vars)), vars))))
    } else if (is.data.frame(xValue)) {
      split(xValue, seq_len(nrow(xValue)))
    } else if (is.list(xValue) && !is.data.frame(xValue)) {
      lapply(xValue, function(el) if (is.data.frame(el)) el[1,,drop=FALSE] else as.data.frame(el))
    } else stop("xValue debe ser data.frame (1+ filas) o lista de filas nombradas.")
  }

  newdata_list <- to_list_newdata(fit, xValue)
  taus <- fit$quantile
  if (length(taus) == 0L) stop("El objeto no tiene niveles en 'fit$quantile'.")

  # --- armar data.frame con polígonos para todas las (tau, newdata) ---
  poly_list <- list(); idx <- 1L
  for (t in taus) for (j in seq_along(newdata_list)) {
    nd <- newdata_list[[j]]
    x0 <- .build_xvec(fit, nd)
    pts <- .collect_points_for_tau(fit, tau = t, x0_vec = x0)  # Kx2
    ang <- atan2(pts[,2], pts[,1]); ord <- order(ang)
    P <- rbind(pts[ord,,drop=FALSE], pts[ord,,drop=FALSE][1,,drop=FALSE])  # cerrar
    poly_list[[idx]] <- data.frame(y1=P[,1], y2=P[,2], tau=rep(t,nrow(P)), xid=rep(j,nrow(P)))
    idx <- idx + 1L
  }
  df_poly <- do.call(rbind, poly_list)
  if (!print_plot) return(df_poly)

  # --- plot ---
  library(ggplot2)
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
                                group = interaction(tau, xid),
                                fill = factor(xid)),
                            alpha = 0.35, colour = NA)
    } else {
      g <- g + geom_polygon(data = df_poly,
                            aes(x = y1, y = y2, group = tau),
                            alpha = 0.35, fill = "lightblue", colour = NA)
    }
  } else {
    if (comparison || length(newdata_list) > 1L) {
      g <- g + geom_path(data = df_poly,
                         aes(x = y1, y = y2,
                             group = interaction(tau, xid),
                             colour = factor(xid),
                             linetype = factor(tau)), linewidth = 0.7)
    } else {
      g <- g + geom_path(data = df_poly,
                         aes(x = y1, y = y2, linetype = factor(tau)),
                         linewidth = 0.7)
    }
  }

  g +
    scale_linetype_discrete(name = expression(tau)) +
    labs(x = "Y1", y = "Y2", colour = "xValue", fill = "xValue") +
    coord_equal() + theme_bw()
}


#' Draw quantile regions (3D) for mo.bqr.svy (plotly + convex hull)
#'
#' @param fit objeto \code{mo.bqr.svy} con d = 3
#' @param xValue valores de predictores donde evaluar la región (mismas reglas que en 2D)
#' @param opacity opacidad de cada cuerpo (0-1)
#' @param datafile (opcional) data.frame con Y observados para superponer
#' @param response (opcional) nombres de columnas Y en datafile, p.ej. c("Y1","Y2","Y3")
#' @param show_points si TRUE y hay datafile/response válidos, superpone puntos observados
#' @param point_opacity opacidad de los puntos observados
#' @param point_size tamaño de los puntos observados
#' @return objeto plotly con una malla por cada xValue (y por el/los tau del modelo)
#' @export
drawQuantileRegion_3D <- function(fit, xValue = NULL, opacity = 0.5,
                                  datafile = NULL, response = c("Y1","Y2","Y3"),
                                  show_points = FALSE, point_opacity = 0.25,
                                  point_size = 2) {
  if (is.null(fit$response_dim) || fit$response_dim != 3L)
    stop("drawQuantileRegion_3D: se requiere un 'fit' con response_dim = 3.")

  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Falta 'plotly'. Instálalo con install.packages('plotly').")
  if (!requireNamespace("geometry", quietly = TRUE))
    stop("Falta 'geometry'. Instálalo con install.packages('geometry').")

  # construir lista de newdata
  to_list_newdata <- function(fit, xValue) {
    if (is.null(xValue)) {
      vars <- attr(stats::terms(fit$terms), "term.labels")
      if (length(vars) == 0L) list(data.frame(row=1)[,FALSE])
      else list(as.data.frame(as.list(setNames(rep(0, length(vars)), vars))))
    } else if (is.data.frame(xValue)) {
      split(xValue, seq_len(nrow(xValue)))
    } else if (is.list(xValue) && !is.data.frame(xValue)) {
      lapply(xValue, function(el) if (is.data.frame(el)) el[1,,drop=FALSE] else as.data.frame(el))
    } else stop("xValue debe ser data.frame (1+ filas) o lista de filas nombradas.")
  }

  newdata_list <- to_list_newdata(fit, xValue)
  taus <- fit$quantile
  if (length(taus) == 0L) stop("El objeto no tiene niveles en 'fit$quantile'.")

  plt <- plotly::plot_ly()
  color_seq <- grDevices::rainbow(max(1L, length(newdata_list)))

  for (j in seq_along(newdata_list)) {
    nd <- newdata_list[[j]]
    x0 <- .build_xvec(fit, nd)

    for (t in taus) {
      pts <- .collect_points_for_tau(fit, tau = t, x0_vec = x0) # K x 3
      colnames(pts) <- c("Y1","Y2","Y3")

      ch <- geometry::convhulln(pts, options = "Qt")
      tri <- if (ncol(ch) == 4) {
        do.call(rbind, lapply(seq_len(nrow(ch)), function(i) {
          v <- ch[i,]
          rbind(c(v[1], v[2], v[3]),
                c(v[1], v[3], v[4]),
                c(v[1], v[2], v[4]),
                c(v[2], v[3], v[4]))
        }))
      } else ch

      i <- tri[,1]-1L; jdx <- tri[,2]-1L; k <- tri[,3]-1L

      plt <- plt |>
        plotly::add_mesh(
          x = pts[,1], y = pts[,2], z = pts[,3],
          i = i, j = jdx, k = k,
          name = paste0("xValue#", j, " | tau=", t),
          opacity = opacity,
          color = color_seq[j]
        )
    }
  }

  # superponer puntos observados (opcional)
  if (isTRUE(show_points) && !is.null(datafile) && all(response %in% names(datafile))) {
    dfp <- stats::na.omit(datafile[, response])
    plt <- plt |>
      plotly::add_markers(
        x = dfp[[response[1]]], y = dfp[[response[2]]], z = dfp[[response[3]]],
        name = "Observed Y", opacity = point_opacity,
        marker = list(size = point_size)
      )
  }

  plt |>
    plotly::layout(
      scene = list(
        xaxis = list(title = "Y1"),
        yaxis = list(title = "Y2"),
        zaxis = list(title = "Y3")
      ),
      title = "Directional quantile bodies"
    )
}
