# =====================================================
# Tests for plotting utilities
# =====================================================

test_that("plot_quantile.bqr.svy works correctly", {
  set.seed(131415)

  # Generate test data
  n <- 20
  x <- seq(-2, 2, length.out = n)
  y <- 1 + 0.5 * x + rnorm(n, 0, 0.3)
  data <- data.frame(x = x, y = y)

  # Fit model
  fit <- bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 15)

  # Use tempfile to avoid writing to user directories
  temp_plot <- tempfile(fileext = ".png")
  png(temp_plot, width = 400, height = 300)

  result <- plot_quantile.bqr.svy(fit, data, predictor = "x", main = "Test bqr.svy")

  dev.off()

  # Test return structure
  expect_true(is.data.frame(result))
  expect_true(all(c("predictor", "predicted") %in% names(result)))
  expect_true(all(is.finite(result$predicted)))

  # Clean up
  if (file.exists(temp_plot)) file.remove(temp_plot)
})

test_that("plot_quantile_with_points.bqr.svy works correctly", {
  set.seed(161718)

  n <- 15
  x <- rnorm(n)
  y <- 2 * x + rnorm(n, 0, 0.5)
  data <- data.frame(x = x, y = y)

  fit <- bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 12)

  temp_plot <- tempfile(fileext = ".png")
  png(temp_plot, width = 400, height = 300)

  result <- plot_quantile_with_points.bqr.svy(fit, data, predictor = "x", main = "Points + Line bqr.svy")

  dev.off()

  expect_true(is.data.frame(result))
  expect_true(all(is.finite(result$predicted)))

  if (file.exists(temp_plot)) file.remove(temp_plot)
})

test_that("drawQuantile1D works correctly", {
  set.seed(202122)

  # Generate test data
  n <- 18
  x <- seq(-1, 1, length.out = n)
  y <- 1.5 - 0.8 * x + rnorm(n, 0, 0.4)
  data <- data.frame(Y = y, x = x)

  # Fit model
  fit <- bqr.svy(Y ~ x, data = data, quantile = 0.5, n_iter = 15)

  result <- drawQuantile1D(
    fit,
    datafile = data,
    response = "Y",
    x_var = "x",
    paintedArea = TRUE,
    print_plot = FALSE # to test data output
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("x", "tau", "yhat") %in% names(result)))
  expect_true(all(is.finite(result$yhat)))
})

test_that("drawQuantileRegion works correctly (2D)", {
  set.seed(232425)

  n <- 20
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y1 <- 1 + 2*x1 + rnorm(n)
  y2 <- -1 + 0.5*x2 + rnorm(n)
  data <- data.frame(Y1 = y1, Y2 = y2, x1 = x1, x2 = x2)

  fit <- mo.bqr.svy(cbind(Y1, Y2) ~ x1 + x2, data = data, quantile = 0.5, n_dir = 5)

  result <- drawQuantileRegion(
    fit,
    datafile = data,
    response = c("Y1","Y2"),
    xValue = data.frame(x1 = 0, x2 = 0),
    paintedArea = FALSE,
    print_plot = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(all(c("y1","y2","tau", "xid") %in% names(result)))
})

test_that("existing plot() method dispatches work without error", {
  set.seed(192021)

  n <- 18
  x <- rnorm(n)
  y <- x + rnorm(n, 0, 0.4)
  data <- data.frame(Y = y, x = x)

  # Single quantile fit
  fit_single <- bqr.svy(Y ~ x, data = data, quantile = 0.5, n_iter = 12)

  expect_no_error({
    plot(fit_single)  # should internally call drawQuantile1D
  })

  # Multi-output fit
  y2 <- -y + rnorm(n)
  data2 <- data.frame(Y1 = y, Y2 = y2, x = x)
  fit_multi <- mo.bqr.svy(cbind(Y1, Y2) ~ x, data = data2, quantile = 0.5)

  expect_no_error({
    plot(fit_multi)  # should internally call drawQuantileRegion
  })
})

