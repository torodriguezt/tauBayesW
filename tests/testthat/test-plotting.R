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

test_that("plot_quantile.mo.bqr.svy works correctly", {
  set.seed(202122)

  # Generate test data
  n <- 18
  x <- seq(-1, 1, length.out = n)
  y <- 1.5 - 0.8 * x + rnorm(n, 0, 0.4)
  data <- data.frame(x = x, y = y)

  # Fit model
  fit <- mo.bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 15)

  temp_plot <- tempfile(fileext = ".png")
  png(temp_plot, width = 400, height = 300)

  result <- plot_quantile.mo.bqr.svy(fit, data, predictor = "x", main = "Test mo.bqr.svy")

  dev.off()

  expect_true(is.data.frame(result))
  expect_true(all(c("predictor", "predicted") %in% names(result)))
  expect_true(all(is.finite(result$predicted)))

  if (file.exists(temp_plot)) file.remove(temp_plot)
})

test_that("plot_quantile_with_points.mo.bqr.svy works correctly", {
  set.seed(232425)

  n <- 15
  x <- rnorm(n)
  y <- -1.2 * x + rnorm(n, 0, 0.5)
  data <- data.frame(x = x, y = y)

  fit <- mo.bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 12)

  temp_plot <- tempfile(fileext = ".png")
  png(temp_plot, width = 400, height = 300)

  result <- plot_quantile_with_points.mo.bqr.svy(fit, data, predictor = "x", main = "Points + Line mo.bqr.svy")

  dev.off()

  expect_true(is.data.frame(result))
  expect_true(all(is.finite(result$predicted)))

  if (file.exists(temp_plot)) file.remove(temp_plot)
})

test_that("existing plot() methods still work without error", {
  set.seed(192021)

  n <- 18
  x <- rnorm(n)
  y <- x + rnorm(n, 0, 0.4)
  data <- data.frame(x = x, y = y)

  # Single quantile object
  fit_single <- bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 12)

  temp_plot1 <- tempfile(fileext = ".png")
  png(temp_plot1, width = 400, height = 300)
  expect_no_error(plot(fit_single, type = "trace"))
  dev.off()

  temp_plot2 <- tempfile(fileext = ".png")
  png(temp_plot2, width = 400, height = 300)
  expect_no_error(plot(fit_single, type = "intervals"))
  dev.off()

  # Multiple-output object
  fit_multi <- mo.bqr.svy(y ~ x, data = data, quantile = c(0.25, 0.75), n_iter = 12)

  temp_plot3 <- tempfile(fileext = ".png")
  png(temp_plot3, width = 600, height = 400)
  expect_no_error(plot(fit_multi, type = "quantiles"))
  dev.off()

  temp_plot4 <- tempfile(fileext = ".png")
  png(temp_plot4, width = 400, height = 300)
  expect_no_error(plot(fit_multi, type = "convergence"))
  dev.off()

  for (f in c(temp_plot1, temp_plot2, temp_plot3, temp_plot4)) {
    if (file.exists(f)) file.remove(f)
  }
})
