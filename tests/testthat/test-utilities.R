# Test for utility and helper functions
test_that("convergence_check works correctly", {
  set.seed(1001)  # Reproducibility

  # Create small dataset for a real bqr.svy fit
  n <- 5000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + 2 * x1 - 0.5 * x2 + rnorm(n, 0, 0.5)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  # Quick fit for testing
  fit <- bqr.svy(y ~ x1 + x2, data = data, quantile = 0.5, niter = 10000, burnin = 2000)

  # Test convergence check using bqr.svy method
  conv_result <- convergence_check(fit, verbose = FALSE)

  expect_true(is.list(conv_result))
  expect_true("rhat" %in% names(conv_result))
  expect_true("neff" %in% names(conv_result))
  expect_true("converged" %in% names(conv_result))

  expect_equal(length(conv_result$rhat), length(fit$beta) + 1)
  expect_equal(length(conv_result$neff), length(fit$beta) + 1)
  expect_true(is.logical(conv_result$converged))

  # R-hat values should be reasonable
  expect_true(all(conv_result$rhat > 0))
  expect_true(all(is.finite(conv_result$rhat)))
})

test_that("summarise_draws_custom works correctly", {
  set.seed(373839)

  # Create small dataset for testing
  n <- 20
  x1 <- rnorm(n)
  y <- 1 + 1.5 * x1 + rnorm(n)
  data <- data.frame(x1 = x1, y = y)

  # Fit model
  fit <- bqr.svy(y ~ x1, data = data, quantile = 0.5, n_iter = 200)

  # Test summarise_draws_custom with real draws
  summary_result <- summarise_draws_custom(fit$draws)

  expect_true(is.data.frame(summary_result))
  expect_equal(nrow(summary_result), length(fit$beta) + 1)
  expect_true("variable" %in% names(summary_result))
  expect_true("mean" %in% names(summary_result))
  expect_true("sd" %in% names(summary_result))
  expect_true("q2.5" %in% names(summary_result))
  expect_true("q97.5" %in% names(summary_result))
})

test_that("mo data generation works for testing", {
  set.seed(404142)  # Reproducibility

  # Test simulated data for mo.bqr.svy
  n <- 30
  x1 <- runif(n, -1, 1)
  x2 <- rnorm(n)
  
  # Simulate multivariate response
  y1 <- 1 + 2*x1 + 0.5*x2 + rnorm(n, 0, 0.5)
  y2 <- -1 + 1.5*x1 + rnorm(n, 0, 0.3)
  y3 <- 0.5 - x2 + rnorm(n, 0, 0.4)
  
  Y <- cbind(y1, y2, y3)
  sim_data <- data.frame(y1, y2, y3, x1, x2)

  # Check data structure
  expect_true(is.data.frame(sim_data))
  expect_equal(nrow(sim_data), n)
  expect_true("y1" %in% names(sim_data))
  expect_true("x1" %in% names(sim_data))
  expect_true("x2" %in% names(sim_data))

  # Check that data is finite
  expect_true(all(is.finite(sim_data$y1)))
  expect_true(all(is.finite(sim_data$y2)))
  expect_true(all(is.finite(sim_data$y3)))
  expect_true(all(is.finite(sim_data$x1)))
  expect_true(all(is.finite(sim_data$x2)))

  # Check response matrix structure
  expect_true(is.matrix(Y))
  expect_equal(ncol(Y), 3)  # Three response variables
})


test_that("edge cases and error handling", {
  # Test with very small datasets
  expect_no_error({
    set.seed(434445)
    n <- 5
    x <- rnorm(n)
    y <- x + rnorm(n, 0, 0.1)
    data <- data.frame(x = x, y = y)

    fit <- bqr.svy(y ~ x, data = data, quantile = 0.5, n_iter = 10)
    expect_s3_class(fit, "bqr.svy")
  })

  # Test with identical predictors (near-singularity)
  expect_no_error({
    set.seed(464748)
    n <- 15
    x1 <- rnorm(n)
    x2 <- x1 + rnorm(n, 0, 1e-6)  # Nearly identical
    y <- x1 + rnorm(n, 0, 0.2)
    data <- data.frame(x1 = x1, x2 = x2, y = y)

    fit <- bqr.svy(y ~ x1 + x2, data = data, quantile = 0.5, n_iter = 10)
    expect_s3_class(fit, "bqr.svy")
  })
})

