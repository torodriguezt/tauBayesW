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

test_that("simulate_mo_bqr_data works correctly", {
  set.seed(404142)  # Reproducibility

  # Test data simulation
  n <- 30
  sim_data <- simulate_mo_bqr_data(n = n, p = 2, seed = 123)

  expect_true(is.list(sim_data))
  expect_true("data" %in% names(sim_data))
  expect_true("true_betas" %in% names(sim_data))
  expect_true("quantiles" %in% names(sim_data))

  # Check data structure
  expect_true(is.data.frame(sim_data$data))
  expect_equal(nrow(sim_data$data), n)
  expect_true("y" %in% names(sim_data$data))
  expect_true("x1" %in% names(sim_data$data))
  expect_true("x2" %in% names(sim_data$data))

  # Check that data is finite
  expect_true(all(is.finite(sim_data$data$y)))
  expect_true(all(is.finite(sim_data$data$x1)))
  expect_true(all(is.finite(sim_data$data$x2)))

  # Check true betas structure
  expect_true(is.matrix(sim_data$true_betas))
  expect_equal(ncol(sim_data$true_betas), 3)  # intercept + x1 + x2

  # Test with different parameters
  sim_data2 <- simulate_mo_bqr_data(n = 20, p = 1, seed = 456)
  expect_equal(nrow(sim_data2$data), 20)
  expect_equal(ncol(sim_data2$true_betas), 2)  # intercept + x1
})

test_that("simulate_bqr_data works correctly", {
  set.seed(505152)

  n <- 50
  betas <- c(1, 2, -0.5)
  sigma <- 0.5

  sim_data <- simulate_bqr_data(n = n, betas = betas, sigma = sigma, seed = 123)

  expect_true(is.list(sim_data))
  expect_true(all(c("data", "weights", "true_betas") %in% names(sim_data)))

  expect_true(is.data.frame(sim_data$data))
  expect_equal(nrow(sim_data$data), n)
  expect_true("y" %in% names(sim_data$data))
  expect_true("x1" %in% names(sim_data$data))
  expect_true("x2" %in% names(sim_data$data))
  expect_true(all(is.finite(sim_data$data$y)))
  expect_true(all(is.finite(sim_data$data$x1)))
  expect_true(all(is.finite(sim_data$data$x2)))

  expect_equal(length(sim_data$weights), n)
  expect_true(all(is.finite(sim_data$weights)))

  expect_equal(sim_data$true_betas, betas)

  fit <- bqr.svy(y ~ x1 + x2,
                 data = sim_data$data,
                 weights = sim_data$weights,
                 quantile = 0.5,
                 niter = 2000,
                 burnin = 500)
  expect_s3_class(fit, "bqr.svy")
  expect_equal(length(fit$beta), length(betas))
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

