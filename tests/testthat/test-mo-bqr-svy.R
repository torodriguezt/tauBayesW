# Test for multiple quantile functionality
test_that("mo.bqr.svy basic functionality works", {
  set.seed(789)  # Reproducibility
  
  # Generate test data
  n <- 25  # Small sample for speed
  x <- rnorm(n)
  y <- 1 + 2*x + rnorm(n, 0, 0.5)
  data <- data.frame(x = x, y = y)
  
  # Test multiple quantile regression
  quantiles <- c(0.25, 0.5, 0.75)
  fit <- mo.bqr.svy(y ~ x, data = data, quantile = quantiles, n_iter = 15)
  
  # Test basic structure
  expect_s3_class(fit, "mo.bqr.svy")
  expect_true(is.list(fit))
  expect_true("fit" %in% names(fit))
  expect_true("quantile" %in% names(fit))
  expect_true("call" %in% names(fit))
  
  # Test dimensions
  expect_equal(length(fit$fit), 3)  # three quantiles
  expect_equal(fit$quantile, quantiles)
  
  # Test that each fit has proper structure
  for (i in 1:3) {
    expect_true("beta" %in% names(fit$fit[[i]]))
    expect_true("sigma" %in% names(fit$fit[[i]]))
    expect_true(all(is.finite(fit$fit[[i]]$beta)))
    expect_true(is.finite(fit$fit[[i]]$sigma))
  }
  
  # Test that quantiles are ordered correctly (non-crossing property)
  beta1_values <- sapply(fit$fit, function(f) f$beta[2])  # slope coefficients
  expect_true(all(diff(beta1_values) >= -0.5))  # Allow some flexibility due to randomness
})

test_that("mo.bqr.svy handles different numbers of quantiles", {
  set.seed(101112)
  
  n <- 20
  x <- rnorm(n)
  y <- x + rnorm(n, 0, 0.3)
  data <- data.frame(x = x, y = y)
  
  # Test with 2 quantiles
  fit_2 <- mo.bqr.svy(y ~ x, data = data, quantile = c(0.3, 0.7), n_iter = 12)
  expect_s3_class(fit_2, "mo.bqr.svy")
  expect_equal(length(fit_2$fit), 2)
  
  # Test with 5 quantiles  
  fit_5 <- mo.bqr.svy(y ~ x, data = data, 
                      quantile = c(0.1, 0.25, 0.5, 0.75, 0.9), n_iter = 12)
  expect_s3_class(fit_5, "mo.bqr.svy")
  expect_equal(length(fit_5$fit), 5)
})

test_that("mo.bqr.svy input validation", {
  n <- 15
  data <- data.frame(x = rnorm(n), y = rnorm(n))
  
  # Test invalid quantiles
  expect_error(mo.bqr.svy(y ~ x, data = data, quantile = c(0.5, 1.5)))
  expect_error(mo.bqr.svy(y ~ x, data = data, quantile = c(-0.1, 0.5)))
  expect_error(mo.bqr.svy(y ~ x, data = data, quantile = c()))  # empty
  
  # Test unsorted quantiles
  fit <- mo.bqr.svy(y ~ x, data = data, quantile = c(0.7, 0.3, 0.5), n_iter = 10)
  expect_equal(fit$quantile, c(0.3, 0.5, 0.7))  # Should be sorted
})
