# Test for summary methods
test_that("summary.bqr.svy works correctly", {
  set.seed(222324)  # Reproducibility

  # Generate test data
  n <- 20
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 1 + 2*x1 - 0.5*x2 + rnorm(n, 0, 0.5)
  data <- data.frame(x1 = x1, x2 = x2, y = y)

  # Fit model
  fit <- bqr.svy(y ~ x1 + x2, data = data, quantile = 0.5, niter = 2000, burnin = 700)

  # Test summary
  summary_result <- summary(fit)

  expect_s3_class(summary_result, "summary.bqr.svy")
  expect_true(is.list(summary_result))
  expect_true("call" %in% names(summary_result))
  expect_true("coefficients" %in% names(summary_result))
  expect_true("quantile" %in% names(summary_result))

  # Test coefficients structure
  expect_true(is.data.frame(summary_result$coefficients))
  expect_equal(nrow(summary_result$coefficients), 3)  # intercept + 2 covariates
  expect_true("Mean" %in% names(summary_result$coefficients))
  expect_true("SD" %in% names(summary_result$coefficients))

  # Test print method
  expect_output(print(summary_result), "Bayesian Quantile Regression")
  expect_output(print(summary_result), "Quantile\\s*\\(tau\\)\\s*:\\s*0\\.5")

})

test_that("summary.bqr.svy works with multiple quantiles", {
  set.seed(333435)
  
  # Generate test data
  n <- 20
  x <- rnorm(n)
  y <- 1 + 2*x + rnorm(n, 0, 0.5)
  data <- data.frame(x = x, y = y)
  
  # Fit model with multiple quantiles (this should create a list of draws)
  fit <- bqr.svy(y ~ x, data = data, quantile = c(0.25, 0.5, 0.75), n_iter = 100, n_chains = 2)
  
  # Test that the fit has the expected structure
  expect_s3_class(fit, "bqr.svy")
  expect_equal(length(fit$quantile), 3)
  expect_true(is.list(fit$draws))  # Should be a list for multiple quantiles
  
  # Test summary - this is where the bug was occurring
  summary_result <- summary(fit)
  
  expect_s3_class(summary_result, "summary.bqr.svy")
  expect_true(is.list(summary_result))
  expect_true("per_tau" %in% names(summary_result))
  expect_equal(length(summary_result$per_tau), 3)  # One for each quantile
  
  # Each per_tau element should have the expected structure
  for (i in 1:3) {
    tau_summary <- summary_result$per_tau[[i]]
    expect_true("tau" %in% names(tau_summary))
    expect_true("coef_summary" %in% names(tau_summary))
    expect_true("full_summary" %in% names(tau_summary))
    expect_true(is.data.frame(tau_summary$coef_summary))
    expect_true("lower_ci" %in% names(tau_summary$coef_summary))
    expect_true("upper_ci" %in% names(tau_summary$coef_summary))
  }
  
  # Test print method
  expect_output(print(summary_result), "Bayesian Quantile Regression")
})

test_that("summary.mo.bqr.svy works correctly", {
  set.seed(252627)

  n <- 15
  x <- rnorm(n)
  y <- 0.5 + 1.5*x + rnorm(n, 0, 0.4)
  data <- data.frame(x = x, y = y)

  # Fit multiple quantile model
  fit <- mo.bqr.svy(y ~ x, data = data, quantile = c(0.25, 0.5, 0.75), n_iter = 15)

  # Test summary
  summary_result <- summary(fit)

  expect_s3_class(summary_result, "summary.mo_bqr.svy")
  expect_true(is.list(summary_result))

  # Test print method
  expect_output(print(summary_result), "Multiple-Output Bayesian Quantile")
  expect_output(print(summary_result), "0.250")
  expect_output(print(summary_result), "0.500")
  expect_output(print(summary_result), "0.750")
})


