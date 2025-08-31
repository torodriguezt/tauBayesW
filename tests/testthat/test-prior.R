# tests/testthat/test-priors.R

test_that("prior_default() and as_bqr_prior() work correctly", {
  p  <- 3
  nm <- c("(Intercept)", paste0("x", 1:(p - 1)))

  # Basic construction
  pr <- prior_default(p = p, names = nm)
  expect_s3_class(pr, "bqr_prior")
  expect_equal(names(pr$b0), nm)
  expect_equal(dimnames(pr$B0), list(nm, nm))

  # Scalar expansion (strip names for pure numeric equality)
  pr2 <- prior_default(p = p, b0 = 0, B0 = 1000, names = nm)
  expect_equal(unname(pr2$b0), rep(0, p))
  expect_equal(unname(diag(pr2$B0)), rep(1000, p))

  # Coercion from legacy list
  legacy <- list(b0 = 0, B0 = 1000)
  pr3 <- as_bqr_prior(legacy, p = p, names = nm, method = "score")
  expect_s3_class(pr3, "bqr_prior")
  expect_equal(unname(pr3$b0), rep(0, p))
  expect_equal(unname(diag(pr3$B0)), rep(1000, p))

  pr4 <- as_bqr_prior(legacy, p = p, names = nm, method = "approximate")
  expect_s3_class(pr4, "bqr_prior")
  expect_equal(unname(pr4$b0), rep(0, p))
  expect_equal(unname(diag(pr4$B0)), rep(1000, p))

  # Validation checks
  expect_error(
    as_bqr_prior(list(b0 = c(1, 2), B0 = diag(1, p)), p = p),
    "length\\(b0\\) must be"
  )
  expect_error(
    as_bqr_prior(list(b0 = rep(0, p), B0 = diag(1, p - 1)), p = p),
    "B0 must be"
  )

  # Print method
  out <- capture.output(print(pr))
  expect_true(any(grepl("bqr_prior", out)))
})

test_that("bqr.svy integrates with informative and default priors", {
  skip_if_not(exists("bqr.svy"), "bqr.svy not available")
  skip_if_not(exists("simulate_bqr_data"), "simulate_bqr_data not available")
  skip_if_not(exists(".MCMC_BWQR_SL"), "C++ backend .MCMC_BWQR_SL (score) not available")

  set.seed(101)
  sim <- simulate_bqr_data(n = 30, betas = c(1, 0.5, -0.5), sigma = 1)

  # --- Prior as classed object (default) ---
  pr_obj <- prior_default(p = 3, b0 = 0, B0 = 100, names = c("(Intercept)", "x1", "x2"))
  fit_obj <- bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = 0.5,
    method   = "score",
    niter    = 1500,
    burnin   = 500,
    thin     = 1,
    prior    = pr_obj
  )
  expect_s3_class(fit_obj, "bwqr_fit")
  expect_s3_class(fit_obj$prior, "bqr_prior")
  expect_length(fit_obj$beta, 3)
  expect_true(is.matrix(fit_obj$draws))

  # --- Prior as informative ---
  pr_inf <- prior_default(
    p     = 3,
    b0    = c(1, 0.5, -0.5),
    B0    = diag(c(0.1, 0.1, 0.1)),
    names = c("(Intercept)", "x1", "x2")
  )
  fit_inf <- bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = 0.5,
    method   = "score",
    niter    = 1500,
    burnin   = 500,
    thin     = 1,
    prior    = pr_inf
  )
  expect_s3_class(fit_inf, "bwqr_fit")
  expect_s3_class(fit_inf$prior, "bqr_prior")
})

test_that("bqr.svy works with approximate method and legacy-list prior", {
  skip_if_not(exists("bqr.svy"), "bqr.svy not available")
  skip_if_not(exists("simulate_bqr_data"), "simulate_bqr_data not available")
  skip_if_not(exists(".MCMC_BWQR_AP"), "C++ backend .MCMC_BWQR_AP (approximate) not available")

  set.seed(202)
  sim <- simulate_bqr_data(n = 25, betas = c(0.5, 0.8, -0.3), sigma = 1)

  # Legacy list prior (will be coerced)
  pr_legacy <- list(b0 = 0, B0 = 200)
  fit <- bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = 0.5,
    method   = "approximate",
    niter    = 1200,
    burnin   = 300,
    thin     = 1,
    prior    = pr_legacy
  )
  expect_s3_class(fit, "bwqr_fit")
  expect_s3_class(fit$prior, "bqr_prior")
})

