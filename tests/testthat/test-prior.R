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
  expect_null(pr3$w_scale)  # score method does not use w_scale

  pr4 <- as_bqr_prior(legacy, p = p, names = nm, method = "approximate")
  expect_equal(pr4$w_scale, 2)  # approximate method uses default w_scale = 2

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

test_that("mo_prior_default() and as_mo_bqr_prior() work correctly", {
  p  <- 3
  nm <- c("(Intercept)", paste0("x", 1:(p - 1)))

  # Basic construction
  pr <- mo_prior_default(p = p, names = nm)
  expect_s3_class(pr, "mo_bqr_prior")
  expect_equal(names(pr$beta_mean), nm)
  expect_equal(dimnames(pr$beta_cov), list(nm, nm))

  # Scalar expansion (strip names for pure numeric equality)
  pr2 <- mo_prior_default(p = p, beta_mean = 0, beta_cov = 1000, names = nm)
  expect_equal(unname(pr2$beta_mean), rep(0, p))
  expect_equal(unname(diag(pr2$beta_cov)), rep(1000, p))

  # Coercion from legacy list
  legacy <- list(beta_mean = 0, beta_cov = 1000, sigma_shape = 1, sigma_rate = 1)
  pr3 <- as_mo_bqr_prior(legacy, p = p, names = nm)
  expect_s3_class(pr3, "mo_bqr_prior")

  # Validation checks
  bad1 <- legacy; bad1$sigma_shape <- -1
  expect_error(as_mo_bqr_prior(bad1, p = p), "sigma_shape")
  bad2 <- legacy; bad2$sigma_rate <- -5
  expect_error(as_mo_bqr_prior(bad2, p = p), "sigma_rate")

  # Print method
  out <- capture.output(print(pr))
  expect_true(any(grepl("mo_bqr_prior", out)))
})


# tests/testthat/test-priors-integration.R

test_that("bqr.svy accepts a bqr_prior object and a legacy-list prior", {
  skip_if_not(exists("bqr.svy"), "bqr.svy not available")
  skip_if_not(exists("simulate_bqr_data"), "simulate_bqr_data not available")
  skip_if_not(exists(".MCMC_BWQR_SL"), "C++ backend .MCMC_BWQR_SL (score) not available")

  set.seed(101)
  sim <- simulate_bqr_data(n = 30, betas = c(1, 0.5, -0.5), sigma = 1)

  # --- Prior as classed object ---
  # p = intercept + 2 slopes
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
  expect_true(is.numeric(fit_obj$beta))

  # --- Prior as legacy list (coercion path) ---
  pr_legacy <- list(b0 = 0, B0 = 50)  # expanded internally
  fit_legacy <- bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = 0.25,
    method   = "score",
    niter    = 1200,
    burnin   = 400,
    thin     = 1,
    prior    = pr_legacy
  )

  expect_s3_class(fit_legacy, "bwqr_fit")
  expect_s3_class(fit_legacy$prior, "bqr_prior")
  expect_length(fit_legacy$beta, 3)
})

test_that("bqr.svy accepts approximate prior with w_scale", {
  skip_if_not(exists("bqr.svy"), "bqr.svy not available")
  skip_if_not(exists("simulate_bqr_data"), "simulate_bqr_data not available")
  skip_if_not(exists(".MCMC_BWQR_AP"), "C++ backend .MCMC_BWQR_AP (approximate) not available")

  set.seed(202)
  sim <- simulate_bqr_data(n = 25, betas = c(0.5, 0.8, -0.3), sigma = 1)

  # Legacy list without w_scale -> should default to 2 inside as_bqr_prior(..., method="approximate")
  pr_legacy <- list(b0 = 0, B0 = 200)
  fit <- bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,   # approximate uses raw weights
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
  expect_true(is.numeric(fit$prior$w_scale))
})

test_that("mo.bqr.svy accepts an mo_bqr_prior object and a legacy-list prior", {
  skip_if_not(exists("mo.bqr.svy"), "mo.bqr.svy not available")
  skip_if_not(exists("simulate_mo_bqr_data"), "simulate_mo_bqr_data not available")
  skip_if_not(exists(".bwqr_weighted_em_cpp"), "C++ EM backend not available")

  set.seed(303)
  sim <- simulate_mo_bqr_data(n = 40, p = 2)  # y ~ x1 + x2, so p=2 -> intercept + 2 slopes => p+1=3

  # --- Prior as classed object ---
  pr_obj <- mo_prior_default(
    p           = 3,
    beta_mean   = 0,
    beta_cov    = 1000,
    sigma_shape = 0.01,
    sigma_rate  = 0.01,
    names       = c("(Intercept)", "x1", "x2")
  )

  fit_obj <- mo.bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = c(0.25, 0.75),
    algorithm = "em",
    max_iter  = 60,
    epsilon   = 1e-6,
    verbose   = FALSE,
    prior     = pr_obj
  )

  expect_s3_class(fit_obj, "mo.bqr.svy")
  expect_s3_class(fit_obj$prior, "mo_bqr_prior")
  expect_length(fit_obj$coefficients, 3)
  expect_true(is.list(fit_obj$fit))
  expect_equal(length(fit_obj$fit), 2)

  # --- Prior as legacy list (coercion path) ---
  pr_legacy <- list(
    beta_mean   = 0,
    beta_cov    = 500,
    sigma_shape = 0.02,
    sigma_rate  = 0.02
  )

  fit_legacy <- mo.bqr.svy(
    y ~ x1 + x2,
    weights  = sim$weights,
    data     = sim$data,
    quantile = c(0.1, 0.5, 0.9),
    algorithm = "em",
    max_iter  = 40,
    verbose   = FALSE,
    prior     = pr_legacy
  )

  expect_s3_class(fit_legacy, "mo.bqr.svy")
  expect_s3_class(fit_legacy$prior, "mo_bqr_prior")
  expect_length(fit_legacy$coefficients, 3)
})

