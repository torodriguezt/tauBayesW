library(pracma)  # for nullspace()


set.seed(123)
d <- 2
n_dir <- 2
p <- 2
coef_names <- c("x1", "x2")

n <- 50
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))
y <- matrix(rnorm(n * d), n, d)
wts <- rep(1, n)

prior <- list(
  beta_mean   = rep(0, p),
  beta_cov    = diag(1e6, p),
  sigma_shape = 1e-3,
  sigma_rate  = 1e-3
)
gamma_prior_var <- 1.0
q <- 0.5
epsilon <- 1e-6
max_iter <- 1000
verbose <- TRUE


# Build U and Gamma

U <- matrix(NA_real_, d, n_dir)
for (k in seq_len(n_dir)) {
  u_k <- rnorm(d)
  u_k <- u_k / sqrt(sum(u_k^2))  # normalize
  U[, k] <- u_k
}
Gamma_list <- lapply(seq_len(n_dir), function(k) pracma::nullspace(t(U[, k])))


for (k in seq_len(n_dir)) {
  u_k <- U[, k]
  gamma_uk <- Gamma_list[[k]]
  r_k <- if (d > 1) ncol(gamma_uk) else 0
  p_ext <- p + r_k

  mu0_ext <- c(prior$beta_mean, rep(0, r_k))
  sigma0_ext <- diag(1e6, p_ext)
  sigma0_ext[1:p, 1:p] <- prior$beta_cov
  if (r_k > 0) {
    sigma0_ext[(p+1):p_ext, (p+1):p_ext] <- diag(gamma_prior_var, r_k)
  }

  u_k_matrix <- matrix(u_k, ncol = 1)
  gamma_k_matrix <- if (d > 1) gamma_uk else matrix(numeric(0), d, 0)

  # -----------------------------
  #  EM C++ function
  # -----------------------------
  #cpp_result <- .bwqr_weighted_em_cpp_sep(
  #  y        = y,
  #  x        = X,
  #  w        = wts,
  #  u        = u_k_matrix,
  #  gamma_u  = gamma_k_matrix,
  #  tau      = q,
  #  mu0      = mu0_ext,
  #  sigma0   = sigma0_ext,
  #  a0       = prior$sigma_shape,
  #  b0       = prior$sigma_rate,
  #  eps      = epsilon,
  #  max_iter = max_iter,
  #  verbose  = verbose
  #)

  #cat("\nEM call completed for direction", k, "\n")
}
