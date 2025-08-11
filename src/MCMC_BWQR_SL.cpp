// src/MCMC_BWQR_SL.cpp
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

/* ------------------ utilities ------------------ */
inline arma::vec rmvnorm(const arma::vec& m, const arma::mat& L) {
  return m + L * randn<vec>(m.n_elem);
}

static double log_post_SL(const arma::vec& beta,
                          const arma::vec& b0,
                          const arma::mat& B_inv,
                          const arma::vec& y,
                          const arma::mat& X,
                          const arma::vec& w,
                          double tau) {
  int n = y.n_elem;
  arma::vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);

  arma::vec res = y - X * beta;
  arma::vec ind = tau - conv_to<vec>::from(res < 0);

  arma::vec w_un = mean(w) * w;                  // w_uf * w_i
  arma::vec s_tau = X.t() * (w_un % ind);

  arma::mat XtW2X = X.t() * (X.each_col() % square(w));
  arma::mat w_cov = (n / (tau * (1.0 - tau))) * inv_sympd(XtW2X);

  double quad = dot(s_tau, w_cov * s_tau) / (2.0 * n);
  double ld   = 0.5 * log_det(2.0 * PI * XtW2X).real();

  return lp - quad - ld;
}

/* ------------------ MCMC BWQR - SL (internal) ------------------ */
Rcpp::List _mcmc_bwqr_sl_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau = 0.5,
                             int n_mcmc  = 10000,
                             int burnin  = 2000,
                             int thin    = 10,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue) {

  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Incompatible dimensions between y, X and w");
  if (burnin >= n_mcmc) stop("burnin must be < n_mcmc");
  if (thin <= 0)        stop("thin must be positive");

  const int p = X.n_cols;

  /* ----- prior -------------------------------------------------- */
  arma::vec b0;
  if (b_prior_mean.isNotNull()) {
    b0 = as<arma::vec>(b_prior_mean);
    if (b0.n_elem != p)
      stop("b_prior_mean must have length equal to ncol(X)");
  } else {
    b0 = arma::zeros<vec>(p);
  }

  arma::mat B_inv;
  if (B_prior_prec.isNotNull()) {
    B_inv = as<arma::mat>(B_prior_prec);
    if (B_inv.n_rows != p || B_inv.n_cols != p)
      stop("B_prior_prec must be a pxp matrix");
  } else {
    B_inv = arma::eye<mat>(p, p) / 1000.0;
  }

  /* ----- base proposal Sigma_prop --------------------------------- */
  arma::mat XtWX = X.t() * (X.each_col() % square(w));
  arma::mat Sigma_prop = (tau * (1.0 - tau) / y.n_elem) * inv_sympd(XtWX);
  arma::mat L_prop = chol(Sigma_prop, "lower");

  /* ----- output ------------------------------------------------ */
  const int n_keep = (n_mcmc - burnin) / thin;
  arma::mat beta_out(n_keep, p, fill::none);
  int accept = 0, k_out = 0;

  arma::vec beta_curr = solve(X, y);   // initial OLS
  double ct = 1.0;

  for (int k = 0; k < n_mcmc; ++k) {
    arma::vec beta_prop = rmvnorm(beta_curr, std::sqrt(ct) * L_prop);

    double logp_prop = log_post_SL(beta_prop, b0, B_inv, y, X, w, tau);
    double logp_curr = log_post_SL(beta_curr, b0, B_inv, y, X, w, tau);

    double log_acc = std::min(0.0, logp_prop - logp_curr);
    if (R::runif(0.0,1.0) < std::exp(log_acc)) {
      beta_curr = beta_prop;
      ++accept;
    }

    ct = std::exp(std::log(ct) +
      std::pow(k + 1.0, -0.8) * (std::exp(log_acc) - 0.234));

    if (k >= burnin && ((k - burnin) % thin == 0))
      beta_out.row(k_out++) = beta_curr.t();
  }

  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["call"]        = "MCMC_BWQR_SL"
  );
}

