// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

/* ------------------------------------------------------------------
 *  MCMC_BWQR_SL.cpp  ―  Semi-likelihood (SL) Metropolis–Hastings     *
 *  sampler para regresión cuantílica bayesiana ponderada (p = 3).    *
 *                                                                    *
 *  Exporta  MCMC_BWQR_SL()   listo para llamar desde R con Rcpp.     *
 *  Inspirado en la función R  bayesQRSL_weighted_bio().              *
 * ------------------------------------------------------------------ */

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

/* ================================================================== */
/* 1. Utilidades                                                      */
/* ================================================================== */

inline arma::vec rmvnorm(const arma::vec& m, const arma::mat& L) {
  return m + L * randn<vec>(m.n_elem);
}

// log-posterior no normalizado (prior gaussiano + veros. SL)
static double log_post_SL(const arma::vec& beta,
                          const arma::vec& b0,
                          const arma::mat& B_inv,
                          const arma::vec& y,
                          const arma::mat& X,
                          const arma::vec& w,
                          double tau) {
  const int n = y.n_elem;
  // prior
  arma::vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);

  // semi-likelihood (Wang & He)
  arma::vec res = y - X * beta;
  arma::vec ind = tau - conv_to<vec>::from(res < 0);

  arma::vec w_un = mean(w) * w;               // w_uf * w_i  (uf = mean)
  arma::vec s_tau = X.t() * (w_un % ind);

  arma::mat XtW2X = X.t() * (X.each_col() % square(w));
  arma::mat w_cov = (n / (tau * (1.0 - tau))) * inv_sympd(XtW2X);

  double quad = dot(s_tau, w_cov * s_tau) / (2.0 * n);
  double ld   = 0.5 * log_det(2.0 * PI * XtW2X).real();

  return lp - quad - ld;
}

/* ================================================================== */
/* 2. MCMC Semi-likelihood (SL) MH                                    */
/* ================================================================== */

// [[Rcpp::export]]
Rcpp::List MCMC_BWQR_SL(const arma::vec& y,
                        const arma::mat& X,
                        const arma::vec& w,
                        double tau = 0.5,
                        int    n_mcmc  = 10000,
                        int    burnin  = 2000,
                        int    thin    = 10,
                        Rcpp::Nullable<Rcpp::NumericVector> b0_ = R_NilValue,
                        Rcpp::Nullable<Rcpp::NumericMatrix> B0_ = R_NilValue) {
  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Dimensiones incompatibles entre y, X y w");
  if (X.n_cols != 3)
    stop("Esta versión SL admite exactamente 3 covariables.");
  if (burnin >= n_mcmc) stop("burnin debe ser < n_mcmc");
  if (thin <= 0)        stop("thin debe ser positivo");

  // prior
  arma::vec b0;
  if (b0_.isNotNull()) b0 = as<arma::vec>(b0_); else b0 = arma::zeros<vec>(3);
  arma::mat B0;
  if (B0_.isNotNull()) B0 = as<arma::mat>(B0_); else B0 = 1000.0 * eye<mat>(3,3);
  arma::mat B_inv = inv_sympd(B0);

  // propuesta base
  arma::mat XtWX = X.t() * (X.each_col() % square(w));
  arma::mat Sigma_prop = (tau * (1.0 - tau) / y.n_elem) * inv_sympd(XtWX);
  arma::mat L_prop = chol(Sigma_prop, "lower");

  // salida
  const int n_keep = (n_mcmc - burnin) / thin;
  arma::mat beta_out(n_keep, 3, fill::none);
  int accept = 0, k_out = 0;

  arma::vec beta_curr = solve(X, y);   // arranque OLS
  double ct = 1.0;

  for (int k = 0; k < n_mcmc; ++k) {
    arma::vec beta_prop = rmvnorm(beta_curr, sqrt(ct) * L_prop);

    double logp_prop = log_post_SL(beta_prop, b0, B_inv, y, X, w, tau);
    double logp_curr = log_post_SL(beta_curr, b0, B_inv, y, X, w, tau);

    double log_acc = std::min(0.0, logp_prop - logp_curr);
    if (R::runif(0.0,1.0) < std::exp(log_acc)) {
      beta_curr = beta_prop;
      ++accept;
    }

    // adaptación ct (Roberts-Rosenthal)
    ct = std::exp(std::log(ct) + std::pow(k + 1.0, -0.8) * (std::exp(log_acc) - 0.234));

    if (k >= burnin && ((k - burnin) % thin == 0))
      beta_out.row(k_out++) = beta_curr.t();
  }

  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["call"]        = "MCMC_BWQR_SL"
  );
}
