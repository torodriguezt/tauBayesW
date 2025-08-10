// src/MCMC_BWQR_AL.cpp
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

#include <RcppArmadillo.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace arma;

/* ------------------------------------------------------------------ *
 * 1.  Inverse-Gaussian sampler IG(mu, lambda)
 * ------------------------------------------------------------------ */
inline double rinvgauss(double mu, double lambda)
{
  const double z  = R::rnorm(0.0, 1.0);
  const double y  = z * z;
  const double x1 = mu + (mu * mu * y) / (2.0 * lambda) -
    (mu / (2.0 * lambda)) *
    std::sqrt(4.0 * mu * lambda * y + mu * mu * y * y);
  const double u  = R::runif(0.0, 1.0);
  return (u <= mu / (mu + x1)) ? x1 : (mu * mu / x1);
}

/* ------------------------------------------------------------------ *
 * 2.  beta | (sigma, v, ...)
 * ------------------------------------------------------------------ */
static arma::vec draw_beta(const arma::mat& X,
                           const arma::vec& w,
                           const arma::vec& v,
                           const arma::vec& y,
                           double sigma,
                           double delta2,
                           double theta,
                           const arma::mat& B_inv,
                           const arma::vec& b_zero)
{
  arma::vec wv    = w / v;
  arma::mat XtWvX = X.t() * (X.each_col() % wv);

  double sigma_eff = std::max(sigma, 1e-12); // avoid /0
  arma::mat A = symmatu(B_inv + XtWvX / (delta2 * sigma_eff));

  arma::mat Sigma;
  bool   ok    = false;
  double ridge = 1e-8;

  for (int tries = 0; tries < 6 && !ok; ++tries) {
    ok = arma::inv_sympd(Sigma, A + ridge * arma::eye<arma::mat>(A.n_rows, A.n_cols));
    if (!ok) ridge *= 10.0;
  }
  if (!ok) {
    Sigma = arma::pinv(A, 1e-12);
    ridge = 1e-6;
  }

  arma::vec rhs = X.t() * ( wv % (y - theta * v) ) / (delta2 * sigma_eff);
  arma::vec mu  = Sigma * rhs;

  arma::mat L;
  ok = arma::chol(L, Sigma + ridge * arma::eye<arma::mat>(Sigma.n_rows, Sigma.n_cols),
                  "lower");
  if (!ok) {
    double ridge_chol = ridge;
    do {
      ridge_chol *= 10.0;
      ok = arma::chol(L, Sigma + ridge_chol *
        arma::eye<arma::mat>(Sigma.n_rows, Sigma.n_cols), "lower");
    } while (!ok && ridge_chol < 1e-2);

    if (!ok) {
      arma::vec eigval;
      arma::mat eigvec;
      arma::eig_sym(eigval, eigvec, Sigma);
      arma::mat Sigma_pd = eigvec *
        arma::diagmat(arma::clamp(eigval, 1e-12, arma::datum::inf)) *
        eigvec.t();
      arma::chol(L, Sigma_pd, "lower");
    }
  }

  return mu + L * arma::randn<arma::vec>(b_zero.n_elem);
}

/* ------------------------------------------------------------------ *
 * 3.  sigma | (beta, v, ...)
 * ------------------------------------------------------------------ */
inline double draw_sigma(const arma::mat& X,
                         const arma::vec& w,
                         const arma::vec& beta,
                         const arma::vec& v,
                         const arma::vec& y,
                         double tau2,
                         double theta,
                         double c0, double C0)
{
  const int    n      = y.n_elem;
  const double alpha1 = c0 + 1.5 * n;

  arma::vec resid = y - X * beta - theta * v;
  double beta1 = C0 +
    arma::sum(w % v) +
    arma::dot((w % resid) / v, resid) / (2.0 * tau2);

  return 1.0 / R::rgamma(alpha1, 1.0 / beta1);
}

/* ------------------------------------------------------------------ *
 * 4.  v | (beta, sigma, ...)
 * ------------------------------------------------------------------ */
static void update_v(arma::vec& v, const arma::mat& X,
                     const arma::vec& w, const arma::vec& beta,
                     const arma::vec& y,
                     double delta2, double theta,
                     double sigma)
{
  const int N = v.n_elem;
  for (int i = 0; i < N; ++i) {
    const double resid  = y[i] - arma::dot(X.row(i), beta);
    const double chi    = w[i] * resid * resid / delta2;
    const double psi    = w[i] * (2.0 / sigma + theta * theta / (delta2 * sigma));

    const double mu     = std::sqrt(chi / psi);
    const double lambda = std::sqrt(chi * psi);

    v[i] = std::max(rinvgauss(mu, lambda), 1e-8);
  }
}

/* ------------------------------------------------------------------ *
 * 5.  Gibbs sampler (función interna, no exportada a R)
 * ------------------------------------------------------------------ */
Rcpp::List _mcmc_bwqr_al_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau    = 0.5,
                             int    n_mcmc = 50000,
                             int    burnin = 10000,
                             int    thin   = 10)
{
  if (y.n_elem != X.n_rows || y.n_elem != w.n_elem)
    stop("Dimensions of y, X, and w must match.");
  if (burnin >= n_mcmc) stop("burnin must be < n_mcmc.");
  if (thin   <= 0)      stop("thin must be positive.");

  const int n = y.n_elem, p = X.n_cols;

  arma::mat beta_chain(n_mcmc, p, arma::fill::zeros);
  arma::vec sigma_chain(n_mcmc, arma::fill::zeros);

  beta_chain.row(0) = arma::solve(X, y).t();
  sigma_chain[0]    = 1.0;
  arma::vec v       = arma::randg<arma::vec>(n, arma::distr_param(2.0, 1.0));

  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));
  const double c0 = 0.001, C0 = 0.001, tau2 = delta2;

  const arma::mat B_inv  = arma::eye<arma::mat>(p, p) / 1000.0;
  const arma::vec b_zero = arma::zeros<arma::vec>(p);

  for (int k = 1; k < n_mcmc; ++k) {
    arma::vec beta_k = draw_beta(X, w, v, y, sigma_chain[k - 1],
                                 delta2, theta, B_inv, b_zero);
    beta_chain.row(k) = beta_k.t();

    update_v(v, X, w, beta_k, y, delta2, theta, sigma_chain[k - 1]);

    sigma_chain[k] = draw_sigma(X, w, beta_k, v, y,
                                tau2, theta, c0, C0);
  }

  arma::uvec keep = arma::regspace<arma::uvec>(burnin + thin, thin, n_mcmc - 1);
  const int   M   = keep.n_elem;

  NumericMatrix beta_out(M, p);
  NumericVector sigma_out(M);

  for (int i = 0; i < M; ++i) {
    for (int j = 0; j < p; ++j)
      beta_out(i, j) = beta_chain(keep[i], j);
    sigma_out[i] = sigma_chain[keep[i]];
  }

  return List::create(
    _["beta"]  = beta_out,
    _["sigma"] = sigma_out
  );
}
