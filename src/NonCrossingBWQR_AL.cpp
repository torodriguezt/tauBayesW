// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(GIGrvg)]]
// [[Rcpp::plugins(cpp14)]]

// --------------------------------------------------------------
// Complete translation of the four BWQR routines:
//   * atualizarBETA   * atualizarSIGMA
//   * atualizarV_GIG  * bayesQRWeighted (complete Gibbs)
// The v update uses GIGrvg::rgig - same as in R -
// para que las cadenas sean comparables semilla a semilla.
// --------------------------------------------------------------

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

/* ============================================================= *
 * 1.  Inverse-Gaussian sampler IG(mu, lambda)                    *
 * ============================================================= */
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

/* ============================================================= *
 * 2.  beta | (sigma, v, ...)                                     *
 * ============================================================= */
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

  double sigma_eff = std::max(sigma, 1e-12);
  arma::mat A = symmatu(B_inv + XtWvX / (delta2 * sigma_eff));

  arma::mat Sigma;
  bool ok = arma::inv_sympd(Sigma, A);
  if (!ok) Sigma = arma::pinv(A, 1e-12);  // fallback

  arma::vec rhs = B_inv * b_zero +
    X.t() * ( wv % (y - theta * v) ) / (delta2 * sigma_eff);
  arma::vec mu  = Sigma * rhs;

  arma::mat L;
  ok = arma::chol(L, Sigma, "lower");
  if (!ok) {
    arma::mat Sigma_pd = Sigma +
      1e-8 * arma::eye<arma::mat>(Sigma.n_rows, Sigma.n_cols);
    arma::chol(L, Sigma_pd, "lower");
  }

  return mu + L * arma::randn<arma::vec>(b_zero.n_elem);
}

/* ============================================================= *
 * 3.  sigma | (beta, v, ...)                                     *
 * ============================================================= */
inline double draw_sigma(const arma::mat& X,
                         const arma::vec& w,
                         const arma::vec& beta,
                         const arma::vec& v,
                         const arma::vec& y,
                         double tau2,
                         double theta,
                         double c0, double C0)
{
  int    n      = y.n_elem;
  double alpha1 = c0 + 1.5 * n;

  arma::vec resid = y - X * beta - theta * v;
  double beta1 = C0 + arma::sum(w % v) +
    arma::dot((w % resid) / v, resid) / (2.0 * tau2);

  return 1.0 / R::rgamma(alpha1, 1.0 / beta1);
}

/* ============================================================= *
 * 4.  v | (beta, sigma, ...)  -  GIG(1/2, chi, psi) via GIGrvg::rgig *
 * ============================================================= */
// [[Rcpp::export]]
arma::vec atualizarV_GIG(const arma::vec& y,
                         const arma::mat& X,
                         const arma::vec& w,
                         const arma::vec& beta,
                         double delta2,
                         double theta,
                         double sigma,
                         int N)
{
  // Namespace y funcion rgig cargadas una sola vez
  static Rcpp::Environment gig_ns =
    Rcpp::Environment::namespace_env("GIGrvg");
  static Rcpp::Function rgig = gig_ns["rgig"];

  arma::vec resid = y - X * beta;
  arma::vec chi   = w % arma::square(resid) / (delta2 * sigma);
  arma::vec psi   = w * (2.0 / sigma + theta * theta / (delta2 * sigma));

  NumericVector v_r = rgig(
    _["n"]      = N,
    _["lambda"] = 0.5,
    _["chi"]    = Rcpp::wrap(chi),
    _["psi"]    = Rcpp::wrap(psi)
  );

  return as<arma::vec>(v_r);
}

/* ============================================================= *
 * 5.  Interfaces exportadas                                      *
 * ============================================================= */
// [[Rcpp::export]]
arma::vec atualizarBETA(const arma::vec& b,
                        const arma::mat& B,
                        const arma::mat& X,
                        const arma::vec& w,
                        double sigma,
                        double delta2,
                        double theta,
                        const arma::vec& v,
                        const arma::vec& y)
{
  arma::mat B_inv;
  bool ok = arma::inv_sympd(B_inv, B);
  if (!ok) stop("La matriz B debe ser PSD.");

  return draw_beta(X, w, v, y, sigma, delta2, theta, B_inv, b);
}

// [[Rcpp::export]]
double atualizarSIGMA(double c0, double C0,
                      const arma::mat& X,
                      const arma::vec& w,
                      const arma::vec& beta,
                      double tau2,
                      double theta,
                      const arma::vec& v,
                      const arma::vec& y,
                      int n)
{
  return draw_sigma(X, w, beta, v, y, tau2, theta, c0, C0);
}

/* ============================================================= *
 *  Gibbs completo  -  PARAMETROS MCMC **OBLIGATORIOS**          *
 * ============================================================= */
// [[Rcpp::export]]
Rcpp::List NonCrossingBWQR_AL(const arma::vec& y,
                              const arma::mat& X,
                              const arma::vec& w,
                              int    n_mcmc,
                              int    burnin_mcmc,
                              int    thin_mcmc,
                              double tau = 0.5)
{
  if (y.n_elem != X.n_rows || y.n_elem != w.n_elem)
    stop("Dimensiones de y, X y w no coinciden.");
  if (burnin_mcmc >= n_mcmc)
    stop("burnin_mcmc < n_mcmc");
  if (thin_mcmc <= 0)
    stop("thin_mcmc debe ser positivo");
  
  const int n = y.n_elem, p = X.n_cols;
  
  arma::mat beta_chain(n_mcmc, p, fill::zeros);
  arma::vec sigma_chain(n_mcmc,        fill::zeros);
  
  beta_chain.row(0) = solve(X, y).t();          // OLS inicial
  sigma_chain[0]    = 1.0;
  arma::vec v       = randg<vec>(n, distr_param(2.0, 1.0));
  
  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));
  const double c0 = 0.001, C0 = 0.001, tau2 = delta2;
  
  arma::mat B  = 1000.0 * eye<mat>(p, p);       // N(0, 1000 I)
  arma::vec b0 = zeros<vec>(p);
  
  /* ---------- Gibbs loop ---------- */
  for (int k = 1; k < n_mcmc; ++k) {
    beta_chain.row(k) =
      atualizarBETA(b0, B, X, w, sigma_chain[k-1],
                    delta2, theta, v, y).t();
    
    v = atualizarV_GIG(y, X, w, beta_chain.row(k).t(),
                       delta2, theta, 1.0, n);
    
    sigma_chain[k] = atualizarSIGMA(c0, C0, X, w,
                                    beta_chain.row(k).t(),
                                    tau2, theta, v, y, n);
  }
  
  /* ---------- sub-muestreo ---------- */
  arma::uvec keep = regspace<uvec>(burnin_mcmc, thin_mcmc, n_mcmc-1);
  const int M = keep.n_elem;
  
  arma::mat beta_out(M, p);
  arma::vec sigma_out(M);
  for (int i = 0; i < M; ++i) {
    beta_out.row(i) = beta_chain.row(keep[i]);
    sigma_out[i]    = sigma_chain[keep[i]];
  }
  
  return List::create(_["beta"]  = beta_out,
                      _["sigma"] = sigma_out);
}