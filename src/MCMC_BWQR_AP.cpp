// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

/* ------------------------------------------------------------------
 *  MCMC_BWQR_AP.cpp  -  Adaptive-proposal Metropolis-Hastings sampler
 *  para regresion cuantilica bayesiana ponderada con **p covariables
 *  arbitrarias**.  En esta version los parametros **n_mcmc**, **burnin**
 *  y **thin** son OBLIGATORIOS (sin valores por defecto) para replicar
 *  exactamente la llamada de la funcion R.
 * ------------------------------------------------------------------ */

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

/* ================================================================== */
/* 1. Utilidades                                                      */
/* ================================================================== */

// Muestreo normal multivariante N(m, Sigma) - requiere L = chol(Sigma, "lower")
inline arma::vec rmvnorm(const arma::vec& m, const arma::mat& L)
{
  return m + L * randn<vec>(m.n_elem);
}

// Log-posterior no normalizado (identico a la referencia R)
static double log_post(const arma::vec& beta,
                       const arma::vec& b0,
                       const arma::mat& B_inv,
                       const arma::vec& y,
                       const arma::mat& X,
                       const arma::vec& w,
                       double tau,
                       double w_scale,
                       // buffers - se reutilizan en cada llamada
                       arma::mat& S,
                       arma::mat& wcA,
                       arma::mat& wc)
{
  arma::vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);      // prior
  
  // Verosimilitud "check-loss" ponderada (Wang & He, 2007)
  double wuf = mean(w) * w_scale;
  arma::vec res = y - X * beta;
  arma::vec ind = tau - conv_to<vec>::from(res < 0);
  
  arma::vec tmp = wuf * w % ind;                   // wuf*w*ind
  arma::vec s_tau = X.t() * tmp;                   // Sum wuf w_i ind_i x_i
  
  // S = diag(w_i ind_i) X (vectorizado)
  S.each_col() = w % ind;
  S %= X;
  
  arma::vec invw = 1.0 / (wuf * w);
  arma::vec fac  = (1.0 - invw) / square(invw);
  wcA = (S.each_col() % fac).t() * S;              // Sum fac_i x_i x_iT
  
  bool ok = inv_sympd(wc, wcA);
  if (!ok) wc = pinv(wcA, 1e-12);
  
  double quad = dot(s_tau, wc * s_tau);
  double ld   = log_det(wcA).real();
  lp += -0.5 * quad - 0.5 * (std::log(2.0 * PI) + ld);
  return lp;
}

/* ================================================================== */
/* 2. Adaptive-proposal Metropolis-Hastings (parametros obligatorios) */
/* ================================================================== */

// [[Rcpp::export]]
Rcpp::List MCMC_BWQR_AP(const arma::vec& y,           // respuesta (n)
                        const arma::mat& X,           // diseno (nxp)
                        const arma::vec& w,           // pesos (n)
                        int n_mcmc,                   // iteraciones totales
                        int burnin,                   // descarte inicial
                        int thin,                     // thinning
                        double tau = 0.5,             // cuantil objetivo
                        double w_scale = 2.0,         // escala-weights
                        Rcpp::Nullable<Rcpp::NumericVector> b0_ = R_NilValue,
                        Rcpp::Nullable<Rcpp::NumericMatrix> B0_ = R_NilValue)
{
  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Las dimensiones de y, X y w deben coincidir.");
  if (n_mcmc <= 0)  stop("n_mcmc debe ser positivo");
  if (burnin < 0)   stop("burnin debe ser no negativo");
  if (thin   <= 0)  stop("thin debe ser positivo");
  if (burnin >= n_mcmc)
    stop("burnin debe ser menor que n_mcmc");
  
  const int p = X.n_cols;
  const int n = y.n_elem;
  
  /* --- Prior ----------------------------------------------------- */
  arma::vec b0 = b0_.isNotNull() ? Rcpp::as<arma::vec>(b0_) : arma::zeros<vec>(p);
  if (b0.n_elem != p)
    stop("b0 debe tener longitud igual a ncol(X)");
  
  arma::mat B0 = B0_.isNotNull() ? Rcpp::as<arma::mat>(B0_) : 100.0 * eye<mat>(p, p);
  if (B0.n_rows != p || B0.n_cols != p)
    stop("B0 debe ser una matriz %dx%d", p, p);
  arma::mat B_inv = inv_sympd(B0);
  
  /* --- Propuesta base Sigma_prop ~ (tau(1-tau)/n)(XT W^2 X)^-1 ----------- */
  arma::mat XtWX = X.t() * (X.each_col() % square(w));
  arma::mat Sigma_prop = (tau * (1.0 - tau) / n) * inv_sympd(XtWX);
  arma::mat L_prop = chol(Sigma_prop, "lower");
  
  /* --- Buffers --------------------------------------------------- */
  arma::mat S(n, p, fill::zeros);
  arma::mat wcA(p, p), wc(p, p);
  
  /* --- Salida ---------------------------------------------------- */
  const int n_keep = (n_mcmc - burnin) / thin;
  arma::mat beta_out(n_keep, p, fill::none);
  int accept = 0;
  
  /* --- Inicializacion ------------------------------------------- */
  arma::vec beta_curr = solve(X, y);   // OLS como punto de arranque
  double ct = 2.38;                    // escala adaptativa inicial
  int k_out = 0;
  
  /* --- MCMC ------------------------------------------------------ */
  for (int k = 0; k < n_mcmc; ++k) {
    // Propuesta β* = β + √ct·L·z
    arma::vec beta_prop = rmvnorm(beta_curr, std::sqrt(ct) * L_prop);
    
    double logp_prop = log_post(beta_prop, b0, B_inv, y, X, w, tau, w_scale,
                                S, wcA, wc);
    double logp_curr = log_post(beta_curr, b0, B_inv, y, X, w, tau, w_scale,
                                S, wcA, wc);
    
    double log_acc = std::min(0.0, logp_prop - logp_curr);
    double acc_prob = std::exp(log_acc);
    
    if (R::runif(0.0, 1.0) < acc_prob) {
      beta_curr = beta_prop;
      ++accept;
    }
    
    // Adaptacion Robbins-Monro para ct
    ct = std::exp(std::log(ct) + std::pow(k + 1.0, -0.8) * (acc_prob - 0.234));
    
    // Almacenar muestra si corresponde
    if (k >= burnin && ((k - burnin) % thin == 0))
      beta_out.row(k_out++) = beta_curr.t();
  }
  
  /* --- Empaquetar resultados ------------------------------------ */
  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["n_mcmc"]      = n_mcmc,
    _["burnin"]      = burnin,
    _["thin"]        = thin,
    _["n_samples"]   = n_keep,
    _["call"]        = "MCMC_BWQR_AP") ;
}