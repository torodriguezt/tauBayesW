// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

/* ------------------------------------------------------------------
 *  MCMC_BWQR_AP.cpp  ―  Adaptive‐proposal Metropolis–Hastings sampler
 *  para regresión cuantílica bayesiana ponderada (p = 3).
 *  Exporta  MCMC_BWQR_AP()  para su uso directo desde R via Rcpp.
 * ------------------------------------------------------------------ */

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

/* ================================================================== */
/* 1. Utilidades                                                      */
/* ================================================================== */

// muestreo normal multivariante N(m, Σ)  —  requiere L = chol(Σ, "lower")
inline arma::vec rmvnorm(const arma::vec& m, const arma::mat& L)
{
  return m + L * randn<vec>(m.n_elem);
}

// log-posterior no normalizado (hasta constante aditiva)
static double log_post(const arma::vec& beta,
                       const arma::vec& b0,
                       const arma::mat& B_inv,
                       const arma::vec& y,
                       const arma::mat& X,
                       const arma::vec& w,
                       double tau,
                       double w_scale,
                       // buffers — se reutilizan en cada llamada
                       arma::mat& S,
                       arma::mat& wcA,
                       arma::mat& wc)
{
  // Prior gaussiano ~ N(b0, B0)   ⇒   −½ (β−b0)^T B⁻¹ (β−b0)
  arma::vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);

  // Verosimilitud tipo check-loss ponderada (Wang & He 2007)
  double wuf = mean(w) * w_scale;
  arma::vec res = y - X * beta;
  arma::vec ind = tau - conv_to<vec>::from(res < 0);   // τ − 𝟙(res<0)

  arma::vec tmp = wuf * w % ind;                       // wuf·w·ind
  arma::vec s_tau = X.t() * tmp;                       // Σ wuf w_i ind_i x_i

  // Construir S = diag(w_i ind_i) X (vectorizado)
  S.each_col() = w % ind;   // n×p
  S %= X;                   // elemento a elemento

  arma::vec invw = 1.0 / (wuf * w);
  arma::vec fac  = (1.0 - invw) / square(invw);
  wcA = (S.each_col() % fac).t() * S;                  // Σ fac_i x_i x_iᵀ

  bool ok = inv_sympd(wc, wcA);
  if(!ok) wc = pinv(wcA, 1e-12);

  double quad = dot(s_tau, wc * s_tau);
  double ld   = log_det(wcA).real();
  lp += -0.5 * quad - 0.5 * (std::log(2.0*PI) + ld);
  return lp;
}

/* ================================================================== */
/* 2. Adaptive Proposal Metropolis–Hastings (AP)                      */
/* ================================================================== */

// [[Rcpp::export]]
Rcpp::List MCMC_BWQR_AP(const arma::vec& y,
                        const arma::mat& X,
                        const arma::vec& w,
                        double tau = 0.5,
                        int n_mcmc = 20000,
                        int burnin = 5000,
                        int thin   = 100,
                        double w_scale = 2.0,
                        Rcpp::Nullable<Rcpp::NumericVector> b0_ = R_NilValue,
                        Rcpp::Nullable<Rcpp::NumericMatrix> B0_ = R_NilValue)
{
  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Las dimensiones de y, X y w deben coincidir.");
  if (X.n_cols != 3)
    stop("Esta versión admite exactamente 3 covariables (p = 3).");
  if (burnin >= n_mcmc) stop("burnin debe ser < n_mcmc");
  if (thin <= 0)        stop("thin debe ser positivo");

  // --- Procesar b0 y B0 (opcional) --------------------------------
  arma::vec b0;
  if (b0_.isNotNull()) {
    b0 = Rcpp::as<arma::vec>(b0_);
    if (b0.n_elem != 3) stop("b0 debe tener longitud 3");
  } else {
    b0 = arma::zeros<vec>(3);
  }

  arma::mat B0;
  if (B0_.isNotNull()) {
    B0 = Rcpp::as<arma::mat>(B0_);
    if (B0.n_rows != 3 || B0.n_cols != 3) stop("B0 debe ser 3×3");
  } else {
    B0 = 100.0 * arma::eye<mat>(3,3);
  }
  arma::mat B_inv = inv_sympd(B0);

  // --- Varianza propuesta de referencia ---------------------------
  arma::mat XtWX = X.t() * (X.each_col() % square(w));
  arma::mat Sigma_prop = (tau * (1.0 - tau) / y.n_elem) * inv_sympd(XtWX);
  arma::mat L_prop = chol(Sigma_prop, "lower");

  // Buffers
  arma::mat S(X.n_rows, 3, fill::zeros);
  arma::mat wcA(3,3), wc(3,3);

  // Salida
  const int n_keep = (n_mcmc - burnin) / thin;
  arma::mat beta_out(n_keep, 3, fill::none);
  int accept = 0;

  // Inicialización (OLS)
  arma::vec beta_curr = solve(X, y);
  double ct = 2.38;   // escala adaptativa inicial
  int k_out = 0;

  for (int k = 0; k < n_mcmc; ++k) {
    // Propuesta
    arma::vec beta_prop = rmvnorm(beta_curr, std::sqrt(ct) * L_prop);

    double logp_prop = log_post(beta_prop, b0, B_inv, y, X, w, tau, w_scale, S, wcA, wc);
    double logp_curr = log_post(beta_curr, b0, B_inv, y, X, w, tau, w_scale, S, wcA, wc);

    double log_acc = std::min(0.0, logp_prop - logp_curr);
    if (R::runif(0.0, 1.0) < std::exp(log_acc)) {
      beta_curr = beta_prop;
      ++accept;
    }

    // Adaptar ct
    ct = std::exp(std::log(ct) + std::pow(k + 1.0, -0.8) * (std::exp(log_acc) - 0.234));

    // Guardar
    if (k >= burnin && ((k - burnin) % thin == 0))
      beta_out.row(k_out++) = beta_curr.t();
  }

  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["call"]        = "MCMC_BWQR_AP"
  );
}
