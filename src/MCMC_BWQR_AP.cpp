// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

// Generador: m + L z, con L lower-triangular (p x p)
inline arma::vec rmvnorm(const arma::vec& m, const arma::mat& L) {
  return m + L * randn<vec>(m.n_elem);
}

/*
 * log_post: prior Normal(b0, B_inv^{-1}) + "approximate" basado en score
 * s_tau(β) = X' [ w ⊙ (τ - 1{y - Xβ < 0}) ]
 * Var(s_tau) ≈ τ(1-τ) X' diag(w) X    (manejo de pesos correcto)
 * Usamos:  ℓ(β) ∝ -0.5 * λ * s' Var^{-1} s  con λ = sum(w)
 *          + constante normalización de N(0, Var/λ)
 */
static double log_post(const arma::vec& beta,
                       const arma::vec& b0,
                       const arma::mat& B_inv,
                       const arma::vec& y,
                       const arma::mat& X,
                       const arma::vec& w,
                       double tau,
                       arma::mat& /*S (no usado)*/,
                       arma::mat& wcA,
                       arma::mat& wc)
{
  const double ridge = 1e-8;

  const int p = beta.n_elem;
  const double lambda = arma::accu(w);     // tamaño efectivo ~ n si w=1

  // Prior normal
  arma::vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);

  // Score en el punto β
  arma::vec res = y - X * beta;
  arma::vec ind = tau - arma::conv_to<arma::vec>::from(res < 0); // τ - 1{r<0}
  arma::vec s_tau = X.t() * (w % ind);                            // X' (w ⊙ ind)

  // Var(score) ≈ τ(1-τ) X' diag(w) X  (independiente de β)
  bool w_all_one = arma::approx_equal(w, arma::ones<arma::vec>(w.n_elem), "absdiff", 1e-12);
  if (w_all_one) {
    wcA = tau * (1.0 - tau) * (X.t() * X);
  } else {
    // X' diag(w) X = X' ( (diag(sqrt(w)) X) )
    arma::mat Xw = X.each_col() % arma::sqrt(w);
    wcA = tau * (1.0 - tau) * (Xw.t() * Xw);
  }
  wcA.diag() += ridge; // SPD

  // wc = Var^{-1}(score)
  bool ok = arma::inv_sympd(wc, wcA);
  if (!ok) wc = arma::pinv(wcA, 1e-12);

  // log |Var|
  double ld = arma::log_det_sympd(wcA);

  // Término cuadrático (concentración con λ) y normalización N(0, Var/λ)
  double quad = dot(s_tau, wc * s_tau);
  lp += -0.5 * lambda * quad
  - 0.5 * ( p * std::log(2.0 * PI) + ld - p * std::log(lambda) );

  return lp;
}


Rcpp::List _mcmc_bwqr_ap_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             double tau = 0.5,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue,
                             int print_progress = 0)
{
  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Dimensions of y, X and w must match.");
  if (n_mcmc <= 0)  stop("n_mcmc must be positive");
  if (burnin < 0)   stop("burnin must be non-negative");
  if (thin   <= 0)  stop("thin must be positive");
  if (burnin >= n_mcmc)
    stop("burnin must be less than n_mcmc");

  const int p = X.n_cols;
  const int n = y.n_elem;
  const double ridge = 1e-8;

  // Prior
  arma::vec b0 = b_prior_mean.isNotNull() ? Rcpp::as<arma::vec>(b_prior_mean) : arma::zeros<vec>(p);
  if (b0.n_elem != p)
    stop("b_prior_mean must have length equal to ncol(X)");

  arma::mat B_inv;
  if (B_prior_prec.isNotNull()) {
    B_inv = Rcpp::as<arma::mat>(B_prior_prec);
    if (B_inv.n_rows != p || B_inv.n_cols != p)
      stop("B_prior_prec must be a pxp matrix");
  } else {
    B_inv = arma::eye<mat>(p, p) / 100.0; // prior débil
  }

  // ===== Propuesta coherente con la curvatura =====
  // Var(score) ≈ τ(1−τ) X' diag(w) X
  arma::mat wcA_prop;
  {
    if (arma::approx_equal(w, arma::ones<arma::vec>(w.n_elem), "absdiff", 1e-12)) {
      wcA_prop = tau * (1.0 - tau) * (X.t() * X);
    } else {
      arma::mat Xw = X.each_col() % arma::sqrt(w);
      wcA_prop = tau * (1.0 - tau) * (Xw.t() * Xw);
    }
    wcA_prop.diag() += ridge;
  }
  double lambda = arma::accu(w);                     // ≈ n
  arma::mat post_prec = lambda * wcA_prop + B_inv;   // precisión aprox.
  arma::mat Sigma_prop = arma::inv_sympd(post_prec); // ~ cov posterior
  // por robustez, un pequeño ridge por si acaso
  Sigma_prop.diag() += 1e-12;

  arma::mat L_prop = chol(Sigma_prop, "lower");

  // Buffers
  arma::mat S(n, p, fill::zeros); // no usado, pero mantenido para firma de log_post
  arma::mat wcA(p, p), wc(p, p);

  const int n_keep = (n_mcmc - burnin) / thin;
  arma::mat beta_out((n_keep > 0 ? n_keep : 0), p, fill::none);
  int accept = 0;

  // Inicialización: OLS (si X no es cuadrada -> MCO por SVD)
  arma::vec beta_curr = arma::solve(X, y); // alternativa: iniciar en rq(τ) desde R

  // Escala adaptativa multiplicativa
  double ct = 2.38; // se ajusta hacia aceptar ~0.234
  int k_out = 0;

  for (int k = 0; k < n_mcmc; ++k) {
    // Progress printing
    if (print_progress > 0 && k % print_progress == 0) {
      Rcpp::Rcout << "Iteration " << k << " of " << n_mcmc << std::endl;
      R_CheckUserInterrupt();
    }

    arma::vec beta_prop = rmvnorm(beta_curr, std::sqrt(ct) * L_prop);

    double logp_prop = log_post(beta_prop, b0, B_inv, y, X, w, tau, S, wcA, wc);
    double logp_curr = log_post(beta_curr, b0, B_inv, y, X, w, tau, S, wcA, wc);

    double log_acc  = std::min(0.0, logp_prop - logp_curr);
    double acc_prob = std::exp(log_acc);

    if (R::runif(0.0, 1.0) < acc_prob) {
      beta_curr = beta_prop;
      ++accept;
    }

    // Actualización Robbins-Monro de la escala (sin tocar la forma de Sigma_prop)
    ct = std::exp(std::log(ct) + std::pow(k + 1.0, -0.8) * (acc_prob - 0.234));

    if (k >= burnin && ((k - burnin) % thin == 0)) {
      if (k_out < n_keep) beta_out.row(k_out++) = beta_curr.t();
    }
  }

  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["n_mcmc"]      = n_mcmc,
    _["burnin"]      = burnin,
    _["thin"]        = thin,
    _["n_samples"]   = n_keep,
    _["call"]        = "MCMC_BWQR_AP"
  );
}
