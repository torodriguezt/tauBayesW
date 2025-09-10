// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp14)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

constexpr double PI = 3.14159265358979323846;

// --- Cholesky seguro con ridge creciente ---
inline bool safe_chol(mat& L, const mat& A, double& ridge, const bool lower=true) {
  const double diag_mean = mean(A.diag());
  ridge = std::max(ridge, 1e-12 * std::max(1.0, std::abs(diag_mean)));
  mat M;
  for (int i = 0; i < 8; ++i) {                   // hasta 8 incrementos de ridge
    M = A + ridge * eye<mat>(A.n_rows, A.n_cols);
    if (chol(L, M, lower ? "lower" : "upper")) return true;
    ridge *= 10.0;
  }
  return false;
}

// --- Resuelve A^{-1} s usando Cholesky: z = A^{-1} s (A SPD) ---
inline vec spd_solve_from_chol(const mat& L, const vec& s) {
  // L es lower-triangular: A = L L'
  vec v = solve(trimatl(L), s, solve_opts::fast);     // L v = s
  vec z = solve(trimatl(L.t()), v, solve_opts::fast); // L' z = v
  return z;
}

// --- log-posterior "Score Likelihood" estable ---
// prior: beta ~ N(b0, B_inv^{-1})
// s_tau(beta) = X' [ w ⊙ (tau - 1{y - X beta < 0}) ]
// Var[s_tau] ≈ tau(1-tau) * XtWX, con XtWX = X' diag(w) X  (NO w^2)
// Pseudo-loglik ∝ -0.5 * lambda * s' Var^{-1} s   (determinante es constante en beta)
static double log_post_SL_stable(const vec& beta,
                                 const vec& b0,
                                 const mat& B_inv,
                                 const vec& y,
                                 const mat& X,
                                 const vec& w_norm,   // pesos normalizados a media 1
                                 double tau,
                                 const mat& L_XtWX,   // chol lower de XtWX (w.r.t. w_norm)
                                 double lambda) {
  // Prior
  vec diff = beta - b0;
  double lp = -0.5 * dot(diff, B_inv * diff);

  // Score s_tau
  vec res  = y - X * beta;
  vec ind  = tau - conv_to<vec>::from(res < 0);      // tau - 1{res<0}
  vec s    = X.t() * (w_norm % ind);                 // X' diag(w) (tau - I)

  // Var^{-1} s = [tau(1-tau) XtWX]^{-1} s = (1/(tau(1-tau))) * XtWX^{-1} s
  vec z = spd_solve_from_chol(L_XtWX, s);            // z = XtWX^{-1} s
  double quad = 0.5 * (lambda / (tau * (1.0 - tau))) * dot(s, z);

  return lp - quad;                                  // sin determinante: constante en beta
}

Rcpp::List _mcmc_bwqr_sl_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau = 0.5,
                             int n_mcmc  = 10000,
                             int burnin  = 2000,
                             int thin    = 10,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue,
                             int print_progress = 1000) {
  // --- Validaciones básicas ---
  if (y.n_elem != X.n_rows || w.n_elem != y.n_elem)
    stop("Incompatible dimensions between y, X y w");
  if (!(tau > 0.0 && tau < 1.0))
    stop("tau debe estar en (0,1)");
  if (burnin >= n_mcmc) stop("burnin debe ser < n_mcmc");
  if (thin <= 0)        stop("thin debe ser positivo");

  const int n = y.n_elem;
  const int p = X.n_cols;

  // --- Prior ---
  vec b0 = arma::zeros<vec>(p);
  if (b_prior_mean.isNotNull()) {
    b0 = as<vec>(b_prior_mean);
    if ((int)b0.n_elem != p) stop("b_prior_mean debe tener longitud ncol(X)");
  }

  mat B_inv = eye<mat>(p, p) / 1e6; // prior muy difuso por defecto
  if (B_prior_prec.isNotNull()) {
    B_inv = as<mat>(B_prior_prec);
    if (B_inv.n_rows != (uword)p || B_inv.n_cols != (uword)p)
      stop("B_prior_prec debe ser pxp");
  }

  // --- Pesos: normalizar a media 1 para estabilidad ---
  vec w_norm = w / mean(w);
  if (w_norm.min() <= 0.0) stop("Todos los pesos deben ser > 0");
  const double lambda = sum(w_norm); // ≈ n si ya estaban normalizados

  // --- XtWX y su Cholesky seguro ---
  // XtWX = X' diag(w) X = X.t() * (X.each_col() % w)
  mat XtWX = X.t() * (X.each_col() % w_norm);
  mat L_XtWX;
  double ridge_X = 0.0;
  if (!safe_chol(L_XtWX, XtWX, ridge_X, true))
    stop("Cholesky de XtWX falló incluso con ridge");

  // --- Propuesta: preacondicionada y estable ---
  // Base: S_base = (B_inv + kappa * XtWX)^{-1}
  const double kappa = 1.0;
  mat Prec = B_inv + kappa * XtWX;
  mat L_Prec;
  double ridge_P = 0.0;
  if (!safe_chol(L_Prec, Prec, ridge_P, true))
    stop("Cholesky de (B_inv + kappa*XtWX) falló");

  // Invertir via Cholesky para obtener cov de propuesta
  // S_base = Prec^{-1}  ⇒  chol(S_base) = inv(L_Prec')  (más estable con solve)
  mat I_p = eye<mat>(p, p);
  mat L_prop_base = solve(trimatl(L_Prec.t()), I_p, solve_opts::fast); // upper solve
  // Escala óptima 2.38^2/p y adaptación multiplicativa ct
  double scale0 = (p > 1) ? (2.38 * 2.38 / p) : 1.0;

  // --- Salida ---
  const int n_keep = (n_mcmc - burnin) / thin;
  mat beta_out(n_keep, p, fill::none);
  int accept = 0, k_out = 0;

  // --- Inicialización ---
  vec beta = solve(X, y); // LS
  double ct = 1.0;

  // Precomputar log posterior actual
  double logp_curr = log_post_SL_stable(beta, b0, B_inv, y, X, w_norm, tau, L_XtWX, lambda);

  // --- MCMC ---
  for (int k = 0; k < n_mcmc; ++k) {
    if (print_progress > 0 && (k + 1) % print_progress == 0) {
      Rprintf("Iteration %d of %d\n", k + 1, n_mcmc);
      R_CheckUserInterrupt();
    }

    // Propuesta: beta + sqrt(ct*scale0) * L_prop_base * z, z~N(0,I)
    vec z = randn<vec>(p);
    vec beta_prop = beta + std::sqrt(ct * scale0) * (L_prop_base * z);

    double logp_prop = log_post_SL_stable(beta_prop, b0, B_inv, y, X, w_norm, tau, L_XtWX, lambda);

    double log_alpha = std::min(0.0, logp_prop - logp_curr);
    double u = R::runif(0.0, 1.0);
    const double acc_prob = std::exp(log_alpha);

    if (u < acc_prob) {
      beta = beta_prop;
      logp_curr = logp_prop;
      ++accept;
    }

    // Adaptación de escala (Robbins–Monro), con límites para estabilidad
    // objetivo ~ 0.234
    double step = std::pow(k + 1.0, -0.8);
    ct = std::exp(std::log(ct) + step * (acc_prob - 0.234));
    ct = std::max(1e-4, std::min(ct, 1e4)); // clamp

    // Guardar
    if (k >= burnin && ((k - burnin) % thin == 0)) {
      beta_out.row(k_out++) = beta.t();
    }
  }

  return List::create(
    _["beta"]        = beta_out,
    _["accept_rate"] = double(accept) / n_mcmc,
    _["ridge_X"]     = ridge_X,
    _["ridge_P"]     = ridge_P,
    _["call"]        = "MCMC_BWQR_SL_STABLE"
  );
}
