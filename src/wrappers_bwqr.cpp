// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]
// [[Rcpp::plugins(cpp17)]]

#include <RcppArmadillo.h>
#include <RcppEigen.h>
using namespace Rcpp;

// ================================================================
//  Declaraciones adelantadas (FORWARD DECLARATIONS)
//  Nota: se agrega 'fix_sigma' al backend EM separable.
// ================================================================

Rcpp::List _bwqr_weighted_em_cpp_sep(  // SEPARABLE (por dirección)
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::MatrixXd& u,
    const Eigen::MatrixXd& gamma_u,
    double tau,
    const Eigen::VectorXd& mu0,        // (p+r)  o  K*(p+r)
    const Eigen::MatrixXd& sigma0,     // (p+r)x(p+r)  o  (K*(p+r))x(K*(p+r))
    double a0,
    double b0,
    double eps,
    int max_iter,
    bool verbose,
    Rcpp::Nullable<Rcpp::NumericVector> fix_sigma = R_NilValue // <<< NUEVO
);

// --- ACTUALIZADO: ALD con 'fix_sigma' en la declaración del backend ---
Rcpp::List _mcmc_bwqr_al_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec,
                             double c0,
                             double C0,
                             int print_progress = 0,
                             Rcpp::Nullable<Rcpp::NumericVector> fix_sigma = R_NilValue);

Rcpp::List _mcmc_bwqr_ap_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             double tau,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec,
                             int print_progress = 0);

Rcpp::List _mcmc_bwqr_sl_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec,
                             int print_progress = 1000);

// ================================================================
//  WRAPPERS EXPORTADOS A R
// ================================================================

// [[Rcpp::export(name = ".bwqr_weighted_em_cpp_sep")]]
Rcpp::List bwqr_weighted_em_cpp_sep_wrap(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::MatrixXd& u,        // d x K
    const Eigen::MatrixXd& gamma_u,  // d x (K*r), puede tener 0 cols
    double tau,
    const Eigen::VectorXd& mu0,      // (p+r) o K*(p+r)
    const Eigen::MatrixXd& sigma0,   // (p+r)x(p+r) o (K*(p+r))x(K*(p+r))
    double a0,
    double b0,
    double eps      = 1e-6,
    int    max_iter = 1000,
    bool   verbose  = false,
    Rcpp::Nullable<Rcpp::NumericVector> fix_sigma = R_NilValue // <<< NUEVO
) {
  return _bwqr_weighted_em_cpp_sep(
    y, x, w, u, gamma_u,
    tau, mu0, sigma0,
    a0, b0, eps, max_iter, verbose,
    fix_sigma
  );
}

// --- ACTUALIZADO: wrapper de AL con fix_sigma ---
// [[Rcpp::export(name = ".MCMC_BWQR_AL")]]
Rcpp::List MCMC_BWQR_AL_wrap(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau = 0.5,
                             int n_mcmc = 50000,
                             int burnin = 10000,
                             int thin   = 10,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue,
                             double c0 = 0.001,
                             double C0 = 0.001,
                             int print_progress = 0,
                             Rcpp::Nullable<Rcpp::NumericVector> fix_sigma = R_NilValue) {
  return _mcmc_bwqr_al_cpp(y, X, w, tau, n_mcmc, burnin, thin,
                           b_prior_mean, B_prior_prec, c0, C0,
                           print_progress, fix_sigma);
}

// [[Rcpp::export(name = ".MCMC_BWQR_AP")]]
Rcpp::List MCMC_BWQR_AP_wrap(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             double tau = 0.5,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue,
                             int print_progress = 0) {
  return _mcmc_bwqr_ap_cpp(y, X, w, n_mcmc, burnin, thin, tau, b_prior_mean, B_prior_prec, print_progress);
}

// [[Rcpp::export(name = ".MCMC_BWQR_SL")]]
Rcpp::List MCMC_BWQR_SL_wrap(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau = 0.5,
                             int n_mcmc = 10000,
                             int burnin = 2000,
                             int thin   = 10,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue,
                             int print_progress = 1000) {
  return _mcmc_bwqr_sl_cpp(y, X, w, tau, n_mcmc, burnin, thin, b_prior_mean, B_prior_prec, print_progress);
}
