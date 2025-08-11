// src/wrappers.cpp
#include <RcppArmadillo.h>
#include <RcppEigen.h>
using namespace Rcpp;

// --------- Declaraciones de funciones internas ----------

// EM (Eigen)
Rcpp::List _bwqr_weighted_em_cpp(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::VectorXd& u,
    const Eigen::VectorXd& gamma_u,
    double tau,
    const Eigen::VectorXd& mu0,
    const Eigen::MatrixXd& sigma0,
    double a0,
    double b0,
    double eps,
    int max_iter,
    bool verbose
);

// ALD MCMC
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
                             double C0);

// Approximate MCMC
Rcpp::List _mcmc_bwqr_ap_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             double tau,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec);

// Score-like MCMC
Rcpp::List _mcmc_bwqr_sl_cpp(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec);

// --------- Wrappers exportados a R ----------

// EM
// [[Rcpp::export(name = ".bwqr_weighted_em_cpp")]]
Rcpp::List bwqr_weighted_em_cpp_wrap(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::VectorXd& u,
    const Eigen::VectorXd& gamma_u,
    double tau,
    const Eigen::VectorXd& mu0,
    const Eigen::MatrixXd& sigma0,
    double a0,
    double b0,
    double eps      = 1e-6,
    int    max_iter = 1000,
    bool   verbose  = false
) {
  return _bwqr_weighted_em_cpp(y, x, w, u, gamma_u,
                               tau, mu0, sigma0,
                               a0, b0, eps, max_iter, verbose);
}

// ALD MCMC
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
                             double C0 = 0.001) {
  return _mcmc_bwqr_al_cpp(y, X, w, tau, n_mcmc, burnin, thin,
                           b_prior_mean, B_prior_prec, c0, C0);
}

// Approximate MCMC
// [[Rcpp::export(name = ".MCMC_BWQR_AP")]]
Rcpp::List MCMC_BWQR_AP_wrap(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             int n_mcmc,
                             int burnin,
                             int thin,
                             double tau = 0.5,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue) {
  return _mcmc_bwqr_ap_cpp(y, X, w, n_mcmc, burnin, thin, tau, b_prior_mean, B_prior_prec);
}

// Score-like MCMC
// [[Rcpp::export(name = ".MCMC_BWQR_SL")]]
Rcpp::List MCMC_BWQR_SL_wrap(const arma::vec& y,
                             const arma::mat& X,
                             const arma::vec& w,
                             double tau = 0.5,
                             int n_mcmc = 10000,
                             int burnin = 2000,
                             int thin   = 10,
                             Rcpp::Nullable<Rcpp::NumericVector> b_prior_mean = R_NilValue,
                             Rcpp::Nullable<Rcpp::NumericMatrix> B_prior_prec = R_NilValue) {
  return _mcmc_bwqr_sl_cpp(y, X, w, tau, n_mcmc, burnin, thin, b_prior_mean, B_prior_prec);
}
