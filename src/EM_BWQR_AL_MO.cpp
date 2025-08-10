// EM_BWQR_AL_MO.cpp - stable version (August 2025)
// ------------------------------------------------------------------
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::interfaces(r, cpp)]]
//
//  * Scaled Bessel-K (expon.scaled = 1) - avoids under/overflow
//  * Maclaurin series for z -> 0
//  * NaN/Inf safeguards
//  * sigma^2 updated right after M-step
//  * "Jitter" 1e-8 on diagonal of A - prevents LDLT from failing
// ------------------------------------------------------------------

#include <RcppEigen.h>
#include <Rmath.h>

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;

// K_{1.5}(z) / K_{0.5}(z) numerically stable
inline double K_ratio(double z) {
  if (z < 1e-8) {
    const double z2 = z * z;
    return 1.0 + z2 / 8.0 + z2 * z2 / 192.0;
  }
  double num = R::bessel_k(z, 1.5, 1); // expon.scaled = 1
  double den = R::bessel_k(z, 0.5, 1);
  return num / den;
}

// ---------- INTERNAL (no export) ----------
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
    double eps      = 1e-6,
    int    max_iter = 1000,
    bool   verbose  = false) {

  const int n = y.rows();
  const int p = x.cols();
  const int m = p + 1; // + y_gamma

  // projections
  VectorXd y_u     = y * u;
  VectorXd y_gamma = y * gamma_u;

  MatrixXd x_star(n, m);
  x_star.leftCols(p) = x;
  x_star.col(p)      = y_gamma;

  // constants
  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));

  // prior precision
  MatrixXd sigma0_inv = sigma0.ldlt().solve(MatrixXd::Identity(m, m));

  // initials
  VectorXd beta_prev = (x_star.transpose() * x_star).ldlt().solve(x_star.transpose() * y_u);
  double sigma_prev  = (y_u.array() - (x_star * beta_prev).array()).square().mean();
  sigma_prev         = std::max(sigma_prev, 1e-3);

  VectorXd e_nu_inv(n), aux_obj(n), e_nu(n), y_aux(n);

  for (int iter = 0; iter < max_iter; ++iter) {
    const double inv_ds = 1.0 / (delta2 * sigma_prev);

    // E-step
    for (int i = 0; i < n; ++i) {
      const double r  = y_u(i) - x_star.row(i).dot(beta_prev);
      const double as = std::max(w(i) * r * r * inv_ds, 1e-12);
      const double bs = std::max(w(i) * (2.0 / sigma_prev + theta * theta * inv_ds), 1e-12);

      const double sa = std::sqrt(as);
      const double sb = std::sqrt(bs);

      e_nu_inv(i) = sb / sa;
      aux_obj(i)  = sa * sb;
      e_nu(i)     = (1.0 / e_nu_inv(i)) * K_ratio(aux_obj(i));
      y_aux(i)    = y_u(i) - theta / e_nu_inv(i);
    }

    if (!e_nu_inv.allFinite() || !e_nu.allFinite())
      stop("NaN/Inf generated in E-step - check data or algorithm.");

    // M-step
    MatrixXd A = sigma0_inv;
    VectorXd B = sigma0_inv * mu0;

    for (int i = 0; i < n; ++i) {
      const double coef = w(i) * e_nu_inv(i) * inv_ds;
      A.noalias() += coef * x_star.row(i).transpose() * x_star.row(i);
      B.noalias() += coef * x_star.row(i) * y_aux(i);
    }

    A.diagonal().array() += 1e-8; // jitter
    VectorXd beta_curr = A.ldlt().solve(B);

    double term1 = 0.0, term2 = 0.0, term3 = 0.0;
    for (int i = 0; i < n; ++i) {
      const double r2 = y_aux(i) - x_star.row(i).dot(beta_curr);
      term1 += w(i) * e_nu_inv(i) * r2 * r2;
      term2 += w(i) * theta * theta * (e_nu(i) - 1.0 / e_nu_inv(i));
      term3 += w(i) * e_nu(i);
    }
    term1 /= (2.0 * delta2);
    term2 /= (2.0 * delta2);

    const double sigma_curr = (term1 + term2 + term3 + b0) / ((3.0 * n + a0 + 1.0) / 2.0);

    const double diff = (beta_curr - beta_prev).cwiseAbs().sum() + std::abs(sigma_curr - sigma_prev);

    if (verbose && iter % 50 == 0)
      Rcpp::Rcout << "iter=" << iter << "  diff=" << diff << "  sigma=" << sigma_curr << std::endl;

    if (diff < eps)
      return List::create(_["beta"] = beta_curr,
                          _["sigma"] = sigma_curr,
                          _["iter"] = iter + 1,
                          _["converged"] = true);

    if (!std::isfinite(sigma_curr))
      stop("sigma became NaN/Inf - algorithm diverged.");

    beta_prev  = beta_curr;
    sigma_prev = sigma_curr;
  }

  if (verbose)
    Rcpp::Rcout << "EM reached max_iter without convergence (diff > eps)." << std::endl;

  return List::create(_["beta"] = beta_prev,
                      _["sigma"] = sigma_prev,
                      _["iter"] = max_iter,
                      _["converged"] = false);
}
