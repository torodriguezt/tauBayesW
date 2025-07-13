#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;

// [[Rcpp::export]]
List bayesQR_weighted_EM_cpp(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::VectorXd& u,
    const Eigen::VectorXd& gamma_u,
    double tau,
    const Eigen::VectorXd& mu0,
    const Eigen::MatrixXd& sigma0,
    double a0,
    double b0
) {
  using Eigen::MatrixXd;
  using Eigen::VectorXd;

  const int n = y.rows();
  const int p = x.cols();
  const int m = p + 1;

  VectorXd y_u     = y * u;
  VectorXd y_gamma = y * gamma_u;

  MatrixXd x_star(n, m);
  x_star.leftCols(p) = x;
  x_star.col(p)      = y_gamma;

  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));
  const double tol    = 1e-5;

  MatrixXd sigma0_inv = sigma0.ldlt().solve(MatrixXd::Identity(m, m));
  VectorXd beta_prev  = (x_star.transpose() * x_star)
                               .ldlt()
                               .solve(x_star.transpose() * y_u);
  double   sigma_prev = 1.0;

  VectorXd resid(n), a_star(n), b_star(n),
  e_nu_inv(n), aux_obj(n), e_nu(n),
  y_aux(n), wy(n);

  while (true) {
    const double inv_ds = 1.0 / (delta2 * sigma_prev);

    for (int i = 0; i < n; ++i) {
      const double r  = y_u(i) - x_star.row(i).dot(beta_prev);
      resid(i)        = r;

      const double as = w(i) * r * r * inv_ds;
      a_star(i)       = (as == 0.0 ? 1e-10 : as);

      const double bs = w(i) * (2.0 / sigma_prev + theta * theta * inv_ds);
      b_star(i)       = bs;

      const double sa = std::sqrt(a_star(i)),
        sb = std::sqrt(bs);

      e_nu_inv(i) = sb / sa;
      aux_obj(i)  = sa * sb;
      e_nu(i)     = (1.0 / e_nu_inv(i)) * (1.0 + 1.0 / aux_obj(i));
    }

    for (int i = 0; i < n; ++i)
      y_aux(i) = y_u(i) - theta / e_nu_inv(i);

    MatrixXd A = sigma0_inv;
    for (int i = 0; i < n; ++i) {
      const double coef = w(i) * e_nu_inv(i) * inv_ds;
      A.noalias()      += coef * x_star.row(i).transpose() * x_star.row(i);
      wy(i)             = w(i) * e_nu_inv(i) * y_aux(i);
    }

    VectorXd B = sigma0_inv * mu0;
    for (int j = 0; j < m; ++j)
      B(j) += inv_ds * x_star.col(j).dot(wy);

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

    const double sigma_curr = (term1 + term2 + term3 + b0) /
      ((3.0 * n + a0 + 1.0) / 2.0);

    const double diff = (beta_curr - beta_prev).cwiseAbs().sum() +
      std::abs(sigma_curr - sigma_prev);

    if (diff < tol) {
      return List::create(
        Named("beta")  = beta_curr,
        Named("sigma") = sigma_curr
      );
    }

    beta_prev  = beta_curr;
    sigma_prev = sigma_curr;
  }
}
