// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::interfaces(r, cpp)]]

#include <RcppEigen.h>
#include <Rmath.h>

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::RowVectorXd;

inline double K_ratio(double z) {
  if (z < 1e-8) {
    const double z2 = z * z;
    return 1.0 + z2 / 8.0 + z2 * z2 / 192.0;
  }
  double num = R::bessel_k(z, 1.5, 1);
  double den = R::bessel_k(z, 0.5, 1);
  return num / den;
}

/*
 * ============================================
 *  SEPARABLE (igual a R por dirección)
 *  - y:     n x d
 *  - x:     n x p
 *  - w:     n
 *  - U:     d x K              (direcciones)
 *  - Gamma: d x (K*r)          (r >= 0; puede ser 0)
 *  - tau:   escalar
 *  - mu0:   (p+r)  o  K*(p+r)  (prior por bloque o apilado)
 *  - sigma0:(p+r)x(p+r)  o  (K*(p+r))x(K*(p+r)) (bloque-diag)
 *
 *  Devuelve:
 *   - beta:  K x (p+r)   (cada fila = parámetros de la dirección k)
 *   - sigma: K           (sigma_k por dirección)
 *   - iter, converged
 * ============================================
 */
Rcpp::List _bwqr_weighted_em_cpp_sep(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::MatrixXd& U,
    const Eigen::MatrixXd& Gamma,
    double tau,
    const Eigen::VectorXd& mu0,
    const Eigen::MatrixXd& sigma0,
    double a0,
    double b0,
    double eps      = 1e-6,
    int    max_iter = 1000,
    bool   verbose  = false) {

  const int n = y.rows();
  const int d = y.cols();
  const int p = x.cols();

  if (w.size() != n) stop("w must have length n.");
  if (U.rows() != d) stop("U must have d rows.");
  if (Gamma.size() > 0 && Gamma.rows() != d) stop("Gamma must have d rows (or 0 cols).");

  const int K = U.cols();
  if (K < 1) stop("U must have at least one direction (K >= 1).");

  const int G = Gamma.cols();                 // puede ser 0
  if (G > 0 && (G % K != 0)) stop("Gamma.cols() must be a multiple of U.cols().");
  const int r = (K > 0) ? (G / std::max(1, K)) : 0;
  const int m_blk = p + r;                    // tamaño del bloque por dirección
  const int m_tot = K * m_blk;                // total si se apila

  // Priors: permitir (p+r) o K*(p+r)
  bool prior_by_block = (mu0.size() == m_blk) && (sigma0.rows() == m_blk && sigma0.cols() == m_blk);

  // Proyecciones
  MatrixXd YU = y * U;              // n x K
  MatrixXd YG; if (r > 0) YG = y * Gamma;  // n x (K*r)

  // Constantes AL
  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));

  // Inicialización por dirección (OLS sobre [X, Y gamma_k])
  MatrixXd beta_prev = MatrixXd::Zero(K, m_blk);  // K x (p+r)
  VectorXd sigma_prev = VectorXd::Constant(K, 1.0);

  for (int k = 0; k < K; ++k) {
    MatrixXd Xstar(n, m_blk);
    Xstar.leftCols(p) = x;
    if (r > 0) {
      Xstar.rightCols(r) = YG.block(0, k * r, n, r);
    }
    VectorXd yk = YU.col(k);
    // OLS
    VectorXd bk = (Xstar.transpose() * Xstar).ldlt().solve(Xstar.transpose() * yk);
    beta_prev.row(k) = bk.transpose();
    // sigma inicial
    VectorXd res = yk - Xstar * bk;
    double s2 = (res.array().square() * w.array()).mean();
    sigma_prev(k) = std::max(1e-3, s2);
  }

  // ===== EM =====
  MatrixXd beta_curr = beta_prev;
  VectorXd sigma_curr = sigma_prev;

  for (int iter = 0; iter < max_iter; ++iter) {

    // -------- E-step y M-step por dirección --------
    for (int k = 0; k < K; ++k) {
      const double inv_ds = 1.0 / (delta2 * sigma_prev(k));

      // Construir Xstar_k
      MatrixXd Xstar(n, m_blk);
      Xstar.leftCols(p) = x;
      if (r > 0) {
        Xstar.rightCols(r) = YG.block(0, k * r, n, r);
      }

      // E-step para la dir k
      VectorXd y_aux_k(n), e_nu_inv_k(n), e_nu_k(n);
      for (int i = 0; i < n; ++i) {
        double fit = Xstar.row(i).dot(beta_prev.row(k));
        const double rres = YU(i, k) - fit;

        const double as = std::max(w(i) * rres * rres * inv_ds, 1e-12);
        const double bs = std::max(w(i) * (2.0 / sigma_prev(k) + theta * theta * inv_ds), 1e-12);

        const double sa = std::sqrt(as);
        const double sb = std::sqrt(bs);

        e_nu_inv_k(i) = sb / sa;
        const double aux_obj = sa * sb;
        e_nu_k(i)     = (1.0 / e_nu_inv_k(i)) * K_ratio(aux_obj);
        y_aux_k(i)    = YU(i, k) - theta / e_nu_inv_k(i);
      }
      if (!e_nu_inv_k.allFinite() || !e_nu_k.allFinite())
        stop("NaN/Inf in E-step (direction k) - check data or algorithm.");

      // Prior por bloque
      VectorXd mu0_blk(m_blk);
      MatrixXd S0inv_blk(m_blk, m_blk);
      if (prior_by_block) {
        mu0_blk = mu0;
        S0inv_blk = sigma0.ldlt().solve(MatrixXd::Identity(m_blk, m_blk));
      } else {
        mu0_blk = mu0.segment(k * m_blk, m_blk);
        S0inv_blk = sigma0.block(k * m_blk, k * m_blk, m_blk, m_blk)
                      .ldlt().solve(MatrixXd::Identity(m_blk, m_blk));
      }

      // M-step para la dir k
      MatrixXd A = S0inv_blk;
      VectorXd B = S0inv_blk * mu0_blk;

      for (int i = 0; i < n; ++i) {
        const double coef = w(i) * e_nu_inv_k(i) * inv_ds;
        // A += coef * x_i^T x_i  (sobre Xstar)
        A.noalias() += coef * (Xstar.row(i).transpose() * Xstar.row(i));
        // B += coef * x_i^T y_aux_i
        B.noalias() += coef * Xstar.row(i).transpose() * y_aux_k(i);
      }

      A.diagonal().array() += 1e-8; // regularización
      VectorXd beta_k = A.ldlt().solve(B);
      beta_curr.row(k) = beta_k.transpose();

      // Actualizar sigma_k
      double term1 = 0.0, term2 = 0.0, term3 = 0.0;
      for (int i = 0; i < n; ++i) {
        const double r2 = y_aux_k(i) - Xstar.row(i).dot(beta_k);
        term1 += w(i) * e_nu_inv_k(i) * r2 * r2;
        term2 += w(i) * theta * theta * (e_nu_k(i) - 1.0 / e_nu_inv_k(i));
        term3 += w(i) * e_nu_k(i);
      }
      term1 /= (2.0 * delta2);
      term2 /= (2.0 * delta2);
      const double denom = (3.0 * n + a0 + 1.0) / 2.0;

      sigma_curr(k) = (term1 + term2 + term3 + b0) / denom;
      if (!std::isfinite(sigma_curr(k)))
        stop("sigma_k became NaN/Inf - algorithm diverged.");
    }

    // Criterio de convergencia global (L1 en todos los bloques)
    double diff = 0.0;
    diff += (beta_curr - beta_prev).cwiseAbs().sum();
    diff += (sigma_curr - sigma_prev).cwiseAbs().sum();

    if (verbose && (iter % 50 == 0)) {
      Rcpp::Rcout << "iter=" << iter << "  diff=" << diff
                  << "  mean(sigma)=" << sigma_curr.mean() << std::endl;
    }

    if (diff < eps) {
      return List::create(
        _["beta"]      = beta_curr,   // K x (p+r)
        _["sigma"]     = sigma_curr,  // K
        _["iter"]      = iter + 1,
        _["converged"] = true
      );
    }

    beta_prev  = beta_curr;
    sigma_prev = sigma_curr;
  }

  if (verbose)
    Rcpp::Rcout << "EM reached max_iter without convergence (diff > eps)." << std::endl;

  return List::create(
    _["beta"]      = beta_prev,
    _["sigma"]     = sigma_prev,
    _["iter"]      = max_iter,
    _["converged"] = false
  );
}

/*
 * ============================================
 *  CONJUNTA (compatibilidad anterior)
 *  - Comparte beta_X entre direcciones
 *  - Mantiene interfaz previa
 * ============================================
 */
Rcpp::List _bwqr_weighted_em_cpp(
    const Eigen::MatrixXd& y,
    const Eigen::MatrixXd& x,
    const Eigen::VectorXd& w,
    const Eigen::MatrixXd& U,
    const Eigen::MatrixXd& Gamma,
    double tau,
    const Eigen::VectorXd& mu0,
    const Eigen::MatrixXd& sigma0,
    double a0,
    double b0,
    double eps      = 1e-6,
    int    max_iter = 1000,
    bool   verbose  = false) {

  const int n = y.rows();
  const int d = y.cols();
  const int p = x.cols();

  if (w.size() != n) stop("w must have length n.");
  if (U.rows() != d) stop("U must have d rows.");
  if (Gamma.size() > 0 && Gamma.rows() != d) stop("Gamma must have d rows (or have 0 cols).");

  const int K = U.cols();
  if (K < 1) stop("U must have at least one direction (K >= 1).");

  const int G = Gamma.cols();          // puede ser 0
  if (G % K != 0) stop("Gamma.cols() must be a multiple of U.cols().");
  const int r = (K > 0) ? (G / K) : 0; // nº de columnas de Gamma por dirección
  const int m = p + G;

  if (mu0.size() != m) stop("mu0 length must be p + G.");
  if (sigma0.rows() != m || sigma0.cols() != m) stop("sigma0 must be (p+G)x(p+G).");

  // Proyecciones
  MatrixXd y_u     = y * U;                 // n x K
  MatrixXd y_gamma;                         // n x G
  if (G > 0) y_gamma = y * Gamma;

  // Constantes AL
  const double delta2 = 2.0 / (tau * (1.0 - tau));
  const double theta  = (1.0 - 2.0 * tau) / (tau * (1.0 - tau));

  // Prior
  MatrixXd sigma0_inv = sigma0.ldlt().solve(MatrixXd::Identity(m, m));

  // ===== Inicialización (tipo M-step con e_nu_inv = 1) =====
  MatrixXd A = sigma0_inv;
  VectorXd B = sigma0_inv * mu0;

  for (int k = 0; k < K; ++k) {
    const int col0 = k * r; // inicio del bloque gamma de esta dirección
    for (int i = 0; i < n; ++i) {
      const double wk = w(i);

      // A_xx
      A.block(0, 0, p, p).noalias() += wk * (x.row(i).transpose() * x.row(i));

      // A_xg, A_gx, A_gg (si r > 0)
      if (r > 0) {
        RowVectorXd yg = y_gamma.row(i).segment(col0, r); // 1 x r
        A.block(0, p + col0, p, r).noalias()       += wk * (x.row(i).transpose() * yg);
        A.block(p + col0, 0, r, p).noalias()       += wk * (yg.transpose() * x.row(i));
        A.block(p + col0, p + col0, r, r).noalias()+= wk * (yg.transpose() * yg);
      }

      // B
      const double yu = y_u(i, k);
      B.head(p).noalias() += wk * x.row(i).transpose() * yu;
      if (r > 0) {
        RowVectorXd yg = y_gamma.row(i).segment(col0, r);
        B.segment(p + col0, r).noalias() += wk * yg.transpose() * yu;
      }
    }
  }

  A.diagonal().array() += 1e-8;
  VectorXd beta_prev = A.ldlt().solve(B);

  // sigma inicial con residuos apilados sobre (i,k)
  double rss = 0.0;
  for (int k = 0; k < K; ++k) {
    const int col0 = k * r;
    for (int i = 0; i < n; ++i) {
      double fit = x.row(i).dot(beta_prev.head(p));
      if (r > 0) {
        RowVectorXd yg = y_gamma.row(i).segment(col0, r);
        fit += yg * beta_prev.segment(p + col0, r);
      }
      const double ri = y_u(i, k) - fit;
      rss += w(i) * ri * ri;
    }
  }
  const double NK = static_cast<double>(n) * static_cast<double>(K);
  double sigma_prev = std::max(rss / std::max(1.0, NK), 1e-3);

  // ===== EM =====
  VectorXd e_nu_inv(n * K), aux_obj(n * K), e_nu(n * K), y_aux(n * K);

  for (int iter = 0; iter < max_iter; ++iter) {
    const double inv_ds = 1.0 / (delta2 * sigma_prev);

    // --- E-step ---
    for (int k = 0; k < K; ++k) {
      const int col0 = k * r;
      for (int i = 0; i < n; ++i) {
        const int idx = k * n + i;

        double fit = x.row(i).dot(beta_prev.head(p));
        if (r > 0) {
          RowVectorXd yg = y_gamma.row(i).segment(col0, r);
          fit += yg * beta_prev.segment(p + col0, r);
        }
        const double rres = y_u(i, k) - fit;

        const double as = std::max(w(i) * rres * rres * inv_ds, 1e-12);
        const double bs = std::max(w(i) * (2.0 / sigma_prev + theta * theta * inv_ds), 1e-12);

        const double sa = std::sqrt(as);
        const double sb = std::sqrt(bs);

        e_nu_inv(idx) = sb / sa;
        aux_obj(idx)  = sa * sb;
        e_nu(idx)     = (1.0 / e_nu_inv(idx)) * K_ratio(aux_obj(idx));
        y_aux(idx)    = y_u(i, k) - theta / e_nu_inv(idx);
      }
    }
    if (!e_nu_inv.allFinite() || !e_nu.allFinite())
      stop("NaN/Inf in E-step - check data or algorithm.");

    // --- M-step ---
    MatrixXd A2 = sigma0_inv;
    VectorXd B2 = sigma0_inv * mu0;

    for (int k = 0; k < K; ++k) {
      const int col0 = k * r;
      for (int i = 0; i < n; ++i) {
        const int idx  = k * n + i;
        const double coef = w(i) * e_nu_inv(idx) * inv_ds;

        // A_xx
        A2.block(0, 0, p, p).noalias() += coef * (x.row(i).transpose() * x.row(i));

        // A_xg / A_gx / A_gg
        if (r > 0) {
          RowVectorXd yg = y_gamma.row(i).segment(col0, r);
          A2.block(0, p + col0, p, r).noalias()        += coef * (x.row(i).transpose() * yg);
          A2.block(p + col0, 0, r, p).noalias()        += coef * (yg.transpose() * x.row(i));
          A2.block(p + col0, p + col0, r, r).noalias() += coef * (yg.transpose() * yg);
        }

        // B
        const double yaux = y_aux(idx);
        B2.head(p).noalias() += coef * x.row(i).transpose() * yaux;
        if (r > 0) {
          RowVectorXd yg = y_gamma.row(i).segment(col0, r);
          B2.segment(p + col0, r).noalias() += coef * yg.transpose() * yaux;
        }
      }
    }

    A2.diagonal().array() += 1e-8;
    VectorXd beta_curr = A2.ldlt().solve(B2);

    // sigma
    double term1 = 0.0, term2 = 0.0, term3 = 0.0;
    for (int k = 0; k < K; ++k) {
      const int col0 = k * r;
      for (int i = 0; i < n; ++i) {
        const int idx = k * n + i;

        double fit = x.row(i).dot(beta_curr.head(p));
        if (r > 0) {
          RowVectorXd yg = y_gamma.row(i).segment(col0, r);
          fit += yg * beta_curr.segment(p + col0, r);
        }
        const double r2 = y_aux(idx) - fit;

        term1 += w(i) * e_nu_inv(idx) * r2 * r2;
        term2 += w(i) * theta * theta * (e_nu(idx) - 1.0 / e_nu_inv(idx));
        term3 += w(i) * e_nu(idx);
      }
    }
    term1 /= (2.0 * delta2);
    term2 /= (2.0 * delta2);

    const double sigma_curr = (term1 + term2 + term3 + b0) / ((3.0 * (n * (double)K) + a0 + 1.0) / 2.0);

    const double diff = (beta_curr - beta_prev).cwiseAbs().sum() + std::abs(sigma_curr - sigma_prev);

    if (verbose && iter % 50 == 0)
      Rcpp::Rcout << "iter=" << iter << "  diff=" << diff << "  sigma=" << sigma_curr << std::endl;

    if (diff < eps) {
      return List::create(_["beta"]      = beta_curr,  // length p+G
                          _["sigma"]     = sigma_curr,
                          _["iter"]      = iter + 1,
                          _["converged"] = true);
    }

    if (!std::isfinite(sigma_curr))
      stop("sigma became NaN/Inf - algorithm diverged.");

    beta_prev  = beta_curr;
    sigma_prev = sigma_curr;
  }

  if (verbose)
    Rcpp::Rcout << "EM reached max_iter without convergence (diff > eps)." << std::endl;

  return List::create(_["beta"]      = beta_prev,
                      _["sigma"]     = sigma_prev,
                      _["iter"]      = max_iter,
                      _["converged"] = false);
}
