# tauBayesW: Bayesian Weighted Quantile Regression

[![R-CMD-check](https://github.com/torodriguezt/tauBayesW/workflows/R-CMD-check/badge.svg)](https://github.com/torodriguezt/tauBayesW/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/tauBayesW)](https://CRAN.R-project.org/package=tauBayesW)

**tauBayesW** is an R package for **Bayesian weighted quantile regression** for complex survey designs. The package provides both single and multiple quantile estimation using efficient MCMC and EM algorithms with fast C++ implementations.

<p align="center">
  <img src="web/public/logo_tau.png" width="180" alt="tauBayesW logo"/>
</p>

---

## ⚙️ Features

- **Survey weights**: Handles complex survey designs with observation weights
- **Multiple algorithms**: MCMC (ALD, Score, Approximate) and EM methods  
- **Fast computation**: C++ implementations using Rcpp, RcppEigen, and RcppArmadillo
- **Comprehensive output**: Detailed summaries with convergence diagnostics
- **Visualization**: Built-in plotting functions with ggplot2
- **CRAN ready**: Full documentation and examples

---

## 🚀 Performance Benchmark

The following benchmark compares the R and C++ versions of the main algorithms on a large simulated dataset with 100,000 observations and 10 predictors.

| Algorithm            | R Time (seg) | C++ Time (seg) | R RAM  | C++ RAM | Speedup Factor | Memory Saving |
|----------------------|--------------|----------------|--------|---------|----------------|----------------|
| EM_BWQR_AL_MO        | 2.44         | 0.0032         | 2.3 GB | 190 MB  | ×769           | ~12×           |
| MCMC_BWQR_AL         | 12.3         | 0.01           | 2.0 GB | 50 MB   | ×1100          | ~40×           |
| MCMC_BWQR_AP         | 21.78        | 2.81           | ? GB   | ? GB    | ×7.8           | ?              |
| MCMC_BWQR_SL         | 9.7          | 1.4            | ?      | ?       | ×7.1           | ~7.2×          |
| NonCrossingBWQR_AL   | 21.7         | 0.9            | 5.4 GB | 0.7 GB  | ×24.1          | ~7.7×          |

> Test environment: R 4.4.2, Windows 11, Intel i5 13600-K, 32 GB RAM

---

## 📦 Installation

### From GitHub (development version)

```r
# Install 'devtools' (or 'remotes') once
install.packages("devtools")

# Install the latest tauBayesW from GitHub
devtools::install_github("torodriguezt/tauBayesW")

# Load the package
library(tauBayesW)
```

---

## 🎯 Main Functions

### Single Quantile Estimation: `bqr.svy()`

Fits Bayesian quantile regression for a single quantile using MCMC methods:

- **ALD (Asymmetric Laplace Distribution)**: Uses asymmetric Laplace likelihood
- **Score**: Score-based approach  
- **Approximate**: Approximate methods for faster computation

```r
library(tauBayesW)

# Simulate data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- runif(n) 
y <- 1 + 2*x1 - 0.5*x2 + rnorm(n)
weights <- runif(n, 0.5, 2)
data <- data.frame(y, x1, x2)

# Fit single quantile regression
fit <- bqr.svy(y ~ x1 + x2, weights = weights, data = data, 
               quantile = 0.5, method = "ald", niter = 1000)
summary(fit)
```

### Multiple Quantile Estimation: `mo.bqr.svy()`

Fits Bayesian quantile regression for multiple quantiles using EM algorithm:

```r
# Fit multiple quantile regression
fit_multi <- mo.bqr.svy(y ~ x1 + x2, weights = weights, data = data, 
                        quantile = c(0.25, 0.5, 0.75), algorithm = 'em')
summary(fit_multi)
```

### Visualization: `plot_quantile()`

Create plots showing fitted quantile curves with credible intervals:

```r
# Plot results
plot_quantile(fit, data, predictor = "x1")
plot_quantile(fit_multi, data, predictor = "x1", quantile_select = 0.5)
```

---

## 📚 Methods

The package implements methods based on:

- Yu, K. and Moyeed, R. A. (2001). Bayesian quantile regression. *Statistics & Probability Letters*, 54(4), 437-447.
- Kozumi, H. and Kobayashi, G. (2011). Gibbs sampling methods for Bayesian quantile regression. *Journal of Statistical Computation and Simulation*, 81(11), 1565-1578.

---

## 👥 Authors

- **Marcus L. Nascimento**  
  Postdoctoral Researcher, School of Applied Mathematics, Fundação Getulio Vargas (FGV EMAp)

- **Kelly Christina Mota Gonçalves**  
  Professor, Department of Statistics, Federal University of Rio de Janeiro (UFRJ)

- **Johntan Cardona Jiménez**  
  Professor, Department of Statistics, Universidad Nacional de Colombia (UNAL)

- **Tomás Rodríguez Taborda**  
  Student, Department of Statistics and Department of Computer and Decision Sciences, Universidad Nacional de Colombia (UNAL)

---

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.
