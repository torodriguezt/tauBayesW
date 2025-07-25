# tauBayesW

**tauBayesW** is an R package for **Bayesian directional quantile regression**, tailored for multivariate responses projected along custom directions.

The package provides algorithms based on **Expectation-Maximization (EM)** and **Markov Chain Monte Carlo (MCMC)**, to compute posterior distributions of quantile regression parameters.  
It is the result of translating original R scripts — developed for academic research — into **high-performance C++** via Rcpp and Eigen.

<p align="center">
  <img src="web/public/logo_tau.png" width="180" alt="tauBayesW logo"/>
</p>

---

## ⚙️ Features

- Directional quantile estimation for bivariate responses  
- EM and MCMC algorithms 
- Custom weights 
- Fast and scalable C++ backend  

---

## 🚀 Performance Benchmark

The following benchmark compares the R and C++ versions of the main EM algorithm on a large simulated dataset with 100,000 observations and 10 predictors.

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

## 👥 Authors

- **Marcus L. Nascimento**  
  Postdoctoral Researcher, School of Applied Mathematics, Fundação Getulio Vargas (FGV EMAp)

- **Kelly Christina Mota Gonçalves**  
  Professor, Department of Statistics, Federal University of Rio de Janeiro (UFRJ)

- **Johntan Cardona Jiménez**  
  Professor, Department of Statistics, Universidad Nacional de Colombia (UNAL)

- **Tomás Rodríguez Taborda**  
  Student, Department of Statistics and Department of Computer and Decision Sciences, Universidad Nacional de Colombia (UNAL)
