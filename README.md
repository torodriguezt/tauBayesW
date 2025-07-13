# TauBayesW

**TauBayesW** is an R package for **Bayesian directional quantile regression**, tailored for multivariate responses projected along custom directions.

The package provides algorithms based on **Expectation-Maximization (EM)** and **Markov Chain Monte Carlo (MCMC)**, to compute posterior distributions of quantile regression parameters.  
It is the result of translating original R scripts — developed for academic research — into **high-performance C++** via Rcpp and Eigen.

---

## ⚙️ Features

- Directional quantile estimation for bivariate responses  
- EM and MCMC algorithms with Bayesian regularization  
- Custom weights and prior specification  
- Fast and scalable C++ backend  
- Ideal for geostatistical and directional data analysis  

---

## 🚀 Performance Benchmark

The following benchmark compares the R and C++ versions of the main EM algorithm on a large simulated dataset with 100,000 observations and 10 predictors.

| Version | Time (min) | Peak RAM Usage |
|---------|------------|----------------|
| R       | 31.2       | 6.8 GB         |
| C++     | 0.56       | 0.9 GB         |

> Test environment: R 4.4.2, Windows 11, Intel i5 13600-K, 32 GB RAM

---
