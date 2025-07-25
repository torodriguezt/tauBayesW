% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EM_BWQR_AL_MO.R
\name{bayesQR_weighted_EM_cpp}
\alias{bayesQR_weighted_EM_cpp}
\title{EM Algorithm for Bayesian Weighted Quantile Regression (AL)}
\usage{
bayesQR_weighted_EM_cpp(y, x, w, u, gamma_u, tau, mu0, sigma0, a0, b0)
}
\arguments{
\item{y}{Response matrix (n × q), where each column represents a direction.}

\item{x}{Design matrix (n × p).}

\item{w}{Observation weights vector of length n.}

\item{u}{Unit vector (length q) indicating the direction of interest.}

\item{gamma_u}{Orthogonal complement direction for residual adjustment.}

\item{tau}{Quantile level (between 0 and 1).}

\item{mu0}{Prior mean vector for \eqn{\beta}.}

\item{sigma0}{Prior covariance matrix for \eqn{\beta}.}

\item{a0}{Shape parameter for the inverse gamma prior on \eqn{\sigma}.}

\item{b0}{Rate parameter for the inverse gamma prior on \eqn{\sigma}.}
}
\value{
A list with elements:
\describe{
\item{beta}{Estimated posterior mode of regression coefficients.}
\item{sigma}{Estimated posterior mode of the scale parameter.}
}
}
\description{
Implements the Expectation-Maximization (EM) algorithm for Bayesian
quantile regression with asymmetric Laplace (AL) likelihood and observation
weights. This function solves the model using moment-based approximations.
}
