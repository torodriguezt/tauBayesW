# =====================================================
# Imports and linking configuration
# =====================================================

#' @useDynLib tauBayesW, .registration = TRUE
#' @import Rcpp
#'
#' @importFrom stats acf mad median predict qnorm quantile rnorm runif sd var setNames delete.response model.frame model.matrix model.response terms
#' @importFrom graphics plot lines points axis legend arrows grid mtext par segments
#' @importFrom grDevices adjustcolor
NULL
