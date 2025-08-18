## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----installation, eval=FALSE-------------------------------------------------
# # From CRAN (when available)
# install.packages("tauBayesW")
#
# # From GitHub (development version)
# # remotes::install_github("torodriguezt/tauBayesW")

## ----setup--------------------------------------------------------------------
library(tauBayesW)
set.seed(123)

## ----citation, eval=FALSE-----------------------------------------------------
# citation("tauBayesW")

## ----basic-example------------------------------------------------------------
# Simulate survey data with design weights
n <- 500
x1 <- rnorm(n)
x2 <- rbinom(n, 1, 0.4)
y <- 1 + 2*x1 + 1.5*x2 + rnorm(n)
weights <- runif(n, 0.5, 2)  # Survey weights

data <- data.frame(y, x1, x2)

# Single quantile regression (median)
model_median <- bqr.svy(y ~ x1 + x2,
                        data = data,
                        weights = weights,
                        quantile = 0.5,
                        iter = 2000,
                        verbose = FALSE)

summary(model_median)

## ----multiple-quantiles-------------------------------------------------------
# Multiple quantiles simultaneously
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

model_multi <- mo.bqr.svy(y ~ x1 + x2,
                          data = data,
                          weights = weights,
                          quantile = quantiles,
                          max_iter = 500,
                          verbose = FALSE)

summary(model_multi)

