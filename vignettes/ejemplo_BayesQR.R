library(bayesQR)

# 1. Cargar dataset
data(Prostate, package = "bayesQR")

# 2. Centrar covariables (restar media, sin escalar)
covars <- Prostate[, colnames(Prostate) != "lpsa"]
covars_centered <- as.data.frame(scale(covars, center = TRUE, scale = FALSE))

# 3. Dataset con respuesta + covariables centradas
Prostate_centrada <- cbind(lpsa = Prostate$lpsa, covars_centered)

# 4. Matriz de diseño (incluye intercepto automáticamente)
X <- model.matrix(lpsa ~ ., data = Prostate_centrada)

# 5. Vector de respuesta y pesos iguales
y <- Prostate_centrada$lpsa
w <- rep(1, length(y))

coef_cpp <- colMeans(as.matrix(fit_cpp$beta))

fit_tau <-  bqr.svy(
  lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,
  data     = Prostate_centrada,
  quantile = 0.75,
  method   = "score",
  prior    = prior_tbw,
  niter    = 50000,
  burnin = 25000
)

coef_tau <- fit_tau$beta


prior_bqr <- bayesQR::prior(
  lpsa ~ ., data = Prostate_centrada,
  beta0 = rep(0, ncol(X)), 
  V0 = diag(1e6, ncol(X))
)

fit_bqr <- bayesQR(
  lpsa ~ ., data = Prostate_centrada,
  quantile = 0.75, ndraw = 60000, prior = prior_bqr, keep = 5
)

draws_bqr <- fit_bqr[[1]][["betadraw"]]
colnames(draws_bqr) <- colnames(X)
coef_bqr <- colMeans(draws_bqr)


resumen <- data.frame(
  Coef      = colnames(X),
  bayesQR   = coef_bqr,
  tau = coef_tau,
  difference2 = coef_bqr -coef_tau
)

resumen




