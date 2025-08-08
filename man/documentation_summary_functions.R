# ======================================================================
#  Documentacion adicional para las funciones de resumen mejoradas
# ======================================================================

#' Summary method for Bayesian Quantile Regression objects
#'
#' This function provides a comprehensive summary of Bayesian quantile regression
#' results, including posterior estimates, credible intervals, and MCMC diagnostics.
#' The output follows a Stan-like format with R-hat values, effective sample sizes,
#' and other convergence diagnostics.
#'
#' @param object An object of class 'bqr.svy' returned by \code{\link{bqr.svy}}
#' @param probs A numeric vector of length 2 giving the quantiles for credible intervals.
#'   Default is c(0.025, 0.975) for 95% intervals.
#' @param digits Number of decimal places to display in the output. Default is 3.
#' @param ... Additional arguments (currently unused)
#'
#' @return A summary object of class 'summary.bqr.svy' containing:
#' \itemize{
#'   \item Model information (method, quantile, chains, draws, etc.)
#'   \item Posterior summary statistics for each parameter
#'   \item MCMC diagnostics (R-hat, effective sample size)
#'   \item Convergence warnings if applicable
#' }
#'
#' @details
#' The summary provides the following information:
#' \itemize{
#'   \item \strong{Mean}: Posterior mean of each parameter
#'   \item \strong{SD}: Posterior standard deviation
#'   \item \strong{Credible Intervals}: Based on specified quantiles
#'   \item \strong{R-hat}: Potential scale reduction factor (should be < 1.1)
#'   \item \strong{ESS_bulk}: Bulk effective sample size (should be > 100)
#' }
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{print.summary.bqr.svy}}, 
#'   \code{\link{plot.bqr.svy}}
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' fit <- bqr.svy(y ~ x1 + x2, data = mydata, quantile = 0.5)
#' 
#' # Default summary
#' summary(fit)
#' 
#' # Custom credible intervals
#' summary(fit, probs = c(0.05, 0.95))
#' 
#' # More decimal places
#' summary(fit, digits = 4)
#' }
#'
#' @export
# summary.bqr.svy <- function(object, probs = c(0.025, 0.975), digits = 3, ...)

#' Summary method for Multiple-Output Bayesian Quantile Regression objects
#'
#' This function provides a comprehensive summary of multiple-output Bayesian 
#' quantile regression results, showing estimates for each quantile, convergence 
#' information, and iteration counts.
#'
#' @param object An object of class 'mo.bqr.svy' returned by \code{\link{mo.bqr.svy}}
#' @param digits Number of decimal places to display in the output. Default is 3.
#' @param ... Additional arguments (currently unused)
#'
#' @return A summary object of class 'summary.mo.bqr.svy' containing:
#' \itemize{
#'   \item Model information (algorithm, quantiles, directions)
#'   \item Convergence summary across all quantiles
#'   \item Detailed results for each quantile including coefficients and variance
#'   \item Convergence warnings for non-converged quantiles
#' }
#'
#' @details
#' For each quantile, the summary provides:
#' \itemize{
#'   \item \strong{Convergence status}: Whether the EM algorithm converged
#'   \item \strong{Iterations}: Number of iterations until convergence
#'   \item \strong{Coefficients}: Point estimates for all parameters
#'   \item \strong{Variance}: Estimated error variance (sigma^2)
#' }
#'
#' @seealso \code{\link{mo.bqr.svy}}, \code{\link{print.summary.mo.bqr.svy}}, 
#'   \code{\link{plot.mo.bqr.svy}}
#'
#' @examples
#' \dontrun{
#' # Fit a multiple quantile model
#' fit <- mo.bqr.svy(y ~ x1 + x2, data = mydata, 
#'                   quantile = c(0.25, 0.5, 0.75))
#' 
#' # Default summary
#' summary(fit)
#' 
#' # More decimal places
#' summary(fit, digits = 4)
#' }
#'
#' @export
# summary.mo.bqr.svy <- function(object, digits = 3, ...)

#' Plot method for Bayesian Quantile Regression objects
#'
#' Creates diagnostic plots for Bayesian quantile regression results,
#' including trace plots, density plots, and credible interval plots.
#'
#' @param x An object of class 'bqr.svy'
#' @param type Type of plot to create. Options are:
#'   \itemize{
#'     \item "trace": MCMC trace plots for convergence diagnosis
#'     \item "density": Posterior density plots
#'     \item "intervals": 95% credible interval plots
#'   }
#' @param pars Character vector of parameter names to plot. If NULL (default),
#'   all parameters are plotted.
#' @param ... Additional arguments passed to plotting functions
#'
#' @return The input object (invisibly)
#'
#' @seealso \code{\link{bqr.svy}}, \code{\link{summary.bqr.svy}}
#'
#' @examples
#' \dontrun{
#' fit <- bqr.svy(y ~ x1 + x2, data = mydata, quantile = 0.5)
#' 
#' # Trace plots for convergence
#' plot(fit, type = "trace")
#' 
#' # Posterior densities
#' plot(fit, type = "density")
#' 
#' # Credible intervals
#' plot(fit, type = "intervals")
#' 
#' # Plot only specific parameters
#' plot(fit, type = "trace", pars = c("(Intercept)", "x1"))
#' }
#'
#' @export
# plot.bqr.svy <- function(x, type = c("trace", "density", "intervals"), pars = NULL, ...)

#' Plot method for Multiple-Output Bayesian Quantile Regression objects
#'
#' Creates diagnostic plots for multiple-output Bayesian quantile regression results,
#' showing how estimates vary across quantiles and convergence information.
#'
#' @param x An object of class 'mo.bqr.svy'
#' @param type Type of plot to create. Options are:
#'   \itemize{
#'     \item "quantiles": Coefficient estimates across quantiles
#'     \item "convergence": Convergence status and iteration counts by quantile
#'   }
#' @param ... Additional arguments passed to plotting functions
#'
#' @return The input object (invisibly)
#'
#' @seealso \code{\link{mo.bqr.svy}}, \code{\link{summary.mo.bqr.svy}}
#'
#' @examples
#' \dontrun{
#' fit <- mo.bqr.svy(y ~ x1 + x2, data = mydata, 
#'                   quantile = c(0.25, 0.5, 0.75))
#' 
#' # Coefficient estimates across quantiles
#' plot(fit, type = "quantiles")
#' 
#' # Convergence diagnostics
#' plot(fit, type = "convergence")
#' }
#'
#' @export
# plot.mo.bqr.svy <- function(x, type = c("quantiles", "convergence"), ...)

#' Check MCMC convergence diagnostics
#'
#' Examines convergence diagnostics for Bayesian quantile regression models,
#' checking R-hat values, effective sample sizes, and EM algorithm convergence.
#'
#' @param object A fitted model object (bqr.svy or mo.bqr.svy)
#' @param ... Additional arguments passed to specific methods
#'
#' @return Logical indicating whether all diagnostics pass (TRUE) or 
#'   convergence issues were detected (FALSE). Also prints diagnostic messages.
#'
#' @seealso \code{\link{summary.bqr.svy}}, \code{\link{summary.mo.bqr.svy}}
#'
#' @examples
#' \dontrun{
#' # For MCMC models
#' fit1 <- bqr.svy(y ~ x, data = mydata)
#' convergence_check(fit1)
#' 
#' # For EM models  
#' fit2 <- mo.bqr.svy(y ~ x, data = mydata, quantile = c(0.25, 0.75))
#' convergence_check(fit2)
#' }
#'
#' @export
# convergence_check <- function(object, ...)

cat("Documentacion creada para las nuevas funciones.\n")
cat("Esta documentacion sigue el formato roxygen2 estandar de R.\n")
