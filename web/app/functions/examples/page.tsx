"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Play, Download, FileText, Database, TrendingUp, BarChart } from "lucide-react"
import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Separator } from "@/components/ui/separator"
import { Alert, AlertDescription } from "@/components/ui/alert"

export default function ExamplesPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const basicWorkflowCode = `# Complete workflow: Data simulation to model interpretation
library(tauBayesW)

# 1. Data Simulation
set.seed(42)
sim_data <- simulate_bqr_data(
    n     = 300,
    betas = c(2.0, 1.5, -0.8),  # intercept + slopes
    sigma = 1.2,
    seed  = 42
)

# 2. Prior Specification (informative)
prior <- prior_default(
    p  = 3,                             # number of coefficients
    b0 = c(0, 1.4, -0.7),               # prior means
    B0 = diag(c(0.25, 0.25, 0.25)),     # smaller variances = more concentrated prior
    c0 = 2,                             # ALD hyperparameter
    C0 = 1,
    names = c("(Intercept)", "x1", "x2")
)

# 3. Fit models (single quantile = 0.5)
fit_ald <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "ald",
    prior    = prior,
    niter    = 2000,
    burnin   = 500,
    thin     = 5
)

fit_score <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "score",
    prior    = prior,
    niter    = 2000,
    burnin   = 500,
    thin     = 5
)

fit_ap <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "approximate",
    prior    = prior,
    niter    = 2000,
    burnin   = 500,
    thin     = 5
)

# 4. Summaries
cat("\n--- ALD ---\n")
print(fit_ald$beta)
cat("\n--- Score ---\n")
print(fit_score$beta)
cat("\n--- Approximate ---\n")
print(fit_ap$beta)

# 5. Model Validation
cat("\n=== Model Validation ===\n")
cat("True coefficients:     ", sim_data$true_betas, "\n")
cat("ALD Estimated:         ", fit_ald$beta, "\n")
cat("Score Estimated:       ", fit_score$beta, "\n")
cat("Approximate Estimated: ", fit_ap$beta, "\n")

# 6. Visualization
par(mfrow = c(2, 2))
plot(fit_ald)
plot(fit_score)
plot(fit_ap)

# Custom plot function: regression line with points
plot_quantile_with_points.bqr.svy <- function(object, data, predictor,
                                              main = NULL, ...) {
    if (!inherits(object, "bqr.svy"))
        stop("Object must be of class 'bqr.svy'.")
    response_var <- all.vars(object$formula)[1]
    if (is.null(main)) {
        main <- paste("Quantile Regression (tau =", object$quantile, ") vs", predictor)
    }
    plot(data[[predictor]], data[[response_var]],
         col = adjustcolor("steelblue", 0.4),
         pch = 16, xlab = predictor, ylab = response_var,
         main = main)
    grid()
    plot_quantile.bqr.svy(object, data, predictor, add = TRUE, ...)
}

# Plot regression lines with points for each model
par(mfrow = c(1, 3))
plot_quantile_with_points.bqr.svy(fit_ald,   sim_data$data, "x1", main = "ALD vs x1")
plot_quantile_with_points.bqr.svy(fit_score, sim_data$data, "x1", main = "Score vs x1")
plot_quantile_with_points.bqr.svy(fit_ap,    sim_data$data, "x1", main = "Approximate vs x1")`

  const multipleQuantilesCode = `# Multiple quantiles analysis using EM algorithm
library(tauBayesW)

# =====================================================
# 1. Data Simulation
# =====================================================
set.seed(42)
sim_data <- simulate_mo_bqr_data(
    n          = 300,
    p          = 2,                      # predictors (excluding intercept)
    beta_true  = c(2.0, 1.5, -0.8),      # intercept + slopes
    seed       = 42
)

# =====================================================
# 2. Prior Specification (informative)
# =====================================================
prior_mo <- mo_prior_default(
    p            = 3,                             # intercept + slopes
    beta_mean    = c(0, 1.4, -0.7),               # prior means
    beta_cov     = diag(c(0.25, 0.25, 0.25)),     # small variances
    sigma_shape  = 2,                             # IG shape
    sigma_rate   = 1,                             # IG rate
    names        = c("(Intercept)", "x1", "x2")
)

# =====================================================
# 3. Fit model (multiple quantiles, EM algorithm)
# =====================================================
fit_mo <- mo.bqr.svy(
    formula   = y ~ x1 + x2,
    data      = sim_data$data,
    weights   = sim_data$weights,
    quantile  = c(0.25, 0.5, 0.75),
    algorithm = "em",
    prior     = prior_mo,
    max_iter  = 500,
    verbose   = TRUE
)

# =====================================================
# 4. Summaries
# =====================================================
print(fit_mo)

# =====================================================
# 5. Model Validation
# =====================================================
cat("\n=== Model Validation ===\n")
cat("True coefficients: ", sim_data$true_betas, "\n")
for (q in seq_along(fit_mo$quantile)) {
    cat(sprintf("Quantile %.2f Estimated: %s\n",
                fit_mo$quantile[q],
                paste(round(fit_mo$fit[[q]]$beta, 6), collapse = " ")))
}

# =====================================================
# 6. Visualization
# =====================================================
# Standard plot (built-in)
plot(fit_mo)

# Custom plot: one panel per quantile
par(mfrow = c(1, length(fit_mo$quantile)))
for (q in seq_along(fit_mo$quantile)) {
    plot_quantile_with_points.mo.bqr.svy(
        fit_mo,
        sim_data$data,
        predictor = "x1",
        main = paste("Tau =", fit_mo$quantile[q])
    )
}
`

  const priorSpecificationCode = `# Prior specification and model comparison
library(tauBayesW)

# =====================================================
# 1. Data Simulation
# =====================================================
set.seed(123)
sim_data <- simulate_bqr_data(
    n     = 200,
    betas = c(1.5, 2.0, -1.2),
    sigma = 0.8,
    seed  = 123
)

# =====================================================
# 2. Different Prior Specifications
# =====================================================

# Vague prior (default)
prior_vague <- prior_default(
    p     = 3,
    names = c("(Intercept)", "x1", "x2")
)

# Informative prior (concentrated around true values)
prior_informed <- prior_default(
    p     = 3,
    b0    = c(1.5, 2.0, -1.2),           # close to true values
    B0    = diag(c(0.1, 0.1, 0.1)),      # small variances
    c0    = 3,                           # more concentrated ALD
    C0    = 2,
    names = c("(Intercept)", "x1", "x2")
)

# Mildly informative prior
prior_mild <- prior_default(
    p     = 3,
    b0    = c(0, 1.5, -1.0),             # somewhat informed
    B0    = diag(c(1.0, 0.5, 0.5)),      # moderate precision
    c0    = 2,
    C0    = 1,
    names = c("(Intercept)", "x1", "x2")
)

# =====================================================
# 3. Model Fitting with Different Priors
# =====================================================

# ALD method with different priors
fit_vague <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "ald",
    prior    = prior_vague,
    niter    = 3000,
    burnin   = 1000,
    thin     = 5
)

fit_informed <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "ald",
    prior    = prior_informed,
    niter    = 3000,
    burnin   = 1000,
    thin     = 5
)

fit_mild <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "ald",
    prior    = prior_mild,
    niter    = 3000,
    burnin   = 1000,
    thin     = 5
)

# =====================================================
# 4. Prior Sensitivity Analysis
# =====================================================
cat("=== Prior Sensitivity Analysis ===\n")
cat("True coefficients:    ", sim_data$true_betas, "\n")
cat("Vague prior:          ", round(summary(fit_vague)$coefficients[,"Mean"], 3), "\n")
cat("Informed prior:       ", round(summary(fit_informed)$coefficients[,"Mean"], 3), "\n")
cat("Mildly informed:      ", round(summary(fit_mild)$coefficients[,"Mean"], 3), "\n")

# Calculate posterior standard deviations
cat("\nPosterior Standard Deviations:\n")
cat("Vague prior:          ", round(summary(fit_vague)$coefficients[,"SD"], 3), "\n")
cat("Informed prior:       ", round(summary(fit_informed)$coefficients[,"SD"], 3), "\n")
cat("Mildly informed:      ", round(summary(fit_mild)$coefficients[,"SD"], 3), "\n")

# =====================================================
# 5. Method Comparison (same prior)
# =====================================================
cat("\n=== Method Comparison (mild prior) ===\n")

fit_score_mild <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "score",
    prior    = prior_mild,
    niter    = 3000,
    burnin   = 1000,
    thin     = 5
)

fit_approx_mild <- bqr.svy(
    formula  = y ~ x1 + x2,
    data     = sim_data$data,
    weights  = sim_data$weights,
    quantile = 0.5,
    method   = "approximate",
    prior    = prior_mild,
    niter    = 3000,
    burnin   = 1000,
    thin     = 5
)

cat("ALD method:           ", round(summary(fit_mild)$coefficients[,"Mean"], 3), "\n")
cat("Score method:         ", round(summary(fit_score_mild)$coefficients[,"Mean"], 3), "\n")
cat("Approximate method:   ", round(summary(fit_approx_mild)$coefficients[,"Mean"], 3), "\n")
`

  return (
    <div className="min-h-screen bg-background">
      <div className="container mx-auto p-6">
        <Link href="/" className="inline-flex items-center mb-6 text-muted-foreground hover:text-foreground">
          <ArrowLeft className="mr-2 h-4 w-4" />
          Back to Documentation
        </Link>

        <div className="max-w-4xl mx-auto">
          <div className="mb-8">
            <h1 className="text-4xl font-bold mb-4">Complete Examples</h1>
            <p className="text-xl text-muted-foreground mb-4">
              End-to-end workflows and real-world applications using tauBayesW
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">Complete Workflows</Badge>
            </div>
          </div>

          <Tabs defaultValue="basic" className="w-full">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="basic">Basic Workflow</TabsTrigger>
              <TabsTrigger value="multiple">Multiple Quantiles</TabsTrigger>
              <TabsTrigger value="priors">Prior Specification</TabsTrigger>
            </TabsList>

            <TabsContent value="basic" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <TrendingUp className="h-5 w-5 text-blue-500" />
                    Complete Basic Workflow
                  </CardTitle>
                  <CardDescription>
                    From data simulation to model interpretation - a comprehensive example
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm max-h-96">
                      <code>{basicWorkflowCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(basicWorkflowCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="multiple" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <BarChart className="h-5 w-5 text-green-500" />
                    Multiple Quantiles Analysis
                  </CardTitle>
                  <CardDescription>
                    Simultaneous estimation of multiple quantiles using EM algorithm
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm max-h-96">
                      <code>{multipleQuantilesCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(multipleQuantilesCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="priors" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <FileText className="h-5 w-5 text-orange-500" />
                    Prior Specification and Model Comparison
                  </CardTitle>
                  <CardDescription>
                    Advanced prior specification techniques and sensitivity analysis across different methods
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm max-h-96">
                      <code>{priorSpecificationCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(priorSpecificationCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
