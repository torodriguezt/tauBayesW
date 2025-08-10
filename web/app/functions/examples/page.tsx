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
  n = 300,
  betas = c(2.0, 1.5, -0.8),  # intercept + 2 slopes
  sigma = 1.2,
  seed = 42
)

# Examine the simulated data
print("Data structure:")
str(sim_data)
print("Summary statistics:")
summary(sim_data$data)

# 2. Prior Specification
prior <- prior_default(
  p = 3,                    # number of coefficients
  b0 = c(0, 0, 0),         # prior means
  B0 = diag(c(100, 100, 100))  # prior covariances
)

# 3. Model Fitting
model <- bqr.svy(
  y ~ x1 + x2,
  data = sim_data$data,
  weights = sim_data$weights,
  quantile = 0.75,          # 75th percentile
  method = "ALD",           # Asymmetric Laplace Distribution
  n_mcmc = 15000,
  burnin = 3000,
  verbose = TRUE
)

# 4. Model Summary and Diagnostics
print(model)
summary_results <- summary(model)
print(summary_results)

# 5. Convergence Assessment
conv_check <- convergence_check(model, verbose = TRUE)

# 6. Visualization
plot(model)
plot_quantile.bqr.svy(model, which_x = "x1")

# 7. Posterior Analysis
posterior_samples <- model$draws
beta_posterior_means <- apply(posterior_samples[, 1:3], 2, mean)
beta_credible_intervals <- apply(posterior_samples[, 1:3], 2, 
                                quantile, probs = c(0.025, 0.975))

print("Posterior coefficient means:")
print(beta_posterior_means)
print("95% Credible intervals:")
print(beta_credible_intervals)`

  const multipleQuantilesCode = `# Multiple quantile analysis workflow
library(tauBayesW)

# 1. Data Generation for Multiple Quantiles
set.seed(123)
sim_data <- simulate_mo_bqr_data(
  n = 250, 
  p = 3,    # 3 covariates (excluding intercept)
  beta_true = c(1.0, 2.0, -1.5, 0.8),  # intercept + 3 slopes
  seed = 123
)

# 2. Multiple Quantile Model Fitting
quantiles_of_interest <- c(0.1, 0.25, 0.5, 0.75, 0.9)

multi_model <- mo.bqr.svy(
  y ~ .,  # use all available predictors
  data = sim_data$data,
  weights = sim_data$weights,
  quantiles = quantiles_of_interest,
  max_iter = 200,
  tol = 1e-6,
  verbose = TRUE
)

# 3. Model Summary
print(multi_model)
multi_summary <- summary(multi_model)
print(multi_summary)

# 4. Convergence Check for EM Algorithm
convergence_check(multi_model, verbose = TRUE)

# 5. Visualization of Results
plot(multi_model, type = "quantiles")
plot(multi_model, type = "coefficients")
plot(multi_model, type = "convergence")

# 6. Extract Results for Specific Quantiles
# Access median regression results
median_results <- multi_model$fit[[3]]  # 3rd quantile is 0.5
print("Median regression coefficients:")
print(median_results$beta)

# Compare coefficients across quantiles
coef_comparison <- sapply(multi_model$fit, function(x) x$beta)
colnames(coef_comparison) <- paste0("Q", quantiles_of_interest)
print("Coefficients across quantiles:")
print(coef_comparison)

# 7. Quantile-specific Analysis
for(i in seq_along(quantiles_of_interest)) {
  cat("\\n=== Quantile:", quantiles_of_interest[i], "===\\n")
  fit_i <- multi_model$fit[[i]]
  cat("Converged:", fit_i$converged, "\\n")
  cat("Iterations:", fit_i$iter, "\\n")
  cat("Coefficients:", paste(round(fit_i$beta, 3), collapse = ", "), "\\n")
}`

  const surveyWeightsCode = `# Advanced survey weights and complex sampling
library(tauBayesW)

# 1. Realistic Survey Data Simulation
set.seed(456)
n_population <- 1000
n_sample <- 200

# Create a stratified sampling scenario
# Population with different strata
population_data <- data.frame(
  id = 1:n_population,
  stratum = sample(1:4, n_population, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  x1 = rnorm(n_population),
  x2 = rnorm(n_population),
  auxiliary = runif(n_population)
)

# Create response with stratum-specific effects
population_data$y <- with(population_data, {
  2 + 1.5*x1 - 0.8*x2 + 0.5*(stratum-2.5) + rnorm(n_population, sd = 1.0)
})

# Stratified sampling with unequal selection probabilities
sample_indices <- c()
for(s in 1:4) {
  stratum_indices <- which(population_data$stratum == s)
  n_stratum <- length(stratum_indices)
  # Different sampling rates by stratum
  sample_rate <- c(0.15, 0.20, 0.25, 0.30)[s]
  n_sample_stratum <- round(n_stratum * sample_rate)
  sample_indices <- c(sample_indices, 
                     sample(stratum_indices, n_sample_stratum))
}

# Extract sample data
sample_data <- population_data[sample_indices, ]

# Calculate sampling weights (inverse probability weighting)
sample_data$weights <- NA
for(s in 1:4) {
  n_pop_stratum <- sum(population_data$stratum == s)
  n_sample_stratum <- sum(sample_data$stratum == s)
  sample_rate <- n_sample_stratum / n_pop_stratum
  sample_data$weights[sample_data$stratum == s] <- 1 / sample_rate
}

# Normalize weights
sample_data$weights <- sample_data$weights / mean(sample_data$weights)

print("Survey design summary:")
print(table(sample_data$stratum))
print("Weight distribution:")
print(summary(sample_data$weights))

# 2. Weighted Quantile Regression Analysis
weighted_model <- bqr.svy(
  y ~ x1 + x2 + factor(stratum),
  data = sample_data,
  weights = weights,
  quantile = 0.5,
  method = "ALD",
  n_mcmc = 12000,
  burnin = 2000
)

# 3. Compare with Unweighted Analysis
unweighted_model <- bqr.svy(
  y ~ x1 + x2 + factor(stratum),
  data = sample_data,
  weights = NULL,  # No weights
  quantile = 0.5,
  method = "ALD",
  n_mcmc = 12000,
  burnin = 2000
)

# 4. Compare Results
print("Weighted model summary:")
summary(weighted_model)
print("\\nUnweighted model summary:")
summary(unweighted_model)

# Extract and compare coefficients
weighted_coefs <- apply(weighted_model$draws[, 1:6], 2, mean)
unweighted_coefs <- apply(unweighted_model$draws[, 1:6], 2, mean)

comparison <- data.frame(
  Weighted = weighted_coefs,
  Unweighted = unweighted_coefs,
  Difference = weighted_coefs - unweighted_coefs
)
print("\\nCoefficient comparison:")
print(comparison)

# 5. Visualization
par(mfrow = c(1, 2))
plot_quantile.bqr.svy(weighted_model, which_x = "x1", 
                      main = "Weighted Analysis")
plot_quantile.bqr.svy(unweighted_model, which_x = "x1", 
                      main = "Unweighted Analysis")`

  const performanceCode = `# Performance optimization and large-scale analysis
library(tauBayesW)

# 1. Performance Comparison Setup
test_sample_sizes <- c(500, 1000, 2000, 5000)
methods <- c("ALD", "Score", "Approximate")
performance_results <- data.frame()

# 2. Benchmarking Function
benchmark_method <- function(n, method_name) {
  # Generate data
  sim_data <- simulate_bqr_data(n = n, betas = c(1, 0.5, -0.3), sigma = 1)
  
  # Time the analysis
  start_time <- Sys.time()
  
  # Adjust MCMC settings based on sample size
  n_mcmc <- min(10000, max(5000, n))
  burnin <- min(2000, max(1000, n/5))
  
  model <- bqr.svy(
    y ~ x1 + x2,
    data = sim_data$data,
    weights = sim_data$weights,
    quantile = 0.5,
    method = method_name,
    n_mcmc = n_mcmc,
    burnin = burnin,
    verbose = FALSE
  )
  
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Convergence check
  conv_check <- convergence_check(model, verbose = FALSE)
  converged <- all(conv_check$converged, na.rm = TRUE)
  
  return(list(
    time = elapsed_time,
    converged = converged,
    n_samples = n,
    method = method_name,
    mean_rhat = mean(conv_check$rhat, na.rm = TRUE),
    mean_ess = mean(conv_check$neff, na.rm = TRUE)
  ))
}

# 3. Run Performance Tests
cat("Running performance benchmarks...\\n")
for(n in test_sample_sizes) {
  for(method in methods) {
    cat("Testing n =", n, "with method =", method, "\\n")
    
    # Run benchmark
    result <- benchmark_method(n, method)
    
    # Store results
    performance_results <- rbind(performance_results, data.frame(
      sample_size = n,
      method = method,
      time_seconds = result$time,
      converged = result$converged,
      mean_rhat = result$mean_rhat,
      mean_ess = result$mean_ess
    ))
  }
}

# 4. Performance Analysis
print("Performance benchmark results:")
print(performance_results)

# Calculate speedup factors
ald_times <- performance_results[performance_results$method == "ALD", "time_seconds"]
score_times <- performance_results[performance_results$method == "Score", "time_seconds"]
approx_times <- performance_results[performance_results$method == "Approximate", "time_seconds"]

speedup_score <- ald_times / score_times
speedup_approx <- ald_times / approx_times

print("\\nSpeedup factors relative to ALD method:")
speedup_df <- data.frame(
  sample_size = test_sample_sizes,
  Score_speedup = speedup_score,
  Approximate_speedup = speedup_approx
)
print(speedup_df)

# 5. Memory Usage Optimization Tips
cat("\\n=== Memory Optimization Tips ===\\n")
cat("1. Use thinning for large MCMC runs: thin = 5 or thin = 10\\n")
cat("2. For n > 5000, consider method = 'Approximate'\\n")
cat("3. Use mo.bqr.svy() for multiple quantiles instead of separate calls\\n")
cat("4. Monitor memory with: gc(); pryr::mem_used()\\n")

# 6. Large-scale Example (demonstration only - commented out)
# Uncomment for actual large-scale analysis
# large_sim <- simulate_bqr_data(n = 10000, betas = c(1, 0.5, -0.3))
# large_model <- bqr.svy(y ~ x1 + x2, data = large_sim$data, 
#                       weights = large_sim$weights, 
#                       method = "Approximate", n_mcmc = 5000)`

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
              <Badge variant="secondary">Real Applications</Badge>
              <Badge variant="secondary">Best Practices</Badge>
            </div>
          </div>

          <Tabs defaultValue="basic" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="basic">Basic Workflow</TabsTrigger>
              <TabsTrigger value="multiple">Multiple Quantiles</TabsTrigger>
              <TabsTrigger value="survey">Survey Weights</TabsTrigger>
              <TabsTrigger value="performance">Performance</TabsTrigger>
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

              <Card>
                <CardHeader>
                  <CardTitle>Key Steps Explained</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">1. Data Simulation</h4>
                    <p className="text-sm text-muted-foreground">
                      Generate realistic synthetic data with known parameters for testing and validation.
                    </p>
                  </div>
                  <div>
                    <h4 className="font-semibold mb-2">2. Prior Specification</h4>
                    <p className="text-sm text-muted-foreground">
                      Set up appropriate prior distributions for Bayesian inference.
                    </p>
                  </div>
                  <div>
                    <h4 className="font-semibold mb-2">3. Model Fitting</h4>
                    <p className="text-sm text-muted-foreground">
                      Run MCMC algorithm to obtain posterior samples of model parameters.
                    </p>
                  </div>
                  <div>
                    <h4 className="font-semibold mb-2">4. Diagnostics & Validation</h4>
                    <p className="text-sm text-muted-foreground">
                      Check convergence, effective sample sizes, and model adequacy.
                    </p>
                  </div>
                  <div>
                    <h4 className="font-semibold mb-2">5. Interpretation</h4>
                    <p className="text-sm text-muted-foreground">
                      Extract meaningful insights from posterior distributions and credible intervals.
                    </p>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="multiple" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <BarChart className="h-5 w-5 text-green-500" />
                    Multiple Quantile Analysis
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

              <Alert>
                <AlertDescription>
                  The EM algorithm in mo.bqr.svy() is particularly efficient for multiple quantiles, 
                  providing faster computation than running separate MCMC analyses for each quantile.
                </AlertDescription>
              </Alert>
            </TabsContent>

            <TabsContent value="survey" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Database className="h-5 w-5 text-purple-500" />
                    Complex Survey Weights
                  </CardTitle>
                  <CardDescription>
                    Handling stratified sampling and survey weights in quantile regression
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm max-h-96">
                      <code>{surveyWeightsCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(surveyWeightsCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Survey Weights Best Practices</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">Weight Construction</h4>
                    <ul className="text-sm text-muted-foreground list-disc list-inside space-y-1">
                      <li>Use inverse probability weighting for stratified samples</li>
                      <li>Normalize weights to have mean = 1 for interpretability</li>
                      <li>Consider post-stratification adjustments if needed</li>
                    </ul>
                  </div>
                  <div>
                    <h4 className="font-semibold mb-2">Model Comparison</h4>
                    <ul className="text-sm text-muted-foreground list-disc list-inside space-y-1">
                      <li>Always compare weighted vs. unweighted results</li>
                      <li>Large differences indicate informative sampling</li>
                      <li>Weighted results correct for selection bias</li>
                    </ul>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="performance" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <FileText className="h-5 w-5 text-orange-500" />
                    Performance Optimization
                  </CardTitle>
                  <CardDescription>
                    Benchmarking different methods and optimizing for large datasets
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm max-h-96">
                      <code>{performanceCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(performanceCode)}
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
