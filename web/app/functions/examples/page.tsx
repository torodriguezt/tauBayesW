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
  quantile = 0.5,
  method = "ALD",
  n_mcmc = 10000,
  burnin = 2000,
  prior = prior,
  verbose = TRUE
)

# 4. Convergence Diagnostics
print("Convergence diagnostics:")
conv_results <- convergence_check(model)
print(conv_results)

# 5. Model Summary and Interpretation
print("Complete model summary:")
summary(model)

print("Basic model output:")
print(model)

# 6. Visualization
plot(model)

# 7. Custom Plotting
plot_quantile.bqr.svy(model, which_x = "x1", 
                      main = "Quantile Regression Results",
                      xlab = "X1 Variable", 
                      ylab = "Response Y")

# 8. Model Comparison with True Values
cat("\\n=== Model Validation ===\\n")
cat("True coefficients:", sim_data$betas, "\\n")
cat("Estimated coefficients:", coef(model), "\\n")
cat("Estimation error:", abs(coef(model) - sim_data$betas), "\\n")`

  const multipleQuantilesCode = `# Multiple quantiles analysis using EM algorithm
library(tauBayesW)

# 1. Generate data with heteroscedastic errors
set.seed(123)
n <- 250
x1 <- rnorm(n)
x2 <- runif(n, -1, 1)

# Heteroscedastic error structure
error_sd <- 0.5 + 0.3 * abs(x1)  # variance depends on x1
errors <- rnorm(n, 0, error_sd)

y <- 1 + 2*x1 - 0.5*x2 + errors
weights <- runif(n, 0.8, 1.5)
data <- data.frame(y, x1, x2)

# 2. Fit multiple quantiles simultaneously
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

multi_model <- mo.bqr.svy(
  y ~ x1 + x2,
  data = data,
  weights = weights,
  quantile = quantiles,
  algorithm = "em",
  max_iter = 1000,
  tolerance = 1e-6,
  verbose = TRUE
)

# 3. Examine results for all quantiles
print("Multi-quantile model summary:")
summary(multi_model)

# 4. Extract specific quantile results
print("Results for median (0.5 quantile):")
median_results <- multi_model$results[[3]]  # 3rd element = 0.5 quantile
print(median_results)

# 5. Convergence diagnostics
print("Convergence status:")
conv_check <- convergence_check(multi_model)
print(conv_check)

# 6. Coefficient comparison across quantiles
cat("\\n=== Coefficients across quantiles ===\\n")
for(i in seq_along(quantiles)) {
  cat("Quantile", quantiles[i], ":")
  cat(" Coefficients =", round(multi_model$results[[i]]$coefficients, 3), "\\n")
}

# 7. Plotting multiple quantiles
plot(multi_model, type = "quantiles")
plot(multi_model, type = "coefficients")`

  const surveyWeightsCode = `# Complex survey data analysis with survey weights
library(tauBayesW)

# 1. Simulate realistic survey data with sampling bias
set.seed(789)
n_population <- 10000
n_sample <- 500

# Population with complex structure
pop_x1 <- rnorm(n_population, 0, 1)
pop_x2 <- rbinom(n_population, 1, 0.4)  # binary covariate

# True population relationship
pop_y <- 2 + 1.5*pop_x1 + 0.8*pop_x2 + rnorm(n_population, 0, 1)

# Create informative sampling (bias toward higher x1 values)
sampling_prob <- pnorm(pop_x1, mean = 0, sd = 1)
sampled_indices <- sample(n_population, n_sample, prob = sampling_prob)

# Extract sample
sample_data <- data.frame(
  y = pop_y[sampled_indices],
  x1 = pop_x1[sampled_indices],
  x2 = pop_x2[sampled_indices]
)

# Calculate inverse probability weights
sample_weights <- 1 / sampling_prob[sampled_indices]
sample_weights <- sample_weights / mean(sample_weights)  # normalize

# 2. Compare weighted vs unweighted analysis
cat("=== Unweighted Analysis ===\\n")
unweighted_model <- bqr.svy(
  y ~ x1 + x2,
  data = sample_data,
  quantile = 0.5,
  method = "ALD",
  n_mcmc = 8000,
  burnin = 1500
)

cat("\\n=== Weighted Analysis ===\\n")
weighted_model <- bqr.svy(
  y ~ x1 + x2,
  data = sample_data,
  weights = sample_weights,
  quantile = 0.5,
  method = "ALD", 
  n_mcmc = 8000,
  burnin = 1500
)

# 3. Results comparison
cat("\\n=== Comparison with Population Truth ===\\n")
cat("True population coefficients: [2.0, 1.5, 0.8]\\n")
cat("Unweighted estimates:", round(coef(unweighted_model), 3), "\\n")
cat("Weighted estimates:", round(coef(weighted_model), 3), "\\n")

# Calculate bias
true_coefs <- c(2.0, 1.5, 0.8)
unweighted_bias <- abs(coef(unweighted_model) - true_coefs)
weighted_bias <- abs(coef(weighted_model) - true_coefs)

cat("\\nAbsolute bias (unweighted):", round(unweighted_bias, 3), "\\n")
cat("Absolute bias (weighted):", round(weighted_bias, 3), "\\n")
cat("Bias reduction:", round(unweighted_bias - weighted_bias, 3), "\\n")

# 4. Diagnostic summaries
print("\\n=== Weighted Model Summary ===")
summary(weighted_model)

print("\\n=== Unweighted Model Summary ===")  
summary(unweighted_model)

# 5. Visualization comparison
plot_quantile.bqr.svy(weighted_model, which_x = "x1", 
                      main = "Weighted Analysis")
plot_quantile.bqr.svy(unweighted_model, which_x = "x1", 
                      main = "Unweighted Analysis")`

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
              <TabsTrigger value="survey">Survey Weights</TabsTrigger>
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

            <TabsContent value="survey" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Database className="h-5 w-5 text-purple-500" />
                    Survey Weights Implementation
                  </CardTitle>
                  <CardDescription>
                    Handling complex survey designs with informative sampling and weights
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
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
