"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Download, Wrench, Database, BarChart } from "lucide-react"
import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Alert, AlertDescription } from "@/components/ui/alert"

export default function UtilitiesPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const simulationCode = `# Data simulation functions for different sampling designs
library(tauBayesW)

# 1. Poisson sampling design
N <- 1000  # Population size
n <- 200   # Sample size

sim_poisson <- artificial_data_poi(N = N, n = n)

# Examine the simulated data
str(sim_poisson)
head(sim_poisson$sample_data)  # Response variable
head(sim_poisson$x_matrix)     # Design matrix (intercept + covariate)
head(sim_poisson$weights)      # Survey weights

# 2. Stratified sampling design
n_est <- 5  # Number of strata

sim_stratified <- artificial_data_est(N = N, n = n, n_est = n_est)

# Check stratified sample
length(sim_stratified$sample_data)  # Should be close to n
head(sim_stratified$weights)

# 3. Systematic sampling design
sim_systematic <- artificial_data_sys(N = N, n = n)

# Compare the three sampling designs
cat("Sample sizes:\n")
cat("Poisson:", length(sim_poisson$sample_data), "\n")
cat("Stratified:", length(sim_stratified$sample_data), "\n") 
cat("Systematic:", length(sim_systematic$sample_data), "\n")

# Use the simulated data in bqr.svy
fit_poisson <- bqr.svy(
  sample_data ~ x_matrix[,2],  # Exclude intercept column
  weights = sim_poisson$weights,
  quantile = 0.5
)`

  const priorCode = `# Prior specification functions
library(tauBayesW)

## 1) Simulate data for testing
set.seed(123)
N <- 1000
n <- 200
sim_data <- artificial_data_poi(N = N, n = n)

# Create a data frame for easier use
data <- data.frame(
  y = sim_data$sample_data,
  x1 = sim_data$x_matrix[,2]  # Exclude intercept column
)
weights <- sim_data$weights

## 2) Unified prior interface - prior()
# Default vague prior for univariate model (bqr.svy)
prior_univ_default <- prior(p = 2)  # intercept + x1

# Informative prior for univariate model
prior_univ_info <- prior(
  p = 2,
  type = "univariate",
  beta_mean = c(2, 1.5),        # Prior means close to true values
  beta_cov = diag(c(0.5, 0.5)), # More informative
  sigma_shape = 2,              # For ALD method
  sigma_rate = 1,
  names = c("(Intercept)", "x1")
)

# Default vague prior for multivariate model (mo.bqr.svy)
prior_multi_default <- prior(
  p = 2, 
  type = "multivariate"
)

# Informative prior for multivariate model
prior_multi_info <- prior(
  p = 2,
  type = "multivariate", 
  beta_mean = c(2, 1.5),
  beta_cov = diag(c(1, 1)),
  sigma_shape = 2,
  sigma_rate = 1,
  names = c("(Intercept)", "x1")
)

## 3) Legacy prior functions (still available)
# prior_default() for bqr.svy models
legacy_prior <- prior_default(
  p = 2,
  b0 = c(2, 1.5),      # prior means
  B0 = diag(0.5, 2),   # prior covariance
  c0 = 2,              # IG shape for sigma^2
  C0 = 1,              # IG rate for sigma^2  
  names = c("(Intercept)", "x1")
)

# mo_prior_default() for mo.bqr.svy models
legacy_mo_prior <- mo_prior_default(
  p = 2,
  beta_mean = c(2, 1.5),
  beta_cov = diag(1, 2),
  sigma_shape = 2,
  sigma_rate = 1,
  names = c("(Intercept)", "x1")
)

## 4) Fit models with different priors
# Single quantile with informative prior
fit_univ <- bqr.svy(
  y ~ x1,
  data = data,
  weights = weights,
  quantile = 0.5,
  method = "ald",
  prior = prior_univ_info,
  niter = 3000,
  burnin = 1000
)

# Multiple quantiles with function-based priors
taus <- c(0.25, 0.5, 0.75)

# Prior function that varies by quantile
prior_fun <- function(tau, p, names) {
  # More informative priors for extreme quantiles
  var_factor <- 1 + 2 * abs(tau - 0.5)
  prior(
    p = p,
    type = "multivariate",
    beta_mean = rep(0, p),
    beta_cov = diag(var_factor, p),
    sigma_shape = 2,
    sigma_rate = 1,
    names = names
  )
}

fit_multi <- mo.bqr.svy(
  y ~ x1,
  data = data,
  weights = weights,
  quantile = taus,
  prior = prior_fun,
  max_iter = 300,
  verbose = FALSE
)

## 5) Print and examine priors
print(prior_univ_info)
print(prior_multi_info)

## 6) Model comparison
summary(fit_univ)
summary(fit_multi)`

  const conversionCode = `# Prior conversion utilities
library(tauBayesW)

# Convert list to bqr_prior object
prior_list <- list(
  b0 = c(0, 1),
  B0 = diag(100, 2),
  c0 = 0.01,
  C0 = 0.01
)

# Convert to proper bqr_prior object
bqr_prior_obj <- as_bqr_prior(prior_list)
print(bqr_prior_obj)

# Convert list to mo_bqr_prior object
mo_prior_list <- list(
  beta_mean = c(0, 1),
  beta_cov = diag(100, 2),
  sigma_shape = 0.01,
  sigma_rate = 0.01
)

mo_bqr_prior_obj <- as_mo_bqr_prior(mo_prior_list)
print(mo_bqr_prior_obj)

# Use converted priors in models
sim_data <- artificial_data_poi(N = 500, n = 100)
data <- data.frame(
  y = sim_data$sample_data,
  x1 = sim_data$x_matrix[,2]
)

# Fit with converted priors
fit <- bqr.svy(
  y ~ x1,
  data = data,
  weights = sim_data$weights,
  quantile = 0.5,
  prior = bqr_prior_obj
)`

  return (
    <div className="min-h-screen bg-background">
      <div className="container mx-auto p-6">
        <Link href="/" className="inline-flex items-center mb-6 text-muted-foreground hover:text-foreground">
          <ArrowLeft className="mr-2 h-4 w-4" />
          Back to Documentation
        </Link>

        <div className="max-w-4xl mx-auto">
          <div className="mb-8">
            <h1 className="text-4xl font-bold mb-4">Utility Functions</h1>
            <p className="text-xl text-muted-foreground mb-4">
              Essential utilities for data simulation, prior specification, and model conversion
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">Data Simulation</Badge>
              <Badge variant="secondary">Prior Specification</Badge>
              <Badge variant="secondary">Object Conversion</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="simulation">Data Simulation</TabsTrigger>
              <TabsTrigger value="priors">Prior Specification</TabsTrigger>
              <TabsTrigger value="conversion">Conversion</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Available Utility Functions</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The tauBayesW package provides comprehensive utility functions to support 
                    the quantile regression workflow, from data preparation to model specification.
                  </p>
                </CardContent>
              </Card>

              <div className="grid gap-6 md:grid-cols-3">
                <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <Database className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                      </div>
                      <CardTitle className="text-lg">Data Simulation</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>artificial_data_poi()</code> - Poisson sampling</li>
                      <li>• <code>artificial_data_est()</code> - Stratified sampling</li>
                      <li>• <code>artificial_data_sys()</code> - Systematic sampling</li>
                      <li>• Realistic survey weight generation</li>
                    </ul>
                  </CardContent>
                </Card>

                <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                        <Wrench className="h-4 w-4 text-green-600 dark:text-green-400" />
                      </div>
                      <CardTitle className="text-lg">Prior Specification</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>prior()</code> - Unified interface</li>
                      <li>• <code>prior_default()</code> - For bqr.svy</li>
                      <li>• <code>mo_prior_default()</code> - For mo.bqr.svy</li>
                      <li>• Flexible prior construction</li>
                    </ul>
                  </CardContent>
                </Card>

                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <BarChart className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">Conversion</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>as_bqr_prior()</code> - Convert to bqr_prior</li>
                      <li>• <code>as_mo_bqr_prior()</code> - Convert to mo_bqr_prior</li>
                      <li>• Legacy list format support</li>
                      <li>• Seamless integration</li>
                    </ul>
                  </CardContent>
                </Card>
              </div>
            </TabsContent>

            <TabsContent value="simulation" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Data Simulation Functions</CardTitle>
                  <CardDescription>
                    Generate synthetic datasets with different survey sampling designs
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{simulationCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(simulationCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Function Details</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">artificial_data_poi(N, n)</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Generates data using Poisson sampling design
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>Creates population of size N with linear relationship</li>
                      <li>Samples n observations using Poisson sampling probabilities</li>
                      <li>Returns sample data, design matrix, and survey weights</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">artificial_data_est(N, n, n_est)</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Generates data using stratified sampling design
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>Creates stratified sample with n_est strata</li>
                      <li>Uses auxiliary variable to define strata boundaries</li>
                      <li>Ensures representative sampling across strata</li>
                      <li>Requires the 'sampling' package</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">artificial_data_sys(N, n)</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Generates data using systematic sampling design
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>Implements systematic sampling with size variable</li>
                      <li>Ensures regular interval sampling from ordered population</li>
                      <li>Provides balanced representation across the population</li>
                      <li>Requires the 'sampling' package</li>
                    </ul>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="priors" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Prior Specification</CardTitle>
                  <CardDescription>
                    Unified and flexible prior construction for all model types
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{priorCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(priorCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Alert>
                <AlertDescription>
                  The unified <code>prior()</code> function automatically creates the appropriate prior 
                  object based on the model type. Use <code>type = "univariate"</code> for bqr.svy 
                  models and <code>type = "multivariate"</code> for mo.bqr.svy models.
                </AlertDescription>
              </Alert>
            </TabsContent>

            <TabsContent value="conversion" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Prior Conversion Utilities</CardTitle>
                  <CardDescription>
                    Convert legacy list formats to proper prior objects
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{conversionCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(conversionCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Alert>
                <AlertDescription>
                  Conversion functions enable seamless integration of legacy code and provide 
                  flexibility when working with different prior specifications. They ensure 
                  proper object structure and validation.
                </AlertDescription>
              </Alert>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
