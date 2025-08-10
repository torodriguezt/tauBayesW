"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Play, Download, Wrench, Database, BarChart } from "lucide-react"
import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Separator } from "@/components/ui/separator"
import { Alert, AlertDescription } from "@/components/ui/alert"

export default function UtilitiesPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const simulationCode = `# Data simulation for quantile regression
library(tauBayesW)

# Single quantile data simulation
sim_single <- simulate_bqr_data(
  n = 200,
  betas = c(1.5, 2.0, -0.5),  # intercept + 2 slopes
  sigma = 0.8,
  seed = 123
)

# Examine simulated data
str(sim_single)
head(sim_single$data)

# Multiple quantile data simulation
sim_multi <- simulate_mo_bqr_data(
  n = 150, 
  p = 2,  # number of covariates (excluding intercept)
  beta_true = c(1.0, 0.5, -0.3),  # true coefficients
  seed = 456
)

# Check simulation results
print(sim_multi$beta_true)
summary(sim_multi$data)`

  const priorCode = `# Prior specification functions
library(tauBayesW)

# Default priors for single quantile models
prior_single <- prior_default(
  p = 3,        # number of coefficients (including intercept)
  b0 = 0,       # prior mean (scalar expanded to vector)
  B0 = 1000     # prior variance (scalar expanded to diagonal matrix)
)

# Custom prior with different means and variances
prior_custom <- prior_default(
  p = 3,
  b0 = c(0, 1, -0.5),           # different prior means
  B0 = diag(c(100, 50, 200)),   # different prior variances
  w_scale = 2                   # weight scale for approximate method
)

# Multiple quantile priors
prior_multi <- mo_prior_default(
  p = 3,
  b0 = 0,
  B0 = 1000
)

# Convert priors
as_bqr_prior(prior_multi, method = "ALD")
as_mo_bqr_prior(prior_single)`

  const convergenceCode = `# Convergence diagnostics
library(tauBayesW)

# Simulate and fit a model
sim <- simulate_bqr_data(n = 100, betas = c(1, 0.5, -0.3))
model <- bqr.svy(y ~ ., data = sim$data, weights = sim$weights, 
                 quantile = 0.5, method = "ALD")

# Check convergence
conv_diagnostics <- convergence_check(model, 
                                     rhat_threshold = 1.1,
                                     ess_ratio_threshold = 0.1,
                                     verbose = TRUE)

# Access specific diagnostics
print("R-hat values:")
print(conv_diagnostics$rhat)

print("Effective sample sizes:")
print(conv_diagnostics$neff)

print("Convergence status:")
print(conv_diagnostics$converged)

# For multiple quantile models
multi_model <- mo.bqr.svy(y ~ ., data = sim$data, weights = sim$weights,
                          quantiles = c(0.25, 0.5, 0.75))
convergence_check(multi_model)`

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
              Essential utilities for data simulation, prior specification, and model diagnostics
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">Data Simulation</Badge>
              <Badge variant="secondary">Prior Specification</Badge>
              <Badge variant="secondary">Convergence Diagnostics</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="simulation">Data Simulation</TabsTrigger>
              <TabsTrigger value="priors">Prior Specification</TabsTrigger>
              <TabsTrigger value="diagnostics">Diagnostics</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Available Utility Functions</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The tauBayesW package provides comprehensive utility functions to support 
                    the quantile regression workflow, from data preparation to model validation.
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
                      <li>• <code>simulate_bqr_data()</code></li>
                      <li>• <code>simulate_mo_bqr_data()</code></li>
                      <li>• Realistic synthetic datasets</li>
                      <li>• Controlled parameter testing</li>
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
                      <li>• <code>prior_default()</code></li>
                      <li>• <code>mo_prior_default()</code></li>
                      <li>• <code>as_bqr_prior()</code></li>
                      <li>• <code>as_mo_bqr_prior()</code></li>
                    </ul>
                  </CardContent>
                </Card>

                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <BarChart className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">Diagnostics</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>convergence_check()</code></li>
                      <li>• R-hat statistics</li>
                      <li>• Effective sample sizes</li>
                      <li>• EM convergence monitoring</li>
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
                    Generate synthetic datasets for testing and validation
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
                    <h4 className="font-semibold mb-2">simulate_bqr_data()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Generates data for single quantile regression analysis
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>Creates design matrix with specified number of covariates</li>
                      <li>Generates response variable with controlled noise level</li>
                      <li>Includes realistic survey weights</li>
                      <li>Returns data frame ready for modeling</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">simulate_mo_bqr_data()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Generates data optimized for multiple quantile analysis
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>Designed for testing multiple quantile algorithms</li>
                      <li>Includes heteroscedastic error structures</li>
                      <li>Provides ground truth for validation</li>
                      <li>Supports complex survey design simulation</li>
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
                    Flexible prior construction and conversion utilities
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

              <Card>
                <CardHeader>
                  <CardTitle>Prior Types and Usage</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">Multivariate Normal Priors</h4>
                    <p className="text-sm text-muted-foreground">
                      All methods use multivariate normal priors on regression coefficients:
                      β ~ N(b₀, B₀)
                    </p>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">Method-Specific Parameters</h4>
                    <ul className="text-sm space-y-2">
                      <li><strong>ALD and Score methods:</strong> Only require b₀ and B₀</li>
                      <li><strong>Approximate method:</strong> Additionally uses w_scale parameter</li>
                      <li><strong>EM algorithms:</strong> Support hierarchical prior structures</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">Prior Conversion</h4>
                    <p className="text-sm text-muted-foreground">
                      Convert between single and multiple quantile prior formats using 
                      as_bqr_prior() and as_mo_bqr_prior() functions.
                    </p>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="diagnostics" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Convergence Diagnostics</CardTitle>
                  <CardDescription>
                    Comprehensive convergence assessment for MCMC and EM algorithms
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{convergenceCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(convergenceCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Diagnostic Metrics</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <h4 className="font-semibold mb-2">R-hat Statistic (Gelman-Rubin)</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Measures convergence of MCMC chains. Values close to 1.0 indicate convergence.
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>R̂ &lt; 1.01: Excellent convergence</li>
                      <li>R̂ &lt; 1.1: Good convergence</li>
                      <li>R̂ &gt; 1.1: Poor convergence, consider more iterations</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">Effective Sample Size (ESS)</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Measures the effective number of independent samples.
                    </p>
                    <ul className="text-sm list-disc list-inside space-y-1">
                      <li>ESS ratio &gt; 0.1: Good efficiency</li>
                      <li>ESS ratio &gt; 0.01: Acceptable efficiency</li>
                      <li>ESS ratio &lt; 0.01: Consider thinning or more iterations</li>
                    </ul>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">EM Algorithm Diagnostics</h4>
                    <p className="text-sm text-muted-foreground">
                      For mo.bqr.svy models, diagnostics focus on EM convergence criteria,
                      iteration counts, and final parameter stability.
                    </p>
                  </div>
                </CardContent>
              </Card>

              <Alert>
                <AlertDescription>
                  Regular convergence checking is essential for reliable Bayesian inference. 
                  The convergence_check() function provides automated assessment with 
                  customizable thresholds for different analysis requirements.
                </AlertDescription>
              </Alert>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
