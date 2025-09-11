"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Play, Download } from "lucide-react"
import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Separator } from "@/components/ui/separator"
import { Alert, AlertDescription } from "@/components/ui/alert"

export default function BqrSvyPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Single and Multiple Quantile Bayesian Regression
library(tauBayesW)

# =====================================================
# Simulate data
# =====================================================
set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- runif(n)
y <- 1 + 2*x1 - 0.5*x2 + rnorm(n)
weights <- runif(n, 0.5, 2)
data_df <- data.frame(y, x1, x2)

# =====================================================
# Create prior using new unified interface
# =====================================================
# Informative prior for univariate models
prior_info <- prior(
  p = 3,                                         # intercept + 2 slopes
  type = "univariate",                           # for bqr.svy models
  beta_mean = c(1, 2, -0.5),                     # prior means close to true values
  beta_cov = diag(c(0.5, 0.3, 0.3)),             # informative covariances
  sigma_shape = 3,                               # for ALD method
  sigma_rate = 2,
  names = c("(Intercept)", "x1", "x2")
)

# =====================================================
# Fit: Single quantile (ALD method)
# =====================================================
fit_ald <- bqr.svy(
  y ~ x1 + x2,
  data = data_df,
  weights = weights,
  quantile = 0.5,                               # single quantile
  method = "ald",
  prior = prior_info,
  niter = 5000,
  burnin = 1000
)

# =====================================================
# Fit: Multiple quantiles (Score method) 
# =====================================================
fit_score_multi <- bqr.svy(
  y ~ x1 + x2,
  data = data_df,
  weights = weights,
  quantile = c(0.25, 0.5, 0.75),               # multiple quantiles
  method = "score",
  prior = prior_info,
  niter = 5000,
  burnin = 1000
)

# =====================================================
# Fit: Multiple quantiles (Approximate method)
# =====================================================
fit_approx_multi <- bqr.svy(
  y ~ x1 + x2,
  data = data_df,
  weights = weights,
  quantile = c(0.1, 0.25, 0.5, 0.75, 0.9),     # five quantiles
  method = "approximate",
  prior = prior_info,
  niter = 5000,
  burnin = 1000
)

# =====================================================
# View results
# =====================================================
print(fit_ald)
print(fit_score_multi)
print(fit_approx_multi)

# Plot results
plot(fit_ald)
plot(fit_score_multi)
plot(fit_approx_multi)
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
            <h1 className="text-4xl font-bold mb-4">bqr.svy()</h1>
            <p className="text-xl text-muted-foreground mb-4">
              Single or Multiple Quantile Bayesian Regression for Complex Surveys
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">MCMC Methods</Badge>
              <Badge variant="secondary">Multiple Quantiles</Badge>
              <Badge variant="secondary">Survey Weights</Badge>
              <Badge variant="secondary">Asymmetric Laplace</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="usage">Usage</TabsTrigger>
              <TabsTrigger value="arguments">Arguments</TabsTrigger>
              <TabsTrigger value="examples">Examples</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Description</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    Fits Bayesian quantile regression models for single or multiple quantiles using MCMC methods,
                    accounting for complex survey designs with observation weights. The function supports efficient
                    simultaneous estimation of multiple quantiles by passing a vector of target quantiles.
                  </p>
                  <p>
                    The function implements three MCMC approaches:
                  </p>
                  <ul className="list-disc list-inside space-y-2 ml-4">
                    <li><strong>ALD (Asymmetric Laplace Distribution):</strong> Gibbs sampler with full posterior inference</li>
                    <li><strong>Score:</strong> Score-based pseudo-likelihood with adaptive Metropolis-Hastings</li>
                    <li><strong>Approximate:</strong> Empirical pseudo-likelihood approach for complex surveys</li>
                  </ul>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Key Features</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="list-disc list-inside space-y-2">
                    <li>Single or multiple quantile estimation</li>
                    <li>Survey weights integration with proper normalization</li>
                    <li>Three optimized MCMC algorithms (ALD, Score, Approximate)</li>
                    <li>Comprehensive convergence diagnostics (R-hat, ESS)</li>
                    <li>Automatic summary methods for multiple quantiles</li>
                    <li>Fast C++ implementation (760×-1100× speedup)</li>
                  </ul>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="usage" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Function Signature</CardTitle>
                </CardHeader>
                <CardContent>
                  <pre className="bg-muted p-4 rounded-lg overflow-x-auto">
                    <code>{`bqr.svy(formula, data, weights = NULL, 
        quantile = 0.5,  # can be vector for multiple quantiles
        method = c("ald", "score", "approximate"), prior = NULL,
        niter = 50000, burnin = 10000, thin = 1, ...)`}</code>
                  </pre>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="arguments" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Arguments</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                      <div>
                        <h4 className="font-semibold">formula</h4>
                        <p className="text-sm text-muted-foreground">Model formula (e.g., y ~ x1 + x2)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">data</h4>
                        <p className="text-sm text-muted-foreground">Data frame containing variables</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">weights</h4>
                        <p className="text-sm text-muted-foreground">Survey weights (numeric vector or formula)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">quantile</h4>
                        <p className="text-sm text-muted-foreground">Single quantile value or vector of quantiles to estimate (0 &lt; tau &lt; 1). For multiple quantiles, use c(0.25, 0.5, 0.75)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">method</h4>
                        <p className="text-sm text-muted-foreground">MCMC method: "ALD", "Score", or "Approximate"</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">prior</h4>
                        <p className="text-sm text-muted-foreground">Object created using prior_default specifiying prior distribution for parameters</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">n_mcmc</h4>
                        <p className="text-sm text-muted-foreground">Number of MCMC iterations</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">burnin</h4>
                        <p className="text-sm text-muted-foreground">Number of burn-in iterations</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">verbose</h4>
                        <p className="text-sm text-muted-foreground">Print progress messages</p>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="examples" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Example Usage</CardTitle>
                  <CardDescription>
                    Complete example with data simulation, model fitting, and visualization
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{exampleCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(exampleCode)}
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
