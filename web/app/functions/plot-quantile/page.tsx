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

export default function PlotQuantilePage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Plotting quantile regression results
library(tauBayesW)

## 1) Simulate data
set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- runif(n)
y  <- 1 + 2*x1 - 0.5*x2 + rnorm(n)
weights <- runif(n, 0.5, 2)
data <- data.frame(y, x1, x2)

## 2) Informative prior for bqr.svy (single-output)
prior_info <- prior_default(
  p     = 3,                     # (Intercept), x1, x2
  b0    = c(0.8, 1.9, -0.4),     # prior means (near the DGP)
  B0    = diag(0.5, 3),          # smaller variances => more informative
  c0    = 2,                     # IG(shape) for ALD scale
  C0    = 1,                     # IG(rate)  for ALD scale
  names = c("(Intercept)", "x1", "x2")
)

## 3) Fit bqr.svy with the three methods
niter  <- 5000
burnin <- 1000
thin   <- 5
tau    <- 0.5

fit_ald <- bqr.svy(
  y ~ x1 + x2,
  data     = data,
  weights  = weights,
  quantile = tau,
  method   = "ald",
  prior    = prior_info,
  niter    = niter, burnin = burnin, thin = thin
)

fit_score <- bqr.svy(
  y ~ x1 + x2,
  data     = data,
  weights  = weights,
  quantile = tau,
  method   = "score",
  prior    = prior_info,
  niter    = niter, burnin = burnin, thin = thin
)

fit_aprx <- bqr.svy(
  y ~ x1 + x2,
  data     = data,
  weights  = weights,
  quantile = tau,
  method   = "approximate",
  prior    = prior_info,
  niter    = niter, burnin = burnin, thin = thin
)

## 4) Informative prior for mo.bqr.svy (multi-output uses its own prior format)
mo_prior_info <- list(
  beta_mean   = c(0.8, 1.9, -0.4),
  beta_cov    = diag(0.5, 3),
  sigma_shape = 2,
  sigma_rate  = 1
)

## 5) Fit mo.bqr.svy (EM by default)
fit_multi <- mo.bqr.svy(
  y ~ x1 + x2,
  data      = data,
  weights   = weights,
  quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
  algorithm = "em",
  prior     = mo_prior_info,
  epsilon   = 1e-6,
  max_iter  = 500,
  verbose   = FALSE
)

## 6) Quick coefficient comparison
res <- rbind(
  "True (sim)" = c(1, 2, -0.5),
  "ALD"        = round(fit_ald$beta,   3),
  "Score"      = round(fit_score$beta, 3),
  "Approx"     = round(fit_aprx$beta,  3)
)
print(res)

## 7) Base plots (S3 plot methods)
plot(fit_ald)
plot(fit_multi)`

  const advancedCode = `# Quantile regression plotting options

plot_quantile_with_points.bqr.svy(
  object    = fit_ald,
  data      = data,
  predictor = "x1",
  alpha     = 0.6
)

plot_quantile_with_points.mo.bqr.svy(
  object    = fit_multi,
  data      = data,
  predictor = "x1",
  alpha     = 0.6
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
            <h1 className="text-4xl font-bold mb-4">Plot Functions</h1>
            <p className="text-xl text-muted-foreground mb-4">
              Visualization Functions for Quantile Regression Models
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">Visualization</Badge>
              <Badge variant="secondary">Graphics</Badge>
              <Badge variant="secondary">Diagnostic Plots</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-4">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="functions">Functions</TabsTrigger>
              <TabsTrigger value="examples">Examples</TabsTrigger>
              <TabsTrigger value="advanced">Advanced</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Description</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The tauBayesW package provides comprehensive visualization functions for 
                    quantile regression models, supporting both single and multiple quantile analyses.
                  </p>
                  <p>
                    These functions create publication-ready plots that help interpret model results,
                    assess fit quality, and communicate findings effectively.
                  </p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Available Plot Types</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="list-disc list-inside space-y-2">
                    <li><strong>Quantile curves:</strong> Shows fitted quantile regression lines</li>
                    <li><strong>Scatter plots with quantiles:</strong> Data points with overlaid quantile curves</li>
                    <li><strong>Coefficient plots:</strong> Displays coefficient estimates across quantiles</li>
                    <li><strong>Convergence plots:</strong> MCMC/EM algorithm diagnostics</li>
                  </ul>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="functions" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Main Plotting Functions</CardTitle>
                </CardHeader>
                <CardContent className="space-y-6">
                  <div>
                    <h4 className="font-semibold mb-2">plot.bqr.svy()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      S3 method for single quantile models
                    </p>
                    <pre className="bg-muted p-2 rounded text-sm">
                      plot(object, ...)
                    </pre>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">plot.mo.bqr.svy()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      S3 method for multiple quantile models
                    </p>
                    <pre className="bg-muted p-2 rounded text-sm">
                      plot(object, type = c("quantiles", "coefficients", "convergence"), ...)
                    </pre>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">plot_quantile.bqr.svy()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Specialized quantile curve plotting
                    </p>
                    <pre className="bg-muted p-2 rounded text-sm">
                      plot_quantile.bqr.svy(object, which_x = NULL, ...)
                    </pre>
                  </div>

                  <div>
                    <h4 className="font-semibold mb-2">plot_quantile_with_points()</h4>
                    <p className="text-sm text-muted-foreground mb-2">
                      Scatter plot with quantile overlay
                    </p>
                    <pre className="bg-muted p-2 rounded text-sm">
                      plot_quantile_with_points(object, which_x = NULL, alpha = 0.6, ...)
                    </pre>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Arguments</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div>
                      <h4 className="font-semibold">object</h4>
                      <p className="text-sm text-muted-foreground">Fitted model object (bqr.svy or mo.bqr.svy)</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">which_x</h4>
                      <p className="text-sm text-muted-foreground">Variable name for x-axis (univariate plots)</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">type</h4>
                      <p className="text-sm text-muted-foreground">Plot type for multiple quantile models</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">alpha</h4>
                      <p className="text-sm text-muted-foreground">Transparency level for points (0-1)</p>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="examples" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Basic Examples</CardTitle>
                  <CardDescription>
                    Essential plotting examples for quantile regression models
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

            <TabsContent value="advanced" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Advanced Plotting Options</CardTitle>
                  <CardDescription>
                    Customization and advanced visualization techniques
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{advancedCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(advancedCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                  
                  <Alert>
                    <AlertDescription>
                      All plotting functions support standard R graphics parameters like 
                      main, xlab, ylab, col, lwd, etc. for complete customization.
                    </AlertDescription>
                  </Alert>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Plot Types for Multiple Quantiles</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="list-disc list-inside space-y-2">
                    <li><strong>quantiles:</strong> Shows all fitted quantile curves together</li>
                    <li><strong>coefficients:</strong> Coefficient paths across quantiles</li>
                    <li><strong>convergence:</strong> EM algorithm convergence diagnostics</li>
                  </ul>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
