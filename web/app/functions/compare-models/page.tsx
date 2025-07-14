"use client"

import { useState } from "react"
import { ArrowLeft, Download, Copy, Check } from "lucide-react"
import Link from "next/link"
import Image from "next/image"
import { getImageSrc } from "@/lib/image-utils"

import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"

export default function NonCrossingBWQRALPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const handleBack = () => {
    if (window.history.length > 1) {
      window.history.back()
    } else {
      window.location.href = '/'
    }
  }

  const exampleCode = `library(tauBayesW)
library(GIGrvg)  # Required for GIG sampling
set.seed(123)
n <- 200
p <- 3
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1.2, -0.6, 0.9)
y <- X %*% beta_true + rnorm(n, 0, 0.4)

w <- 1 / (1 + 0.2 * abs(X[,2]))

# Run Non-Crossing BWQR
nc_result <- NonCrossingBWQR_AL(
  y = y,
  X = X,
  w = w,
  n_mcmc = 15000,
  burnin_mcmc = 3000,
  thin_mcmc = 5,
  tau = 0.5
)

# Extract results
beta_samples <- nc_result$beta
sigma_samples <- nc_result$sigma

# Posterior summaries
beta_mean <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, c(0.025, 0.975))
sigma_mean <- mean(sigma_samples)

# Print results
print("Posterior means for beta:")
print(beta_mean)
print("95% Credible intervals for beta:")
print(beta_ci)
print(paste("Posterior mean for sigma:", round(sigma_mean, 4)))`

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="container mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 xl:px-12 2xl:px-16 flex h-14 items-center">
          <button 
            onClick={handleBack}
            className="flex items-center space-x-2 mr-6 hover:text-blue-600 transition-colors cursor-pointer"
          >
            <ArrowLeft className="h-4 w-4" />
            <span className="font-medium">Back to Documentation</span>
          </button>
          <div className="flex items-center space-x-2">
            <Image
              src={getImageSrc("/logo_tau.png")}
              alt="tauBayesW Logo"
              width={24}
              height={24}
              className="rounded"
              onError={(e) => {
                // Fallback al logo por defecto si no se puede cargar
                const target = e.target as HTMLElement
                target.style.display = 'none'
                const fallback = target.nextElementSibling as HTMLElement
                if (fallback) fallback.style.display = 'flex'
              }}
            />
            <div className="h-6 w-6 items-center justify-center rounded bg-blue-600 text-white text-xs font-bold hidden">
              R
            </div>
            <span className="font-bold">tauBayesW</span>
          </div>
        </div>
      </header>

      <div className="container mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 xl:px-12 2xl:px-16 py-8">
        <div className="max-w-4xl mx-auto">{/* Contenido centrado pero responsive */}
        {/* Function Header */}
        <section className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-red-100 dark:bg-red-900 flex items-center justify-center">
              <Download className="h-6 w-6 text-red-600 dark:text-red-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">NonCrossingBWQR_AL()</h1>
              <p className="text-xl text-muted-foreground">
                Non-Crossing Bayesian Weighted Quantile Regression with Asymmetric Laplace
              </p>
            </div>
          </div>
          <div className="flex flex-wrap gap-2">
            <Badge variant="secondary">Bayesian</Badge>
            <Badge variant="secondary">Gibbs Sampling</Badge>
            <Badge variant="secondary">Non-Crossing</Badge>
            <Badge variant="secondary">Asymmetric Laplace</Badge>
            <Badge variant="secondary">GIG Sampling</Badge>
            <Badge variant="secondary">C++</Badge>
          </div>
        </section>

        {/* Description */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Description</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground leading-relaxed">
                The <code className="bg-muted px-2 py-1 rounded text-sm">NonCrossingBWQR_AL()</code> function implements 
                a sophisticated Gibbs sampler for Bayesian Weighted Quantile Regression using the Asymmetric Laplace 
                distribution. This algorithm ensures non-crossing quantile estimates through proper Bayesian 
                modeling and features optimized sampling from Generalized Inverse-Gaussian distributions 
                using the GIGrvg package.
              </p>
            </CardContent>
          </Card>
        </section>

        {/* Key Features */}
        <section className="mb-8">
          <h2 className="text-2xl font-bold mb-6">Key Features</h2>
          <div className="grid md:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Download className="h-5 w-5 text-red-500" />
                  Non-Crossing Quantiles
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Ensures monotonic quantile estimates through proper Bayesian hierarchical modeling structure.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Check className="h-5 w-5 text-green-500" />
                  Complete Gibbs Sampler
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Full conditional sampling for β, σ, and v parameters with optimal convergence properties.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Copy className="h-5 w-5 text-blue-500" />
                  GIG Sampling
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Optimized Generalized Inverse-Gaussian sampling using GIGrvg for efficient v parameter updates.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <ArrowLeft className="h-5 w-5 text-purple-500" />
                  Numerical Stability
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Robust matrix operations with fallback methods for near-singular covariance matrices.</p>
              </CardContent>
            </Card>
          </div>
        </section>

        {/* Syntax */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Syntax</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="relative">
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto">
                  <code>{`NonCrossingBWQR_AL(y, X, w, n_mcmc, burnin_mcmc, 
                      thin_mcmc, tau = 0.5)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("NonCrossingBWQR_AL(y, X, w, n_mcmc, burnin_mcmc, thin_mcmc, tau = 0.5)")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Parameters */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Parameters</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">y</code>
                    <p className="text-sm text-muted-foreground">
                      Numeric vector of response variables (n × 1).
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">X</code>
                    <p className="text-sm text-muted-foreground">
                      Design matrix of covariates (n × p).
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">w</code>
                    <p className="text-sm text-muted-foreground">
                      Vector of observation weights (n × 1).
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">n_mcmc</code>
                    <p className="text-sm text-muted-foreground">
                      Total number of MCMC iterations.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">burnin_mcmc</code>
                    <p className="text-sm text-muted-foreground">
                      Number of burn-in iterations to discard.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">thin_mcmc</code>
                    <p className="text-sm text-muted-foreground">
                      Thinning interval for output.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">tau</code>
                    <p className="text-sm text-muted-foreground">
                      Quantile level (default: 0.5).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>
        {/* Examples */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Example</CardTitle>
              <CardDescription>
                Non-crossing quantile regression with GIG sampling
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="relative">
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                  <code>{exampleCode}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard(exampleCode)}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Return Value */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Return Value</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="mb-4">The function returns a list containing:</p>
              <div className="grid gap-3">
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">beta</code>
                  <span>Matrix of posterior samples for regression coefficients (rows = samples, cols = coefficients)</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">sigma</code>
                  <span>Vector of posterior samples for the scale parameter σ</span>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Dependencies */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Dependencies</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div className="p-4 border-l-4 border-orange-500 bg-orange-50 dark:bg-orange-950">
                  <h4 className="font-medium text-orange-800 dark:text-orange-200">Required Package</h4>
                  <p className="text-sm text-orange-700 dark:text-orange-300">
                    This function requires the <code className="bg-muted px-1 rounded">GIGrvg</code> package 
                    for efficient Generalized Inverse-Gaussian sampling. Install with: 
                    <code className="bg-muted px-1 rounded ml-1">install.packages("GIGrvg")</code>
                  </p>
                </div>
                <div className="p-4 border-l-4 border-blue-500 bg-blue-50 dark:bg-blue-950">
                  <h4 className="font-medium text-blue-800 dark:text-blue-200">C++ Implementation</h4>
                  <p className="text-sm text-blue-700 dark:text-blue-300">
                    Uses optimized RcppArmadillo for fast matrix operations with robust fallback methods 
                    for numerical stability.
                  </p>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>
        </div>
      </div>
    </div>
  )
}
