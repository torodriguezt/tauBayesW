"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Check, Package } from "lucide-react"
import Link from "next/link"
import Image from "next/image"
import { getImageSrc } from "@/lib/image-utils"
import { useApp } from "@/contexts/AppContext"
import { ThemeToggle } from "@/components/theme-toggle"
import { LanguageSelector } from "@/components/language-selector"

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
  const { t } = useApp()

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

# Generate sample data
set.seed(123)
n <- 200
p <- 3
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1.2, -0.6, 0.9)
y <- X %*% beta_true + rnorm(n, 0, 0.4)

# Create non-uniform weights
w <- 1 / (1 + 0.2 * abs(X[,2]))

# Basic NonCrossingBWQR_AL estimation
result <- NonCrossingBWQR_AL(
  y = y,
  X = X,
  w = w,
  n_mcmc = 15000,
  burnin_mcmc = 3000,
  thin_mcmc = 5,
  tau = 0.5
)

# Multiple quantiles with non-crossing constraints
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
multi_result <- lapply(quantiles, function(q) {
  NonCrossingBWQR_AL(
    y = y,
    X = X,
    w = w,
    n_mcmc = 10000,
    burnin_mcmc = 2000,
    thin_mcmc = 3,
    tau = q
  )
})

# Extract and summarize results
beta_samples <- result$beta
sigma_samples <- result$sigma

# Posterior summaries
beta_mean <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, c(0.025, 0.975))
sigma_mean <- mean(sigma_samples)
sigma_ci <- quantile(sigma_samples, c(0.025, 0.975))

# Display results
print("Posterior means for beta:")
print(beta_mean)
print("95% Credible intervals for beta:")
print(beta_ci)
print(paste("Posterior mean for sigma:", round(sigma_mean, 4)))
print(paste("95% CI for sigma:", round(sigma_ci, 4)))`

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
            <span className="font-medium">{t("help")}</span>
          </button>
          <div className="flex items-center space-x-2">
            <Image
              src={getImageSrc("/logo_tau.png")}
              alt="tauBayesW Logo"
              width={24}
              height={24}
              className="rounded"
              onError={(e) => {
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
          <div className="ml-auto flex items-center space-x-2">
            <ThemeToggle />
            <LanguageSelector />
          </div>
        </div>
      </header>

      <div className="container mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 xl:px-12 2xl:px-16 py-8">
        <div className="max-w-4xl mx-auto">
        {/* Function Header */}
        <div className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
              <Package className="h-6 w-6 text-green-600 dark:text-green-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">NonCrossingBWQR_AL()</h1>
              <p className="text-xl text-muted-foreground">
                {t("nonCrossingDesc")}
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Bayesian</Badge>
            <Badge variant="outline">Quantile Regression</Badge>
            <Badge variant="outline">Non-Crossing</Badge>
            <Badge variant="outline">Gibbs Sampling</Badge>
          </div>
        </div>

        {/* Description */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Description</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground leading-relaxed mb-4">
                The <code>NonCrossingBWQR_AL()</code> function implements a Gibbs sampler for Bayesian Weighted Quantile Regression using the Asymmetric Laplace distribution with non-crossing constraints. This ensures that quantile curves maintain their proper ordering across the entire covariate space.
              </p>
              <p className="text-muted-foreground leading-relaxed">
                The algorithm features optimized sampling from Generalized Inverse-Gaussian distributions using the GIGrvg package and includes robust matrix operations with fallback methods for numerical stability.
              </p>
            </CardContent>
          </Card>
        </section>

        {/* Syntax */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Syntax</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="relative">
                <pre className="bg-muted p-4 rounded-lg text-sm" style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
                  <code>{`NonCrossingBWQR_AL(y, X, w, n_mcmc, burnin_mcmc, thin_mcmc, tau = 0.5)`}</code>
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
                <div className="grid grid-cols-1 gap-4">
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">y</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Numeric vector of response variables (n × 1).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">X</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Design matrix of covariates (n × p).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">w</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Vector of observation weights (n × 1).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">n_mcmc</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Total number of MCMC iterations.
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">burnin_mcmc</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Number of burn-in iterations to discard.
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">thin_mcmc</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Thinning interval for output.
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">tau</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Quantile level (default: 0.5).
                    </p>
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
              <CardTitle>Examples</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="relative">
                <pre className="bg-muted p-4 rounded-lg text-sm" style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
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

        {/* Key Features */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Key Features</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="p-4 border rounded-lg">
                  <h4 className="font-medium text-sm mb-2">Non-Crossing Constraints</h4>
                  <p className="text-xs text-muted-foreground">
                    Ensures proper ordering of quantile curves across all covariate values.
                  </p>
                </div>
                <div className="p-4 border rounded-lg">
                  <h4 className="font-medium text-sm mb-2">Asymmetric Laplace</h4>
                  <p className="text-xs text-muted-foreground">
                    Uses the Asymmetric Laplace representation for quantile regression.
                  </p>
                </div>
                <div className="p-4 border rounded-lg">
                  <h4 className="font-medium text-sm mb-2">GIG Sampling</h4>
                  <p className="text-xs text-muted-foreground">
                    Optimized Generalized Inverse-Gaussian sampling using GIGrvg.
                  </p>
                </div>
                <div className="p-4 border rounded-lg">
                  <h4 className="font-medium text-sm mb-2">Numerical Stability</h4>
                  <p className="text-xs text-muted-foreground">
                    Robust matrix operations with fallback methods for near-singular matrices.
                  </p>
                </div>
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
              <div className="space-y-3">
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">beta</h4>
                  <p className="text-xs text-muted-foreground">Matrix of posterior samples for regression coefficients (rows = samples, cols = coefficients).</p>
                </div>
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">sigma</h4>
                  <p className="text-xs text-muted-foreground">Vector of posterior samples for the scale parameter σ.</p>
                </div>
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">v</h4>
                  <p className="text-xs text-muted-foreground">Matrix of posterior samples for auxiliary variables from the Gibbs sampler.</p>
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
              <div className="p-4 border-l-4 border-orange-500 bg-orange-50 dark:bg-orange-950">
                <h4 className="font-medium text-orange-800 dark:text-orange-200">Required Package</h4>
                <p className="text-sm text-orange-700 dark:text-orange-300">
                  This function requires the <code className="bg-muted px-1 rounded">GIGrvg</code> package 
                  for efficient Generalized Inverse-Gaussian sampling. Install with: 
                  <code className="bg-muted px-1 rounded ml-1">install.packages("GIGrvg")</code>
                </p>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* See Also */}
        <section>
          <Card>
            <CardHeader>
              <CardTitle>See Also</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <Link href="/functions/advanced-normality-test">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">MCMC_BWQR_AL()</h4>
                      <p className="text-sm text-muted-foreground">Standard MCMC for asymmetric Laplace BWQR</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/bayesian-qreg">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">MCMC_BWQR_SL()</h4>
                      <p className="text-sm text-muted-foreground">MCMC for skewed Laplace BWQR</p>
                    </CardContent>
                  </Card>
                </Link>
              </div>
            </CardContent>
          </Card>
        </section>
        </div>
      </div>
    </div>
  )
}
