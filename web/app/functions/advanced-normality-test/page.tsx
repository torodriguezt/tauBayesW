"use client"

import { useState } from "react"
import { ArrowLeft, Code, Copy, Check, Package } from "lucide-react"
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
import { useApp } from "@/contexts/AppContext"
import { ThemeToggle } from "@/components/theme-toggle"
import { LanguageSelector } from "@/components/language-selector"

export default function MCMCBWQRALPage() {
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

# Generate sample data
set.seed(123)
n <- 200
p <- 3
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1.5, -0.8, 0.6)
y <- X %*% beta_true + rnorm(n, 0, 0.5)

# Add weights for informative sampling
weights <- runif(n, 0.5, 2)

# Basic MCMC_BWQR_AL estimation
result <- MCMC_BWQR_AL(
  y = y,
  X = X,
  tau = 0.5,
  weights = weights,
  iter = 1000,
  burn = 500
)

# Multiple quantiles
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
multi_result <- MCMC_BWQR_AL(
  y = y,
  X = X,
  tau = quantiles,
  weights = weights,
  iter = 2000,
  burn = 1000
)

# Custom prior specifications
custom_result <- MCMC_BWQR_AL(
  y = y,
  X = X,
  tau = 0.75,
  weights = weights,
  iter = 3000,
  burn = 1500,
  prior = list(
    sigma_beta = 5,
    sigma_tau = 0.5
  ),
  thin = 2
)

# View results
summary(result)
plot(result)
print(result$beta_samples[1:10, ])`

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
            <div className="h-12 w-12 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
              <Package className="h-6 w-6 text-blue-600 dark:text-blue-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">MCMC_BWQR_AL()</h1>
              <p className="text-xl text-muted-foreground">
                {t("mcmcALDesc")}
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Bayesian</Badge>
            <Badge variant="outline">Quantile Regression</Badge>
            <Badge variant="outline">MCMC</Badge>
            <Badge variant="outline">Asymmetric Laplace</Badge>
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
                The <code>MCMC_BWQR_AL()</code> function implements a Markov Chain Monte Carlo (MCMC) algorithm for Bayesian Weighted Quantile Regression using the Asymmetric Laplace distribution. This function provides full Bayesian inference with uncertainty quantification and posterior sampling.
              </p>
              <p className="text-muted-foreground leading-relaxed">
                The algorithm uses Gibbs sampling to estimate regression coefficients at specified quantile levels, accounting for weighted observations and providing complete posterior distributions for all parameters.
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
                  <code>{`MCMC_BWQR_AL(y, X, tau = 0.5, weights = NULL, iter = 1000, burn = 500, prior = NULL, thin = 1)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("MCMC_BWQR_AL(y, X, tau = 0.5, weights = NULL, iter = 1000, burn = 500, prior = NULL, thin = 1)")}
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
                      Numeric vector of response variable values.
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">X</code>
                      <Badge variant="destructive" className="text-xs">Required</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Design matrix of covariates (n x p matrix).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">tau</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Quantile level(s) to estimate. Can be a single value or vector (default: 0.5).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">weights</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Observation weights for handling informative sampling (default: NULL).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">iter</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Number of MCMC iterations (default: 1000).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">burn</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Number of burn-in iterations to discard (default: 500).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">prior</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      List containing prior hyperparameters (default: NULL for uninformative priors).
                    </p>
                  </div>
                  <div className="space-y-2 p-4 border rounded-lg">
                    <div className="flex items-center gap-2">
                      <code className="bg-muted px-2 py-1 rounded text-sm font-medium">thin</code>
                      <Badge variant="secondary" className="text-xs">Optional</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground">
                      Thinning interval for MCMC chain (default: 1).
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

        {/* Return Value */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Return Value</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">beta_samples</h4>
                  <p className="text-xs text-muted-foreground">Matrix of posterior samples for regression coefficients.</p>
                </div>
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">tau_samples</h4>
                  <p className="text-xs text-muted-foreground">Matrix of posterior samples for scale parameters.</p>
                </div>
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">sigma_samples</h4>
                  <p className="text-xs text-muted-foreground">Vector of posterior samples for error variance.</p>
                </div>
                <div className="p-3 border rounded-lg">
                  <h4 className="font-medium text-sm">convergence</h4>
                  <p className="text-xs text-muted-foreground">List containing convergence diagnostics and chain statistics.</p>
                </div>
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
                <Link href="/functions/compare-models">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">MCMC_BWQR_AP()</h4>
                      <p className="text-sm text-muted-foreground">Bayesian quantile regression with adaptive priors</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/bayesian-qreg">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">MCMC_BWQR_SL()</h4>
                      <p className="text-sm text-muted-foreground">Bayesian quantile regression with skewed Laplace</p>
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
