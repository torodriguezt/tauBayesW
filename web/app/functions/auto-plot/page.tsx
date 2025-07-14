"use client"

import { useState } from "react"
import { ArrowLeft, Code, Copy, Check, BookOpen } from "lucide-react"
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

export default function MCMCBWQRAPPage() {
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

set.seed(123)
n <- 150
p <- 4
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(2.0, -1.2, 0.8, -0.5)
y <- X %*% beta_true + (1 + 0.5 * X[,1]^2) * rnorm(n, 0, 0.3)

w <- 1 / (1 + 0.5 * X[,1]^2)

# Run MCMC for 75th percentile regression
mcmc_result <- MCMC_BWQR_AP(
  y = y,
  X = X,
  w = w,
  n_mcmc = 20000,
  burnin = 5000,
  thin = 5,
  tau = 0.75,
  w_scale = 2.0
)

beta_samples <- mcmc_result$beta
accept_rate <- mcmc_result$accept_rate

# Posterior summaries
beta_mean <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, c(0.025, 0.975))

# Print results
print(paste("Acceptance rate:", round(accept_rate, 3)))
print("Posterior means:")
print(beta_mean)
print("95% Credible intervals:")
print(beta_ci)`

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
        <div className="max-w-4xl mx-auto">
        {/* Function Header */}
        <section className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
              <BookOpen className="h-6 w-6 text-purple-600 dark:text-purple-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">MCMC_BWQR_AP()</h1>
              <p className="text-xl text-muted-foreground">
                Adaptive-Proposal Metropolis-Hastings for Bayesian Weighted Quantile Regression
              </p>
            </div>
          </div>
          <div className="flex flex-wrap gap-2">
            <Badge variant="secondary">Bayesian</Badge>
            <Badge variant="secondary">MCMC</Badge>
            <Badge variant="secondary">Metropolis-Hastings</Badge>
            <Badge variant="secondary">Adaptive Proposal</Badge>
            <Badge variant="secondary">Quantile Regression</Badge>
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
                The <code className="bg-muted px-2 py-1 rounded text-sm">MCMC_BWQR_AP()</code> function implements 
                an advanced Adaptive-Proposal Metropolis-Hastings sampler for Bayesian Weighted Quantile Regression. 
                This sophisticated algorithm features automatic proposal covariance adaptation using Robbins-Monro 
                stochastic approximation, targeting optimal acceptance rates (≈23.4%) for efficient posterior exploration.
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
                  <BookOpen className="h-5 w-5 text-purple-500" />
                  Adaptive Proposal
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Automatic tuning of proposal covariance using Robbins-Monro algorithm for optimal efficiency.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Check className="h-5 w-5 text-green-500" />
                  Metropolis-Hastings
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Robust MCMC sampling with acceptance-rejection mechanism for complex posterior distributions.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Copy className="h-5 w-5 text-blue-500" />
                  Check-Loss Function
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Uses weighted asymmetric check-loss function based on Wang & He (2007) formulation.</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <ArrowLeft className="h-5 w-5 text-orange-500" />
                  High Performance
                </CardTitle>
              </CardHeader>
              <CardContent>
                <p>Optimized C++ implementation with RcppArmadillo for efficient matrix operations.</p>
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
                  <code>{`MCMC_BWQR_AP(y, X, w, n_mcmc, burnin, thin, 
             tau = 0.5, w_scale = 2.0, b0 = NULL, B0 = NULL)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("MCMC_BWQR_AP(y, X, w, n_mcmc, burnin, thin, tau = 0.5, w_scale = 2.0, b0 = NULL, B0 = NULL)")}
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
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">burnin</code>
                    <p className="text-sm text-muted-foreground">
                      Number of burn-in iterations to discard.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">thin</code>
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
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">w_scale</code>
                    <p className="text-sm text-muted-foreground">
                      Weight scaling factor (default: 2.0).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">b0, B0</code>
                    <p className="text-sm text-muted-foreground">
                      Prior mean and covariance matrix.
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
                Advanced example with heteroscedastic data and adaptive MCMC
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
                  <span>Matrix of posterior samples for regression coefficients</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">accept_rate</code>
                  <span>Overall acceptance rate for the MCMC sampler</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">n_mcmc</code>
                  <span>Total number of MCMC iterations performed</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">burnin</code>
                  <span>Number of burn-in iterations used</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">thin</code>
                  <span>Thinning interval applied</span>
                </div>
                <div className="flex items-start gap-3">
                  <code className="bg-muted px-2 py-1 rounded text-sm mt-1">n_samples</code>
                  <span>Number of posterior samples returned</span>
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
