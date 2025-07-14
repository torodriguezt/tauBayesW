'use client'

import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { ArrowLeft, Package, Zap, FileText, BarChart, AlertTriangle, TrendingUp } from "lucide-react"
import Link from "next/link"
import Image from "next/image"
import { getImageSrc } from "@/lib/image-utils"

export default function MCMCBWQRALPage() {
  const handleBack = () => {
    if (window.history.length > 1) {
      window.history.back()
    } else {
      window.location.href = '/'
    }
  }
  return (
    <div className="min-h-screen bg-background">
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
          {/* Hero Section */}
          <section className="mb-12">
            <div className="flex items-center gap-3 mb-6">
              <div className="h-12 w-12 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                <Package className="h-6 w-6 text-green-600 dark:text-green-400" />
              </div>
              <div>
                <h1 className="text-3xl font-bold">MCMC_BWQR_AL()</h1>
                <p className="text-xl text-muted-foreground">MCMC for Bayesian Weighted Quantile Regression with Asymmetric Laplace</p>
              </div>
            </div>
            
            <div className="flex flex-wrap gap-2 mb-6">
              <Badge variant="secondary">Bayesian</Badge>
              <Badge variant="secondary">MCMC</Badge>
              <Badge variant="secondary">Quantile Regression</Badge>
              <Badge variant="secondary">Asymmetric Laplace</Badge>
              <Badge variant="secondary">Gibbs Sampling</Badge>
              <Badge variant="secondary">C++</Badge>
            </div>

            <Card>
              <CardContent className="pt-6">
                <p className="text-lg leading-relaxed">
                  The <code className="bg-muted px-2 py-1 rounded text-sm">MCMC_BWQR_AL()</code> function implements 
                  a robust Markov Chain Monte Carlo (MCMC) Gibbs sampler for Bayesian Weighted Quantile Regression using the 
                  Asymmetric Laplace distribution.
                </p>
              </CardContent>
            </Card>
          </section>

          {/* Key Features */}
          <section className="mb-12">
            <h2 className="text-2xl font-bold mb-6">Key Features</h2>
            <div className="grid md:grid-cols-2 gap-6">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <TrendingUp className="h-5 w-5 text-purple-500" />
                    Gibbs Sampling
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Advanced Gibbs sampler with robust numerical stabilization and adaptive regularization techniques.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <BarChart className="h-5 w-5 text-green-500" />
                    Inverse-Gaussian Sampling
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Efficient sampling from Generalized Inverse-Gaussian distribution using optimized algorithms.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <AlertTriangle className="h-5 w-5 text-orange-500" />
                    Numerical Stability
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Advanced handling of near-singular matrices with adaptive ridge regularization and fallback methods.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Zap className="h-5 w-5 text-amber-500" />
                    C++ Performance
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>High-performance implementation using RcppArmadillo with optimized linear algebra operations.</p>
                </CardContent>
              </Card>
            </div>
          </section>

          {/* Usage */}
          <section className="mb-12">
            <h2 className="text-2xl font-bold mb-6">Usage</h2>
            <Card>
              <CardHeader>
                <CardTitle>Function Syntax</CardTitle>
              </CardHeader>
              <CardContent>
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                  <code>{`MCMC_BWQR_AL(y, X, w, tau = 0.5, n_mcmc = 50000, 
             burnin = 10000, thin = 10)`}</code>
                </pre>
              </CardContent>
            </Card>
          </section>

          {/* Parameters */}
          <section className="mb-12">
            <h2 className="text-2xl font-bold mb-6">Parameters</h2>
            <div className="space-y-4">
              <Card>
                <CardContent className="pt-6">
                  <div className="grid gap-4">
                    <div className="grid grid-cols-4 gap-4 items-center border-b pb-2">
                      <div className="font-semibold">Parameter</div>
                      <div className="font-semibold">Type</div>
                      <div className="font-semibold">Required</div>
                      <div className="font-semibold">Description</div>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">y</code>
                      <span className="text-sm">numeric vector</span>
                      <Badge variant="destructive" className="w-fit">Required</Badge>
                      <span className="text-sm">Response variable vector</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">X</code>
                      <span className="text-sm">numeric matrix</span>
                      <Badge variant="destructive" className="w-fit">Required</Badge>
                      <span className="text-sm">Design matrix of covariates</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">tau</code>
                      <span className="text-sm">numeric</span>
                      <Badge variant="destructive" className="w-fit">Required</Badge>
                      <span className="text-sm">Quantile level (0 &lt; tau &lt; 1)</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">w</code>
                      <span className="text-sm">numeric vector</span>
                      <Badge variant="destructive" className="w-fit">Required</Badge>
                      <span className="text-sm">Observation weights vector</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">tau</code>
                      <span className="text-sm">numeric</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Quantile level (0 &lt; tau &lt; 1), default: 0.5</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">n_mcmc</code>
                      <span className="text-sm">integer</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Total MCMC iterations (default: 50000)</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">burnin</code>
                      <span className="text-sm">integer</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Burn-in period (default: 10000)</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">thin</code>
                      <span className="text-sm">integer</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Thinning interval (default: 10)</span>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
          </section>

          {/* Example */}
          <section className="mb-12">
            <h2 className="text-2xl font-bold mb-6">Example</h2>
            <Card>
              <CardHeader>
                <CardTitle>Basic MCMC Analysis</CardTitle>
                <CardDescription>
                  Example of using MCMC_BWQR_AL for Bayesian quantile regression
                </CardDescription>
              </CardHeader>
              <CardContent>
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                  <code>{`library(tauBayesW)

# Generate sample data
set.seed(123)
n <- 200
p <- 3
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1.5, -0.8, 0.6)
y <- X %*% beta_true + rt(n, df = 3) * 0.5 

w <- 1 / (1 + 0.5 * (X[,1])^2)  

# Run MCMC for 90th percentile regression
mcmc_result <- MCMC_BWQR_AL(
  y = y,
  X = X,
  w = w,
  tau = 0.9,
  n_mcmc = 50000,
  burnin = 10000,
  thin = 10
)

# Extract posterior samples
beta_samples <- mcmc_result$beta
sigma_samples <- mcmc_result$sigma

# Posterior summaries
beta_mean <- apply(beta_samples, 2, mean)
beta_ci <- apply(beta_samples, 2, quantile, c(0.025, 0.975))

print("Posterior means:")
print(beta_mean)
print("95% Credible intervals:")
print(beta_ci)`}</code>
                </pre>
              </CardContent>
            </Card>
          </section>

          {/* Return Value */}
          <section className="mb-12">
            <h2 className="text-2xl font-bold mb-6">Return Value</h2>
            <Card>
              <CardContent className="pt-6">
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


         
        </div>
      </div>
    </div>
  )
}
