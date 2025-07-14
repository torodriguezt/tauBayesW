'use client'

import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { ArrowLeft, Code, Zap, FileText, BarChart, AlertTriangle } from "lucide-react"
import Link from "next/link"
import Image from "next/image"

export default function EMBWQRALMOPage() {
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
              src="/logo_tau.png"
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
              <div className="h-12 w-12 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                <Code className="h-6 w-6 text-blue-600 dark:text-blue-400" />
              </div>
              <div>
                <h1 className="text-3xl font-bold">EM_BWQR_AL_MO()</h1>
                <p className="text-xl text-muted-foreground">EM Algorithm for Bayesian Weighted Quantile Regression with Asymmetric Laplace</p>
              </div>
            </div>
            
            <div className="flex flex-wrap gap-2 mb-6">
              <Badge variant="secondary">Bayesian</Badge>
              <Badge variant="secondary">Quantile Regression</Badge>
              <Badge variant="secondary">EM Algorithm</Badge>
              <Badge variant="secondary">Asymmetric Laplace</Badge>
              <Badge variant="secondary">C++</Badge>
            </div>

            <Card>
              <CardContent className="pt-6">
                <p className="text-lg leading-relaxed">
                  The <code className="bg-muted px-2 py-1 rounded text-sm">EM_BWQR_AL_MO()</code> function implements an 
                  Expectation-Maximization (EM) algorithm for Bayesian Weighted Quantile Regression using the Asymmetric 
                  Laplace distribution. This function is particularly useful for modeling conditional quantiles when 
                  observations have different weights or when dealing with heteroscedastic data.
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
                    <Zap className="h-5 w-5 text-amber-500" />
                    EM Algorithm
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Efficient Expectation-Maximization algorithm for parameter estimation with fast convergence properties.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <BarChart className="h-5 w-5 text-green-500" />
                    Weighted Regression
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Supports weighted observations, allowing for different importance or reliability of data points.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <AlertTriangle className="h-5 w-5 text-orange-500" />
                    Asymmetric Laplace
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>Uses Asymmetric Laplace distribution for robust quantile regression modeling.</p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Code className="h-5 w-5 text-blue-500" />
                    C++ Implementation
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <p>High-performance C++ backend with Rcpp integration for computational efficiency.</p>
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
                  <code>{`EM_BWQR_AL_MO(y, X, tau, weights = NULL, maxiter = 1000, 
              tol = 1e-6, beta_init = NULL, sigma_init = NULL)`}</code>
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
                      <code className="text-sm bg-muted px-2 py-1 rounded">weights</code>
                      <span className="text-sm">numeric vector</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Observation weights (default: NULL)</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">maxiter</code>
                      <span className="text-sm">integer</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Maximum number of iterations (default: 1000)</span>
                    </div>
                    <div className="grid grid-cols-4 gap-4 items-start">
                      <code className="text-sm bg-muted px-2 py-1 rounded">tol</code>
                      <span className="text-sm">numeric</span>
                      <Badge variant="secondary" className="w-fit">Optional</Badge>
                      <span className="text-sm">Convergence tolerance (default: 1e-6)</span>
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
                <CardTitle>Basic Usage</CardTitle>
                <CardDescription>
                  Example of using EM_BWQR_AL_MO for quantile regression
                </CardDescription>
              </CardHeader>
              <CardContent>
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                  <code>{`library(tauBayesW)

# Generate sample data
set.seed(123)
n <- 100
p <- 3
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1, -0.5, 0.8)
y <- X %*% beta_true + rnorm(n, 0, 0.5)

# Create weights (optional)
weights <- runif(n, 0.5, 1.5)

# Fit EM algorithm for median regression (tau = 0.5)
result <- EM_BWQR_AL_MO(
  y = y,
  X = X,
  tau = 0.5,
  weights = weights,
  maxiter = 1000,
  tol = 1e-6
)

# View results
print(result$beta)        # Estimated coefficients
print(result$sigma)       # Estimated scale parameter
print(result$iterations)  # Number of iterations
print(result$converged)   # Convergence status`}</code>
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
                    <span>Estimated regression coefficients</span>
                  </div>
                  <div className="flex items-start gap-3">
                    <code className="bg-muted px-2 py-1 rounded text-sm mt-1">sigma</code>
                    <span>Estimated scale parameter</span>
                  </div>
                  <div className="flex items-start gap-3">
                    <code className="bg-muted px-2 py-1 rounded text-sm mt-1">iterations</code>
                    <span>Number of iterations until convergence</span>
                  </div>
                  <div className="flex items-start gap-3">
                    <code className="bg-muted px-2 py-1 rounded text-sm mt-1">converged</code>
                    <span>Logical indicating whether algorithm converged</span>
                  </div>
                  <div className="flex items-start gap-3">
                    <code className="bg-muted px-2 py-1 rounded text-sm mt-1">loglik</code>
                    <span>Final log-likelihood value</span>
                  </div>
                </div>
              </CardContent>
            </Card>
          </section>

          {/* Related Functions */}
          <section>
            <h2 className="text-2xl font-bold mb-6">Related Functions</h2>
            <Card>
              <CardHeader>
                <CardTitle>Explore More</CardTitle>
                <CardDescription>
                  Other functions in the tauBayesW package that complement this analysis
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid md:grid-cols-2 gap-4">
                  <Link href="/functions/MCMC_BWQR_AL">
                    <Card className="hover:shadow-md transition-shadow cursor-pointer">
                      <CardContent className="p-4">
                        <h4 className="font-medium mb-1">MCMC_BWQR_AL()</h4>
                        <p className="text-sm text-muted-foreground">MCMC approach for Bayesian quantile regression</p>
                      </CardContent>
                    </Card>
                  </Link>
                  <Link href="/functions/bayesian-qreg">
                    <Card className="hover:shadow-md transition-shadow cursor-pointer">
                      <CardContent className="p-4">
                        <h4 className="font-medium mb-1">Bayesian QReg Overview</h4>
                        <p className="text-sm text-muted-foreground">Compare different Bayesian approaches</p>
                      </CardContent>
                    </Card>
                  </Link>
                  <Link href="/functions/robust-describe">
                    <Card className="hover:shadow-md transition-shadow cursor-pointer">
                      <CardContent className="p-4">
                        <h4 className="font-medium mb-1">robust_describe()</h4>
                        <p className="text-sm text-muted-foreground">Analyze data before modeling</p>
                      </CardContent>
                    </Card>
                  </Link>
                  <Link href="/functions/compare-models">
                    <Card className="hover:shadow-md transition-shadow cursor-pointer">
                      <CardContent className="p-4">
                        <h4 className="font-medium mb-1">Model Comparison</h4>
                        <p className="text-sm text-muted-foreground">Compare different modeling approaches</p>
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
