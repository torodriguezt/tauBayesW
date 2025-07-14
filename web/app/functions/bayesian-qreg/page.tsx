"use client"

import { useState } from "react"
import { ArrowLeft, FileText, Copy, Check } from "lucide-react"
import Link from "next/link"

import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"

export default function BayesianQregPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Basic Bayesian quantile regression
x <- rnorm(100)
y <- 2 + 3*x + rnorm(100)

# Using EM algorithm (default)
model_em <- bayesian_qreg(x, y, tau = 0.5, method = "em")
summary(model_em)

# Using MCMC algorithm
model_mcmc <- bayesian_qreg(
  x = x, 
  y = y, 
  tau = 0.5, 
  method = "mcmc",
  iterations = 5000,
  burn_in = 1000
)

# Multiple quantiles
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
models <- lapply(quantiles, function(q) {
  bayesian_qreg(x, y, tau = q, method = "em")
})

# Plot results
plot(model_em)
plot(model_mcmc, type = "trace")`

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="container mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 xl:px-12 2xl:px-16 flex h-14 items-center">
          <Link href="/" className="flex items-center space-x-2 mr-6">
            <ArrowLeft className="h-4 w-4" />
            <span className="font-medium">Back to Documentation</span>
          </Link>
          <div className="flex items-center space-x-2">
            <div className="flex h-6 w-6 items-center justify-center rounded bg-blue-600 text-white text-xs font-bold">
              R
            </div>
            <span className="font-bold">tauBayesW</span>
          </div>
        </div>
      </header>

      <div className="container mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 xl:px-12 2xl:px-16 py-8">
        <div className="max-w-4xl mx-auto">{/* Contenido centrado pero responsive */}
        {/* Function Header */}
        <div className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-orange-100 dark:bg-orange-900 flex items-center justify-center">
              <FileText className="h-6 w-6 text-orange-600 dark:text-orange-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">bayesian_qreg()</h1>
              <p className="text-xl text-muted-foreground">
                Bayesian quantile regression via EM or MCMC
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Bayesian</Badge>
            <Badge variant="outline">Quantile Regression</Badge>
            <Badge variant="outline">MCMC</Badge>
            <Badge variant="outline">EM Algorithm</Badge>
          </div>
        </div>

        {/* Description */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Description</CardTitle>
            </CardHeader>
            <CardContent>
              <p className="text-muted-foreground leading-relaxed">
                The <code>bayesian_qreg()</code> function implements Bayesian quantile regression 
                using both Expectation-Maximization (EM) and Markov Chain Monte Carlo (MCMC) algorithms. 
                This approach provides robust regression analysis that is less sensitive to outliers 
                and allows for uncertainty quantification through posterior distributions.
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
                <pre className="bg-muted p-4 rounded-lg overflow-x-auto">
                  <code>{`bayesian_qreg(x, y, tau = 0.5, method = "em", ...)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("bayesian_qreg(x, y, tau = 0.5, method = \"em\", ...)")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Methods Comparison */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Algorithm Comparison</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-4">
                  <div className="p-4 border rounded-lg bg-blue-50 dark:bg-blue-950">
                    <h4 className="font-medium text-blue-800 dark:text-blue-200 mb-2">EM Algorithm</h4>
                    <ul className="text-sm text-blue-700 dark:text-blue-300 space-y-1">
                      <li>• Faster convergence</li>
                      <li>• Deterministic results</li>
                      <li>• Point estimates</li>
                      <li>• Good for large datasets</li>
                      <li>• Less computational intensive</li>
                    </ul>
                  </div>
                </div>
                <div className="space-y-4">
                  <div className="p-4 border rounded-lg bg-green-50 dark:bg-green-950">
                    <h4 className="font-medium text-green-800 dark:text-green-200 mb-2">MCMC Algorithm</h4>
                    <ul className="text-sm text-green-700 dark:text-green-300 space-y-1">
                      <li>• Full posterior distributions</li>
                      <li>• Uncertainty quantification</li>
                      <li>• Credible intervals</li>
                      <li>• More flexible priors</li>
                      <li>• Better for inference</li>
                    </ul>
                  </div>
                </div>
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
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">x</code>
                    <p className="text-sm text-muted-foreground">
                      Matrix or data frame of predictor variables.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">y</code>
                    <p className="text-sm text-muted-foreground">
                      Numeric vector of response variable.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">tau</code>
                    <p className="text-sm text-muted-foreground">
                      Quantile level between 0 and 1 (default: 0.5 for median).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">method</code>
                    <p className="text-sm text-muted-foreground">
                      Algorithm to use: "em" or "mcmc" (default: "em").
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Additional Parameters */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Method-Specific Parameters</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div>
                  <h4 className="font-medium mb-3">EM Algorithm</h4>
                  <div className="space-y-3">
                    <div className="text-sm">
                      <code className="bg-muted px-1 rounded text-xs">max_iter</code>
                      <span className="text-muted-foreground ml-2">Maximum iterations (default: 1000)</span>
                    </div>
                    <div className="text-sm">
                      <code className="bg-muted px-1 rounded text-xs">tolerance</code>
                      <span className="text-muted-foreground ml-2">Convergence tolerance (default: 1e-6)</span>
                    </div>
                  </div>
                </div>
                <div>
                  <h4 className="font-medium mb-3">MCMC Algorithm</h4>
                  <div className="space-y-3">
                    <div className="text-sm">
                      <code className="bg-muted px-1 rounded text-xs">iterations</code>
                      <span className="text-muted-foreground ml-2">MCMC iterations (default: 10000)</span>
                    </div>
                    <div className="text-sm">
                      <code className="bg-muted px-1 rounded text-xs">burn_in</code>
                      <span className="text-muted-foreground ml-2">Burn-in period (default: 2000)</span>
                    </div>
                    <div className="text-sm">
                      <code className="bg-muted px-1 rounded text-xs">thin</code>
                      <span className="text-muted-foreground ml-2">Thinning interval (default: 1)</span>
                    </div>
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
              <p className="text-muted-foreground mb-4">
                Returns an object of class "bayesian_qreg" containing:
              </p>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">coefficients</Badge>
                    <span className="text-sm">Estimated regression coefficients</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">fitted.values</Badge>
                    <span className="text-sm">Fitted quantile values</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">residuals</Badge>
                    <span className="text-sm">Model residuals</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">tau</Badge>
                    <span className="text-sm">Quantile level used</span>
                  </li>
                </ul>
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">method</Badge>
                    <span className="text-sm">Algorithm used for estimation</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">convergence</Badge>
                    <span className="text-sm">Convergence information</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">posterior</Badge>
                    <span className="text-sm">MCMC samples (if method="mcmc")</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">call</Badge>
                    <span className="text-sm">Original function call</span>
                  </li>
                </ul>
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
                      <h4 className="font-medium mb-1">compare_models()</h4>
                      <p className="text-sm text-muted-foreground">Compare different models</p>
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
              </div>
            </CardContent>
          </Card>
        </section>
        </div>
      </div>
    </div>
  )
}
