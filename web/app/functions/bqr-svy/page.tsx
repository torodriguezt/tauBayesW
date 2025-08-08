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

  const exampleCode = `# Single quantile Bayesian regression
library(tauBayesW)

# Simulate data
set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- runif(n)
y <- 1 + 2*x1 - 0.5*x2 + rnorm(n)
weights <- runif(n, 0.5, 2)
data <- data.frame(y, x1, x2)

# Fit single quantile model
model <- bqr.svy(y ~ x1 + x2, 
                 data = data,
                 weights = weights,
                 quantile = 0.5,
                 method = "ALD",
                 n_mcmc = 5000,
                 burnin = 1000)

# Print results
print(model)

# Summary with detailed diagnostics
summary(model)

# Plot results
plot(model)`

  const summaryCode = `# Summary methods for bqr.svy objects

# Basic summary
summary(model)

# Print method with convergence diagnostics
print(model)

# Access specific components
coef(model)          # Coefficients
fitted(model)        # Fitted values
residuals(model)     # Residuals
logLik(model)        # Log-likelihood`

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
              Single Quantile Bayesian Regression for Complex Surveys
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">MCMC Methods</Badge>
              <Badge variant="secondary">Survey Weights</Badge>
              <Badge variant="secondary">Asymmetric Laplace</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-5">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="usage">Usage</TabsTrigger>
              <TabsTrigger value="arguments">Arguments</TabsTrigger>
              <TabsTrigger value="examples">Examples</TabsTrigger>
              <TabsTrigger value="summary">Summary Methods</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Description</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    Fits a Bayesian quantile regression model for a single quantile using MCMC methods,
                    accounting for complex survey designs with observation weights.
                  </p>
                  <p>
                    The function implements three MCMC approaches:
                  </p>
                  <ul className="list-disc list-inside space-y-2 ml-4">
                    <li><strong>ALD (Asymmetric Laplace Distribution):</strong> Uses asymmetric Laplace likelihood</li>
                    <li><strong>Score:</strong> Uses score-based approach for faster computation</li>
                    <li><strong>Approximate:</strong> Uses approximate methods for very large datasets</li>
                  </ul>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Key Features</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="list-disc list-inside space-y-2">
                    <li>Survey weights integration</li>
                    <li>Multiple MCMC algorithms</li>
                    <li>Convergence diagnostics</li>
                    <li>Robust to outliers</li>
                    <li>Fast C++ implementation</li>
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
                    <code>{`bqr.svy(formula, data, weights = NULL, quantile = 0.5, 
        method = c("ALD", "Score", "Approximate"), 
        n_mcmc = 10000, burnin = 2000, verbose = TRUE, ...)`}</code>
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
                        <p className="text-sm text-muted-foreground">Quantile to estimate (0 < tau < 1)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">method</h4>
                        <p className="text-sm text-muted-foreground">MCMC method: "ALD", "Score", or "Approximate"</p>
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

            <TabsContent value="summary" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Summary and Print Methods</CardTitle>
                  <CardDescription>
                    Available methods for examining model results and diagnostics
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{summaryCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(summaryCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                  
                  <Alert>
                    <AlertDescription>
                      The summary() method provides detailed convergence diagnostics including R-hat values, 
                      effective sample sizes, and posterior intervals for all parameters.
                    </AlertDescription>
                  </Alert>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  )
}
