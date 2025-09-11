"use client"

import { useState } from "react"
import { ArrowLeft, Copy, Download, Wrench, Database, BarChart } from "lucide-react"
import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Badge } from "@/components/ui/badge"
import { Alert, AlertDescription } from "@/components/ui/alert"

export default function UtilitiesPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const priorCode = `# Creating priors with tauBayesW
library(tauBayesW)

# Load artificial data
data_est <- artificial_data_est(n = 100)

## 1) Basic prior creation
# Default prior for bqr.svy (univariate model)
my_prior <- prior(type = "univariate")

# Default prior for mo.bqr.svy (multivariate model)  
my_mo_prior <- prior(type = "multivariate")

## 2) Informative priors
# For single quantile regression
info_prior <- prior(
  type = "univariate",
  p = 3,                        # Number of parameters (intercept + 2 covariates)
  beta_mean = c(0, 1, -0.5),    # Prior means for coefficients
  beta_cov = diag(c(1, 0.5, 0.5)), # Prior covariance matrix
  sigma_shape = 2,              # Shape parameter for sigma
  sigma_rate = 1,               # Rate parameter for sigma
  names = c("(Intercept)", "X1", "X2")
)

# For multiple quantile regression
info_mo_prior <- prior(
  type = "multivariate",
  p = 3,
  beta_mean = c(0, 1, -0.5),
  beta_cov = diag(c(1, 0.5, 0.5)),
  sigma_shape = 2,
  sigma_rate = 1,
  names = c("(Intercept)", "X1", "X2")
)

## 3) Using priors in models
# Single quantile model
fit <- bqr.svy(formula = Y ~ X1 + X2, 
               data = data_est, 
               prior = info_prior,
               tau = 0.5)

# Multiple quantile model
fit_mo <- mo.bqr.svy(formula = Y ~ X1 + X2, 
                     data = data_est, 
                     prior = info_mo_prior,
                     tau = c(0.25, 0.5, 0.75))

## 4) Print and summary functions
# Print the prior
print(info_prior)

# Print model results
print(fit)

# Get model summary
summary(fit)
summary(fit_mo)`

  return (
    <div className="min-h-screen bg-background">
      <div className="container mx-auto p-6">
        <Link href="/" className="inline-flex items-center mb-6 text-muted-foreground hover:text-foreground">
          <ArrowLeft className="mr-2 h-4 w-4" />
          Back to Documentation
        </Link>

        <div className="max-w-4xl mx-auto">
          <div className="mb-8">
            <h1 className="text-4xl font-bold mb-4">Utility Functions</h1>
            <p className="text-xl text-muted-foreground mb-4">
              Essential utilities for prior specification, printing, and summary methods
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">Prior Creation</Badge>
              <Badge variant="secondary">Print Methods</Badge>
              <Badge variant="secondary">Summary Methods</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="priors">Prior Creation</TabsTrigger>
              <TabsTrigger value="methods">Print & Summary</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Available Utility Functions</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The tauBayesW package provides essential utility functions for 
                    creating priors and examining model results.
                  </p>
                </CardContent>
              </Card>

              <div className="grid gap-6 md:grid-cols-3">
                <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                        <Wrench className="h-4 w-4 text-green-600 dark:text-green-400" />
                      </div>
                      <CardTitle className="text-lg">Prior Creation</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>prior()</code> - Unified prior interface</li>
                      <li>• Default and informative priors</li>
                      <li>• Univariate and multivariate types</li>
                      <li>• Easy parameter specification</li>
                    </ul>
                  </CardContent>
                </Card>

                <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <Database className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                      </div>
                      <CardTitle className="text-lg">Print Methods</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>print()</code> - Display model objects</li>
                      <li>• Prior information display</li>
                      <li>• Model fit results</li>
                      <li>• Clean formatted output</li>
                    </ul>
                  </CardContent>
                </Card>

                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <BarChart className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">Summary Methods</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <ul className="text-sm space-y-2">
                      <li>• <code>summary()</code> - Detailed model summaries</li>
                      <li>• Parameter estimates</li>
                      <li>• Convergence diagnostics</li>
                      <li>• Statistical inference</li>
                    </ul>
                  </CardContent>
                </Card>
              </div>
            </TabsContent>

            <TabsContent value="priors" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Prior Creation</CardTitle>
                  <CardDescription>
                    How to create and use priors in tauBayesW models
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="relative">
                    <pre className="bg-muted p-4 rounded-lg overflow-x-auto text-sm">
                      <code>{priorCode}</code>
                    </pre>
                    <Button
                      size="sm"
                      variant="outline"
                      className="absolute top-2 right-2"
                      onClick={() => copyToClipboard(priorCode)}
                    >
                      {copied ? "Copied!" : <Copy className="h-4 w-4" />}
                    </Button>
                  </div>
                </CardContent>
              </Card>

              <Alert>
                <AlertDescription>
                  The <code>prior()</code> function automatically creates the appropriate prior 
                  object based on the model type. Use <code>type = "univariate"</code> for bqr.svy 
                  models and <code>type = "multivariate"</code> for mo.bqr.svy models.
                </AlertDescription>
              </Alert>
            </TabsContent>

            <TabsContent value="methods" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Print and Summary Methods</CardTitle>
                  <CardDescription>
                    Display and examine model results
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-6">
                  <div className="space-y-4">
                    <div className="border-l-4 border-blue-500 pl-4">
                      <h4 className="font-semibold">print() Method</h4>
                      <p className="text-sm text-muted-foreground mt-1">
                        Displays basic information about priors and fitted models in a clean format.
                      </p>
                      <code className="text-sm bg-muted px-2 py-1 rounded mt-2 block">
                        print(fit)
                      </code>
                    </div>

                    <div className="border-l-4 border-green-500 pl-4">
                      <h4 className="font-semibold">summary() Method</h4>
                      <p className="text-sm text-muted-foreground mt-1">
                        Provides detailed statistical summaries including parameter estimates, 
                        credible intervals, and convergence diagnostics.
                      </p>
                      <code className="text-sm bg-muted px-2 py-1 rounded mt-2 block">
                        summary(fit)
                      </code>
                    </div>
                  </div>

                  <Alert>
                    <AlertDescription>
                      Both <code>print()</code> and <code>summary()</code> methods work with 
                      prior objects, bqr.svy models, and mo.bqr.svy models to provide 
                      appropriate output for each object type.
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
