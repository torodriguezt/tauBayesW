"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Copy, InfoIcon } from "lucide-react"

const exampleCode = `# Basic plotting examples with tauBayesW

# Load the package
library(tauBayesW)

# Create artificial data
data_est <- artificial_data_est(n = 100)

# Create prior
my_prior <- prior(type = "univariate")

# Fit a quantile regression model
fit <- bqr.svy(formula = Y ~ X1 + X2, 
               data = data_est, 
               prior = my_prior,
               tau = 0.5)

# Plot the fit
plot(fit, type = "fit")

# Plot density
plot(fit, type = "density")

# Plot trace
plot(fit, type = "trace")

# Multiple plot types
plot(fit, type = c("fit", "density"))
`

export default function PlotQuantilePage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = async (text: string) => {
    try {
      await navigator.clipboard.writeText(text)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (err) {
      console.error('Failed to copy text: ', err)
    }
  }

  return (
    <div className="container mx-auto py-6">
      <div className="space-y-6">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Plot Functions</h1>
          <p className="text-muted-foreground mt-2">
            Visualization functions for Bayesian quantile regression models in tauBayesW
          </p>
        </div>

        <div className="max-w-4xl">
          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="functions">Plot Types</TabsTrigger>
              <TabsTrigger value="examples">Examples</TabsTrigger>
            </TabsList>

            <TabsContent value="overview" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Plot Function Overview</CardTitle>
                  <CardDescription>
                    Essential plotting capabilities for tauBayesW models
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The <code>plot()</code> function provides essential visualization tools for 
                    Bayesian quantile regression models fitted with tauBayesW. It supports 
                    three main plot types to help you understand your model results.
                  </p>
                  
                  <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <div className="p-4 border rounded-lg">
                      <h4 className="font-semibold text-sm mb-2">Fit Plots</h4>
                      <p className="text-sm text-muted-foreground">
                        Visualize model fit and residuals
                      </p>
                    </div>
                    <div className="p-4 border rounded-lg">
                      <h4 className="font-semibold text-sm mb-2">Density Plots</h4>
                      <p className="text-sm text-muted-foreground">
                        Parameter posterior distributions
                      </p>
                    </div>
                    <div className="p-4 border rounded-lg">
                      <h4 className="font-semibold text-sm mb-2">Trace Plots</h4>
                      <p className="text-sm text-muted-foreground">
                        MCMC chain convergence diagnostics
                      </p>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="functions" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Available Plot Types</CardTitle>
                  <CardDescription>
                    The plot() function supports three essential plot types
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-6">
                  <div className="space-y-4">
                    <div className="border-l-4 border-blue-500 pl-4">
                      <h4 className="font-semibold">Fit Plots (type = "fit")</h4>
                      <p className="text-sm text-muted-foreground mt-1">
                        Shows the fitted quantile regression line along with the data points. 
                        Useful for assessing model fit and identifying outliers.
                      </p>
                      <code className="text-sm bg-muted px-2 py-1 rounded mt-2 block">
                        plot(model, type = "fit")
                      </code>
                    </div>

                    <div className="border-l-4 border-green-500 pl-4">
                      <h4 className="font-semibold">Density Plots (type = "density")</h4>
                      <p className="text-sm text-muted-foreground mt-1">
                        Displays the posterior density distributions of model parameters. 
                        Essential for understanding parameter uncertainty.
                      </p>
                      <code className="text-sm bg-muted px-2 py-1 rounded mt-2 block">
                        plot(model, type = "density")
                      </code>
                    </div>

                    <div className="border-l-4 border-orange-500 pl-4">
                      <h4 className="font-semibold">Trace Plots (type = "trace")</h4>
                      <p className="text-sm text-muted-foreground mt-1">
                        Shows MCMC chain traces for convergence diagnostics. 
                        Critical for validating the Bayesian sampling process.
                      </p>
                      <code className="text-sm bg-muted px-2 py-1 rounded mt-2 block">
                        plot(model, type = "trace")
                      </code>
                    </div>
                  </div>

                  <Alert>
                    <InfoIcon className="h-4 w-4" />
                    <AlertTitle>Multiple Plot Types</AlertTitle>
                    <AlertDescription>
                      You can combine plot types by passing a vector: 
                      <code className="ml-1">plot(model, type = c("fit", "density"))</code>
                    </AlertDescription>
                  </Alert>
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
                  
                  <Alert>
                    <InfoIcon className="h-4 w-4" />
                    <AlertTitle>Plot Usage</AlertTitle>
                    <AlertDescription>
                      Use these basic plot types to visualize your Bayesian quantile regression results. 
                      The fit plot shows your model against the data, density plots show parameter 
                      distributions, and trace plots help diagnose MCMC convergence.
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
