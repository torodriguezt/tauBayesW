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

export default function MoBqrSvyPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Multiple quantile Bayesian regression
library(tauBayesW)

# Simulate data
set.seed(123)
sim_data <- simulate_mo_bqr_data(n = 200, 
                                 beta = c(1, 2, -0.5), 
                                 seed = 123)

# Fit multiple quantile model
model <- mo.bqr.svy(y ~ x1 + x2, 
                    data = sim_data$data,
                    weights = sim_data$weights,
                    quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                    max_iter = 100,
                    tol = 1e-4,
                    verbose = TRUE)

# Print results
print(model)

# Summary with detailed results for all quantiles
summary(model)

# Plot multiple quantiles
plot(model)`

  const summaryCode = `# Summary methods for mo.bqr.svy objects

# Comprehensive summary for all quantiles
summary(model)

# Print method with convergence information
print(model)`

  return (
    <div className="min-h-screen bg-background">
      <div className="container mx-auto p-6">
        <Link href="/" className="inline-flex items-center mb-6 text-muted-foreground hover:text-foreground">
          <ArrowLeft className="mr-2 h-4 w-4" />
          Back to Documentation
        </Link>

        <div className="max-w-4xl mx-auto">
          <div className="mb-8">
            <h1 className="text-4xl font-bold mb-4">mo.bqr.svy()</h1>
            <p className="text-xl text-muted-foreground mb-4">
              Multiple Output Bayesian Quantile Regression for Complex Surveys
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">EM Algorithm</Badge>
              <Badge variant="secondary">Multiple Quantiles</Badge>
              <Badge variant="secondary">Survey Weights</Badge>
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
                    Fits Bayesian quantile regression models for multiple quantiles simultaneously using 
                    an Expectation-Maximization (EM) algorithm, accounting for complex survey designs 
                    with observation weights.
                  </p>
                  <p>
                    Although the quantiles are estimated individually, this function is efficient when you need to estimate several quantiles 
                    from the same model, as it leverages the EM algorithm to adjust all 
                    quantiles in a single run by passing multiple quantiles at once.
                  </p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Key Features</CardTitle>
                </CardHeader>
                <CardContent>
                  <ul className="list-disc list-inside space-y-2">
                    <li>Simultaneous estimation of multiple quantiles</li>
                    <li>EM algorithm for computational efficiency</li>
                    <li>Survey weights integration</li>
                    <li>Automatic convergence monitoring</li>
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
                    <code>{`mo.bqr.svy(formula, data, weights = NULL, 
           quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), algorithm = "em",
           prior = NULL, epsilon = 1e-6, max_iter = 1000, verbose = TRUE, ...)`}</code>
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
                        <h4 className="font-semibold">quantiles</h4>
                        <p className="text-sm text-muted-foreground">Vector of quantiles to estimate</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">algorithm</h4>
                        <p className="text-sm text-muted-foreground">Expected-Maximization algorithm</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">prior</h4>
                        <p className="text-sm text-muted-foreground">Object created using mo_prior_default specifiying prior distribution for parameters</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">max_iter</h4>
                        <p className="text-sm text-muted-foreground">Maximum number of EM iterations</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">epsilon</h4>
                        <p className="text-sm text-muted-foreground">Convergence tolerance</p>
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
                    Complete example with data simulation, model fitting, and analysis
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
                    Available methods for examining multiple quantile results
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
                      The summary() method provides convergence information for each quantile using the Vehtari et al. (2021) criteria, 
                      coefficient estimates, and diagnostic plots for the EM algorithm convergence.
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
