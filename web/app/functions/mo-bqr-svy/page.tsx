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

  const exampleCode = `# Multiple quantile Bayesian regression with multidirectional estimation
library(tauBayesW)

# Simulate multivariate data
set.seed(123)
sim_data <- simulate_mo_bqr_data(n = 200, p = 2,
                                 beta_true = c(1, 2, -0.5), 
                                 seed = 123)

# Example 1: Joint estimation mode (default)
model_joint <- mo.bqr.svy(y ~ x1 + x2, 
                          data = sim_data$data,
                          weights = sim_data$weights,
                          quantile = c(0.1, 0.25, 0.5, 0.75, 0.9),
                          em_mode = "joint",
                          n_dir = 3,
                          max_iter = 100,
                          verbose = TRUE)

# Example 2: Separable estimation mode
model_sep <- mo.bqr.svy(y ~ x1 + x2, 
                        data = sim_data$data,
                        weights = sim_data$weights,
                        quantile = c(0.1, 0.5, 0.9),
                        em_mode = "separable",
                        n_dir = 2,
                        max_iter = 100)

# Example 3: With quantile-specific priors
# Define priors per quantile using a function
prior_fn <- function(tau, p, names) {
  # More concentrated priors for extreme quantiles
  variance <- ifelse(tau < 0.2 | tau > 0.8, 0.1, 1.0)
  mo_prior_default(p = p, 
                   beta_cov = diag(variance, p), 
                   names = names)
}

model_priors <- mo.bqr.svy(y ~ x1 + x2,
                           data = sim_data$data,
                           quantile = c(0.1, 0.5, 0.9),
                           prior = prior_fn,
                           em_mode = "joint")

# Print results
print(model_joint)
summary(model_joint)

# Plot multiple quantiles
plot(model_joint)`

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
              Multidirectional Bayesian Quantile Regression for Complex Surveys with Multiple Quantiles
            </p>
            <div className="flex gap-2 mb-4">
              <Badge variant="secondary">EM Algorithm</Badge>
              <Badge variant="secondary">Multiple Quantiles</Badge>
              <Badge variant="secondary">Multidirectional</Badge>
              <Badge variant="secondary">Survey Weights</Badge>
              <Badge variant="secondary">3D Visualization</Badge>
              <Badge variant="outline">Joint/Separable Modes</Badge>
            </div>
          </div>

          <Tabs defaultValue="overview" className="w-full">
            <TabsList className="grid w-full grid-cols-6">
              <TabsTrigger value="overview">Overview</TabsTrigger>
              <TabsTrigger value="usage">Usage</TabsTrigger>
              <TabsTrigger value="arguments">Arguments</TabsTrigger>
              <TabsTrigger value="examples">Examples</TabsTrigger>
              <TabsTrigger value="plotting">3D Plotting</TabsTrigger>
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
                    an Expectation-Maximization (EM) algorithm with multidirectional capabilities, 
                    accounting for complex survey designs with observation weights.
                  </p>
                  <p>
                    The function supports two estimation modes: <strong>joint</strong> estimation (all directions 
                    estimated simultaneously) and <strong>separable</strong> estimation (each direction estimated 
                    independently). This enables modeling of multivariate responses with configurable search 
                    directions and flexible prior specification per quantile and direction.
                  </p>
                  <p>
                    Multiple quantiles can be estimated efficiently in a single function call, with automatic 
                    convergence monitoring and comprehensive summary methods adapted for both single and 
                    multiple quantile scenarios.
                  </p>
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
           quantile = c(0.1, 0.25, 0.5, 0.75, 0.9), 
           algorithm = "em", prior = NULL, n_dir = 1,
           em_mode = c("joint", "separable"),
           epsilon = 1e-6, max_iter = 1000, verbose = FALSE, ...)`}</code>
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
                        <p className="text-sm text-muted-foreground">Vector of quantiles to estimate (0, 1)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">algorithm</h4>
                        <p className="text-sm text-muted-foreground">EM algorithm for multiple quantiles</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">prior</h4>
                        <p className="text-sm text-muted-foreground">Prior specification (mo_bqr_prior object, function, or list)</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">n_dir</h4>
                        <p className="text-sm text-muted-foreground">Number of search directions for multidirectional estimation</p>
                      </div>
                      <div>
                        <h4 className="font-semibold">em_mode</h4>
                        <p className="text-sm text-muted-foreground">"joint" (simultaneous) or "separable" (independent by direction)</p>
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

            <TabsContent value="plotting" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>3D Quantile Body Visualization</CardTitle>
                  <CardDescription>
                    Advanced 3D plotting for multivariate responses (d = 3)
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <p>
                    The <code>plot_quantile_body3d.mo.bqr.svy()</code> function creates interactive 3D visualizations 
                    of quantile bodies for models with 3-dimensional responses. This function draws the directional 
                    quantile body in ℝ³ for a fitted mo.bqr.svy model at a fixed covariate configuration.
                  </p>

                  <div className="bg-muted p-4 rounded-lg">
                    <h4 className="font-semibold mb-2">Example Usage:</h4>
                    <pre className="text-sm overflow-x-auto">
{`# Fit model with 3D response
fit3d <- mo.bqr.svy(cbind(y1, y2, y3) ~ x1 + x2, 
                    data = mydata,
                    quantile = 0.5, 
                    algorithm = "em", 
                    n_dir = 60, 
                    r = 0)

# Create interactive 3D plot
plot_quantile_body3d.mo.bqr.svy(
  fit3d,
  tau = 0.5,
  data = mydata,
  fixed_values = list(x1 = 0, x2 = 0),
  engine = "plotly",
  opacity = 0.6,
  show_points = TRUE,
  col = "#D1495B"
)`}
                    </pre>
                  </div>

                  <Alert>
                    <AlertDescription>
                      The 3D plotting function requires the response to be exactly 3-dimensional. 
                      For responses with different dimensions, use the standard 2D plotting functions.
                    </AlertDescription>
                  </Alert>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Function Arguments</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                    <div>
                      <h4 className="font-semibold">object</h4>
                      <p className="text-muted-foreground">Fitted mo.bqr.svy object with d=3</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">tau</h4>
                      <p className="text-muted-foreground">Quantile level to display (default: first)</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">data</h4>
                      <p className="text-muted-foreground">Original data for observed points overlay</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">fixed_values</h4>
                      <p className="text-muted-foreground">Named list of covariate values</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">engine</h4>
                      <p className="text-muted-foreground">"plotly" (interactive) or "rgl" (OpenGL)</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">opacity</h4>
                      <p className="text-muted-foreground">Mesh transparency (0-1)</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">show_points</h4>
                      <p className="text-muted-foreground">Display vertex points</p>
                    </div>
                    <div>
                      <h4 className="font-semibold">dirs</h4>
                      <p className="text-muted-foreground">Subset of directions to plot</p>
                    </div>
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
