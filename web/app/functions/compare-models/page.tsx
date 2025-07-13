"use client"

import { useState } from "react"
import { ArrowLeft, Download, Copy, Check } from "lucide-react"
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

export default function CompareModelsPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Fit multiple models
x <- rnorm(100)
y <- 2 + 3*x + rnorm(100)

model1 <- bayesian_qreg(x, y, tau = 0.5, method = "em")
model2 <- bayesian_qreg(x, y, tau = 0.25, method = "em")
model3 <- bayesian_qreg(x, y, tau = 0.75, method = "mcmc")

# Compare using WAIC
models <- list(
  "Median" = model1,
  "Q25" = model2,
  "Q75" = model3
)

comparison <- compare_models(models, criterion = "WAIC")
print(comparison)

# Compare using DIC
comparison_dic <- compare_models(models, criterion = "DIC")

# Plot comparison
plot(comparison)
summary(comparison)`

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
      <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="container flex h-14 items-center">
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

      <div className="container py-8 max-w-4xl">
        {/* Function Header */}
        <div className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-red-100 dark:bg-red-900 flex items-center justify-center">
              <Download className="h-6 w-6 text-red-600 dark:text-red-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">compare_models()</h1>
              <p className="text-xl text-muted-foreground">
                Advanced model comparison using WAIC and DIC criteria
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Model Selection</Badge>
            <Badge variant="outline">WAIC</Badge>
            <Badge variant="outline">DIC</Badge>
            <Badge variant="outline">Bayesian</Badge>
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
                The <code>compare_models()</code> function provides comprehensive model comparison 
                for Bayesian quantile regression models using information criteria. It implements 
                both WAIC (Widely Applicable Information Criterion) and DIC (Deviance Information Criterion) 
                to help you select the best model for your data.
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
                  <code>{`compare_models(models, criterion = "WAIC", ...)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("compare_models(models, criterion = \"WAIC\", ...)")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Criteria Comparison */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Information Criteria Comparison</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-4">
                  <div className="p-4 border rounded-lg bg-blue-50 dark:bg-blue-950">
                    <h4 className="font-medium text-blue-800 dark:text-blue-200 mb-2">WAIC</h4>
                    <p className="text-sm text-blue-700 dark:text-blue-300 mb-3">
                      Widely Applicable Information Criterion
                    </p>
                    <ul className="text-sm text-blue-700 dark:text-blue-300 space-y-1">
                      <li>• More general and robust</li>
                      <li>• Works with complex posteriors</li>
                      <li>• Better for hierarchical models</li>
                      <li>• Asymptotically equivalent to LOO-CV</li>
                      <li>• Recommended for most cases</li>
                    </ul>
                  </div>
                </div>
                <div className="space-y-4">
                  <div className="p-4 border rounded-lg bg-green-50 dark:bg-green-950">
                    <h4 className="font-medium text-green-800 dark:text-green-200 mb-2">DIC</h4>
                    <p className="text-sm text-green-700 dark:text-green-300 mb-3">
                      Deviance Information Criterion
                    </p>
                    <ul className="text-sm text-green-700 dark:text-green-300 space-y-1">
                      <li>• Classical Bayesian criterion</li>
                      <li>• Based on deviance and complexity</li>
                      <li>• Computationally simpler</li>
                      <li>• Good for simple models</li>
                      <li>• May be unstable for some models</li>
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
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">models</code>
                    <p className="text-sm text-muted-foreground">
                      List of fitted bayesian_qreg models to compare. Each model should have a name.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">criterion</code>
                    <p className="text-sm text-muted-foreground">
                      Information criterion to use: "WAIC" (default) or "DIC".
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">sort</code>
                    <p className="text-sm text-muted-foreground">
                      Logical value indicating whether to sort results by criterion value.
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">delta</code>
                    <p className="text-sm text-muted-foreground">
                      Logical value indicating whether to calculate differences from best model.
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
                Returns an object of class "model_comparison" containing:
              </p>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">criterion</Badge>
                    <span className="text-sm">Information criterion values for each model</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">se</Badge>
                    <span className="text-sm">Standard errors of criterion values</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">delta</Badge>
                    <span className="text-sm">Differences from the best model</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">weights</Badge>
                    <span className="text-sm">Model weights (relative probabilities)</span>
                  </li>
                </ul>
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">best</Badge>
                    <span className="text-sm">Name of the best model</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">n_models</Badge>
                    <span className="text-sm">Number of models compared</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">criterion_used</Badge>
                    <span className="text-sm">Which criterion was used</span>
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

        {/* Interpretation */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Interpretation Guidelines</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div className="p-4 border-l-4 border-green-500 bg-green-50 dark:bg-green-950">
                  <h4 className="font-medium text-green-800 dark:text-green-200">Lower values are better</h4>
                  <p className="text-sm text-green-700 dark:text-green-300">
                    Both WAIC and DIC favor models with lower values. The model with the lowest 
                    criterion value is considered the best.
                  </p>
                </div>
                <div className="p-4 border-l-4 border-blue-500 bg-blue-50 dark:bg-blue-950">
                  <h4 className="font-medium text-blue-800 dark:text-blue-200">Model weights</h4>
                  <p className="text-sm text-blue-700 dark:text-blue-300">
                    Weights represent the relative probability that each model is the best. 
                    Higher weights indicate stronger support for the model.
                  </p>
                </div>
                <div className="p-4 border-l-4 border-orange-500 bg-orange-50 dark:bg-orange-950">
                  <h4 className="font-medium text-orange-800 dark:text-orange-200">Delta values</h4>
                  <p className="text-sm text-orange-700 dark:text-orange-300">
                    Differences ≤ 2 suggest substantial support; 4-7 indicate less support; 
                    &gt; 10 suggest essentially no support for the model.
                  </p>
                </div>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Available Methods */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Available Methods</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="space-y-2">
                  <h4 className="font-medium">S3 Methods</h4>
                  <ul className="text-sm space-y-1">
                    <li><code className="bg-muted px-1 rounded text-xs">print()</code> - Display results</li>
                    <li><code className="bg-muted px-1 rounded text-xs">summary()</code> - Detailed summary</li>
                    <li><code className="bg-muted px-1 rounded text-xs">plot()</code> - Visualization</li>
                  </ul>
                </div>
                <div className="space-y-2">
                  <h4 className="font-medium">Utility Functions</h4>
                  <ul className="text-sm space-y-1">
                    <li><code className="bg-muted px-1 rounded text-xs">best_model()</code> - Extract best model</li>
                    <li><code className="bg-muted px-1 rounded text-xs">model_weights()</code> - Get weights</li>
                    <li><code className="bg-muted px-1 rounded text-xs">delta_criterion()</code> - Get deltas</li>
                  </ul>
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
                <Link href="/functions/bayesian-qreg">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">bayesian_qreg()</h4>
                      <p className="text-sm text-muted-foreground">Fit models to compare</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/auto-plot">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">auto_plot()</h4>
                      <p className="text-sm text-muted-foreground">Visualize model results</p>
                    </CardContent>
                  </Card>
                </Link>
              </div>
            </CardContent>
          </Card>
        </section>
      </div>
    </div>
  )
}
