"use client"

import { useState } from "react"
import { ArrowLeft, Package, Copy, Check } from "lucide-react"
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

export default function AdvancedNormalityTestPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Basic usage
data <- rnorm(100)
result <- advanced_normality_test(data)
print(result)

# With custom parameters
result <- advanced_normality_test(
  x = data,
  alpha = 0.01,
  methods = c("shapiro", "anderson", "ks")
)

# All available methods
result <- advanced_normality_test(data, methods = "all")
summary(result)`

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
            <div className="h-12 w-12 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
              <Package className="h-6 w-6 text-green-600 dark:text-green-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">advanced_normality_test()</h1>
              <p className="text-xl text-muted-foreground">
                Comprehensive battery of normality tests
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Testing</Badge>
            <Badge variant="outline">Normality</Badge>
            <Badge variant="outline">Statistical Tests</Badge>
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
                The <code>advanced_normality_test()</code> function provides a comprehensive suite of 
                normality tests to assess whether your data follows a normal distribution. It includes 
                multiple statistical tests and provides detailed results with interpretation guidelines.
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
                  <code>{`advanced_normality_test(x, alpha = 0.05, methods = "all")`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("advanced_normality_test(x, alpha = 0.05, methods = \"all\")")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Available Methods */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Available Test Methods</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="space-y-3">
                  <div className="p-3 border rounded-lg">
                    <h4 className="font-medium text-sm">Shapiro-Wilk Test</h4>
                    <p className="text-xs text-muted-foreground">Most powerful test for small samples (&lt; 5000)</p>
                    <code className="text-xs bg-muted px-1 rounded">shapiro</code>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <h4 className="font-medium text-sm">Anderson-Darling Test</h4>
                    <p className="text-xs text-muted-foreground">Sensitive to deviations in the tails</p>
                    <code className="text-xs bg-muted px-1 rounded">anderson</code>
                  </div>
                </div>
                <div className="space-y-3">
                  <div className="p-3 border rounded-lg">
                    <h4 className="font-medium text-sm">Kolmogorov-Smirnov Test</h4>
                    <p className="text-xs text-muted-foreground">Good for larger sample sizes</p>
                    <code className="text-xs bg-muted px-1 rounded">ks</code>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <h4 className="font-medium text-sm">Jarque-Bera Test</h4>
                    <p className="text-xs text-muted-foreground">Based on skewness and kurtosis</p>
                    <code className="text-xs bg-muted px-1 rounded">jarque</code>
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
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">x</code>
                    <p className="text-sm text-muted-foreground">
                      Numeric vector containing the data to test for normality.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">alpha</code>
                    <p className="text-sm text-muted-foreground">
                      Significance level for the tests (default: 0.05).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">methods</code>
                    <p className="text-sm text-muted-foreground">
                      Vector of test methods to use, or "all" for all available tests.
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

        {/* Interpretation */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Interpretation Guidelines</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <div className="p-4 border-l-4 border-green-500 bg-green-50 dark:bg-green-950">
                  <h4 className="font-medium text-green-800 dark:text-green-200">P-value &gt; α (e.g., 0.05)</h4>
                  <p className="text-sm text-green-700 dark:text-green-300">
                    Fail to reject null hypothesis. Data appears to be normally distributed.
                  </p>
                </div>
                <div className="p-4 border-l-4 border-red-500 bg-red-50 dark:bg-red-950">
                  <h4 className="font-medium text-red-800 dark:text-red-200">P-value ≤ α (e.g., 0.05)</h4>
                  <p className="text-sm text-red-700 dark:text-red-300">
                    Reject null hypothesis. Data does not appear to be normally distributed.
                  </p>
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
                <Link href="/functions/robust-describe">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">robust_describe()</h4>
                      <p className="text-sm text-muted-foreground">Robust descriptive statistics</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/auto-plot">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">auto_plot()</h4>
                      <p className="text-sm text-muted-foreground">Visualize normality</p>
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
