"use client"

import { useState } from "react"
import { ArrowLeft, Code, Copy, Check } from "lucide-react"
import Link from "next/link"
import Image from "next/image"

import { Button } from "@/components/ui/button"
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import { Badge } from "@/components/ui/badge"
import { Separator } from "@/components/ui/separator"

export default function RobustDescribePage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const handleBack = () => {
    if (window.history.length > 1) {
      window.history.back()
    } else {
      window.location.href = '/'
    }
  }

  const exampleCode = `# Basic usage
data <- c(1, 2, 3, 4, 5, 100) # data with outlier
result <- robust_describe(data)
print(result)

# With custom parameters
result <- robust_describe(
  x = data,
  trim = 0.2,
  na.rm = TRUE,
  ci = 0.99
)

# View confidence intervals
result$confidence_interval`

  return (
    <div className="min-h-screen bg-background">
      {/* Header */}
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
        <div className="max-w-4xl mx-auto">{/* Contenido centrado pero responsive */}
        {/* Function Header */}
        <div className="mb-8">
          <div className="flex items-center gap-3 mb-4">
            <div className="h-12 w-12 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
              <Code className="h-6 w-6 text-blue-600 dark:text-blue-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">robust_describe()</h1>
              <p className="text-xl text-muted-foreground">
                Robust descriptive statistics for data with outliers
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Statistics</Badge>
            <Badge variant="outline">Robust</Badge>
            <Badge variant="outline">Outliers</Badge>
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
                The <code>robust_describe()</code> function provides comprehensive descriptive statistics 
                that are resistant to outliers. It calculates trimmed means, confidence intervals, and 
                provides outlier detection. This function is particularly useful when working with datasets 
                that may contain extreme values that could skew traditional statistical measures.
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
                  <code>{`robust_describe(x, trim = 0.1, na.rm = TRUE, ci = 0.95)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("robust_describe(x, trim = 0.1, na.rm = TRUE, ci = 0.95)")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
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
                      Numeric vector or data frame containing the data to be analyzed.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">trim</code>
                    <p className="text-sm text-muted-foreground">
                      Proportion of observations to be trimmed from each end (default: 0.1).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">na.rm</code>
                    <p className="text-sm text-muted-foreground">
                      Logical value indicating whether NA values should be removed (default: TRUE).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">ci</code>
                    <p className="text-sm text-muted-foreground">
                      Confidence level for interval estimation (default: 0.95).
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                </div>
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
                Returns a list containing the following elements:
              </p>
              <ul className="space-y-2">
                <li className="flex items-start gap-2">
                  <Badge variant="outline" className="text-xs mt-0.5">mean</Badge>
                  <span className="text-sm">Trimmed mean of the data</span>
                </li>
                <li className="flex items-start gap-2">
                  <Badge variant="outline" className="text-xs mt-0.5">median</Badge>
                  <span className="text-sm">Robust median value</span>
                </li>
                <li className="flex items-start gap-2">
                  <Badge variant="outline" className="text-xs mt-0.5">sd</Badge>
                  <span className="text-sm">Robust standard deviation</span>
                </li>
                <li className="flex items-start gap-2">
                  <Badge variant="outline" className="text-xs mt-0.5">ci</Badge>
                  <span className="text-sm">Confidence interval for the trimmed mean</span>
                </li>
                <li className="flex items-start gap-2">
                  <Badge variant="outline" className="text-xs mt-0.5">outliers</Badge>
                  <span className="text-sm">Detected outliers in the dataset</span>
                </li>
              </ul>
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

        {/* See Also */}
        <section>
          <Card>
            <CardHeader>
              <CardTitle>See Also</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <Link href="/functions/advanced-normality-test">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">advanced_normality_test()</h4>
                      <p className="text-sm text-muted-foreground">Test data normality</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/auto-plot">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">auto_plot()</h4>
                      <p className="text-sm text-muted-foreground">Visualize your data</p>
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
