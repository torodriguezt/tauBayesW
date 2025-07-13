"use client"

import { useState } from "react"
import { ArrowLeft, BookOpen, Copy, Check } from "lucide-react"
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

export default function AutoPlotPage() {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const exampleCode = `# Basic usage
data <- rnorm(100)
auto_plot(data)

# Specify plot type
auto_plot(data, type = "histogram")
auto_plot(data, type = "boxplot")
auto_plot(data, type = "qq")

# Customize theme and save
auto_plot(
  x = data,
  type = "distribution",
  theme = "classic",
  save = TRUE,
  filename = "my_plot.png"
)`

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
            <div className="h-12 w-12 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
              <BookOpen className="h-6 w-6 text-purple-600 dark:text-purple-400" />
            </div>
            <div>
              <h1 className="text-3xl font-bold tracking-tight">auto_plot()</h1>
              <p className="text-xl text-muted-foreground">
                Smart automatic data visualization
              </p>
            </div>
          </div>
          <div className="flex gap-2">
            <Badge variant="secondary">Visualization</Badge>
            <Badge variant="outline">ggplot2</Badge>
            <Badge variant="outline">Automatic</Badge>
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
                The <code>auto_plot()</code> function provides intelligent, automatic data visualization 
                with minimal user input. It analyzes your data and creates appropriate plots with 
                professional styling using ggplot2. Perfect for exploratory data analysis and quick insights.
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
                  <code>{`auto_plot(x, type = "auto", theme = "minimal", save = FALSE, ...)`}</code>
                </pre>
                <Button
                  variant="ghost"
                  size="sm"
                  className="absolute top-2 right-2"
                  onClick={() => copyToClipboard("auto_plot(x, type = \"auto\", theme = \"minimal\", save = FALSE, ...)")}
                >
                  {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
                </Button>
              </div>
            </CardContent>
          </Card>
        </section>

        {/* Plot Types */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Available Plot Types</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="space-y-3">
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">auto</Badge>
                      <h4 className="font-medium text-sm">Automatic Selection</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      Intelligently chooses the best plot type based on data characteristics
                    </p>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">histogram</Badge>
                      <h4 className="font-medium text-sm">Histogram</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      Distribution visualization with optimal bin width
                    </p>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">boxplot</Badge>
                      <h4 className="font-medium text-sm">Box Plot</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      Summary statistics and outlier detection
                    </p>
                  </div>
                </div>
                <div className="space-y-3">
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">qq</Badge>
                      <h4 className="font-medium text-sm">Q-Q Plot</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      Normality assessment against theoretical quantiles
                    </p>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">density</Badge>
                      <h4 className="font-medium text-sm">Density Plot</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      Smooth density estimation curve
                    </p>
                  </div>
                  <div className="p-3 border rounded-lg">
                    <div className="flex items-center gap-2 mb-1">
                      <Badge variant="outline" className="text-xs">scatter</Badge>
                      <h4 className="font-medium text-sm">Scatter Plot</h4>
                    </div>
                    <p className="text-xs text-muted-foreground">
                      For bivariate data relationships
                    </p>
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
                      Numeric vector, data frame, or matrix to be plotted.
                    </p>
                    <Badge variant="destructive" className="text-xs">Required</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">type</code>
                    <p className="text-sm text-muted-foreground">
                      Plot type: "auto", "histogram", "boxplot", "qq", "density", "scatter".
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">theme</code>
                    <p className="text-sm text-muted-foreground">
                      ggplot2 theme: "minimal", "classic", "void", "dark".
                    </p>
                    <Badge variant="secondary" className="text-xs">Optional</Badge>
                  </div>
                  <div className="space-y-2">
                    <code className="bg-muted px-2 py-1 rounded text-sm font-medium">save</code>
                    <p className="text-sm text-muted-foreground">
                      Logical value indicating whether to save the plot.
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

        {/* Features */}
        <section className="mb-8">
          <Card>
            <CardHeader>
              <CardTitle>Key Features</CardTitle>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Smart</Badge>
                    <span className="text-sm">Automatic plot type selection</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Fast</Badge>
                    <span className="text-sm">Optimized rendering for large datasets</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Flexible</Badge>
                    <span className="text-sm">Multiple customization options</span>
                  </li>
                </ul>
                <ul className="space-y-2">
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Professional</Badge>
                    <span className="text-sm">Publication-ready styling</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Interactive</Badge>
                    <span className="text-sm">Works with plotly for interactivity</span>
                  </li>
                  <li className="flex items-start gap-2">
                    <Badge variant="outline" className="text-xs mt-0.5">Export</Badge>
                    <span className="text-sm">Multiple output formats supported</span>
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
                <Link href="/functions/robust-describe">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">robust_describe()</h4>
                      <p className="text-sm text-muted-foreground">Get statistics before plotting</p>
                    </CardContent>
                  </Card>
                </Link>
                <Link href="/functions/advanced-normality-test">
                  <Card className="hover:shadow-md transition-shadow cursor-pointer">
                    <CardContent className="p-4">
                      <h4 className="font-medium mb-1">advanced_normality_test()</h4>
                      <p className="text-sm text-muted-foreground">Test normality before Q-Q plots</p>
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
