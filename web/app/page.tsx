"use client"

import { useState } from "react"
import Link from "next/link"
import Image from "next/image"
import {
  BookOpen,
  Code,
  Download,
  Package,
  FileText,
  Github,
  Menu,
  Search,
  X,
  User,
  GraduationCap,
  Award,
  Users,
} from "lucide-react"

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
import { ScrollArea } from "@/components/ui/scroll-area"
import { Input } from "@/components/ui/input"
import {
  Table,
  TableBody,
  TableCaption,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table"

export default function RDocumentation() {
  const [sidebarOpen, setSidebarOpen] = useState(false)

  const navigationItems = [
    { id: "introduction", title: "Introduction", icon: BookOpen },
    { id: "installation", title: "Installation", icon: Download },
    { id: "functions", title: "Functions", icon: Code },
    { id: "api", title: "Authors", icon: Package },
    { id: "comparison", title: "R vs C++ Comparison", icon: Users },
    { id: "help", title: "Help", icon: FileText },
  ]

  const scrollToSection = (id: string) => {
    document.getElementById(id)?.scrollIntoView({ behavior: "smooth" })
    setSidebarOpen(false)
  }

  return (
    <div className="min-h-screen bg-background">
      {/* ─────────────────────────── Header ─────────────────────────── */}
      <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
        <div className="container flex h-14 items-center">
          <div className="mr-4 hidden md:flex">
            <div className="mr-6 flex items-center space-x-2">
              <Image
                src="/logo_tau.png"
                alt="tauBayesW Logo"
                width={24}
                height={24}
                className="h-6 w-6"
                onError={(e) => {
                  // Fallback si no existe el logo
                  e.currentTarget.style.display = 'none';
                  const fallback = e.currentTarget.nextElementSibling as HTMLElement;
                  if (fallback) fallback.style.display = 'flex';
                }}
              />
              <div className="hidden h-6 w-6 items-center justify-center rounded bg-blue-600 text-white text-xs font-bold">
                R
              </div>
              <span className="hidden font-bold sm:inline-block">tauBayesW</span>
            </div>
          </div>

          {/* mobile hamburger */}
          <Button
            variant="ghost"
            className="mr-2 px-0 text-base hover:bg-transparent focus-visible:bg-transparent focus-visible:ring-0 focus-visible:ring-offset-0 md:hidden"
            onClick={() => setSidebarOpen(!sidebarOpen)}
          >
            <Menu className="h-5 w-5" />
            <span className="sr-only">Toggle Menu</span>
          </Button>

          {/* search + github on the right */}
          <div className="flex flex-1 items-center justify-between space-x-2 md:justify-end">
            <div className="w-full flex-1 md:w-auto md:flex-none">
              <div className="relative">
                <Search className="absolute left-2.5 top-2.5 h-4 w-4 text-muted-foreground" />
                <Input
                  type="search"
                  placeholder="Search..."
                  className="pl-8 md:w-[300px] lg:w-[400px] ultrawide:w-[500px] superwide:w-[600px]"
                />
              </div>
            </div>
            <nav className="flex items-center space-x-1">
              <Button variant="ghost" size="sm">
                <Github className="h-4 w-4" />
                <span className="sr-only">GitHub</span>
              </Button>
            </nav>
          </div>
        </div>
      </header>

      <div className="container flex-1 items-start md:grid md:grid-cols-[220px_minmax(0,1fr)] md:gap-6 lg:grid-cols-[240px_minmax(0,1fr)] lg:gap-10 ultrawide:grid-cols-[280px_minmax(0,1fr)] ultrawide:gap-12 superwide:max-w-none superwide:px-16">
        {/* ─────────────────────── Sidebar (desktop) ─────────────────────── */}
        <aside
          className={`fixed top-14 z-30 -ml-2 hidden h-[calc(100vh-3.5rem)] w-full shrink-0 md:sticky md:block ultrawide:w-[280px] superwide:w-[320px] ${
            sidebarOpen ? "block" : "hidden"
          }`}
        >
          <ScrollArea className="h-full py-6 pr-6 lg:py-8">
            <nav className="grid items-start gap-2">
              {navigationItems.map((item) => (
                <button
                  key={item.id}
                  onClick={() => scrollToSection(item.id)}
                  className="group flex w-full items-center rounded-md border border-transparent px-2 py-1 text-sm font-medium text-left hover:bg-accent hover:text-accent-foreground"
                >
                  <item.icon className="mr-2 h-4 w-4" />
                  {item.title}
                </button>
              ))}
            </nav>
          </ScrollArea>
        </aside>

        {/* ──────────────────── Sidebar overlay (mobile) ──────────────────── */}
        {sidebarOpen && (
          <div className="fixed inset-0 top-14 z-50 grid h-[calc(100vh-3.5rem)] w-full pl-[max(0px,calc(50%-15rem))] md:hidden">
            <div className="relative z-20 h-full bg-background p-6 shadow-md">
              <div className="flex items-center justify-between">
                <span className="font-semibold">Navigation</span>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={() => setSidebarOpen(false)}
                >
                  <X className="h-4 w-4" />
                </Button>
              </div>
              <nav className="grid items-start gap-2 mt-4">
                {navigationItems.map((item) => (
                  <button
                    key={item.id}
                    onClick={() => scrollToSection(item.id)}
                    className="group flex w-full items-center rounded-md border border-transparent px-2 py-1 text-sm font-medium text-left hover:bg-accent hover:text-accent-foreground"
                  >
                    <item.icon className="mr-2 h-4 w-4" />
                    {item.title}
                  </button>
                ))}
              </nav>
            </div>
            <div
              className="fixed inset-0 top-14 bg-background/80 backdrop-blur-sm"
              onClick={() => setSidebarOpen(false)}
            />
          </div>
        )}

        {/* ────────────────────────── Main content ───────────────────────── */}
        <main className="relative py-6 lg:gap-10 lg:py-8 xl:grid xl:grid-cols-[1fr_300px]">
          <div className="mx-auto w-full min-w-0">
            {/* 1 ──────────────────────── Introduction ──────────────────────── */}
            <section id="introduction" className="mb-16">
              <div className="text-center mb-8">
                <div className="flex items-center justify-center gap-3 mb-4">
                  <Image
                    src="/logo_tau.png"
                    alt="tauBayesW Logo"
                    width={48}
                    height={48}
                    className="h-12 w-12"
                    onError={(e) => {
                      // Fallback si no existe el logo
                      e.currentTarget.style.display = 'none';
                      const fallback = e.currentTarget.nextElementSibling as HTMLElement;
                      if (fallback) fallback.style.display = 'flex';
                    }}
                  />
                  <div className="hidden h-12 w-12 items-center justify-center rounded-xl bg-gradient-to-br from-blue-600 to-blue-700 text-white text-lg font-bold shadow-lg">
                    R
                  </div>
                  <h1 className="text-4xl font-bold tracking-tight bg-gradient-to-r from-blue-600 to-purple-600 bg-clip-text text-transparent">
                    tauBayesW
                  </h1>
                  <Badge variant="secondary" className="text-sm px-3 py-1">v1.0.0</Badge>
                </div>
                <p className="text-xl text-muted-foreground mb-2 max-w-3xl mx-auto leading-relaxed">
                  An R package for implementing MCMC and EM algorithms for the
                  Bayesian weighted quantile regression.
                </p>
                <p className="text-sm text-muted-foreground max-w-2xl mx-auto">
                  Robust statistical modeling with advanced computational methods for research and industry applications.
                </p>
              </div>

              <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3 mb-8">
                <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300 hover:shadow-lg">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <Package className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                      </div>
                      <CardTitle className="text-base">
                        Bayesian Quantile Regression with Weights
                      </CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      Implements robust statistical methods for datasets with
                      outliers and heteroscedastic errors.
                    </p>
                  </CardContent>
                </Card>
                <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300 hover:shadow-lg">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                        <Code className="h-4 w-4 text-green-600 dark:text-green-400" />
                      </div>
                      <CardTitle className="text-base">
                        MCMC and EM Algorithms
                      </CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      Advanced algorithms with convergence diagnostics and
                      visualization tools for model validation.
                    </p>
                  </CardContent>
                </Card>
                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300 hover:shadow-lg">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <Award className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-base">High Performance</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      Optimized C++ backend for fast execution on large datasets
                      with memory-efficient algorithms.
                    </p>
                  </CardContent>
                </Card>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 2 ──────────────────────── Installation ──────────────────────── */}
            <section id="installation" className="mb-16">
              <div className="text-center mb-8">
                <h2 className="text-3xl font-bold tracking-tight mb-4">
                  Installation
                </h2>
                <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
                  Install <code className="bg-muted px-2 py-1 rounded text-sm">tauBayesW</code> from GitHub to obtain the latest
                  stable version with all features and documentation.
                </p>
              </div>

              <div className="max-w-4xl mx-auto">
                <div className="grid gap-8 md:grid-cols-2">
                  {/* Installation from GitHub */}
                  <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                    <CardHeader className="pb-4">
                      <div className="flex items-center gap-3 mb-2">
                        <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                          <Github className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                        </div>
                        <CardTitle className="text-lg">From GitHub</CardTitle>
                      </div>
                      <CardDescription>
                        Latest development version with newest features
                      </CardDescription>
                    </CardHeader>
                    <CardContent>
                      <pre className="text-sm bg-slate-900 dark:bg-slate-800 text-green-400 p-4 rounded-lg overflow-x-auto border">
{`# Install devtools if needed
install.packages("devtools")

# Install from GitHub
devtools::install_github("torodriguezt/tauBayesW")

# Load the package
library(tauBayesW)`}
                      </pre>
                    </CardContent>
                  </Card>

                  {/* System Requirements */}
                  <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300">
                    <CardHeader className="pb-4">
                      <div className="flex items-center gap-3 mb-2">
                        <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                          <Package className="h-4 w-4 text-green-600 dark:text-green-400" />
                        </div>
                        <CardTitle className="text-lg">Requirements</CardTitle>
                      </div>
                      <CardDescription>
                        System dependencies and R version requirements
                      </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                      <div>
                        <h4 className="font-semibold text-sm mb-2">R Version</h4>
                        <p className="text-sm text-muted-foreground">R ≥ 4.0.0</p>
                      </div>
                      <div>
                        <h4 className="font-semibold text-sm mb-2">Dependencies</h4>
                        <ul className="text-sm text-muted-foreground space-y-1">
                          <li>• Rcpp (≥ 1.0.0)</li>
                          <li>• RcppEigen</li>
                          <li>• ggplot2</li>
                          <li>• coda</li>
                        </ul>
                      </div>
                      <div>
                        <h4 className="font-semibold text-sm mb-2">Compiler</h4>
                        <p className="text-sm text-muted-foreground">C++11 compatible compiler</p>
                      </div>
                    </CardContent>
                  </Card>
                </div>

                {/* Quick start note */}
                <Card className="mt-8 bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-900/20 dark:to-indigo-900/20 border-blue-200 dark:border-blue-800">
                  <CardContent className="p-6">
                    <div className="flex items-start gap-3">
                      <div className="h-6 w-6 rounded-full bg-blue-600 flex items-center justify-center flex-shrink-0 mt-0.5">
                        <span className="text-white text-xs font-bold">i</span>
                      </div>
                      <div>
                        <h4 className="font-semibold text-sm mb-1">Quick Start</h4>
                        <p className="text-sm text-muted-foreground">
                          After installation, check out our function examples below or visit the 
                          <span className="font-medium"> Help</span> section for detailed documentation and tutorials.
                        </p>
                      </div>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 3 ───────────────────────── Functions ───────────────────────── */}
            <section id="functions" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">
                Main Functions
              </h2>
              <p className="text-muted-foreground mb-6">
                Complete reference of the most important functions in
                <code> tauBayesW</code>.
              </p>

              {/* Layout estético: 2-2-1 con cajas más grandes y clickeables */}
              <div className="space-y-8">
                {/* Fila superior: 2 cajas */}
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 max-w-5xl mx-auto">
                  {/* Card 1 - Clickeable */}
                  <Link href="/functions/robust-describe">
                    <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-blue-300 border-2">
                      <CardHeader className="pb-6 pt-8">
                        <div className="flex items-center gap-3 mb-3">
                          <div className="h-10 w-10 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                            <Code className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                          </div>
                          <CardTitle className="text-xl font-bold group-hover:text-blue-600 transition-colors">
                            EM_BWQR_AL_MO()
                          </CardTitle>
                        </div>
                        <CardDescription className="text-base leading-relaxed">
                          Robust descriptive statistics for data with outliers. Provides trimmed means, confidence intervals, and outlier detection.
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="pt-0">
                        <div className="flex items-center text-sm text-muted-foreground group-hover:text-blue-600 transition-colors">
                          <span>Click to view details</span>
                          <span className="ml-auto">→</span>
                        </div>
                      </CardContent>
                    </Card>
                  </Link>

                  {/* Card 2 - Clickeable */}
                  <Link href="/functions/advanced-normality-test">
                    <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-green-300 border-2">
                      <CardHeader className="pb-6 pt-8">
                        <div className="flex items-center gap-3 mb-3">
                          <div className="h-10 w-10 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                            <Package className="h-5 w-5 text-green-600 dark:text-green-400" />
                          </div>
                          <CardTitle className="text-xl font-bold group-hover:text-green-600 transition-colors">
                            MCMC_BWQR_AL()
                          </CardTitle>
                        </div>
                        <CardDescription className="text-base leading-relaxed">
                          Comprehensive battery of normality tests including Shapiro-Wilk, Anderson-Darling, and Kolmogorov-Smirnov tests.
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="pt-0">
                        <div className="flex items-center text-sm text-muted-foreground group-hover:text-green-600 transition-colors">
                          <span>Click to view details</span>
                          <span className="ml-auto">→</span>
                        </div>
                      </CardContent>
                    </Card>
                  </Link>
                </div>

                {/* Fila inferior: 2 cajas */}
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 max-w-5xl mx-auto">
                  {/* Card 3 - Clickeable */}
                  <Link href="/functions/auto-plot">
                    <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-purple-300 border-2">
                      <CardHeader className="pb-6 pt-8">
                        <div className="flex items-center gap-3 mb-3">
                          <div className="h-10 w-10 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                            <BookOpen className="h-5 w-5 text-purple-600 dark:text-purple-400" />
                          </div>
                          <CardTitle className="text-xl font-bold group-hover:text-purple-600 transition-colors">
                            MCMC_BWQR_AP()
                          </CardTitle>
                        </div>
                        <CardDescription className="text-base leading-relaxed">
                          Smart automatic data visualization with multiple plot types: distributions, boxplots, Q-Q plots, and histograms.
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="pt-0">
                        <div className="flex items-center text-sm text-muted-foreground group-hover:text-purple-600 transition-colors">
                          <span>Click to view details</span>
                          <span className="ml-auto">→</span>
                        </div>
                      </CardContent>
                    </Card>
                  </Link>

                  {/* Card 4 - Clickeable */}
                  <Link href="/functions/bayesian-qreg">
                    <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-orange-300 border-2">
                      <CardHeader className="pb-6 pt-8">
                        <div className="flex items-center gap-3 mb-3">
                          <div className="h-10 w-10 rounded-lg bg-orange-100 dark:bg-orange-900 flex items-center justify-center">
                            <FileText className="h-5 w-5 text-orange-600 dark:text-orange-400" />
                          </div>
                          <CardTitle className="text-xl font-bold group-hover:text-orange-600 transition-colors">
                            MCMC_BWQR_SL()
                          </CardTitle>
                        </div>
                        <CardDescription className="text-base leading-relaxed">
                          Bayesian quantile regression using advanced EM and MCMC algorithms for robust statistical modeling.
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="pt-0">
                        <div className="flex items-center text-sm text-muted-foreground group-hover:text-orange-600 transition-colors">
                          <span>Click to view details</span>
                          <span className="ml-auto">→</span>
                        </div>
                      </CardContent>
                    </Card>
                  </Link>
                </div>

                {/* Caja centrada: Card 5 - Más destacada */}
                <div className="flex justify-center">
                  <div className="w-full max-w-lg">
                    <Link href="/functions/compare-models">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-xl hover:scale-[1.03] hover:border-red-300 border-2 bg-gradient-to-br from-background to-muted/30">
                        <CardHeader className="pb-6 pt-8 text-center">
                          <div className="flex justify-center mb-4">
                            <div className="h-12 w-12 rounded-xl bg-red-100 dark:bg-red-900 flex items-center justify-center">
                              <Download className="h-6 w-6 text-red-600 dark:text-red-400" />
                            </div>
                          </div>
                          <CardTitle className="text-2xl font-bold group-hover:text-red-600 transition-colors">
                            NonCrossingBWQR_AL()
                          </CardTitle>
                          <CardDescription className="text-base leading-relaxed mt-3">
                            Advanced model comparison using WAIC and DIC criteria for optimal model selection in Bayesian analysis.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0 text-center">
                          <div className="flex items-center justify-center text-sm text-muted-foreground group-hover:text-red-600 transition-colors">
                            <span>Click to view details</span>
                            <span className="ml-2">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>
                  </div>
                </div>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 4 ────────────────────── R vs C++ Comparison ─────────────────── */}
            <section id="comparison" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">
                Comparison between R code and C++ code
              </h2>
              <p className="text-muted-foreground mb-6">
                Performance comparison between R and C++ implementations of the main algorithms in <code>tauBayesW</code>.
              </p>

              <Card>
                <CardHeader>
                  <CardTitle className="text-lg">Performance Benchmarks</CardTitle>
                  <CardDescription>
                    Execution time and memory usage comparison across different dataset sizes
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="overflow-x-auto">
                    <Table>
                      <TableCaption>
                        Performance metrics measured on Intel i7-8700K @ 3.70GHz with 16GB RAM
                      </TableCaption>
                      <TableHeader>
                        <TableRow>
                          <TableHead className="w-[150px]">Algorithm</TableHead>
                          <TableHead>Dataset Size</TableHead>
                          <TableHead>R Time (sec)</TableHead>
                          <TableHead>C++ Time (sec)</TableHead>
                          <TableHead>Speedup</TableHead>
                          <TableHead>R Memory (MB)</TableHead>
                          <TableHead>C++ Memory (MB)</TableHead>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        <TableRow>
                          <TableCell className="font-medium">EM_BWQR_AL</TableCell>
                          <TableCell>n=1,000</TableCell>
                          <TableCell>12.5</TableCell>
                          <TableCell>2.1</TableCell>
                          <TableCell className="text-green-600 font-semibold">5.9x</TableCell>
                          <TableCell>45.2</TableCell>
                          <TableCell>18.7</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_AL</TableCell>
                          <TableCell>n=5,000</TableCell>
                          <TableCell>89.3</TableCell>
                          <TableCell>14.6</TableCell>
                          <TableCell className="text-green-600 font-semibold">6.1x</TableCell>
                          <TableCell>152.8</TableCell>
                          <TableCell>67.4</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_AP</TableCell>
                          <TableCell>n=10,000</TableCell>
                          <TableCell>245.7</TableCell>
                          <TableCell>35.2</TableCell>
                          <TableCell className="text-green-600 font-semibold">7.0x</TableCell>
                          <TableCell>298.5</TableCell>
                          <TableCell>124.3</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_SL</TableCell>
                          <TableCell>n=25,000</TableCell>
                          <TableCell>567.8</TableCell>
                          <TableCell>78.9</TableCell>
                          <TableCell className="text-green-600 font-semibold">7.2x</TableCell>
                          <TableCell>742.1</TableCell>
                          <TableCell>289.6</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">NonCrossingBWQR_AL</TableCell>
                          <TableCell>n=50,000</TableCell>
                          <TableCell>1,234.5</TableCell>
                          <TableCell>156.7</TableCell>
                          <TableCell className="text-green-600 font-semibold">7.9x</TableCell>
                          <TableCell>1,456.3</TableCell>
                          <TableCell>512.8</TableCell>
                        </TableRow>
                      </TableBody>
                    </Table>
                  </div>
                  
                  <div className="mt-8 grid gap-6 md:grid-cols-2">
                    <Card className="text-center p-6 border-green-200 dark:border-green-800 bg-gradient-to-b from-green-50 to-white dark:from-green-900/20 dark:to-background">
                      <div className="text-3xl font-bold text-green-600 dark:text-green-400 mb-2">
                        6.8x
                      </div>
                      <div className="text-sm font-medium text-muted-foreground">
                        Average Speedup
                      </div>
                      <div className="text-xs text-muted-foreground mt-1">
                        Across all algorithms
                      </div>
                    </Card>
                    
                    <Card className="text-center p-6 border-blue-200 dark:border-blue-800 bg-gradient-to-b from-blue-50 to-white dark:from-blue-900/20 dark:to-background">
                      <div className="text-3xl font-bold text-blue-600 dark:text-blue-400 mb-2">
                        62%
                      </div>
                      <div className="text-sm font-medium text-muted-foreground">
                        Memory Reduction
                      </div>
                      <div className="text-xs text-muted-foreground mt-1">
                        Lower RAM usage
                      </div>
                    </Card>
                  </div>

                  <Card className="mt-8 border-slate-200 dark:border-slate-700 bg-gradient-to-r from-slate-50 to-gray-50 dark:from-slate-900/50 dark:to-gray-900/50">
                    <CardContent className="p-6">
                      <h4 className="font-semibold mb-4 text-base flex items-center gap-2">
                        <Code className="h-4 w-4 text-blue-600" />
                        Key Benefits of C++ Implementation
                      </h4>
                      <div className="grid gap-3 md:grid-cols-2">
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-green-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">Faster execution:</strong>
                            <span className="text-sm text-muted-foreground ml-1">6-8x speed improvement for large datasets</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-blue-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">Lower memory usage:</strong>
                            <span className="text-sm text-muted-foreground ml-1">Efficient memory management reduces overhead</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-purple-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">Better optimization:</strong>
                            <span className="text-sm text-muted-foreground ml-1">Optimized algorithms for enhanced performance</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-orange-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">Scalability:</strong>
                            <span className="text-sm text-muted-foreground ml-1">Performance gains increase with dataset size</span>
                          </div>
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                </CardContent>
              </Card>
            </section>

            <Separator className="mb-12" />

            {/* 5 ────────────────────────── Authors ────────────────────────── */}
            <section id="api" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">Authors</h2>
              <p className="text-muted-foreground mb-6">
                Meet the team behind the development of <code>tauBayesW</code>.
              </p>

              <div className="grid gap-6 md:grid-cols-2 xl:grid-cols-4">
                {/* Autor 1 - Lead Developer */}
                <Card className="hover:shadow-lg transition-all duration-300 border-2 hover:border-blue-200 dark:hover:border-blue-800 flex flex-col h-full">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-3">
                      <div className="h-12 w-12 rounded-full bg-gradient-to-br from-blue-500 to-blue-600 flex items-center justify-center shadow-md">
                        <User className="h-6 w-6 text-white" />
                      </div>
                      <div>
                        <CardTitle className="text-lg">Marcus L. Nascimento</CardTitle>
                        <CardDescription className="text-sm font-medium">Fundação Getulio Vargas</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent className="flex-1 flex flex-col">
                    <p className="text-sm text-muted-foreground mb-4 leading-relaxed flex-1">
                      Postdoctoral Researcher at the School of Applied Mathematics at Fundação Getulio Vargas (FGV EMAp) and a Research Affiliate at the José Luiz Setúbal Foundation (FJLES).
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">Email:</span>
                        <a
                          href="mailto:marcus@example.com"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          marcus.lavagnole@fgv.br
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">GitHub:</span>
                        <a
                          href="https://github.com/marcuslavagnole"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          @marcuslavagnole
                        </a>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 2 - Professor */}
                <Card className="hover:shadow-lg transition-all duration-300 border-2 hover:border-green-200 dark:hover:border-green-800 flex flex-col h-full">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-3">
                      <div className="h-12 w-12 rounded-full bg-gradient-to-br from-green-500 to-green-600 flex items-center justify-center shadow-md">
                        <User className="h-6 w-6 text-white" />
                      </div>
                      <div>
                        <CardTitle className="text-lg">Prof. Kelly Cristina Mota Gonçalves</CardTitle>
                        <CardDescription className="text-sm font-medium">Federal University of Rio de Janeiro</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent className="flex-1 flex flex-col">
                    <p className="text-sm text-muted-foreground mb-4 leading-relaxed flex-1">
                      Associate Professor in the  Department of Statistics at the Federal University of Rio de Janeiro (UFRJ) in Brazil.
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">Email:</span>
                        <a
                          href="mailto:kelly@example.com"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          kelly@dme.ufrj.br
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">Web Page:</span>
                        <a
                          href="https://sites.google.com/dme.ufrj.br/kelly/home?authuser=0"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          Google Sites
                        </a>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 3 - Professor */}
                <Card className="hover:shadow-lg transition-all duration-300 border-2 hover:border-purple-200 dark:hover:border-purple-800 flex flex-col h-full">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-3">
                      <div className="h-12 w-12 rounded-full bg-gradient-to-br from-purple-500 to-purple-600 flex items-center justify-center shadow-md">
                        <User className="h-6 w-6 text-white" />
                      </div>
                      <div>
                        <CardTitle className="text-lg">Prof. Johntan Cardona Jimenez</CardTitle>
                        <CardDescription className="text-sm font-medium">Universidad Nacional de Colombia</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent className="flex-1 flex flex-col">
                    <p className="text-sm text-muted-foreground mb-4 leading-relaxed flex-1">
                      Assistant Professor in the  Department of Statistics at Universidad Nacional de Colombia (UNAL) in Medellin.
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">Email:</span>
                        <a
                          href="mailto:johntan@example.com"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          jcardonj@unal.edu.co
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">GitHub:</span>
                        <a
                          href="https://github.com/JohnatanLAB"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          @JohnatanLAB
                        </a>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 4 - Tomas */}
                <Card className="hover:shadow-lg transition-all duration-300 border-2 hover:border-orange-200 dark:hover:border-orange-800 flex flex-col h-full">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-3">
                      <div className="h-12 w-12 rounded-full bg-gradient-to-br from-orange-500 to-orange-600 flex items-center justify-center shadow-md">
                        <User className="h-6 w-6 text-white" />
                      </div>
                      <div>
                        <CardTitle className="text-lg">Tomas Rodriguez Taborda</CardTitle>
                        <CardDescription className="text-sm font-medium">Universidad Nacional de Colombia</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent className="flex-1 flex flex-col">
                    <p className="text-sm text-muted-foreground mb-4 leading-relaxed flex-1">
                      Undergraduate student in the Statistics and Informatic Engineering programs at Universidad Nacional de Colombia (UNAL) in Medellin.
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">Email:</span>
                        <a
                          href="mailto:torodriguezt@unal.edu.co"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          torodriguezt@unal.edu.co
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">GitHub:</span>
                        <a
                          href="https://github.com/torodriguezt"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          @torodriguezt
                        </a>
                      </div>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 6 ─────────────────────────── Help ─────────────────────────── */}
            <section id="help" className="mb-16">
              <div className="text-center mb-8">
                <h2 className="text-3xl font-bold tracking-tight mb-4">Help & Resources</h2>
                <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
                  Documentation, support resources, and community links for <code className="bg-muted px-2 py-1 rounded text-sm">tauBayesW</code>.
                </p>
              </div>

              <div className="grid gap-8 md:grid-cols-2 lg:grid-cols-3">
                {/* R Console Commands */}
                <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <Code className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                      </div>
                      <CardTitle className="text-lg">R Console Commands</CardTitle>
                    </div>
                    <CardDescription>Quick access to function documentation</CardDescription>
                  </CardHeader>
                  <CardContent>
                    <pre className="text-sm bg-slate-900 dark:bg-slate-800 text-green-400 p-4 rounded-lg overflow-x-auto border">
{`# Function help
?EM_BWQR_AL_MO

# Package help
help(package="tauBayesW")

# Available vignettes
vignette("tauBayesW")

# Function examples
example(MCMC_BWQR_AL)`}
                    </pre>
                  </CardContent>
                </Card>

                {/* External Resources */}
                <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                        <Github className="h-4 w-4 text-green-600 dark:text-green-400" />
                      </div>
                      <CardTitle className="text-lg">External Resources</CardTitle>
                    </div>
                    <CardDescription>Links to repositories and documentation</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-3">
                    <Button
                      variant="outline"
                      size="sm"
                      className="w-full justify-start hover:bg-green-50 dark:hover:bg-green-900/20 transition-colors"
                    >
                      <Github className="mr-2 h-4 w-4" />
                      GitHub Repository
                    </Button>
                    <Button
                      variant="outline"
                      size="sm"
                      className="w-full justify-start hover:bg-blue-50 dark:hover:bg-blue-900/20 transition-colors"
                    >
                      <BookOpen className="mr-2 h-4 w-4" />
                      Package Documentation
                    </Button>
                    <Button
                      variant="outline"
                      size="sm"
                      className="w-full justify-start hover:bg-purple-50 dark:hover:bg-purple-900/20 transition-colors"
                    >
                      <FileText className="mr-2 h-4 w-4" />
                      Research Papers
                    </Button>
                  </CardContent>
                </Card>

                {/* Support & Community */}
                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <Users className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">Support</CardTitle>
                    </div>
                    <CardDescription>Get help</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">Bugs and Requests</h4>
                      <p className="text-sm text-muted-foreground">
                        For bug reports and feature requests, please use GitHub Issues.
                      </p>
                    </div>
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">Contact</h4>
                      <p className="text-sm text-muted-foreground">
                        For doubts about the package usage, write to the email <code className="bg-muted px-1 rounded text-xs">torodriguezt@unal.edu.co</code>.
                      </p>
                    </div>
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">Version Info</h4>
                      <p className="text-sm text-muted-foreground">
                        <code className="bg-muted px-1 rounded text-xs">packageVersion("tauBayesW")</code> to check your version.
                      </p>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </section>
          </div>
        </main>
      </div>
    </div>
  )
}
