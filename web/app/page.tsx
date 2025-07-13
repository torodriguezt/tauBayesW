"use client"

import { useState } from "react"
import Link from "next/link"
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

export default function RDocumentation() {
  const [sidebarOpen, setSidebarOpen] = useState(false)

  const navigationItems = [
    { id: "introduction", title: "Introduction", icon: BookOpen },
    { id: "installation", title: "Installation", icon: Download },
    { id: "functions", title: "Functions", icon: Code },
    { id: "api", title: "Authors", icon: Package },
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
              <div className="flex h-6 w-6 items-center justify-center rounded bg-blue-600 text-white text-xs font-bold">
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
            <section id="introduction" className="mb-12">
              <div className="flex items-center gap-2 mb-4">
                <div className="flex h-8 w-8 items-center justify-center rounded bg-blue-600 text-white text-sm font-bold">
                  R
                </div>
                <h1 className="text-3xl font-bold tracking-tight">tauBayesW</h1>
                <Badge variant="secondary">v1.0.0</Badge>
              </div>
              <p className="text-xl text-muted-foreground mb-6">
                An R package for implementing MCMC and EM algorithms for the
                Bayesian weighted quantile regression.
              </p>

              <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3 mb-8">
                <Card>
                  <CardHeader className="pb-3">
                    <CardTitle className="text-base">
                      Bayesian Quantile Regression with Weights
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground">
                      Implements robust statistical methods for datasets with
                      outliers.
                    </p>
                  </CardContent>
                </Card>
                <Card>
                  <CardHeader className="pb-3">
                    <CardTitle className="text-base">
                      MCMC and EM Algorithms
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground">
                      Advanced algorithms with ggplot2 integration for visual
                      diagnostics.
                    </p>
                  </CardContent>
                </Card>
                <Card>
                  <CardHeader className="pb-3">
                    <CardTitle className="text-base">Performance</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground">
                      Optimized for fast execution via C++ back-end.
                    </p>
                  </CardContent>
                </Card>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 2 ──────────────────────── Installation ──────────────────────── */}
            <section id="installation" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">
                Installation
              </h2>
              <p className="text-muted-foreground mb-6">
                Install <code>tauBayesW</code> from GitHub to obtain the latest
                stable version.
              </p>

              <div className="grid gap-6 md:grid-cols-2">
                <div>
                  <h3 className="text-lg font-semibold mb-2">From GitHub</h3>
                  <Card>
                    <CardContent className="p-4">
                      <pre className="text-sm bg-muted p-3 rounded overflow-x-auto">
{`# Install devtools if needed
install.packages("devtools")

# Install from GitHub
devtools::install_github("user/tauBayesW")

library(tauBayesW)`}
                      </pre>
                    </CardContent>
                  </Card>
                </div>
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

            {/* 4 ────────────────────────── Authors ────────────────────────── */}
            <section id="api" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">Authors</h2>
              <p className="text-muted-foreground mb-6">
                Meet the team behind the development of <code>tauBayesW</code>.
              </p>

              <div className="grid gap-6 md:grid-cols-2 xl:grid-cols-3">
                {/* Autor 1 - Lead Developer */}
                <Card className="hover:shadow-md transition-shadow duration-300">
                  <CardHeader>
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-10 w-10 rounded-full bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <User className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                      </div>
                      <div>
                        <CardTitle className="text-base">Marcus L. Nascimento</CardTitle>
                        <CardDescription>Fundação Getulio Vargas</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-2">
                      Creator and lead developer of the tauBayesW package. Designed the Bayesian quantile regression framework and implemented EM/MCMC algorithms.
                    </p>
                    <div className="text-sm">
                      <p>
                        <span className="font-semibold">Email:</span>{" "}
                        <a
                          href="mailto:tomas@example.com"
                          className="text-blue-600 hover:underline"
                        >
                          tomas@example.com
                        </a>
                      </p>
                      <p>
                        <span className="font-semibold">GitHub:</span>{" "}
                        <a
                          href="https://github.com/tomas"
                          className="text-blue-600 hover:underline"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          @tomas
                        </a>
                      </p>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 2 - Professor */}
                <Card className="hover:shadow-md transition-shadow duration-300">
                  <CardHeader>
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-10 w-10 rounded-full bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <User className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                      </div>
                      <div>
                        <CardTitle className="text-base">Prof. Kelly Cristina Mota Gonçalves</CardTitle>
                        <CardDescription>Federal University of Rio de Janeiro</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-2">
                      Advisor on Bayesian statistics and model validation. Helped define the theoretical underpinnings of the methodology.
                    </p>
                    <div className="text-sm">
                      <p>
                        <span className="font-semibold">Email:</span>{" "}
                        <a
                          href="mailto:mlopez@example.com"
                          className="text-blue-600 hover:underline"
                        >
                          mlopez@example.com
                        </a>
                      </p>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 3 - Professor */}
                <Card className="hover:shadow-md transition-shadow duration-300">
                  <CardHeader>
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-10 w-10 rounded-full bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <User className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                      </div>
                      <div>
                        <CardTitle className="text-base">Prof. Johntan Cardona Jimenez</CardTitle>
                        <CardDescription>Universidad Nacional de Colombia</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-2">
                      Supervised the EM algorithm development and offered insights on optimization and convergence analysis.
                    </p>
                    <div className="text-sm">
                      <p>
                        <span className="font-semibold">Email:</span>{" "}
                        <a
                          href="mailto:jtorres@example.com"
                          className="text-blue-600 hover:underline"
                        >
                          jtorres@example.com
                        </a>
                      </p>
                    </div>
                  </CardContent>
                </Card>

                {/* Autor 4 - Collaborator */}
                <Card className="hover:shadow-md transition-shadow duration-300">
                  <CardHeader>
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-10 w-10 rounded-full bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <User className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                      </div>
                      <div>
                        <CardTitle className="text-base">Tomas Rodriguez Taborda</CardTitle>
                        <CardDescription>Universidad Nacional de Colombia</CardDescription>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-2">
                      Supervised the EM algorithm development and offered insights on optimization and convergence analysis.
                    </p>
                    <div className="text-sm">
                      <p>
                        <span className="font-semibold">Email:</span>{" "}
                        <a
                          href="mailto:jtorres@example.com"
                          className="text-blue-600 hover:underline"
                        >
                          jtorres@example.com
                        </a>
                      </p>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </section>


            <Separator className="mb-12" />

            {/* 5 ─────────────────────────── Help ─────────────────────────── */}
            <section id="help" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">Help</h2>
              <p className="text-muted-foreground mb-6">
                Support resources and documentation commands.
              </p>

              <Card>
                <CardHeader>
                  <CardTitle className="text-lg">Help Commands</CardTitle>
                  <CardDescription>Quick R docs and links</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="grid gap-4 md:grid-cols-2">
                    {/* R console commands */}
                    <div>
                      <h4 className="font-semibold mb-2">R Console</h4>
                      <pre className="text-sm bg-muted p-2 rounded">
{`# Function help
?robust_describe

# Available vignettes
vignette("tauBayesW")

# Examples
example(robust_describe)`}
                      </pre>
                    </div>

                    {/* External resources */}
                    <div>
                      <h4 className="font-semibold mb-2">Resources</h4>
                      <div className="space-y-2">
                        <Button
                          variant="outline"
                          size="sm"
                          className="w-full justify-start bg-transparent"
                        >
                          <Github className="mr-2 h-4 w-4" />
                          GitHub Repository
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </section>
          </div>
        </main>
      </div>
    </div>
  )
}
