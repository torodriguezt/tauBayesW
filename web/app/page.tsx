"use client"

import { useState } from "react"
import Link from "next/link"
import Image from "next/image"
import { getImageSrc } from "@/lib/image-utils"
import { useApp } from "@/contexts/AppContext"
import { ThemeToggle } from "@/components/theme-toggle"
import { LanguageSelector } from "@/components/language-selector"
import {
  BookOpen,
  Code,
  Download,
  Package,
  FileText,
  Github,
  Menu,
  X,
  User,
  Award,
  Users,
  Languages,
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
  const { t } = useApp()

  const navigationItems = [
    { id: "introduction", title: t("introduction"), icon: BookOpen },
    { id: "installation", title: t("installation"), icon: Download },
    { id: "functions", title: t("functions"), icon: Code },
    { id: "api", title: t("authors"), icon: Package },
    { id: "comparison", title: t("comparison"), icon: Users },
    { id: "help", title: t("help"), icon: FileText },
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
                src={getImageSrc("/logo_tau.png")}
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
              <span className="hidden font-bold sm:inline-block">{t("tauBayesW")}</span>
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

          {/* Theme toggle and language selector on the right */}
          <div className="flex flex-1 items-center justify-end space-x-2">
            <nav className="flex items-center space-x-1">
              <LanguageSelector />
              <ThemeToggle />
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
                    src={getImageSrc("/logo_tau.png")}
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
                    {t("tauBayesW")}
                  </h1>
                  <Badge variant="secondary" className="text-sm px-3 py-1">v1.0.0</Badge>
                </div>
                <p className="text-xl text-muted-foreground mb-2 max-w-3xl mx-auto leading-relaxed">
                  {t("subtitle")}
                </p>
                <p className="text-sm text-muted-foreground max-w-2xl mx-auto">
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
                        {t("bayesianQR")}
                      </CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      {t("bayesianQRDesc")}
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
                        {t("mcmcEM")}
                      </CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      {t("mcmcEMDesc")}
                    </p>
                  </CardContent>
                </Card>
                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300 hover:shadow-lg">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <Award className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-base">{t("highPerf")}</CardTitle>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground leading-relaxed">
                      {t("highPerfDesc")}
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
                  {t("installationTitle")}
                </h2>
                <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
                  {t("installationDesc")}
                </p>
              </div>

              <div className="max-w-5xl mx-auto">
                <div className="grid gap-8 md:grid-cols-2">
                  {/* Installation from GitHub */}
                  <Card className="border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                    <CardHeader className="pb-4">
                      <div className="flex items-center gap-3 mb-2">
                        <div className="h-8 w-8 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                          <Github className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                        </div>
                        <CardTitle className="text-lg">{t("fromGithub")}</CardTitle>
                      </div>
                      <CardDescription>
                        {t("fromGithubDesc")}
                      </CardDescription>
                    </CardHeader>
                    <CardContent>
                      <div className="text-sm bg-slate-900 dark:bg-slate-800 text-green-400 p-4 rounded-lg border">
                        <div className="space-y-2 font-mono">
                          <div className="text-gray-400"># Install devtools if needed</div>
                          <div>install.packages("devtools")</div>
                          <div className="mt-2 text-gray-400"># Install from GitHub</div>
                          <div className="break-words">devtools::install_github("torodriguezt/tauBayesW")</div>
                          <div className="mt-2 text-gray-400"># Load the package</div>
                          <div>library(tauBayesW)</div>
                        </div>
                      </div>
                    </CardContent>
                  </Card>

                  {/* System Requirements */}
                  <Card className="border-2 hover:border-green-200 dark:hover:border-green-800 transition-all duration-300">
                    <CardHeader className="pb-4">
                      <div className="flex items-center gap-3 mb-2">
                        <div className="h-8 w-8 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                          <Package className="h-4 w-4 text-green-600 dark:text-green-400" />
                        </div>
                        <CardTitle className="text-lg">{t("requirements")}</CardTitle>
                      </div>
                      <CardDescription>
                        {t("requirementsDesc")}
                      </CardDescription>
                    </CardHeader>
                    <CardContent className="space-y-4">
                      <div>
                        <h4 className="font-semibold text-sm mb-2">{t("rVersion")}</h4>
                        <p className="text-sm text-muted-foreground">R ≥ 4.0.0</p>
                      </div>
                      <div>
                        <h4 className="font-semibold text-sm mb-2">{t("dependencies")}</h4>
                        <ul className="text-sm text-muted-foreground space-y-1">
                          <li>• Rcpp (≥ 1.0.0)</li>
                          <li>• RcppArmadillo</li>
                          <li>• RcppEigen</li>
                        </ul>
                      </div>
                      <div>
                        <h4 className="font-semibold text-sm mb-2">{t("compiler")}</h4>
                        <p className="text-sm text-muted-foreground">{t("compilerReq")}</p>
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
                        <h4 className="font-semibold text-sm mb-1">{t("quickStart")}</h4>
                        <p className="text-sm text-muted-foreground">
                          {t("quickStartDesc")}
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
                The tauBayesW package provides comprehensive functions for Bayesian quantile regression analysis with survey weights, 
                supporting both single and multiple quantile estimation with advanced algorithmic approaches.
              </p>

                {/* Layout: 3 main functions + utilities */}
                <div className="space-y-8">
                  {/* Three main functions */}
                  <div className="grid grid-cols-1 lg:grid-cols-3 gap-8 max-w-6xl mx-auto">
                    {/* Card 1 - bqr.svy */}
                    <Link href="/functions/bqr-svy">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-blue-300 border-2">
                        <CardHeader className="pb-6 pt-8">
                          <div className="flex items-center gap-3 mb-3">
                            <div className="h-10 w-10 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                              <Code className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                            </div>
                            <CardTitle className="text-xl font-bold group-hover:text-blue-600 transition-colors">
                              bqr.svy()
                            </CardTitle>
                          </div>
                          <CardDescription className="text-base leading-relaxed">
                            Single or multiple quantile Bayesian regression using MCMC methods (ALD, Score, Approximate) with survey weights support.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0">
                          <div className="flex items-center text-sm text-muted-foreground group-hover:text-blue-600 transition-colors">
                            <span>View Documentation</span>
                            <span className="ml-auto">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>

                    {/* Card 2 - mo.bqr.svy */}
                    <Link href="/functions/mo-bqr-svy">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-green-300 border-2">
                        <CardHeader className="pb-6 pt-8">
                          <div className="flex items-center gap-3 mb-3">
                            <div className="h-10 w-10 rounded-lg bg-green-100 dark:bg-green-900 flex items-center justify-center">
                              <Package className="h-5 w-5 text-green-600 dark:text-green-400" />
                            </div>
                            <CardTitle className="text-xl font-bold group-hover:text-green-600 transition-colors">
                              mo.bqr.svy()
                            </CardTitle>
                          </div>
                          <CardDescription className="text-base leading-relaxed">
                            Multiple quantile estimation using EM algorithm with multidirectional capabilities (joint or separable modes) for efficient simultaneous analysis.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0">
                          <div className="flex items-center text-sm text-muted-foreground group-hover:text-green-600 transition-colors">
                            <span>View Documentation</span>
                            <span className="ml-auto">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>

                    {/* Card 3 - plot_quantile */}
                    <Link href="/functions/plot-quantile">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-purple-300 border-2">
                        <CardHeader className="pb-6 pt-8">
                          <div className="flex items-center gap-3 mb-3">
                            <div className="h-10 w-10 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                              <BookOpen className="h-5 w-5 text-purple-600 dark:text-purple-400" />
                            </div>
                            <CardTitle className="text-xl font-bold group-hover:text-purple-600 transition-colors">
                              plot_quantile()
                            </CardTitle>
                          </div>
                          <CardDescription className="text-base leading-relaxed">
                            Comprehensive visualization functions for quantile regression models with customizable plotting options.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0">
                          <div className="flex items-center text-sm text-muted-foreground group-hover:text-purple-600 transition-colors">
                            <span>View Documentation</span>
                            <span className="ml-auto">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>
                  </div>

                  {/* Utilities and Examples Section */}
                  <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 max-w-4xl mx-auto mt-12">
                    {/* Utilities */}
                    <Link href="/functions/utilities">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-orange-300 border-2">
                        <CardHeader className="pb-6 pt-8">
                          <div className="flex items-center gap-3 mb-3">
                            <div className="h-10 w-10 rounded-lg bg-orange-100 dark:bg-orange-900 flex items-center justify-center">
                              <Package className="h-5 w-5 text-orange-600 dark:text-orange-400" />
                            </div>
                            <CardTitle className="text-xl font-bold group-hover:text-orange-600 transition-colors">
                              Utility Functions
                            </CardTitle>
                          </div>
                          <CardDescription className="text-base leading-relaxed">
                            Data simulation, prior specification, convergence diagnostics, and other essential utilities.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0">
                          <div className="flex items-center text-sm text-muted-foreground group-hover:text-orange-600 transition-colors">
                            <span>View Documentation</span>
                            <span className="ml-auto">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>

                    {/* Examples */}
                    <Link href="/functions/examples">
                      <Card className="group cursor-pointer transition-all duration-300 hover:shadow-lg hover:scale-[1.02] hover:border-indigo-300 border-2">
                        <CardHeader className="pb-6 pt-8">
                          <div className="flex items-center gap-3 mb-3">
                            <div className="h-10 w-10 rounded-lg bg-indigo-100 dark:bg-indigo-900 flex items-center justify-center">
                              <FileText className="h-5 w-5 text-indigo-600 dark:text-indigo-400" />
                            </div>
                            <CardTitle className="text-xl font-bold group-hover:text-indigo-600 transition-colors">
                              Complete Examples
                            </CardTitle>
                          </div>
                          <CardDescription className="text-base leading-relaxed">
                            End-to-end workflows, real-world applications, and comprehensive analysis examples.
                          </CardDescription>
                        </CardHeader>
                        <CardContent className="pt-0">
                          <div className="flex items-center text-sm text-muted-foreground group-hover:text-indigo-600 transition-colors">
                            <span>View Examples</span>
                            <span className="ml-auto">→</span>
                          </div>
                        </CardContent>
                      </Card>
                    </Link>
                  </div>                {/* Summary methods info */}
                <div className="mt-12 bg-muted/30 rounded-lg p-6">
                  <h3 className="text-lg font-semibold mb-3">Summary and Print Methods</h3>
                  <p className="text-muted-foreground mb-4">
                    All functions include comprehensive summary() and print() methods for detailed model diagnostics:
                  </p>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                    <div>
                      <h4 className="font-medium text-foreground">For bqr.svy objects:</h4>
                      <ul className="text-muted-foreground mt-1 space-y-1">
                        <li>• MCMC convergence diagnostics</li>
                        <li>• Posterior intervals and estimates</li>
                        <li>• R-hat and effective sample sizes</li>
                        <li>• Automatic adaptation for multiple quantiles</li>
                      </ul>
                    </div>
                    <div>
                      <h4 className="font-medium text-foreground">For mo.bqr.svy objects:</h4>
                      <ul className="text-muted-foreground mt-1 space-y-1">
                        <li>• EM algorithm convergence</li>
                        <li>• Multi-quantile coefficient estimates</li>
                        <li>• Quantile-specific diagnostics</li>
                      </ul>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            <Separator className="mb-12" />

            <section id="algorithms" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">
                Core Algorithms
              </h2>
              <p className="text-muted-foreground mb-6">
                The package implements five highly optimized algorithms for Bayesian weighted quantile regression, 
                including multidirectional EM algorithms and multiple MCMC methods with significant performance improvements.
              </p>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                {/* MCMC_BWQR_AL */}
                <Card className="h-full">
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-lg">
                      <span className="flex h-2 w-2 rounded-full bg-green-500"></span>
                      MCMC_BWQR_AL
                    </CardTitle>
                    <CardDescription>
                      MCMC with Asymmetric Laplace Distribution
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-3">
                      Gibbs sampler implementing the asymmetric Laplace likelihood for single and multiple quantiles 
                      with comprehensive posterior inference.
                    </p>
                    <div className="text-xs text-muted-foreground">
                      <span className="font-semibold">Performance:</span> 1100× faster than R implementation
                    </div>
                  </CardContent>
                </Card>

                {/* EM_BWQR_AL_MO */}
                <Card className="h-full border-2 border-blue-200 dark:border-blue-800">
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-lg">
                      <span className="flex h-2 w-2 rounded-full bg-blue-500"></span>
                      EM_BWQR_AL_MO
                    </CardTitle>
                    <CardDescription>
                      Multidirectional EM Algorithm for Multiple Quantiles
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-3">
                      Advanced EM implementation supporting multivariate responses and multidirectional estimation 
                      with two modes: joint estimation and separable estimation by direction.
                    </p>
                    <div className="space-y-1 text-xs text-muted-foreground">
                      <div><span className="font-semibold">Joint Mode:</span> Simultaneous estimation across all directions</div>
                      <div><span className="font-semibold">Separable Mode:</span> Independent estimation per direction</div>
                      <div><span className="font-semibold">Performance:</span> 769× faster than R implementation</div>
                    </div>
                  </CardContent>
                </Card>

                {/* MCMC_BWQR_AP */}
                <Card className="h-full">
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-lg">
                      <span className="flex h-2 w-2 rounded-full bg-orange-500"></span>
                      MCMC_BWQR_AP
                    </CardTitle>
                    <CardDescription>
                      MCMC with Approximate Posterior
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-3">
                      Adaptive Metropolis-Hastings using empirical pseudo-likelihood for efficient sampling 
                      in complex survey designs.
                    </p>
                    <div className="text-xs text-muted-foreground">
                      <span className="font-semibold">Performance:</span> 7.8× faster than R implementation
                    </div>
                  </CardContent>
                </Card>

                {/* MCMC_BWQR_SL */}
                <Card className="h-full">
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2 text-lg">
                      <span className="flex h-2 w-2 rounded-full bg-purple-500"></span>
                      MCMC_BWQR_SL
                    </CardTitle>
                    <CardDescription>
                      MCMC with Score Likelihood
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <p className="text-sm text-muted-foreground mb-3">
                      Score-based pseudo-likelihood approach for robust quantile regression 
                      with survey weights and multiple quantile support.
                    </p>
                    <div className="text-xs text-muted-foreground">
                      <span className="font-semibold">Performance:</span> 7.1× faster than R implementation
                    </div>
                  </CardContent>
                </Card>
              </div>

              <div className="mt-6 p-4 bg-muted/30 rounded-lg">
                <h4 className="font-semibold mb-2">Technical Innovations</h4>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm text-muted-foreground">
                  <div>
                    <strong>Multidirectional Estimation:</strong> Support for multivariate responses with configurable 
                    search directions and flexible prior specification per quantile and direction.
                  </div>
                  <div>
                    <strong>Multiple Quantiles:</strong> Efficient simultaneous estimation of multiple quantiles 
                    with automatic summary methods and convergence diagnostics.
                  </div>
                  <div>
                    <strong>High Performance:</strong> C++ implementations with 760×-1100× speedups and 85-90% 
                    memory usage reduction compared to R.
                  </div>
                  <div>
                    <strong>Survey Integration:</strong> Native support for complex survey weights with proper 
                    normalization and handling across all algorithms.
                  </div>
                </div>
              </div>
            </section>

            <Separator className="mb-12" />

            {/* 4 ────────────────────── R vs C++ Comparison ─────────────────── */}
            <section id="comparison" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">
                {t("comparisonTitle")}
              </h2>
              <p className="text-muted-foreground mb-6">
                {t("comparisonDesc")}
              </p>

              <Card>
                <CardHeader>
                  <CardTitle className="text-lg">{t("perfBenchmarks")}</CardTitle>
                  <CardDescription>
                    {t("perfBenchmarksDesc")}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="overflow-x-auto">
                    <Table>
                      <TableCaption>
                        {t("perfMetrics")}
                      </TableCaption>
                      <TableHeader>
                        <TableRow>
                          <TableHead className="w-[180px]">{t("algorithm")}</TableHead>
                          <TableHead>{t("rTime")}</TableHead>
                          <TableHead>{t("cppTime")}</TableHead>
                          <TableHead>{t("rMemory")}</TableHead>
                          <TableHead>{t("cppMemory")}</TableHead>
                          <TableHead>{t("speedup")}</TableHead>
                          <TableHead>{t("memorySaving")}</TableHead>
                        </TableRow>
                      </TableHeader>
                      <TableBody>
                        <TableRow>
                          <TableCell className="font-medium">EM_BWQR_AL_MO</TableCell>
                          <TableCell>2.44</TableCell>
                          <TableCell>0.0032</TableCell>
                          <TableCell>2.3 GB</TableCell>
                          <TableCell>190 MB</TableCell>
                          <TableCell className="text-green-600 font-semibold">×769</TableCell>
                          <TableCell className="text-blue-600 font-semibold">~12×</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_AL</TableCell>
                          <TableCell>12.3</TableCell>
                          <TableCell>0.01</TableCell>
                          <TableCell>2.0 GB</TableCell>
                          <TableCell>50 MB</TableCell>
                          <TableCell className="text-green-600 font-semibold">×1100</TableCell>
                          <TableCell className="text-blue-600 font-semibold">~40×</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_AP</TableCell>
                          <TableCell>21.78</TableCell>
                          <TableCell>2.81</TableCell>
                          <TableCell className="text-muted-foreground">-</TableCell>
                          <TableCell className="text-muted-foreground">-</TableCell>
                          <TableCell className="text-green-600 font-semibold">×7.8</TableCell>
                          <TableCell className="text-muted-foreground">-</TableCell>
                        </TableRow>
                        <TableRow>
                          <TableCell className="font-medium">MCMC_BWQR_SL</TableCell>
                          <TableCell>9.7</TableCell>
                          <TableCell>1.4</TableCell>
                          <TableCell className="text-muted-foreground">-</TableCell>
                          <TableCell className="text-muted-foreground">-</TableCell>
                          <TableCell className="text-green-600 font-semibold">×7.1</TableCell>
                          <TableCell className="text-blue-600 font-semibold">~7.2×</TableCell>
                        </TableRow>
                      </TableBody>
                    </Table>
                  </div>
                  
                  <div className="mt-8 grid gap-6 md:grid-cols-2">
                    <Card className="text-center p-6 border-green-200 dark:border-green-800 bg-gradient-to-b from-green-50 to-white dark:from-green-900/20 dark:to-background">
                      <div className="text-3xl font-bold text-green-600 dark:text-green-400 mb-2">
                        ×384
                      </div>
                      <div className="text-sm font-medium text-muted-foreground">
                        {t("avgSpeedup")}
                      </div>
                      <div className="text-xs text-muted-foreground mt-1">
                        {t("acrossAlgorithms")}
                      </div>
                    </Card>
                    
                    <Card className="text-center p-6 border-blue-200 dark:border-blue-800 bg-gradient-to-b from-blue-50 to-white dark:from-blue-900/20 dark:to-background">
                      <div className="text-3xl font-bold text-blue-600 dark:text-blue-400 mb-2">
                        ×17
                      </div>
                      <div className="text-sm font-medium text-muted-foreground">
                        {t("avgMemorySaving")}
                      </div>
                      <div className="text-xs text-muted-foreground mt-1">
                        {t("acrossMeasured")}
                      </div>
                    </Card>
                  </div>

                  <Card className="mt-8 border-slate-200 dark:border-slate-700 bg-gradient-to-r from-slate-50 to-gray-50 dark:from-slate-900/50 dark:to-gray-900/50">
                    <CardContent className="p-6">
                      <h4 className="font-semibold mb-4 text-base flex items-center gap-2">
                        <Code className="h-4 w-4 text-blue-600" />
                        {t("keyBenefits")}
                      </h4>
                      <div className="grid gap-3 md:grid-cols-2">
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-green-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">{t("extremeSpeed")}</strong>
                            <span className="text-sm text-muted-foreground ml-1">{t("extremeSpeedDesc")}</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-blue-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">{t("massiveMemory")}</strong>
                            <span className="text-sm text-muted-foreground ml-1">{t("massiveMemoryDesc")}</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-purple-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">{t("highPerfComputing")}</strong>
                            <span className="text-sm text-muted-foreground ml-1">{t("highPerfComputingDesc")}</span>
                          </div>
                        </div>
                        <div className="flex items-start gap-3">
                          <div className="h-2 w-2 bg-orange-500 rounded-full mt-2 flex-shrink-0"></div>
                          <div>
                            <strong className="text-sm">{t("prodReady")}</strong>
                            <span className="text-sm text-muted-foreground ml-1">{t("prodReadyDesc")}</span>
                          </div>
                        </div>
                      </div>
                      
                      <div className="mt-6 p-4 border rounded-lg bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-900/20 dark:to-indigo-900/20 border-blue-200 dark:border-blue-800">
                        <h5 className="font-semibold text-sm mb-2 text-blue-800 dark:text-blue-200">
                          Technical Implementation Details
                        </h5>
                        <ul className="text-sm text-muted-foreground space-y-1">
                          <li>• <strong>RcppEigen:</strong> Optimized linear algebra with SIMD vectorization</li>
                          <li>• <strong>Memory Pool Allocation:</strong> Reduced memory fragmentation and allocation overhead</li>
                          <li>• <strong>Parallel Processing:</strong> Multi-threaded matrix operations where applicable</li>
                          <li>• <strong>Numerical Stability:</strong> Advanced algorithms for inverse-Gaussian sampling</li>
                          <li>• <strong>Cache Efficiency:</strong> Memory-aligned data structures for optimal CPU cache usage</li>
                        </ul>
                      </div>
                    </CardContent>
                  </Card>
                </CardContent>
              </Card>
            </section>

            <Separator className="mb-12" />

            {/* 5 ────────────────────────── Authors ────────────────────────── */}
            <section id="api" className="mb-12">
              <h2 className="text-2xl font-bold tracking-tight mb-4">{t("authorsTitle")}</h2>
              <p className="text-muted-foreground mb-6">
                {t("authorsDesc")}
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
                      {t("marcusDesc")}
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("email")}:</span>
                        <a
                          href="mailto:marcus.lavagnole@fgv.br"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          marcus.lavagnole@fgv.br
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("github")}:</span>
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
                      {t("kellyDesc")}
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("email")}:</span>
                        <a
                          href="mailto:kelly@dme.ufrj.br"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          kelly@dme.ufrj.br
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("webPage")}:</span>
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
                      {t("johnatanDesc")}
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("email")}:</span>
                        <a
                          href="mailto:jcardonj@unal.edu.co"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          jcardonj@unal.edu.co
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("github")}:</span>
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
                      {t("tomasDesc")}
                    </p>
                    <div className="space-y-2 text-sm">
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("email")}:</span>
                        <a
                          href="mailto:torodriguezt@unal.edu.co"
                          className="text-blue-600 hover:text-blue-700 hover:underline transition-colors"
                        >
                          torodriguezt@unal.edu.co
                        </a>
                      </div>
                      <div className="flex items-center gap-2">
                        <span className="font-semibold text-slate-700 dark:text-slate-300">{t("github")}:</span>
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
                <h2 className="text-3xl font-bold tracking-tight mb-4">{t("helpTitle")}</h2>
                <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
                  {t("helpDesc")}
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
                      <CardTitle className="text-lg">{t("rConsole")}</CardTitle>
                    </div>
                    <CardDescription>{t("rConsoleDesc")}</CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="text-sm bg-slate-900 dark:bg-slate-800 text-green-400 p-4 rounded-lg border">
                      <div className="space-y-2 font-mono">
                        <div className="text-gray-400"># Function help</div>
                        <div>?MCMC_BWQR_AL</div>
                        <div className="mt-2 text-gray-400"># Package help</div>
                        <div>help(package="tauBayesW")</div>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* Research Paper */}
                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <FileText className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">{t("researchPaper")}</CardTitle>
                    </div>
                    <CardDescription>{t("researchPaperDesc")}</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="p-4 border rounded-lg bg-gradient-to-r from-purple-50 to-indigo-50 dark:from-purple-900/20 dark:to-indigo-900/20 border-purple-200 dark:border-purple-800">
                      <h4 className="font-semibold text-sm mb-2 text-purple-800 dark:text-purple-200">
                        {t("journalName")}
                      </h4>
                      <p className="text-sm text-muted-foreground mb-3 leading-relaxed">
                        {t("journalDesc")}
                      </p>
                      <Button
                        variant="outline"
                        size="sm"
                        className="w-full justify-center hover:bg-purple-50 dark:hover:bg-purple-900/20 transition-colors border-purple-300 dark:border-purple-700 mb-3"
                        asChild
                      >
                        <a href="https://academic.oup.com/jssam/article-abstract/12/4/1105/7642687" target="_blank" rel="noopener noreferrer">
                          <FileText className="mr-2 h-4 w-4" />
                          {t("readPaper")}
                        </a>
                      </Button>
                      <div className="pt-2 border-t border-purple-200 dark:border-purple-700">
                        <p className="text-xs text-muted-foreground text-center">
                          {t("paperAuthors")}
                        </p>
                      </div>
                    </div>
                  </CardContent>
                </Card>

                {/* Support & Community */}
                <Card className="border-2 hover:border-purple-200 dark:hover:border-purple-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center gap-3 mb-2">
                      <div className="h-8 w-8 rounded-lg bg-purple-100 dark:bg-purple-900 flex items-center justify-center">
                        <Users className="h-4 w-4 text-purple-600 dark:text-purple-400" />
                      </div>
                      <CardTitle className="text-lg">{t("support")}</CardTitle>
                    </div>
                    <CardDescription>{t("supportDesc")}</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">{t("bugsRequests")}</h4>
                      <p className="text-sm text-muted-foreground">
                        {t("bugsRequestsDesc")}
                      </p>
                    </div>
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">{t("contact")}</h4>
                      <p className="text-sm text-muted-foreground">
                        {t("contactDesc")}
                      </p>
                    </div>
                    <div className="space-y-2">
                      <h4 className="font-semibold text-sm">{t("versionInfo")}</h4>
                      <p className="text-sm text-muted-foreground">
                        <code className="bg-muted px-1 rounded text-xs">{t("versionInfoDesc")}</code>
                      </p>
                    </div>
                  </CardContent>
                </Card>
              </div>
            </section>

            {/* GitHub Repository Section */}
            <section className="mb-16">
              <div className="text-center">
                <Card className="max-w-md mx-auto border-2 hover:border-blue-200 dark:hover:border-blue-800 transition-all duration-300">
                  <CardHeader className="pb-4">
                    <div className="flex items-center justify-center gap-3 mb-2">
                      <div className="h-10 w-10 rounded-lg bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                        <Github className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                      </div>
                      <CardTitle className="text-xl">{t("sourceCode")}</CardTitle>
                    </div>
                    <CardDescription>{t("sourceCodeDesc")}</CardDescription>
                  </CardHeader>
                  <CardContent>
                    <Button
                      variant="default"
                      size="lg"
                      className="w-full bg-blue-600 hover:bg-blue-700 dark:bg-blue-600 dark:hover:bg-blue-700"
                      asChild
                    >
                      <a href="https://github.com/torodriguezt/tauBayesW" target="_blank" rel="noopener noreferrer">
                        <Github className="mr-2 h-4 w-4" />
                        {t("visitGithub")}
                      </a>
                    </Button>
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
