"use client"

import React, { createContext, useContext, useState, useEffect } from 'react'

type Theme = 'light' | 'dark'
type Language = 'en' | 'es' | 'pt'

interface AppContextType {
  theme: Theme
  language: Language
  toggleTheme: () => void
  setLanguage: (lang: Language) => void
  t: (key: string) => string
}

const AppContext = createContext<AppContextType | undefined>(undefined)

// Traducciones
const translations = {
  en: {
    // Header
    tauBayesW: "tauBayesW",
    
    // Navigation
    introduction: "Introduction",
    installation: "Installation", 
    functions: "Functions",
    authors: "Authors",
    comparison: "R vs C++ Comparison",
    help: "Help",
    
    // Introduction
    subtitle: "An R package for implementing MCMC and EM algorithms for the Bayesian weighted quantile regression.",
    
    // Feature cards
    bayesianQR: "Bayesian Quantile Regression with Weights",
    bayesianQRDesc: "Corrects bias from informative sampling and captures effects across the distribution.",
    mcmcEM: "MCMC and EM Algorithms", 
    mcmcEMDesc: "Enable efficient estimation and full Bayesian inference in complex and large-sample models.",
    highPerf: "High Performance",
    highPerfDesc: "Implementation using C++ to have a much faster execution compared to R.",
    
    // Installation
    installationTitle: "Installation",
    installationDesc: "Install tauBayesW from GitHub to obtain the latest stable version with all features and documentation.",
    fromGithub: "From GitHub",
    fromGithubDesc: "Latest development version with newest features",
    requirements: "Requirements",
    requirementsDesc: "System dependencies and R version requirements",
    rVersion: "R Version",
    dependencies: "Dependencies",
    compiler: "Compiler",
    compilerReq: "C++11 compatible compiler",
    quickStart: "Quick Start",
    quickStartDesc: "After installation, check out our function examples below or visit the Help section for detailed documentation and tutorials.",
    
    // Functions
    functionsTitle: "Main Functions",
    functionsDesc: "Complete reference of the most important functions in tauBayesW.",
    clickToView: "Click to view details",
    
    // Function descriptions
    bqrSvyDesc: "MCMC methods for single or multiple quantiles with comprehensive posterior inference and diagnostics.",
    moBqrSvyDesc: "EM algorithm for multiple quantiles with multidirectional estimation (joint or separable modes).",
    plotQuantileDesc: "Comprehensive visualization tools for quantile regression results with customizable plotting options.",
    utilitiesDesc: "Data simulation, prior specification, convergence diagnostics, and model comparison tools.",
    examplesDesc: "Complete end-to-end workflows demonstrating real-world applications and analysis patterns.",
    
    // Comparison
    comparisonTitle: "Comparison between R code and C++ code",
    comparisonDesc: "Performance comparison between R and C++ implementations of the main algorithms in tauBayesW.",
    perfBenchmarks: "Performance Benchmarks",
    perfBenchmarksDesc: "Execution time and memory usage comparison",
    perfMetrics: "Performance metrics measured on R 4.4.2, Windows 11, Intel i5 13600-K with 32GB RAM",
    algorithm: "Algorithm",
    rTime: "R Time (sec)",
    cppTime: "C++ Time (sec)",
    rMemory: "R Memory",
    cppMemory: "C++ Memory",
    speedup: "Speedup",
    memorySaving: "Memory Saving",
    avgSpeedup: "Average Speedup",
    avgMemorySaving: "Average Memory Saving",
    acrossAlgorithms: "Across all 5 algorithms",
    acrossMeasured: "Across measured algorithms",
    keyBenefits: "Key Benefits of C++ Implementation",
    extremeSpeed: "Extreme Speed:",
    extremeSpeedDesc: "Faster execution than R implementations",
    massiveMemory: "Massive Memory Savings:",
    massiveMemoryDesc: "Reduce memory usage from GB to MB (up to 40× reduction)",
    highPerfComputing: "High-Performance Computing:",
    highPerfComputingDesc: "Optimized algorithms enable analysis of large datasets",
    prodReady: "Test environment",
    prodReadyDesc: "Tested on Intel i5 13600-K, 32GB RAM",
    
    // Authors
    authorsTitle: "Authors",
    authorsDesc: "Meet the team behind the development of tauBayesW.",
    email: "Email",
    github: "GitHub",
    webPage: "Web Page",
    
    // Author descriptions
    marcusDesc: "Postdoctoral Researcher at the School of Applied Mathematics at Fundação Getulio Vargas (FGV EMAp) and a Research Affiliate at the José Luiz Setúbal Foundation (FJLES).",
    kellyDesc: "Associate Professor in the Department of Statistics at the Federal University of Rio de Janeiro (UFRJ) in Brazil.",
    johnatanDesc: "Assistant Professor in the Department of Statistics at Universidad Nacional de Colombia (UNAL) in Medellin.",
    tomasDesc: "Undergraduate student in the Statistics and Informatic Engineering programs at Universidad Nacional de Colombia (UNAL) in Medellin.",
    
    // Help
    helpTitle: "Help & Resources",
    helpDesc: "Documentation, support resources, and community links for tauBayesW.",
    rConsole: "R Console Commands",
    rConsoleDesc: "Quick access to function documentation",
    researchPaper: "Research Paper",
    researchPaperDesc: "Published research and methodology",
    journalName: "Journal of Survey Statistics and Methodology",
    journalDesc: "The complete methodology and theoretical foundation behind the algorithms implemented in this package.",
    readPaper: "Read Full Paper",
    paperAuthors: "Authors: Marcus L. Nascimento & Kelly Cristina Mota Gonçalves",
    support: "Support",
    supportDesc: "Get help",
    bugsRequests: "Bugs and Requests",
    bugsRequestsDesc: "For bug reports and feature requests, please use GitHub Issues.",
    contact: "Contact",
    contactDesc: "For doubts about the package usage, write to the email torodriguezt@unal.edu.co.",
    versionInfo: "Version Info",
    versionInfoDesc: "packageVersion(\"tauBayesW\") to check your version.",
    
    // GitHub
    sourceCode: "Source Code",
    sourceCodeDesc: "View the complete source code and contribute",
    visitGithub: "Visit GitHub Repository",
  },
  es: {
    // Header
    tauBayesW: "tauBayesW",
    
    // Navigation
    introduction: "Introducción",
    installation: "Instalación",
    functions: "Funciones", 
    authors: "Autores",
    comparison: "Comparación R vs C++",
    help: "Ayuda",
    
    // Introduction
    subtitle: "Un paquete de R para implementar algoritmos MCMC y EM para la regresión cuantílica bayesiana ponderada.",
    
    // Feature cards
    bayesianQR: "Regresión Cuantílica Bayesiana con Pesos",
    bayesianQRDesc: "Corrige el sesgo del muestreo informativo y captura efectos a través de la distribución.",
    mcmcEM: "Algoritmos MCMC y EM",
    mcmcEMDesc: "Permiten estimación eficiente e inferencia bayesiana completa en modelos complejos y de muestras grandes.",
    highPerf: "Alto Rendimiento",
    highPerfDesc: "Implementación usando C++ para tener una ejecución mucho más rápida comparada con R.",
    
    // Installation
    installationTitle: "Instalación",
    installationDesc: "Instala tauBayesW desde GitHub para obtener la última versión estable con todas las características y documentación.",
    fromGithub: "Desde GitHub",
    fromGithubDesc: "Última versión de desarrollo con las características más nuevas",
    requirements: "Requisitos",
    requirementsDesc: "Dependencias del sistema y requisitos de versión de R",
    rVersion: "Versión de R",
    dependencies: "Dependencias",
    compiler: "Compilador",
    compilerReq: "Compilador compatible con C++11",
    quickStart: "Inicio Rápido",
    quickStartDesc: "Después de la instalación, revisa nuestros ejemplos de funciones abajo o visita la sección de Ayuda para documentación detallada y tutoriales.",
    
    // Functions
    functionsTitle: "Funciones Principales",
    functionsDesc: "Referencia completa de las funciones más importantes en tauBayesW.",
    clickToView: "Clic para ver detalles",
    
    // Function descriptions
    bqrSvyDesc: "Métodos MCMC para uno o múltiples cuantiles con inferencia posterior completa y diagnósticos.",
    moBqrSvyDesc: "Algoritmo EM para múltiples cuantiles con estimación multidireccional (modos conjunto o separable).",
    plotQuantileDesc: "Herramientas de visualización completas para resultados de regresión cuantílica con opciones de gráficos personalizables.",
    utilitiesDesc: "Simulación de datos, especificación de priors, diagnósticos de convergencia y herramientas de comparación de modelos.",
    examplesDesc: "Flujos de trabajo completos que demuestran aplicaciones del mundo real y patrones de análisis.",
    
    // Comparison
    comparisonTitle: "Comparación entre código R y código C++",
    comparisonDesc: "Comparación de rendimiento entre implementaciones de R y C++ de los algoritmos principales en tauBayesW.",
    perfBenchmarks: "Benchmarks de Rendimiento",
    perfBenchmarksDesc: "Comparación de tiempo de ejecución y uso de memoria en diferentes tamaños de conjuntos de datos",
    perfMetrics: "Métricas de rendimiento medidas en R 4.4.2, Windows 11, Intel i5 13600-K con 32GB RAM",
    algorithm: "Algoritmo",
    rTime: "Tiempo R (seg)",
    cppTime: "Tiempo C++ (seg)",
    rMemory: "Memoria R",
    cppMemory: "Memoria C++",
    speedup: "Aceleración",
    memorySaving: "Ahorro de Memoria",
    avgSpeedup: "Aceleración Promedio",
    avgMemorySaving: "Ahorro Promedio de Memoria",
    acrossAlgorithms: "Entre los 5 algoritmos",
    acrossMeasured: "Entre algoritmos medidos",
    keyBenefits: "Beneficios Clave de la Implementación C++",
    extremeSpeed: "Velocidad Extrema:",
    extremeSpeedDesc: "Hasta 1100× más rápida ejecución que implementaciones de R",
    massiveMemory: "Ahorro Masivo de Memoria:",
    massiveMemoryDesc: "Reduce el uso de memoria de GB a MB (hasta 40× reducción)",
    highPerfComputing: "Computación de Alto Rendimiento:",
    highPerfComputingDesc: "Algoritmos optimizados permiten análisis de conjuntos de datos grandes",
    prodReady: "Listo para Producción:",
    prodReadyDesc: "Probado en hardware moderno (Intel i5 13600-K, 32GB RAM)",
    
    // Authors
    authorsTitle: "Autores",
    authorsDesc: "Conoce al equipo detrás del desarrollo de tauBayesW.",
    email: "Correo",
    github: "GitHub",
    webPage: "Página Web",
    
    // Author descriptions
    marcusDesc: "Investigador Postdoctoral en la Escuela de Matemáticas Aplicadas de la Fundação Getulio Vargas (FGV EMAp) y Afiliado de Investigación en la Fundación José Luiz Setúbal (FJLES).",
    kellyDesc: "Profesora Asociada en el Departamento de Estadística de la Universidad Federal de Río de Janeiro (UFRJ) en Brasil.",
    johnatanDesc: "Profesor Asistente en el Departamento de Estadística de la Universidad Nacional de Colombia (UNAL) en Medellín.",
    tomasDesc: "Estudiante de pregrado en los programas de Estadística e Ingeniería Informática en la Universidad Nacional de Colombia (UNAL) en Medellín.",
    
    // Help
    helpTitle: "Ayuda y Recursos",
    helpDesc: "Documentación, recursos de soporte y enlaces de la comunidad para tauBayesW.",
    rConsole: "Comandos de Consola R",
    rConsoleDesc: "Acceso rápido a documentación de funciones",
    researchPaper: "Artículo de Investigación",
    researchPaperDesc: "Investigación publicada y metodología",
    journalName: "Revista de Estadísticas de Encuestas y Metodología",
    journalDesc: "La metodología completa y fundamento teórico detrás de los algoritmos implementados en este paquete.",
    readPaper: "Leer Artículo Completo",
    paperAuthors: "Autores: Marcus L. Nascimento & Kelly Cristina Mota Gonçalves",
    support: "Soporte",
    supportDesc: "Obtener ayuda",
    bugsRequests: "Errores y Solicitudes",
    bugsRequestsDesc: "Para reportes de errores y solicitudes de características, por favor usa GitHub Issues.",
    contact: "Contacto",
    contactDesc: "Para dudas sobre el uso del paquete, escribe al correo torodriguezt@unal.edu.co.",
    versionInfo: "Información de Versión",
    versionInfoDesc: "packageVersion(\"tauBayesW\") para verificar tu versión.",
    
    // GitHub
    sourceCode: "Código Fuente",
    sourceCodeDesc: "Ver el código fuente completo y contribuir",
    visitGithub: "Visitar Repositorio GitHub",
  },
  pt: {
    // Header
    tauBayesW: "tauBayesW",
    
    // Navigation
    introduction: "Introdução",
    installation: "Instalação",
    functions: "Funções",
    authors: "Autores", 
    comparison: "Comparação R vs C++",
    help: "Ajuda",
    
    // Introduction
    subtitle: "Um pacote R para implementar algoritmos MCMC e EM para regressão quantílica bayesiana ponderada.",
    
    // Feature cards
    bayesianQR: "Regressão Quantílica Bayesiana com Pesos",
    bayesianQRDesc: "Corrige viés de amostragem informativa e captura efeitos através da distribuição.",
    mcmcEM: "Algoritmos MCMC e EM",
    mcmcEMDesc: "Permitem estimação eficiente e inferência bayesiana completa em modelos complexos e de grandes amostras.",
    highPerf: "Alto Desempenho",
    highPerfDesc: "Implementação usando C++ para ter execução muito mais rápida comparada ao R.",
    
    // Installation
    installationTitle: "Instalação",
    installationDesc: "Instale tauBayesW do GitHub para obter a versão estável mais recente com todas as funcionalidades e documentação.",
    fromGithub: "Do GitHub",
    fromGithubDesc: "Última versão de desenvolvimento com recursos mais novos",
    requirements: "Requisitos",
    requirementsDesc: "Dependências do sistema e requisitos de versão do R",
    rVersion: "Versão do R",
    dependencies: "Dependências",
    compiler: "Compilador",
    compilerReq: "Compilador compatível com C++11",
    quickStart: "Início Rápido",
    quickStartDesc: "Após a instalação, confira nossos exemplos de funções abaixo ou visite a seção Ajuda para documentação detalhada e tutoriais.",
    
    // Functions
    functionsTitle: "Funções Principais",
    functionsDesc: "Referência completa das funções mais importantes no tauBayesW.",
    clickToView: "Clique para ver detalhes",
    
    // Function descriptions
    bqrSvyDesc: "Métodos MCMC para um ou múltiplos quantis com inferência posterior completa e diagnósticos.",
    moBqrSvyDesc: "Algoritmo EM para múltiplos quantis com estimação multidirecional (modos conjunto ou separável).",
    plotQuantileDesc: "Ferramentas de visualização abrangentes para resultados de regressão quantílica com opções de gráficos personalizáveis.",
    utilitiesDesc: "Simulação de dados, especificação de priors, diagnósticos de convergência e ferramentas de comparação de modelos.",
    examplesDesc: "Fluxos de trabalho completos demonstrando aplicações do mundo real e padrões de análise.",
    
    // Comparison
    comparisonTitle: "Comparação entre código R e código C++",
    comparisonDesc: "Comparação de desempenho entre implementações R e C++ dos algoritmos principais no tauBayesW.",
    perfBenchmarks: "Benchmarks de Desempenho",
    perfBenchmarksDesc: "Comparação de tempo de execução e uso de memória em diferentes tamanhos de conjuntos de dados",
    perfMetrics: "Métricas de desempenho medidas no R 4.4.2, Windows 11, Intel i5 13600-K com 32GB RAM",
    algorithm: "Algoritmo",
    rTime: "Tempo R (seg)",
    cppTime: "Tempo C++ (seg)",
    rMemory: "Memória R",
    cppMemory: "Memória C++",
    speedup: "Aceleração",
    memorySaving: "Economia de Memória",
    avgSpeedup: "Aceleração Média",
    avgMemorySaving: "Economia Média de Memória",
    acrossAlgorithms: "Entre todos os 5 algoritmos",
    acrossMeasured: "Entre algoritmos medidos",
    keyBenefits: "Principais Benefícios da Implementação C++",
    extremeSpeed: "Velocidade Extrema:",
    extremeSpeedDesc: "Até 1100× execução mais rápida que implementações R",
    massiveMemory: "Economia Massiva de Memória:",
    massiveMemoryDesc: "Reduz uso de memória de GB para MB (até 40× redução)",
    highPerfComputing: "Computação de Alto Desempenho:",
    highPerfComputingDesc: "Algoritmos otimizados permitem análise de grandes conjuntos de dados",
    prodReady: "Pronto para Produção:",
    prodReadyDesc: "Testado em hardware moderno (Intel i5 13600-K, 32GB RAM)",
    
    // Authors
    authorsTitle: "Autores",
    authorsDesc: "Conheça a equipe por trás do desenvolvimento do tauBayesW.",
    email: "Email",
    github: "GitHub", 
    webPage: "Página Web",
    
    // Author descriptions
    marcusDesc: "Pesquisador Pós-Doutoral na Escola de Matemática Aplicada da Fundação Getulio Vargas (FGV EMAp) e Afiliado de Pesquisa na Fundação José Luiz Setúbal (FJLES).",
    kellyDesc: "Professora Associada no Departamento de Estatística da Universidade Federal do Rio de Janeiro (UFRJ) no Brasil.",
    johnatanDesc: "Professor Assistente no Departamento de Estatística da Universidad Nacional de Colombia (UNAL) em Medellín.",
    tomasDesc: "Estudante de graduação nos programas de Estatística e Engenharia Informática na Universidad Nacional de Colombia (UNAL) em Medellín.",
    
    // Help
    helpTitle: "Ajuda e Recursos",
    helpDesc: "Documentação, recursos de suporte e links da comunidade para tauBayesW.",
    rConsole: "Comandos do Console R",
    rConsoleDesc: "Acesso rápido à documentação de funções",
    researchPaper: "Artigo de Pesquisa",
    researchPaperDesc: "Pesquisa publicada e metodologia",
    journalName: "Revista de Estatísticas de Pesquisa e Metodologia",
    journalDesc: "A metodologia completa e fundamento teórico por trás dos algoritmos implementados neste pacote.",
    readPaper: "Ler Artigo Completo",
    paperAuthors: "Autores: Marcus L. Nascimento & Kelly Cristina Mota Gonçalves",
    support: "Suporte",
    supportDesc: "Obter ajuda",
    bugsRequests: "Bugs e Solicitações",
    bugsRequestsDesc: "Para relatórios de bugs e solicitações de recursos, por favor use GitHub Issues.",
    contact: "Contato",
    contactDesc: "Para dúvidas sobre o uso do pacote, escreva para o email torodriguezt@unal.edu.co.",
    versionInfo: "Informações da Versão",
    versionInfoDesc: "packageVersion(\"tauBayesW\") para verificar sua versão.",
    
    // GitHub
    sourceCode: "Código Fonte",
    sourceCodeDesc: "Ver o código fonte completo e contribuir",
    visitGithub: "Visitar Repositório GitHub",
  }
}

export function AppProvider({ children }: { children: React.ReactNode }) {
  const [theme, setTheme] = useState<Theme>('light')
  const [language, setLanguage] = useState<Language>('en')

  useEffect(() => {
    // Load theme from localStorage
    const savedTheme = localStorage.getItem('theme') as Theme
    if (savedTheme) {
      setTheme(savedTheme)
    }
    
    // Load language from localStorage
    const savedLanguage = localStorage.getItem('language') as Language
    if (savedLanguage) {
      setLanguage(savedLanguage)
    }
  }, [])

  useEffect(() => {
    // Apply theme to document
    if (theme === 'dark') {
      document.documentElement.classList.add('dark')
    } else {
      document.documentElement.classList.remove('dark')
    }
    localStorage.setItem('theme', theme)
  }, [theme])

  useEffect(() => {
    localStorage.setItem('language', language)
  }, [language])

  const toggleTheme = () => {
    setTheme(prev => prev === 'light' ? 'dark' : 'light')
  }

  const t = (key: string): string => {
    return translations[language][key as keyof typeof translations[typeof language]] || key
  }

  return (
    <AppContext.Provider value={{
      theme,
      language,
      toggleTheme,
      setLanguage,
      t
    }}>
      {children}
    </AppContext.Provider>
  )
}

export function useApp() {
  const context = useContext(AppContext)
  if (context === undefined) {
    throw new Error('useApp must be used within an AppProvider')
  }
  return context
}
