unload_tauBayesW <- function() {
  if ("tauBayesW" %in% loadedNamespaces()) {
    detach("package:tauBayesW", unload = TRUE, character.only = TRUE)
    dll <- system.file("libs", .Platform$r_arch, "tauBayesW.dll", package = "tauBayesW")
    if (file.exists(dll)) try(dyn.unload(dll), silent = TRUE)
  }
}

unload_tauBayesW()
unlink(file.path(.libPaths()[1], "tauBayesW"),      recursive = TRUE, force = TRUE)
unlink(file.path(.libPaths()[1], "00LOCK-tauBayesW"), recursive = TRUE, force = TRUE)

Rcpp::compileAttributes()
devtools::clean_dll()
devtools::install(reload = FALSE, force = TRUE,
                  args = c("--no-multiarch", "--no-staged-install", "--no-test-load"))
