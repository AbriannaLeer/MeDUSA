#!/usr/bin/env Rscript
# ==========================================================================
# MeDUSA Installation Script
#
# Usage:  Rscript install.R            (full install)
#         Rscript install.R --no-db    (skip database download)
# ==========================================================================

# --- Parse arguments ------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
skip_db <- "--no-db" %in% args

# --- Resolve script directory ---------------------------------------------
full_args <- commandArgs(trailingOnly = FALSE)
script_arg <- full_args[grepl("^--file=", full_args)][1]
script_file <- sub("^--file=", "", script_arg)
script_dir <- if (!is.na(script_file) && nzchar(script_file)) {
  dirname(normalizePath(script_file))
} else {
  normalizePath(getwd())
}

# --- Determine project root ----------------------------------------------
project_root <- normalizePath(file.path(script_dir, ".."))

# Set project root
setwd(project_root)
cat("Working directory set to project root:", getwd(), "\n")

# Paths to MeDUSA package and db_setup.R
renv_lib_path <- file.path(project_root, "MeDUSA", "renv", "library")
medusa_path <- "MeDUSA" # relative path from project root
db_setup_path <- file.path("scripts", "db_setup.R") # relative path from project root

cat("============================================\n")
cat("         MeDUSA Installation Script         \n")
cat("============================================\n\n")

# --- Pre-flight checks ----------------------------------------------------
cat("Checking R version... ")
if (getRversion() < "4.0") {
  stop("MeDUSA requires R >= 4.0. Current version: ", getRversion())
}
cat(as.character(getRversion()), "OK\n")

# --- Step 1: Bootstrap renv -----------------------------------------------
cat("\n--- Step 1/4: Setting up renv ---\n")
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv...\n")

  user_lib <- Sys.getenv("RENV_PATHS_LIBRARY", unset = renv_lib_path)
  if (is.na(user_lib) || nchar(user_lib) == 0) {
    user_lib <- file.path(path.expand("~"), "R", "library")
  }
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  if (!user_lib %in% .libPaths()) .libPaths(c(user_lib, .libPaths()))

  install.packages("renv", repos = "https://cloud.r-project.org", lib = user_lib)
}

# --- Step 2: Install dependencies -----------------------------------------
cat("\n--- Step 2/4: Installing R packages ---\n")
if (file.exists("renv.lock")) {
  cat("Restoring packages from renv.lock...\n")
  renv::restore(prompt = FALSE)
} else {
  cat("No renv.lock found. Installing packages from scratch...\n")
  renv::init(bare = TRUE, bioconductor = TRUE)

  if (.Platform$OS.type == "windows") {
    options(install.packages.compile.from.source = "never")
    options(pkgType = "win.binary")
  }

  # Bioconductor and CRAN packages
  renv::install("BiocManager")
  renv::install("bioc::mzR")
  cran_pkgs <- c(
    "dplyr", "ggplot2", "tibble", "tidyr", "pbapply", "devtools", "pheatmap",
    "XML", "randomForest", "caret", "caTools", "AICcmodavg", "Matrix", "matrixStats",
    "ggpubr", "pROC", "duckdb", "readxl", "testthat", "shiny", "shinyjs",
    "shinythemes", "later", "DT"
  )
  renv::install(cran_pkgs)
}

# Install GUI runtime dependencies even when the lockfile is already in sync.
gui_pkgs <- c("shiny", "shinyjs", "shinythemes", "later", "DT")
missing_gui_pkgs <- gui_pkgs[!vapply(gui_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_gui_pkgs) > 0) {
  cat("Ensuring GUI packages are installed:", paste(missing_gui_pkgs, collapse = ", "), "\n")
  renv::install(missing_gui_pkgs)
}

# --- Step 3: Install MeDUSA package ---------------------------------------
cat("\n--- Step 3/4: Building & installing MeDUSA ---\n")
if (!requireNamespace("devtools", quietly = TRUE)) renv::install("devtools")
devtools::document(medusa_path)
devtools::install(medusa_path, dependencies = FALSE)

if (!file.exists("renv.lock")) {
  cat("\nCreating renv.lock for future reproducibility...\n")
  ## Skip external MeDUSA package
  renv::snapshot(exclude = "MeDUSA", prompt = FALSE)
  cat("Commit renv.lock to version control so others can use renv::restore().\n")
}

# --- Step 4: Database setup (optional) -----------------------------------
if (!skip_db) {
  cat("\n--- Step 4/4: Setting up compound databases ---\n")
  if (!file.exists(db_setup_path)) stop("Cannot find db_setup.R at: ", db_setup_path)

  source(db_setup_path)
  db_path <- db_setup()
} else {
  cat("\n--- Step 4/4: Skipping database setup (--no-db) ---\n")
  cat("Run later with: Rscript scripts/db_setup.R\n")
}

# --- Done -----------------------------------------------------------------
cat("\n============================================\n")
cat("      MeDUSA installation complete!\n")
cat("============================================\n\n")
cat("Quick start:\n")
cat("  library(MeDUSA)\n")
if (!skip_db) {
  cat("  # Databases are ready to use with default paths.\n")
} else {
  cat("  # Remember to run database setup before using identify functions.\n")
}
cat("\n")
