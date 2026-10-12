#!/usr/bin/env Rscript
# ==========================================================================
# MeDUSA Uninstall Script
#
# Removes the MeDUSA package, renv library, and optionally databases.
#
# Usage:  Rscript uninstall.R             (keep databases)
#         Rscript uninstall.R --all       (also remove databases)
# ==========================================================================

Sys.setenv(RENV_BOOTSTRAP = "false")
Sys.setenv(RENV_CONSENT = "true")

# --- Resolve script directory ---------------------------------------------
full_args <- commandArgs(trailingOnly = FALSE)
script_arg <- full_args[grepl("^--file=", full_args)][1]
script_file <- sub("^--file=", "", script_arg)
script_dir <- if (!is.na(script_file) && nzchar(script_file)) {
  dirname(normalizePath(script_file))
} else {
  normalizePath(getwd())
}

project_root <- normalizePath(file.path(script_dir, ".."))

setwd(project_root)

args <- commandArgs(trailingOnly = TRUE)
remove_db <- "--all" %in% args

cat("============================================\n")
cat("         MeDUSA Uninstaller                 \n")
cat("============================================\n\n")
cat("Working directory temporarily set to project root:", getwd(), "\n")

# --- Step 1: Remove MeDUSA package ----------------------------------------

cat("--- Step 1/3: Removing MeDUSA package ---\n")
renv_lib_candidates <- c(
  Sys.glob(file.path(project_root, "renv", "library", "*", "*")),
  Sys.glob(file.path(project_root, "renv", "library", "*", "*", "*"))
)
candidate_libs <- unique(c(.libPaths(), renv_lib_candidates))

# Ensure package detection can see project-local renv libraries.
.libPaths(unique(c(candidate_libs, .libPaths())))
installed_libs <- candidate_libs[dir.exists(file.path(candidate_libs, "MeDUSA"))]

if (length(installed_libs) > 0) {
  removed <- 0
  for (pkg_lib in installed_libs) {
    tryCatch(
      {
        remove.packages("MeDUSA", lib = pkg_lib)
        removed <- removed + 1
      },
      error = function(e) {
        message("WARN: Could not remove MeDUSA from ", pkg_lib, ": ", conditionMessage(e))
      }
    )
  }
  if (removed > 0) {
    cat("MeDUSA package removed from", removed, "library location(s).\n")
  } else {
    cat("MeDUSA package found but could not be removed automatically.\n")
  }
} else {
  cat("MeDUSA package not found, skipping.\n")
}

# --- Step 2: Remove renv project library ----------------------------------

cat("\n--- Step 2/3: Removing renv project library ---\n")
renv_dir <- file.path(getwd(), "renv")
rprofile <- file.path(getwd(), ".Rprofile")

if (dir.exists(renv_dir)) {
  unlink(renv_dir, recursive = TRUE, force = TRUE)
  if (dir.exists(renv_dir)) {
    cat("WARN: renv/ could not be fully removed.\n")
    cat("This usually means files are locked by another R session or permissions are restricted.\n")
    cat("Close R/RStudio sessions using this project and retry uninstall.\n")
  } else {
    cat("renv/ directory removed.\n")
  }
} else {
  cat("renv/ directory not found, skipping.\n")
}

if (file.exists(rprofile)) {
  lines <- readLines(rprofile, warn = FALSE)
  renv_lines <- grepl("renv/activate.R", lines, fixed = TRUE)
  if (any(renv_lines)) {
    kept_lines <- lines[!renv_lines]
    if (length(kept_lines) == 0 || all(trimws(kept_lines) == "")) {
      file.remove(rprofile)
      cat(".Rprofile removed.\n")
    } else {
      writeLines(kept_lines, rprofile)
      cat("renv activation removed from .Rprofile; other entries preserved.\n")
    }
  }
}

lockfile <- file.path(getwd(), "renv.lock")
if (file.exists(lockfile)) {
  file.remove(lockfile)
  if (file.exists(lockfile)) {
    cat("WARN: renv.lock could not be removed.\n")
  } else {
    cat("renv.lock removed.\n")
  }
}

# --- Step 3: Remove databases (optional) ----------------------------------

cat("\n--- Step 3/3: Compound databases ---\n")
db_dir <- tools::R_user_dir("MeDUSA", "data")

if (remove_db) {
  if (dir.exists(db_dir)) {
    unlink(db_dir, recursive = TRUE, force = TRUE)
    cat("Database directory removed:", db_dir, "\n")
  } else {
    cat("No database directory found.\n")
  }
} else {
  if (dir.exists(db_dir)) {
    cat("Databases kept at:", db_dir, "\n")
    cat("To also remove databases, run: Rscript uninstall.R --all\n")
  } else {
    cat("No databases found.\n")
  }
}

cat("\n============================================\n")
cat("      MeDUSA uninstall complete.\n")
cat("============================================\n")
