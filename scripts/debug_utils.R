# =============================================================================
# Debug utilities for MeDUSA pipeline
# =============================================================================
# This file provides helper functions to inspect and debug pipeline checkpoints.
#
# -----------------------------------------------------------------------------
# How to use
# -----------------------------------------------------------------------------
# 1. Load this file in your R session:
#
#    source("scripts/debug_utils.R")
#
# 2. Inspect a checkpoint (without loading into environment):
#
#    inspect_checkpoint("step4")
#
#    This will:
#      - show stored objects
#      - print structure
#      - check mz_obj / metadata consistency
#
# 3. Load + inspect a checkpoint:
#
#    inspect_step("step6")
#
#    This will:
#      - load checkpoint variables into global environment
#      - run inspection checks
#
# 4. Inspect a checkpoint from a specific output date:
#
#    inspect_checkpoint("step4", output_date = "2026-03-27")
#
# -----------------------------------------------------------------------------
# Requirements
# -----------------------------------------------------------------------------
# - Checkpoints are expected under:
#     output/<YYYY-MM-DD>/checkpoints/
#
# - If today's checkpoint is not found, the script will automatically try the
#   most recent dated output directory under output/
#
# - This script assumes the following variables exist (or uses defaults):
#     SCRIPT_NAME
#     CSV_FILE
#
# -----------------------------------------------------------------------------
# Typical debugging workflow
# -----------------------------------------------------------------------------
# If pipeline crashes at Step N:
#
#   inspect_checkpoint("step<N-1>")
#
# Example:
#   if Step 7 crashes, run:
#
#   inspect_checkpoint("step6")
#
# This helps identify:
#   - empty mz_obj
#   - metadata mismatch
#   - sample naming issues
#
# =============================================================================

# -----------------------------------------------------------------------------
# Needed globals
# -----------------------------------------------------------------------------
if (!exists("SCRIPT_NAME")) SCRIPT_NAME <- "run_pipeline.R"
if (!exists("CSV_FILE")) CSV_FILE <- "data/sample/example_DIMS_positive_pascal.csv"

# -----------------------------------------------------------------------------
# Helper: find latest dated output directory
# -----------------------------------------------------------------------------
get_latest_output_dir <- function(output_root = "output") {
  if (!dir.exists(output_root)) {
    stop("Output directory not found: ", output_root)
  }

  subdirs <- list.dirs(output_root, full.names = TRUE, recursive = FALSE)
  dir_names <- basename(subdirs)

  # Keep only YYYY-MM-DD directories
  valid_idx <- grepl("^\\d{4}-\\d{2}-\\d{2}$", dir_names)

  subdirs <- subdirs[valid_idx]
  dir_names <- dir_names[valid_idx]

  if (length(subdirs) == 0) {
    stop("No dated output directories found under: ", output_root)
  }

  dir_dates <- as.Date(dir_names)
  subdirs[which.max(dir_dates)]
}

# -----------------------------------------------------------------------------
# Helper: build checkpoint path
# -----------------------------------------------------------------------------
checkpoint_path <- function(step_id, use_latest = TRUE, output_date = NULL) {
  script_base <- tools::file_path_sans_ext(basename(SCRIPT_NAME))
  data_base <- tools::file_path_sans_ext(basename(CSV_FILE))
  run_id <- paste(script_base, data_base, sep = "__")

  # If user explicitly provides a date, use it directly
  if (!is.null(output_date)) {
    checkpoint_dir <- file.path("output", output_date, "checkpoints")
    return(file.path(checkpoint_dir, paste0(run_id, "_", step_id, ".rds")))
  }

  # First try today's checkpoint
  today_dir <- file.path("output", Sys.Date(), "checkpoints")
  today_file <- file.path(today_dir, paste0(run_id, "_", step_id, ".rds"))

  if (file.exists(today_file)) {
    return(today_file)
  }

  # If not found, fall back to latest dated output directory
  if (isTRUE(use_latest)) {
    latest_output_dir <- get_latest_output_dir("output")
    checkpoint_dir <- file.path(latest_output_dir, "checkpoints")
    return(file.path(checkpoint_dir, paste0(run_id, "_", step_id, ".rds")))
  }

  # Otherwise just return today's expected path
  today_file
}

# -----------------------------------------------------------------------------
# Inspect checkpoint contents
# -----------------------------------------------------------------------------
inspect_checkpoint <- function(step_id, use_latest = TRUE, output_date = NULL) {
  checkpoint_file <- checkpoint_path(
    step_id = step_id,
    use_latest = use_latest,
    output_date = output_date
  )

  if (!file.exists(checkpoint_file)) {
    stop("Checkpoint not found: ", checkpoint_file)
  }

  cat("========================================\n")
  cat("Inspecting checkpoint:", checkpoint_file, "\n")
  cat("========================================\n")

  obj <- readRDS(checkpoint_file)

  cat("\n[Objects stored]\n")
  print(names(obj))

  cat("\n[Structure]\n")
  str(obj, max.level = 1)

  # mz_obj info
  if ("mz_obj" %in% names(obj)) {
    cat("\n[mz_obj]\n")
    cat("  Rows:", nrow(obj$mz_obj), "\n")
    cat("  Cols:", ncol(obj$mz_obj), "\n")
    cat("  Column names (first 5):\n")
    print(head(colnames(obj$mz_obj), 5))
  }

  # metadata info
  if ("metadata" %in% names(obj)) {
    cat("\n[metadata]\n")
    cat("  Rows:", nrow(obj$metadata), "\n")
    cat("  Types:", paste(unique(obj$metadata$type), collapse = ", "), "\n")
    cat("  Phenotypes:", paste(unique(obj$metadata$phenotype), collapse = ", "), "\n")
  }

  # consistency check
  if (all(c("mz_obj", "metadata") %in% names(obj))) {
    mz_cols <- colnames(obj$mz_obj)[-1]
    meta_samples <- obj$metadata$sample_name

    mz_cols_clean <- sub("^pos_|^neg_", "", mz_cols)
    mismatch <- setdiff(mz_cols_clean, meta_samples)

    cat("\n[Consistency check]\n")
    if (length(mismatch) == 0) {
      cat("  ✔ mz_obj and metadata are aligned\n")
    } else {
      cat("  ✖ Mismatch detected! Example:\n")
      print(head(mismatch))
    }
  }

  cat("\nDone.\n")
}

# -----------------------------------------------------------------------------
# Load checkpoint into global environment
# -----------------------------------------------------------------------------
load_checkpoint_debug <- function(step_id, use_latest = TRUE, output_date = NULL) {
  checkpoint_file <- checkpoint_path(
    step_id = step_id,
    use_latest = use_latest,
    output_date = output_date
  )

  if (!file.exists(checkpoint_file)) {
    stop("Checkpoint not found: ", checkpoint_file)
  }

  obj_list <- readRDS(checkpoint_file)
  list2env(obj_list, envir = .GlobalEnv)

  cat("  [Checkpoint loaded]", checkpoint_file, "\n")
}

# -----------------------------------------------------------------------------
# Shortcut: load + inspect
# -----------------------------------------------------------------------------
inspect_step <- function(step_id, use_latest = TRUE, output_date = NULL) {
  load_checkpoint_debug(
    step_id = step_id,
    use_latest = use_latest,
    output_date = output_date
  )

  inspect_checkpoint(
    step_id = step_id,
    use_latest = use_latest,
    output_date = output_date
  )
}
