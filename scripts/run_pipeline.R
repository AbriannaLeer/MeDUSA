## ============================================================================
## MeDUSA Pipeline — Run from CSV
## ============================================================================
## Usage:
##   1. Edit the CONFIGURATION section below with your file paths and settings.
##   2. Set RUN_STEP_* flags to control which steps to run vs load from checkpoint.
##   3. Run this entire script in R or RStudio, OR:
##      Rscript scripts/run_pipeline.R
## ============================================================================

# =============================================================================
# Timer utilities

# =============================================================================
SCRIPT_NAME <- tryCatch(
  {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "interactive"
  },
  error = function(e) "interactive"
)

PIPELINE_START <- Sys.time()

cat("============================================================\n")
cat("Script :", SCRIPT_NAME, "\n")
cat("Start  :", format(PIPELINE_START, "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================\n")

runtime_log <- data.frame(
  stage = character(),
  runtime_sec = numeric(),
  stringsAsFactors = FALSE
)

stage_timer <- function(stage_name, expr) {
  start <- Sys.time()
  out <- eval.parent(substitute(expr))
  end <- Sys.time()

  elapsed <- as.numeric(difftime(end, start, units = "secs"))

  cat(sprintf("  [Runtime] %s: %.3f sec\n", stage_name, elapsed))

  runtime_row <- data.frame(
    stage = stage_name,
    runtime_sec = elapsed,
    stringsAsFactors = FALSE
  )

  assign(
    "runtime_log",
    rbind(get("runtime_log", envir = .GlobalEnv), runtime_row),
    envir = .GlobalEnv
  )

  out
}

# Memory usage reporter — prints current RSS in MB
mem_report <- function(label = "") {
  invisible(gc(verbose = FALSE, full = TRUE))
  used_mb <- sum(gc()[, 2])
  cat(sprintf("  [Memory] %s: %.0f MB R-managed\n", label, used_mb))
}

# Load the needed tools to load packages locally
if (!requireNamespace("pkgload", quietly = TRUE)) install.packages("pkgload")
pkgload::load_all("MeDUSA")

# Load other libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Memory and hardware limits to avoid OOM crashes
options(mc.cores = 2)
options(Ncpus = 2)
Sys.setenv(MC_CORES = 2)
Sys.setenv(OMP_NUM_THREADS = 2)
if (requireNamespace("future", quietly = TRUE)) {
  future::plan("multisession", workers = 2)
}

# =============================================================================
# CONFIGURATION
# =============================================================================

# Path to your CSV file (long format with columns: mz, sample_name, intensity)
CSV_FILE <- "data/abrianna/positive.csv"

#CSV_FILE <- "data/sample/example_DIMS_positive_pascal.csv"
# CSV_FILE <- "data/full/full_DIMS_positive_pascal.csv"
# CSV_FILE <- "data/full/full_DIMS_negative_pascal.csv"

# Polarity prefix to add to sample names ("pos" or "neg")
POLARITY <- "pos"
# POLARITY <- "neg"

# Phenotype names for the volcano comparison (exactly 2 phenotypes)
PHENO_A <- "UMUC3"
PHENO_B <- "RT4"

# Step 0 — m/z rounding precision (decimals) before pivoting to wide format.
MZ_ROUND_DIGITS <- 3

# Step 6 — minimum intensity threshold.
#   Package default (mz_filter_magic, positive mode): 5000
#   Package default (mz_filter_magic, negative mode): 2000
STEP6_MIN_INTENSITY <- if (POLARITY == "pos") 5000 else 2000

# Step 6 — missingness threshold
#   Package default (mz_filter_magic / mz_filter_missingness): 0.1
STEP6_MISSINGNESS_THRES <- 0.10

# -----------------------------------------------------------------------------
# STEP SWITCHES
# TRUE  = run the step and save a checkpoint
# FALSE = skip the step and load its checkpoint from disk
# -----------------------------------------------------------------------------
RUN_STEP_0  <- TRUE
RUN_STEP_2  <- TRUE
RUN_STEP_3  <- TRUE
RUN_STEP_4  <- TRUE
RUN_STEP_5  <- TRUE
RUN_STEP_6  <- TRUE
RUN_STEP_7  <- TRUE
RUN_STEP_8A <- TRUE
#RUN_STEP_8B <- TRUE
#RUN_STEP_9  <- TRUE

# -----------------------------------------------------------------------------
# Optional subsampling
# -----------------------------------------------------------------------------
USE_SUBSAMPLE <- FALSE
SUBSAMPLE_PER_SAMPLE <- 25000
SUBSAMPLE_SEED <- 1

# =============================================================================
# Metadata builder
# =============================================================================
build_metadata <- function(sample_names) {
  type <- dplyr::case_when(
    grepl("blank", sample_names) ~ "blank",
    grepl("media", sample_names) ~ "media",
    grepl("qc", sample_names) ~ "qc",
    TRUE ~ "cell"
  )
  phenotype <- dplyr::case_when(
    grepl("UMUC3", sample_names) ~ "UMUC3",
    grepl("RT4", sample_names) ~ "RT4",
    TRUE ~ type
  )
  data.frame(
    sample_name = sample_names,
    type = type,
    phenotype = phenotype,
    filtered_out = FALSE,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Checkpoint utilities
# =============================================================================
script_base <- tools::file_path_sans_ext(basename(SCRIPT_NAME))
data_base <- tools::file_path_sans_ext(basename(CSV_FILE))
run_id <- paste(script_base, data_base, sep = "__")

out_dir <- file.path("output", Sys.Date())
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

checkpoint_dir <- file.path(out_dir, "checkpoints")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

checkpoint_path <- function(step_id) {
  file.path(checkpoint_dir, paste0(run_id, "_", step_id, ".rds"))
}

save_checkpoint <- function(step_id, var_names) {
  checkpoint_file <- checkpoint_path(step_id)

  # Only include variables that actually exist; silently drop NULL/missing ones
  existing <- var_names[vapply(var_names,
                               function(v) exists(v, envir = .GlobalEnv),
                               logical(1))]
  obj_list <- mget(existing, envir = .GlobalEnv, ifnotfound = list(NULL))
  names(obj_list) <- existing

  saveRDS(obj_list, checkpoint_file)
  cat("  [Checkpoint saved]", checkpoint_file, "\n")
}

load_checkpoint <- function(step_id) {
  checkpoint_file <- checkpoint_path(step_id)

  if (!file.exists(checkpoint_file)) {
    stop("Checkpoint not found: ", checkpoint_file)
  }

  obj_list <- readRDS(checkpoint_file)
  list2env(obj_list, envir = .GlobalEnv)

  cat("  [Checkpoint loaded]", checkpoint_file, "\n")
}

run_or_load_step <- function(step_id, stage_name, enabled, checkpoint_vars, expr) {
  if (enabled) {
    stage_timer(stage_name, expr)
    save_checkpoint(step_id, checkpoint_vars)
    gc(verbose = FALSE, full = TRUE)
    mem_report(paste("after", step_id))
  } else {
    cat("\n=== ", stage_name, " ===\n", sep = "")
    cat("  Step disabled. Loading checkpoint instead.\n")
    load_checkpoint(step_id)
    gc(verbose = FALSE, full = TRUE)
    mem_report(paste("loaded", step_id))
  }
}

# =============================================================================
# STEP 0: Load CSV and pivot to wide MZ-OBJ format
# =============================================================================
run_or_load_step(
  step_id = "step0",
  stage_name = "Step 0: Loading CSV and creating MZ-OBJ",
  enabled = RUN_STEP_0,
  checkpoint_vars = c("mz_obj", "metadata"),
  expr = {
    cat("\n=== Step 0: Loading CSV and creating MZ-OBJ ===\n")

    raw_all <- read.csv(CSV_FILE, stringsAsFactors = FALSE)

    if (USE_SUBSAMPLE) {
      set.seed(SUBSAMPLE_SEED)
      raw <- raw_all |>
        dplyr::group_by(sample_name) |>
        dplyr::group_modify(~ .x[
          sample(
            seq_len(nrow(.x)),
            min(SUBSAMPLE_PER_SAMPLE, nrow(.x))
          ), ,
          drop = FALSE
        ]) |>
        dplyr::ungroup()
      rm(raw_all)
      gc(full = TRUE)
    } else {
      raw <- raw_all
      rm(raw_all)
      gc(full = TRUE)
    }

    # Compress feature count by rounding mass values
    raw$mz <- round(raw$mz, MZ_ROUND_DIGITS)
    raw$sample_name <- paste0(POLARITY, "_", raw$sample_name)

    mz_obj <- raw |>
      select(mz, sample_name, intensity) |>
      pivot_wider(
        names_from = sample_name,
        values_from = intensity,
        values_fn = sum,
        values_fill = 0
      ) |>
      as.data.frame()

    unique_samples <- sort(unique(raw$sample_name))

    # Drop raw, we only need mz_obj from here on
    rm(raw)
    gc(full = TRUE)

    cat(
      "  MZ-OBJ created:", nrow(mz_obj), "features x",
      ncol(mz_obj) - 1, "samples (rounded to", MZ_ROUND_DIGITS, "decimals)\n"
    )

    meta_names <- sub(paste0("^", POLARITY, "_"), "", unique_samples)
    metadata <- build_metadata(meta_names)

    cat("  Metadata:", nrow(metadata), "samples\n")
    cat("  Types:", paste(unique(metadata$type), collapse = ", "), "\n")
    cat("  Phenotypes:", paste(unique(metadata$phenotype), collapse = ", "), "\n")
  }
)

# =============================================================================
# STEP 2: Quality Check
# =============================================================================
run_or_load_step(
  step_id = "step2",
  stage_name = "Step 2: Quality Check",
  enabled = RUN_STEP_2,
  checkpoint_vars = c("mz_obj", "metadata", "qc_metrics"),
  expr = {
    cat("\n=== Step 2: Quality Check ===\n")
    qc_metrics <- mz_quality_magic(mz_obj, metadata)
    print(head(qc_metrics))
  }
)

# =============================================================================
# STEP 3: Blank Subtraction
# =============================================================================
run_or_load_step(
  step_id = "step3",
  stage_name = "Step 3: Blank Subtraction",
  enabled = RUN_STEP_3,
  checkpoint_vars = c("mz_obj", "metadata", "qc_metrics"),
  expr = {
    cat("\n=== Step 3: Blank Subtraction ===\n")

    sample_mz <- mztools_filter(mz_obj, metadata, "blank", "type", exclude = TRUE)
    blank_mz  <- mztools_filter(mz_obj, metadata, "blank", "type", exclude = FALSE)

    if (ncol(blank_mz) > 1) {
      mz_obj <- mz_subtraction(sample_mz, blank_mz, threshold = 3)
      cat("  After subtraction:", nrow(mz_obj), "features remaining\n")
    } else {
      cat("  No blanks found, skipping subtraction\n")
      mz_obj <- sample_mz
    }
    rm(sample_mz, blank_mz)
    gc(full = TRUE)
  }
)

# =============================================================================
# STEP 4: Mass Defect Filter
# =============================================================================
run_or_load_step(
  step_id = "step4",
  stage_name = "Step 4: Mass Defect Filter",
  enabled = RUN_STEP_4,
  checkpoint_vars = c("mz_obj", "metadata", "qc_metrics"),
  expr = {
    cat("\n=== Step 4: Mass Defect Filter ===\n")
    mz_obj <- mz_mass_defect(mz_obj)
    cat("  After mass defect:", nrow(mz_obj), "features remaining\n")
  }
)

# =============================================================================
# STEP 5: Isotope Tagging
# =============================================================================
run_or_load_step(
  step_id = "step5",
  stage_name = "Step 5: Isotope Tagging",
  enabled = RUN_STEP_5,
  checkpoint_vars = c("mz_obj", "metadata", "qc_metrics", "isotopes"),
  expr = {
    cat("\n=== Step 5: Isotope Tagging ===\n")
    isotopes <- mz_tag_isotope_hunter(mz_obj)
    cat("  Isotope groups found:", length(isotopes), "\n")
  }
)

# =============================================================================
# STEP 6: Noise Filtering — STRICTER NOW, runs before heavy step 7
# =============================================================================
run_or_load_step(
  step_id = "step6",
  stage_name = "Step 6: Filtering (blacklist, missingness, intensity)",
  enabled = RUN_STEP_6,
  checkpoint_vars = c("mz_obj", "metadata", "qc_metrics", "isotopes"),
  expr = {
    cat("\n=== Step 6: Filtering (blacklist, missingness, intensity) ===\n")
    cat("  Using min_intensity =", STEP6_MIN_INTENSITY,
        ", missingness_threshold =", STEP6_MISSINGNESS_THRES, "\n")

    mz_obj <- mz_filter_magic(
      mz_obj,
      min_intensity         = STEP6_MIN_INTENSITY,
      missingness_threshold = STEP6_MISSINGNESS_THRES,
      blacklist             = TRUE
    )
    cat("  After filtering:", nrow(mz_obj), "features remaining\n")
  }
)

# =============================================================================
# STEP 7: Post-Processing (imputation, normalization, log2)
# =============================================================================
run_or_load_step(
  step_id = "step7",
  stage_name = "Step 7: Post-Processing",
  enabled = RUN_STEP_7,
  checkpoint_vars = c("mz_obj", "metadata", "post", "mzLong", "mzLog"),
  expr = {
    cat("\n=== Step 7: Post-Processing ===\n")

    # Free large upstream objects we no longer need in this step
    if (exists("isotopes",   envir = .GlobalEnv)) rm("isotopes",   envir = .GlobalEnv)
    if (exists("qc_metrics", envir = .GlobalEnv)) rm("qc_metrics", envir = .GlobalEnv)
    gc(full = TRUE)
    mem_report("before mz_post_magic")

    metadata <- metadata[metadata$type != "blank", ]

    post   <- mz_post_magic(mz_obj, metadata, plot = FALSE)
    mzLong <- post$mzLong
    mzLog  <- post$mzLog

    cat("  mzLog:",  nrow(mzLog),  "features x", ncol(mzLog) - 1, "samples\n")
    cat("  mzLong:", nrow(mzLong), "rows\n")

    # Drop the (now redundant) wide pre-normalization matrix
    rm(mz_obj)
    gc(full = TRUE)
  }
)

# =============================================================================
# STEP 8a: Volcano Analysis
# =============================================================================
run_or_load_step(
  step_id = "step8a",
  stage_name = "Step 8a: Volcano Analysis",
  enabled = RUN_STEP_8A,
  checkpoint_vars = c("metadata", "post", "mzLong", "mzLog", "volcano"),
  expr = {
    cat("\n=== Step 8a: Volcano Analysis ===\n")

    phenoA_data <- mztools_filter(mzLog, metadata, PHENO_A)
    phenoB_data <- mztools_filter(mzLog, metadata, PHENO_B)

    volcano <- mzlog_analysis_volcano_magic(phenoA_data, phenoB_data, mzLog, metadata)

    cat("  Significant features (Welch):", sum(volcano$welch$p_05), "\n")

    rm(phenoA_data, phenoB_data)
    gc(full = TRUE)
  }
)

# =============================================================================
# STEP 8b: Random Forest
# =============================================================================
# run_or_load_step(
#   step_id = "step8b",
#   stage_name = "Step 8b: Random Forest",
#   enabled = RUN_STEP_8B,
#   checkpoint_vars = c(
#     "metadata", "post", "mzLong", "mzLog",
#     "volcano", "rf_result", "important_mz"
#   ),
#   expr = {
#     cat("\n=== Step 8b: Random Forest ===\n")
# 
#     rf_result <- NULL
# 
#     cell_meta   <- metadata[metadata$phenotype %in% c(PHENO_A, PHENO_B), ]
#     mzLog_cells <- mztools_filter(mzLog, cell_meta, c(PHENO_A, PHENO_B))
# 
#     n_before <- nrow(mzLog_cells)
#     feature_cols <- setdiff(colnames(mzLog_cells), "mz")
#     finite_rows <- apply(
#       mzLog_cells[, feature_cols], 1,
#       function(r) all(is.finite(r))
#     )
#     mzLog_cells <- mzLog_cells[finite_rows, ]
# 
#     cat(
#       "  Removed", n_before - nrow(mzLog_cells), "non-finite features;",
#       nrow(mzLog_cells), "remaining\n"
#     )
# 
#     rf_matrix   <- data.frame(t(dplyr::select(mzLog_cells, -mz)))
#     feature_ids <- as.character(mzLog_cells$mz)
#     cor_cutoff  <- 0.75
# 
#     if (ncol(rf_matrix) > 1) {
#       cor_mat      <- cor(rf_matrix)
#       high_cor_idx <- caret::findCorrelation(cor_mat, cutoff = cor_cutoff)
#     } else {
#       high_cor_idx <- integer(0)
#     }
# 
#     keep_idx            <- setdiff(seq_along(feature_ids), high_cor_idx)
#     rf_matrix           <- rf_matrix[, keep_idx, drop = FALSE]
#     colnames(rf_matrix) <- feature_ids[keep_idx]
#     cor_data            <- cbind(sample_name = rownames(rf_matrix), rf_matrix)
# 
#     cat(
#       "  RF correlation filter removed", length(high_cor_idx), "features;",
#       ncol(cor_data) - 1, "remaining\n"
#     )
# 
#     if (ncol(cor_data) <= 1) {
#       stop("No RF features remain after correlation filtering.")
#     }
# 
#     n_features <- ncol(cor_data) - 1L
#     feat_size_seq <- seq(50, min(500, n_features), by = 50)
#     if (length(feat_size_seq) == 0) feat_size_seq <- min(10, n_features)
# 
#     rfe_result <- tryCatch(
#       mzlog_rf_select(cor_data, cell_meta, feat_size_seq = feat_size_seq),
#       error = function(e) {
#         cat("  RF select failed with default feature sizes; retrying with smaller sizes...\n")
#         fallback_sizes <- unique(pmax(2, round(seq(10, min(100, n_features), by = 10))))
#         mzlog_rf_select(cor_data, cell_meta, feat_size_seq = fallback_sizes)
#       }
#     )
# 
#     rf_result <- tryCatch(
#       {
#         out <- mzlog_rf(mzLog_cells, cell_meta, rfe_result, plot = FALSE)
#         rf_validate(out)
#         rf_permuted(out, POLARITY)
#         out
#       },
#       error = function(e) {
#         cat("  Random Forest step failed; continuing with volcano-only identification.\n")
#         cat("  RF error:", conditionMessage(e), "\n")
#         NULL
#       }
#     )
# 
#     important_mz <- numeric(0)
#     if (!is.null(rf_result) &&
#         !is.null(rf_result$imp_mz) &&
#         "mz" %in% colnames(rf_result$imp_mz)) {
#       important_mz <- as.numeric(rf_result$imp_mz$mz)
#     }
#     important_mz <- important_mz[is.finite(important_mz)]
# 
#     cat("  Important features from RF:", length(important_mz), "\n")
# 
#     rm(mzLog_cells, rf_matrix, cor_mat, cor_data)
#     gc(full = TRUE)
#   }
# )

# =============================================================================
# STEP 9: Compound Identification
# =============================================================================
# run_or_load_step(
#   step_id = "step9",
#   stage_name = "Step 9: Compound Identification",
#   enabled = RUN_STEP_9,
#   checkpoint_vars = c(
#     "metadata", "post", "mzLong", "mzLog",
#     "volcano", "rf_result", "important_mz", "sig_mz", "all_important_mz",
#     "hmdb_hits", "lipid_hits"
#   ),
#   expr = {
#     cat("\n=== Step 9: Compound Identification ===\n")
# 
#     sig_mz <- as.numeric(volcano$welch$mz[volcano$welch$p_05])
#     sig_mz <- sig_mz[is.finite(sig_mz)]
# 
#     all_important_mz <- unique(c(sig_mz, important_mz))
#     all_important_mz <- all_important_mz[is.finite(all_important_mz)]
# 
#     if (length(all_important_mz) > 0) {
#       cat("  Identifying", length(all_important_mz), "features...\n")
# 
#       hmdb_hits  <- identify_hmdb(all_important_mz, adducts = c("M+H", "M+Na"))
#       lipid_hits <- identify_lipids(all_important_mz, adducts = c("M+H", "M+Na"))
# 
#       cat("  HMDB hits:",  nrow(hmdb_hits),  "\n")
#       cat("  Lipid hits:", nrow(lipid_hits), "\n")
# 
#       write.csv(hmdb_hits,  file.path(out_dir, "hmdb_hits.csv"),  row.names = FALSE)
#       write.csv(lipid_hits, file.path(out_dir, "lipid_hits.csv"), row.names = FALSE)
# 
#       cat("  Results saved to", out_dir, "\n")
#     } else {
#       hmdb_hits  <- data.frame()
#       lipid_hits <- data.frame()
#       cat("  No significant features found for identification.\n")
#     }
#   }
# )

cat("\n=== Pipeline complete! ===\n")

PIPELINE_END <- Sys.time()
TOTAL_RUNTIME <- as.numeric(difftime(PIPELINE_END, PIPELINE_START, units = "secs"))

cat("\n============================================================\n")
cat("Script :", SCRIPT_NAME, "\n")
cat("End    :", format(PIPELINE_END, "%Y-%m-%d %H:%M:%S"), "\n")
cat(sprintf("Total runtime : %.3f sec\n", TOTAL_RUNTIME))
cat("============================================================\n")

# =============================================================================
# Export runtime table + visualization
# =============================================================================
source("scripts/runtime_report.R")