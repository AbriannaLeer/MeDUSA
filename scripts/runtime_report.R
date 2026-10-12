# =============================================================================
# Runtime report and baseline comparison
# =============================================================================
# This section:
#   1. exports runtime summary for the current run
#   2. creates runtime plots
#   3. optionally creates a baseline runtime file
#   4. compares current runtime against baseline
#
# Output files:
#   output/<YYYY-MM-DD>/<run_id>_runtime_summary.csv
#   output/<YYYY-MM-DD>/<run_id>_runtime_plot.png
#   output/<YYYY-MM-DD>/<run_id>_baseline_comparison.csv
#   output/<YYYY-MM-DD>/<run_id>_baseline_comparison_plot.png
#   output/baseline/<data_base>_baseline_runtime.csv
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
AUTO_CREATE_BASELINE <- TRUE

# -----------------------------------------------------------------------------
# Paths and identifiers
# -----------------------------------------------------------------------------
out_dir <- file.path("output", Sys.Date())
baseline_dir <- file.path("output", "baseline")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(baseline_dir, showWarnings = FALSE, recursive = TRUE)

script_base <- tools::file_path_sans_ext(basename(SCRIPT_NAME))
data_base <- tools::file_path_sans_ext(basename(CSV_FILE))
run_id <- paste(script_base, data_base, sep = "__")

current_csv_file <- file.path(out_dir, paste0(run_id, "_runtime_summary.csv"))
current_plot_file <- file.path(out_dir, paste0(run_id, "_runtime_plot.png"))
comparison_csv_file <- file.path(out_dir, paste0(run_id, "_baseline_comparison.csv"))
comparison_plot_file <- file.path(out_dir, paste0(run_id, "_baseline_comparison_plot.png"))

baseline_csv <- file.path(
  baseline_dir,
  paste0(data_base, "_baseline_runtime.csv")
)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------
create_baseline <- function(current_csv, baseline_csv, overwrite = FALSE) {
  dir.create(dirname(baseline_csv), showWarnings = FALSE, recursive = TRUE)

  if (file.exists(baseline_csv) && !overwrite) {
    cat("  Baseline already exists, not overwriting:\n")
    cat("   ", baseline_csv, "\n")
    return(invisible(FALSE))
  }

  ok <- file.copy(current_csv, baseline_csv, overwrite = TRUE)

  if (ok) {
    cat("  Baseline created at:\n")
    cat("   ", baseline_csv, "\n")
  } else {
    warning("Failed to create baseline at: ", baseline_csv)
  }

  invisible(ok)
}

build_runtime_export <- function(runtime_log, total_runtime) {
  runtime_summary <- runtime_log
  runtime_summary$runtime_min <- runtime_summary$runtime_sec / 60

  total_row <- data.frame(
    stage = "Total Pipeline Runtime",
    runtime_sec = total_runtime,
    runtime_min = total_runtime / 60,
    stringsAsFactors = FALSE
  )

  runtime_export <- rbind(runtime_summary, total_row)

  list(
    runtime_summary = runtime_summary,
    runtime_export = runtime_export
  )
}

save_csv_report <- function(df, path, label) {
  write.csv(df, path, row.names = FALSE)
  cat("  ", label, " saved to: ", path, "\n", sep = "")
}

save_plot_report <- function(plot_obj, path, width, height, dpi = 300, label) {
  ggsave(
    filename = path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
  cat("  ", label, " saved to: ", path, "\n", sep = "")
}

make_runtime_plot <- function(runtime_summary, script_name, csv_file) {
  ggplot(runtime_summary, aes(x = reorder(stage, runtime_sec), y = runtime_sec)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "MeDUSA Pipeline Runtime by Stage",
      subtitle = paste(
        "Script:", basename(script_name),
        "| Data:", basename(csv_file)
      ),
      x = "Stage",
      y = "Runtime (seconds)"
    ) +
    theme_minimal(base_size = 12)
}

build_comparison_table <- function(baseline_df, current_df) {
  comparison_df <- merge(
    baseline_df[, c("stage", "runtime_sec")],
    current_df[, c("stage", "runtime_sec")],
    by = "stage",
    suffixes = c("_baseline", "_current"),
    all = TRUE
  )

  comparison_df$delta_sec <- comparison_df$runtime_sec_current - comparison_df$runtime_sec_baseline

  comparison_df$pct_change_vs_baseline <- ifelse(
    !is.na(comparison_df$runtime_sec_baseline) & comparison_df$runtime_sec_baseline != 0,
    (comparison_df$runtime_sec_current - comparison_df$runtime_sec_baseline) /
      comparison_df$runtime_sec_baseline * 100,
    NA_real_
  )

  comparison_df$relative_to_baseline <- ifelse(
    is.na(comparison_df$pct_change_vs_baseline),
    NA_character_,
    ifelse(
      comparison_df$pct_change_vs_baseline < 0,
      paste0(sprintf("%.2f", abs(comparison_df$pct_change_vs_baseline)), "% faster"),
      paste0(sprintf("%.2f", comparison_df$pct_change_vs_baseline), "% slower")
    )
  )

  comparison_df
}

print_total_runtime_comparison <- function(comparison_df) {
  total_cmp <- comparison_df[comparison_df$stage == "Total Pipeline Runtime", ]

  if (nrow(total_cmp) != 1 ||
    is.na(total_cmp$runtime_sec_baseline) ||
    is.na(total_cmp$runtime_sec_current)) {
    return(invisible(NULL))
  }

  baseline_total <- total_cmp$runtime_sec_baseline
  current_total <- total_cmp$runtime_sec_current
  delta_total <- current_total - baseline_total
  pct_total <- (delta_total / baseline_total) * 100

  cat("\n============================================================\n")
  cat(sprintf("Baseline total runtime : %.3f sec\n", baseline_total))
  cat(sprintf("Current total runtime  : %.3f sec\n", current_total))
  cat(sprintf("Delta                  : %+0.3f sec\n", delta_total))

  if (pct_total < 0) {
    cat(sprintf("Change                 : %.2f%% faster than baseline\n", abs(pct_total)))
  } else {
    cat(sprintf("Change                 : %.2f%% slower than baseline\n", pct_total))
  }

  cat("============================================================\n")
}

make_comparison_plot <- function(comparison_df, script_name, csv_file) {
  plot_df <- comparison_df |>
    dplyr::filter(stage != "Total Pipeline Runtime") |>
    tidyr::pivot_longer(
      cols = c(runtime_sec_baseline, runtime_sec_current),
      names_to = "run_type",
      values_to = "runtime_sec"
    )

  plot_df$run_type <- dplyr::recode(
    plot_df$run_type,
    runtime_sec_baseline = "Baseline",
    runtime_sec_current = "Current"
  )

  ggplot(plot_df, aes(x = reorder(stage, runtime_sec), y = runtime_sec, fill = run_type)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(
      title = "Baseline vs Current Runtime by Stage",
      subtitle = paste(
        "Script:", basename(script_name),
        "| Data:", basename(csv_file)
      ),
      x = "Stage",
      y = "Runtime (seconds)",
      fill = "Run"
    ) +
    theme_minimal(base_size = 12)
}

# -----------------------------------------------------------------------------
# Current runtime export
# -----------------------------------------------------------------------------
runtime_objects <- build_runtime_export(runtime_log, TOTAL_RUNTIME)
runtime_summary <- runtime_objects$runtime_summary
runtime_export <- runtime_objects$runtime_export

save_csv_report(runtime_export, current_csv_file, "Runtime table")

p_current <- make_runtime_plot(runtime_summary, SCRIPT_NAME, CSV_FILE)
save_plot_report(p_current, current_plot_file, width = 10, height = 6, label = "Runtime plot")

cat("  Looking for baseline CSV at: ", baseline_csv, "\n", sep = "")

# -----------------------------------------------------------------------------
# Auto-create baseline if missing
# -----------------------------------------------------------------------------
if (!file.exists(baseline_csv) && isTRUE(AUTO_CREATE_BASELINE)) {
  create_baseline(current_csv_file, baseline_csv, overwrite = FALSE)
}

# -----------------------------------------------------------------------------
# Baseline comparison
# -----------------------------------------------------------------------------
if (!file.exists(baseline_csv)) {
  cat("  Baseline CSV not found, skipping baseline comparison.\n")
  cat("  Expected baseline path: ", baseline_csv, "\n", sep = "")
  cat("  You can create it by running:\n")
  cat("    create_baseline(current_csv_file, baseline_csv)\n")
} else {
  baseline_df <- read.csv(baseline_csv, stringsAsFactors = FALSE)

  comparison_df <- build_comparison_table(baseline_df, runtime_export)
  save_csv_report(comparison_df, comparison_csv_file, "Baseline comparison")

  print_total_runtime_comparison(comparison_df)

  p_compare <- make_comparison_plot(comparison_df, SCRIPT_NAME, CSV_FILE)
  save_plot_report(
    p_compare,
    comparison_plot_file,
    width = 11,
    height = 6,
    label = "Baseline comparison plot"
  )
}
