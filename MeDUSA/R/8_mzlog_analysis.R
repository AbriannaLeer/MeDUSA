# *** PCA -----------------------------------------------------
#' MZLOG-OBJ PCA
#'
#' @description
#' A PCA plot is a relatively simple and well received method to compare samples
#' and determine if they can be grouped by phenotype. The mzlog_analysis_pca
#' function scales the log transformed data, removes any blacklisted m/z from
#' consideration, and incorporates the metadata to create a PCA plot that is
#' colored by phenotype for easy interpretation.
#'
#'  - Requires: ggplot2, tibble
#'
#' @param input_mzlog_obj \cr
#'   DataFrame : Log2 of Input MZ-Obj
#' @param metadata \cr
#'   DataFrame : MZ_metadata
#' @param qual_col \cr
#'   String    : relevant column to group by
#' @param plot_title \cr
#'   String    : Title of the plot.
#' @param sample_blacklist \cr
#'   List      : c("bad1", "bad2")
#' @examples
#' mzlog_analysis_pca(input_mzlog_obj, metadata) # run pca analysis with standard parameters
#' mzlog_analysis_pca(input_mzlog_obj, metadata, qual_col = "type", plot_title = "Science!", sample_blacklist = c(100.123, 200.345)) # run pca analysis with custom parameters
#' @return
#' null (only plots)
#' @export
mzlog_analysis_pca <- function(input_mzlog_obj, metadata, qual_col = "phenotype", plot_title = "PCA", sample_blacklist = c()) {
  metadata <- local.meta_polarity_fixer(input_mzlog_obj, metadata)

  mz_data <- as.matrix(input_mzlog_obj[, -which(names(input_mzlog_obj) == "mz")])
  rownames(mz_data) <- input_mzlog_obj$mz

  keep_samples <- setdiff(colnames(mz_data), sample_blacklist)
  t_mz_obj <- t(mz_data[, keep_samples])

  pca_res <- prcomp(t_mz_obj, scale. = TRUE)
  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)

  plot_df <- as.data.frame(pca_res$x)
  plot_df$sample_name <- rownames(plot_df)

  meta_subset <- metadata[match(plot_df$sample_name, metadata$sample_name), ]
  plot_df[[qual_col]] <- meta_subset[[qual_col]]

  ggpubr::ggscatter(plot_df,
    x = "PC1", y = "PC2",
    title = paste("PCA:", local.mz_polarity_guesser(input_mzlog_obj)),
    color = qual_col, size = 0.5, ellipse = TRUE, mean.point = TRUE,
    xlab = paste0("PC1: ", round(var_explained[1] * 100, 1), "%"),
    ylab = paste0("PC2: ", round(var_explained[2] * 100, 1), "%")
  )

  local.save_plot(paste(plot_title, local.mz_polarity_guesser(input_mzlog_obj), sep = "-"))
}
# *** Welch [T-Test] -----------------------------------------------------
#' MZLOG-OBJ T-Test
#'
#' @description
#' To determine if there is a statistical difference between phenotypes, the
#' Welch T-rest is often used. The user is expected to isolate two phenotypes in
#' the data (example provided for code to isolate) to pass into the
#' mzlog_analysis_welch function, which uses Welch t-test function and an FDR
#' correction to output the p-vale for each m/z.
#'
#'  - Requires: dplyr
#''
#' @param phenoA_mz_obj \cr
#'   DataFrame: mz_obj of phenoA samples
#' @param phenoB_mz_obj \cr
#'   DataFrame: mz_obj of phenoB samples
#' @param adjust_method \cr
#'   String: How to p.adjust?
#'   Boolean: False = no adjustment ; True = "fdr"
#' @param cores
#'   Integer: Can I has multithreading? (Need parallel)
#' Dependencies : pbapply, dplyr
#' @examples
#' # To t.test values from "phenoA" over "phenoB"
#' phenoA_mz_obj <- MeDUSA::mztools_filter(input_mzObj, metadata, "phenoA")
#' phenoB_mz_obj <- MeDUSA::mztools_filter(input_mzObj, metadata, "phenoB")
#' mzlog_analysis_welch(phenotype_a, phenotype_b)
#' @return DataFrame with columns:
#'  - mz      : Float: MZ
#'  - p       : Float: p-value
#'  - p_05    : Boolean: is P < 0.05
#'  - p_10    : Boolean: is P < 0.10
#'  - p_15    : Boolean: is P < 0.15
#'  - adjusted: p.adjust($p, adjust_method) *optional, but default
#' @export
mzlog_analysis_welch <- function(phenoA_mz_obj, phenoB_mz_obj, adjust = "fdr", cores = 2) {
  df_l <- local.ensure_mz(phenoA_mz_obj, phenoB_mz_obj, "MeDUSA::mzlog_analysis_welch")

  matA <- as.matrix(df_l$df_a)
  matB <- as.matrix(df_l$df_b)

  n1 <- ncol(matA)
  n2 <- ncol(matB)

  mu1 <- rowMeans(matA, na.rm = TRUE)
  mu2 <- rowMeans(matB, na.rm = TRUE)

  var1 <- matrixStats::rowVars(matA, na.rm = TRUE)
  var2 <- matrixStats::rowVars(matB, na.rm = TRUE)

  v1_n1 <- var1 / n1
  v2_n2 <- var2 / n2
  v_total <- v1_n1 + v2_n2

  t_stat <- (mu1 - mu2) / sqrt(v_total)

  df_num <- v_total^2
  df_den <- (v1_n1^2 / (n1 - 1)) + (v2_n2^2 / (n2 - 1))
  df <- df_num / df_den

  p_vals <- 2 * pt(-abs(t_stat), df)

  out <- data.frame(
    mz = df_l$mz,
    p = p_vals,
    p_05 = p_vals < 0.05,
    p_10 = p_vals < 0.10,
    p_15 = p_vals < 0.15
  )

  if (adjust != FALSE) {
    method <- if (isTRUE(adjust)) "fdr" else adjust
    out$adjusted <- p.adjust(p_vals, method = method)
  }

  return(out)
}


# *** Fold [change] -----------------------------------------------------
#' MZLOG-OBJ Fold Change
#'
#' @description
#' Fold change is one method of comparing two phenotypes, dividing one phenotype
#' by the other. The user is expected to isolate two phenotypes in the data
#' (example provided for code to isolate) to pass into the mzlog_analysis_fold
#' function, which will calculate the mean intensity (default, user defined) of
#' each phenotype per m/z. The mean of one phenotype is then divided by the
#' mean of the other phenotype, yielding one unitless fold change value for
#' each m/z. This allows the user to determine which m/z values are highly
#' up/down regulated or mildly up/down regulated.
#'
#' @param phenoA_mz_obj \cr
#'   DataFrame: mz_obj of phenoA samples
#' @param phenoB_mz_obj \cr
#'   DataFrame: mz_obj of phenoB samples
#' @param fold_math
#'   Method: How to combine Phenotype intensities
#'
#' @examples
#' # To fold values from "phenoA" over "phenoB"
#' phenoA_mz_obj <- MeDUSA::mztools_filter(input_mzObj, metadata, "phenoA")
#' phenoB_mz_obj <- MeDUSA::mztools_filter(input_mzObj, metadata, "phenoB")
#' mzlog_analysis_volcano_magic(phenotype_a, phenotype_b) # run volcano with custom values
#' mzlog_analysis_volcano_magic(phenotype_a, phenotype_b, fold_math = "median") # run volcano with median
#' mzlog_analysis_volcano_magic(phenotype_a, phenotype_b, fold_math = "function(x) x*2") # run volcano with custom apply function (untested)
#' @return DataFrame with columns:
#'  - mz   : Float
#'  - fold : Float
#' @export
mzlog_analysis_fold <- function(phenoA_mz_obj, phenoB_mz_obj, fold_math = "mean") {
  df_l <- local.ensure_mz(phenoA_mz_obj, phenoB_mz_obj, "MeDUSA::mzlog_analysis_fold")

  if (fold_math == "mean") {
    valA <- rowMeans(df_l$df_a, na.rm = TRUE)
    valB <- rowMeans(df_l$df_b, na.rm = TRUE)
  } else if (fold_math == "median") {
    valA <- matrixStats::rowMedians(as.matrix(df_l$df_a), na.rm = TRUE)
    valB <- matrixStats::rowMedians(as.matrix(df_l$df_b), na.rm = TRUE)
  } else {
    # Fallback for custom functions
    valA <- apply(df_l$df_a, 1, fold_math)
    valB <- apply(df_l$df_b, 1, fold_math)
  }

  return(data.frame(mz = df_l$mz, fold = valA - valB))
}
# *** Volcano Magic -----------------------------------------------------
#' Volcano plot from filtered mz data
#'
#' @description
#' A volcano plot plots the fold change on the x-axis and the p-value of the
#' t-test on the y-axis. The function mz_analysis_volcano_magic uses the m/z
#' data set and performs fold change and t-test on the data to populate the
#' plot.
#' Also runs PCA along the way.
#'
#' @param phenotype_a \cr
#'   DataFrame: Filtered metadata for phenotypeA
#' @param phenotype_b \cr
#'   DataFrame: Filtered metadata for phenotypeB
#' @param cores
#'   Integer: Can I has multithreading? (Need parallel)
#' @param input_mzlog_obj \cr
#'   DataFrame : Log2 of Input MZ-Obj
#' @param metadata
#'   DataFrame : Metadata-Obj of Samples
#' @examples
#' phenotype_a <- mztools_filter(mzLog, metadata, "PhenoA")
#' phenotype_b <- mztools_filter(mzLog, metadata, "PhenoB")
#' mzlog_analysis_volcano_magic(phenotype_a, phenotype_b, mzLog, metadata)
#' @return list of mzlog_analysis_welch, mzlog_analysis_fold and plot_volcano  return objects
#' @export
mzlog_analysis_volcano_magic <- function(phenotype_a, phenotype_b, input_mzlog_obj, metadata, cores = 2) {
  welch <- mzlog_analysis_welch(phenotype_a, phenotype_b, cores = cores)

  mzlog_analysis_pca(input_mzlog_obj[welch$p_05, ], metadata, plot_title = "PCA_Welch")

  fold <- mzlog_analysis_fold(phenotype_a, phenotype_b)

  title <- paste("VolcanoPlot", local.mz_polarity_guesser(phenotype_a), sep = "-")
  volcano <- plot_volcano(welch, fold, title)

  mzlog_analysis_pca(input_mzlog_obj[volcano$diff != "NONE", ], metadata, plot_title = "PCA_Volcano")

  return(list("welch" = welch, "fold" = fold, "volcano" = volcano))
}

# *** Heat Map  -----------------------------------------------------
#' MzLog Generate Heat Map
#'
#' @description
#' A heat map is a great way to visualize how a large data set compares over
#' many metabolites. The mzlog_analysis_heatmap function can be used at any
#' point in the processing pipeline to evaluate the phenotypes over all m/z values.
#' It is recommended to only input the data after processing.
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @param metadata \cr
#'   DataFrame : Metadata-Obj of Samples ( Note: metadata and samples must match. E.g. no blanks)
#' @param annotation \cr
#'   String: Colname of metadata to annotate on; requires metadata
#' @param title
#'   String: Plot title
#' @param save_file
#'   String: Path to save the plot.
#'   TRUE: Save to default output directory
#'   NA: Do not save
#' @param plot_label_size
#'   c( Int, Int): Define size of lables for c(mz, samples). Set to 0 to remove a label.
#' @param cluster
#'   c("row"=Boolean,"col"=Boolean): Cluster data? Defaults to False
#' @examples
#' mzlog_analysis_heatmap(input_mzlog_obj) # run standard heatmap (no annotation)
#' mzlog_analysis_heatmap(input_mzlog_obj, metadata) # run heatmap with phenotype annotation
#' mzlog_analysis_heatmap(input_mzlog_obj, filter(metadata, type != "IS_Blank")) # run heatmap with phenotype annotation filtering blanks
#' mzlog_analysis_heatmap(input_mzlog_obj, metadata, annotation = "type") # run heatmap with custom annotation
#' mzlog_analysis_heatmap(input_mzlog_obj, plot_label_size = c(0, 0)) # run heatmap without axis labels
#' @return null (only plots)
#' @export
mzlog_analysis_heatmap <- function(input_mzlog_obj, metadata = NULL, annotation = "phenotype",
                                   title = "HeatMap of intensities", save_file = TRUE,
                                   cluster = NULL, plot_label_size = c(2, 1), ...) {
  plot_obj <- as.matrix(input_mzlog_obj[, -which(names(input_mzlog_obj) == "mz")])
  rownames(plot_obj) <- input_mzlog_obj$mz

  annotation_int <- NULL
  if (!is.null(annotation) && !is.null(metadata)) {
    pol <- local.mz_polarity_guesser(input_mzlog_obj, pos_return = "pos", neg_return = "neg")
    current_samples <- colnames(plot_obj)
    match_idx <- match(current_samples, paste0(pol, "_", metadata$sample_name))

    if (any(is.na(match_idx))) {
      stop("ERROR:MeDUSA::mzlog_analysis_heatmap: metadata and samples do not match")
    }

    annotation_int <- data.frame(
      row.names = current_samples,
      phenotype = metadata[match_idx, annotation]
    )
    names(annotation_int) <- annotation
  }

  if (isTRUE(save_file)) {
    save_file <- file.path(local.output_dir(), paste0(title, ".png"))
  }

  if (is.null(cluster)) cluster <- c("row" = FALSE, "col" = FALSE)

  pheatmap::pheatmap(t(plot_obj),
    cluster_rows = cluster["row"],
    cluster_cols = cluster["col"],
    annotation_row = annotation_int,
    show_rownames = plot_label_size[2] > 0,
    show_colnames = plot_label_size[1] > 0,
    fontsize_row = plot_label_size[2],
    fontsize_col = plot_label_size[1],
    main = title,
    filename = if (is.na(save_file)) NULL else save_file
  )
}
