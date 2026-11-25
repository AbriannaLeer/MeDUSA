# *** Imputation -----------------------------------------------------
#' MZ-OBJ Imputation
#'
#' @description
#' After filtering the data set, there may be zero values as intensity in some
#' instances. Unfortunately, this will hinder the statistical analysis as
#' some of the mathematical functions cannot accept zero values. To accommodate
#' this without impacting the statistical outcome, the mz_post_imputation function
#' is used to replace the zero values with a random number between 1 and a set
#' noise value.
#'
#' Replace all <low_noise ("mostly zeros") with a randomized value
#'   between low_noise and high_noise
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-OBJ
#' @param low_noise \cr
#'   Float     : LowBoundry for "Noise"
#' @param high_noise \cr
#'   Float     : HighBoundry for "Noise"
#' @examples
#' mz_post_imputation(input_mz_obj) # run imputation with standard noise levels
#' mz_post_imputation(input_mz_obj, low_noise = 100, high_noise = 10000) # run imputation with custom noise levels
#' @return MZ-OBJ
#' @export
mz_post_imputation <- function(input_mz_obj, low_noise = 10, high_noise = NULL) {
  if (!hasArg(high_noise)) {
    high_noise <- tryCatch(
      {
        local.mz_polarity_guesser(input_mz_obj, pos_return = 1000, neg_return = 500)
      },
      error = function(e) {
        print(e)
        stop("ERROR: MeDUSA::mz_post_imputation: Could not guess positive or negative from colnames
            Please specify high_noise")
      }
    )
  }
  if (low_noise < 500) {
    print("WARN: MeDUSA::mz_post_imputation: low_noise is < 500. If [input_mz_obj] has an mz column, ensure it is labled 'mz'")
  }
  tmp <- as.matrix(input_mz_obj[colnames(input_mz_obj) != "mz"])

  bool <- tmp < low_noise
  width <- (high_noise - low_noise + 1)
  tmp[bool] <- (low_noise - 1L) + sample.int(width, sum(bool), replace = TRUE)
  input_mz_obj[colnames(input_mz_obj) != "mz"] <- as.data.frame(tmp)
  input_mz_obj
}

# *** Normalization -----------------------------------------------------
#' MZ-OBJ Normalization
#'
#' @description
#' The mz_post_normalization function utilizes Probabilistic Quotient
#' Normalization, or PQN, to limit the effects of general fluctuations and
#' random effects on the results. In single cell measurements, this is important
#' since the size of the cell as well as the volume of media captured and
#' measured with the cell are variable, thus impacting the overall dilution of
#' the cellular material.
#'
#' It is recommended to use PQN after samples and mz have been removed in the
#' standard filtering steps.
#'
#' The algorithm will choose one reference sample if one is not provided. This
#' reference is then used to calculate ratios at each m/z in every sample, and this
#' ratio is the dilution factor that is applied.
#'
#'  - Requires: ggplot2, ggpubr, dplyr
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @param plot \cr
#'   Boolean   : to plot or not to plot
#' @param meta \cr
#'   DataFrame : metadata object
#' @examples
#' mz_post_normalization(input_mz_obj, metadata) # run normalization
#' @return Returns an MZ-OBJ
#' @export
mz_post_normalization <- function(input_mz_obj, metadata, plot = TRUE) {
  metadata <- local.meta_polarity_fixer(input_mz_obj, metadata)
  # TODO revisit, for a refactor
  rownames(input_mz_obj) <- input_mz_obj$mz
  result <- quotNorm(t(dplyr::select(input_mz_obj, -mz)))
  normal <- as.data.frame(t(result$X))
  dilution <- data.frame(normal.dilution = result$dilution)
  dilution$sample <- rownames(dilution)
  join_by <- c("sample")
  names(join_by) <- "sample_name"
  dilution <- dplyr::left_join(metadata, dilution, by = join_by)
  polarity <- local.mz_polarity_guesser(input_mz_obj)

  if (plot) {
    ggpubr::ggboxplot(dilution,
      x = "phenotype", y = "normal.dilution",
      ylab. = "Dilution Factor",
      xlab. = "Phenotype",
      title = "Normalized Dilution Factor per Phenotype",
      subtitle = polarity,
      legend = "none"
    )

    local.save_plot(paste("Normalization", polarity, sep = "-"))
  }
  normal <- cbind("mz" = gsub("X", "", rownames(normal)), normal) # TODO fix 'quotNorm' to not return mz = "X100.2"
  rownames(normal) <- NULL
  normal$mz <- as.numeric(normal$mz)
  normal
}

# *** PivotLonger -----------------------------------------------------
#' MZ-OBJ Pivot Longer
#'
#' @description
#' The mz_post_pivot_longer function does not filter the data at all, but rather
#' rearranges it in preparation for some of the mathematical statistical
#' functions. Instead of a column for each sample, there will be only three
#' columns: m/z, sample, and intensity. Each m/z will appear as many times as
#' there are samples.
#'  - Requires: ggplot2, ggpubr, tidyr
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-OBJ
#' @param plot \cr
#'   Boolean   : to plot or not to plot
#' @examples
#' mz_post_pivot_longer(input_mz_obj) # run pivot
#' @return mzLong-OBJ
#' @export
mz_post_pivot_longer <- function(input_mz_obj, plot = TRUE) {
  input_mzlong <- tidyr::pivot_longer(input_mz_obj, !mz, names_to = "sample_name", values_to = "intensity")
  if (plot) {
    mzlongpost.plot(input_mzlong, local.mz_polarity_guesser(input_mz_obj), "PivotLonger")
  }
  data.frame(input_mzlong)
}

# *** post Log2 -----------------------------------------------------
#' MZ-OBJ Log2
#'
#' @description
#' The mz_post_log function log2 transforms the intensities of the data set. Log
#' transformation of the data is highly recommended so that the observed m/z
#' relationships are proportional and not additive, making the statistical
#' analysis and interpretation more biologically relevant. Note that this
#' function works with the standard mz_object, not the pivot_longer object.
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @examples
#' mz_post_log(input_mz_obj) # log2 mz_obj
#' @return mzLog-OBJ
#' @export
mz_post_log <- function(input_mz_obj) {
  ind <- which(colnames(input_mz_obj) == "mz")
  input_mz_obj[-ind] <- log2(input_mz_obj[-ind])
  input_mz_obj
}

# *** Log Transform -----------------------------------------------------
#' MZLONG-OBJ Log Transform
#'
#' @description
#' The mzlong_post_log function log2 transforms the intensities of the data set,
#' after the data set has been restructured via the pivot_longer function. Log
#' transformation of the data is highly recommended so that the observed m/z
#' relationships are proportional and not additive, making the statistical
#' analysis and interpretation more biologically relevant. Note that this
#' function works with the pivot_longer object, not the standard mz_object.
#'
#'  - Requires: ggplot2
#'
#' @param input_mzlong \cr
#'   DataFrame : Input MZLONG-Obj
#' @param plot \cr
#'   Boolean   : to plot or not to plot
#' @examples
#' mzlong_post_log(input_mz_obj, metadata) # run log2 on a mzlong (pivoted mz_obj)
#' @return MzLong_obj
#' @export
mzlong_post_log <- function(input_mzlong, plot = TRUE) {
  input_mzlong$log <- log2(input_mzlong$intensity)
  if (plot) {
    tmp <- select(input_mzlong, -intensity)
    tmp$intensity <- tmp$log
    mzlongpost.plot(tmp, local.mz_polarity_guesser(input_mzlong), "PivotLongLog")
  }
  input_mzlong
}

mzlongpost.plot <- function(input_mzlong, polarity, plot_title) {
  ggpubr::ggscatter(
    input_mzlong,
    x      = "sample_name", y    = "intensity",
    xlab   = "Sample_Name", ylab = "Intensity",
    shape  = 20,            size = 1,
    color  = "intensity",
    title  = paste(plot_title, polarity, sep = ": "),
    rotate = T
  ) +
    ggplot2::scale_x_discrete(
      label = sapply(strsplit(input_mzlong$sample_name, "_"), function(x) paste(x[-2:-1], collapse = ".")),
      guide = ggplot2::guide_axis(n.dodge = 3)
    ) +
    ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.5)))
  
  local.save_plot(paste(plot_title, polarity, sep = "-"))
  
  ggpubr::ggboxplot(
    input_mzlong,
    x             = "sample_name", y            = "intensity",
    xlab          = "Sample_Name", ylab         = "Intensity",
    outlier.shape = 3,             outlier.size = 0.1,
    title         = paste(plot_title, "box:", polarity, sep = " "),
    legend        = "none",
    rotate        = T
  ) +
    ggplot2::scale_x_discrete(
      label = sapply(strsplit(input_mzlong$sample_name, "_"), function(x) paste(x[-2:-1], collapse = ".")),
      guide = ggplot2::guide_axis(n.dodge = 3)
    ) +
    ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.5)))
  
  local.save_plot(paste(plot_title, "box", polarity, sep = "-"))
}
