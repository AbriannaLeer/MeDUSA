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
#' Replace all <[low_noise] ("mostly zeros") with a randomized value
#'   between [low_noise] and [high_noise]
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

# *** mz_post_magic_stages -----------------------------------------------------
#' MZ-OBJ
#'
#' @description
#' The mz_post_magic_stages is a helper function to execute all stages corresponding to the current post_magic stage.
#' This helper function takes a stage name, the function itself and the function parameters as arguments and excecutes the lambda function.
#'
#'
#' @param stage_name \cr
#'   String: The name of the phase to be executed
#' @param stage_func \cr
#'   Lambda: The function to be executed, as lambda
#' @param ... \cr
#'   Any parameters belonging to the function, using ellipsis for unknown amount of parameters
#' @examples
#' mz_post_magic_stages("mzlong_post_log", mzlong_post_log, input_mzlong, plot) # mzLog-OBJ
#' @return MZ-OBJ
#' @export
mz_post_magic_stages <- function(stage_name, stage_func, ...) {
  result <- tryCatch(
    {
      stage_func(...)
    },
    error = function(e) {
      print(e)
      stop("ERROR: in: ", stage_name)
    }
  )
  result
}

# *** Process Magic -----------------------------------------------------
#' MZ-OBJ Process Magic
#'
#' @description
#' The mz_post_magic function will perform all of the recommended post-processing,
#' including imputation to elimate intenstiy values of zero, PQN normalization
#' to reduce the effects of dilution not attributed to the phenotype,
#' pivot_longer to get the data structurally ready for statistical analysis,
#' and a log2 transform for a biologically relevant proportional comparison of
#' the data.
#'
#'  - Requires: ggplot2, tidyr
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @param metadata \cr
#'   DataFrame : metadata object
#' @param plot \cr
#'   Boolean   : to plot or not to plot
#' @examples
#' mz_post_magic(input_mz_obj, metadata) # run all post processing with standard parameters
#' mz_post_magic(input_mz_obj, metadata, noise = c(100, 10000)) # run all post processing with custom noise
#' @return c(mzLong_obj, mzLog_obj)
#' @export
mz_post_magic <- function(input_mz_obj, metadata, noise = c(10, 5000), plot = TRUE) {
  input_mz_obj <- mz_post_magic_stages(
    "mz_post_imputation",
    mz_post_imputation, input_mz_obj, noise[1], noise[2]
  )
  print("INFO:MeDUSA::mz_post_magic: imputation success")

  input_mz_obj <- mz_post_magic_stages(
    "mz_post_normalization",
    mz_post_normalization, input_mz_obj, metadata, plot
  )
  print("INFO:MeDUSA::mz_post_magic: normalization success")

  out <- mz_post_magic_stages("mz_post_log", mz_post_log, input_mz_obj)
  print("INFO:MeDUSA::mz_post_magic: log_transform success")

  input_mzlong <- mz_post_magic_stages("mz_post_pivot_longer", mz_post_pivot_longer, out)
  print("INFO:MeDUSA::mz_post_magic: pivot_longer success")

  list("mzLong" = input_mzlong, "mzLog" = out)
}
