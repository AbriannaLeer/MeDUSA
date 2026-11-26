# *** Lipid Check -----------------------------------------------------
#' Run Lipid Check
#'
#' @description
#' A quality-control check for two endogenous reference lipids.
#' Each sample must contain both of the following features, matched within the
#' 5 ppm alignment tolerance. Samples (rows) missing either one are removed.
#' Lipid     Formula        Adduct    Theoretical m/z   Likely species
#' PC 34:1   C42H82NO8P     |M+H|+    760.5851          PC(16:0/18:1)
#' PI 38:4   C47H83O13P     |M-H|-    885.5499          PI(18:0/20:4)
#' 
#' Both are highly abundant membrane phospholipids in mammalian cells, so their
#' absence suggests a failed injection, low sample input, or a processing error.
#' On the reference run (Orbitrap, 24,000 FWHM) they were measured with sub-1 ppm
#' error (+0.94 and -0.74 ppm). That confirms the molecular formulas, but without
#' MS/MS the acyl-chain composition and sn-positions are putative.
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @param plot \cr
#'   Boolean   : To plot or not to plot.
#' @param magicNumber1 \cr
#'   Float     : MagicNumber1 in "MN1 * data + MN2"
#' @param magicNumber2 \cr
#'   Float     : MagicNumber2 in "MN1 * data + MN2"
#'
#' Dependencies : ggplot2, ggpubr, dplyr
#' @examples
#' lipid_sanity_check(input_mz_obj) # Run the filter
#' @return Returns an MZ-OBJ
#' @export

lipid_sanity_check <- function(input_mz_obj) {
  print("INFO: Checking lipids...")
  
  

}

# *** Lipid Check Plot -----------------------------------------------------
#' Run Lipid Check Plot
#'
#' @description
#' The results during the lipid check will be visualized with two different line
#' plots. 1: samples, intensity and ppm error, 2: missingness of the lipids in 
#' the samples. Rows are removed from mz-obj where lipids are not shown.
#'
#' @param input_mz_obj \cr
#'   DataFrame : Input MZ-Obj
#' @param plot \cr
#'   Boolean   : To plot or not to plot.
#' @param magicNumber1 \cr
#'   Float     : MagicNumber1 in "MN1 * data + MN2"
#' @param magicNumber2 \cr
#'   Float     : MagicNumber2 in "MN1 * data + MN2"
#'
#' Dependencies : ggplot2, ggpubr, dplyr
#' @examples
#' lipid_check_plot(input_mz_obj) # Run the filter
#' @return Returns two line plots
#' @export

plot_lipid <- function(input_mz_obj) {
  
  # line plot 1 - ppm error
  
  # line plot 2 - missingness
  
}