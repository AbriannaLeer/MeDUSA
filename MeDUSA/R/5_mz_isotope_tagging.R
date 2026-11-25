# *** IsotopeTagging -----------------------------------------------------
#' Find Isotopes (Enhanced Version)
#'
#' @description
#' Analyzes m/z features to identify isotopic clusters. This function 
#' groups peaks that likely belong to the same molecule based on a 
#' specific mass target (e.g., C13 shift).
#'
#' @param input_mz DataFrame or numeric vector : Input MZ-Obj (full mz_obj or mz vector)
#' @param iso_target Float : Mass difference to hunt (e.g., 1.0034 for C13)
#' @param iso_iter Integer : Max number of isotope steps to look for (M+n)
#' @param tol_ppm Float : Mass tolerance in ppm
#' @param cores Integer : Reserved for backward compatibility
#'
#' @return Returns a named list of DataFrames. Each DataFrame contains:
#'   - Group: Cluster Index (1, 2, 3...)
#'   - mz: Measured m/z value
#'   - diff_to_prev: Mass gap from the previous peak in the cluster
#'   - total_dist: Cumulative mass distance from the monoisotopic parent (M+0)
#' @export

mz_tag_isotope_hunter <- function(input_mz, iso_target = 1.0034, iso_iter = 5, tol_ppm = 5e-6, cores = 4, ...) {
  # Accept either an mz_obj data.frame or a numeric vector
  if (!is.null(ncol(input_mz))) {
    input_mz <- input_mz$mz
  }

  mz_vals <- as.numeric(input_mz)
  if (length(mz_vals) == 0L) return(list())

  # Calculate individual tolerance for each m/z
  tol_vals <- mz_vals - (mz_vals / (tol_ppm + 1))
  input_df <- data.frame(mz = mz_vals, tol = tol_vals, stringsAsFactors = FALSE)

  n <- nrow(input_df)
  out <- vector("list", n)

  # Optimization: Sort mz once for interval lookup
  ord <- order(input_df$mz)
  mz_sorted <- input_df$mz[ord]
  window_half <- iso_target * (iso_iter + 0.1)

  for (i in seq_len(n)) {
    mz_i <- input_df$mz[i]
    tol_i <- input_df$tol[i]

    lower_bound <- mz_i - window_half
    upper_bound <- mz_i + window_half

    # Find candidate range in sorted mz vector
    left_idx <- findInterval(lower_bound, mz_sorted) + 1L
    right_idx <- findInterval(upper_bound, mz_sorted)

    if (left_idx > right_idx || left_idx < 1L || right_idx < 1L) next

    # Map back to original row indices and sort to preserve legacy behavior
    candidate_idx <- sort(ord[left_idx:right_idx])
    hunt_zone <- input_df[candidate_idx, , drop = FALSE]

    # Filter within strict search window
    hunt_zone <- hunt_zone[(lower_bound < hunt_zone$mz) & (hunt_zone$mz < upper_bound), , drop = FALSE]
    if (nrow(hunt_zone) == 0L) next

    # Apply isotope matching formula
    x <- hunt_zone$mz[
      ((((abs(hunt_zone$mz - mz_i) + tol_i) %% iso_target) - tol_i) < tol_i)
    ]

    # Store as a temporary DataFrame if cluster found
    if (length(x) > 1) {
      out[[i]] <- data.frame(
        Group = 0, # Placeholder for numbering
        mz = x,
        diff_to_prev = c(0, diff(x)), 
        total_dist = x - x[1],        
        stringsAsFactors = FALSE
      )
    }
  }

  # Remove empty entries
  out <- out[!sapply(out, is.null)]
  if (length(out) == 0L) return(list())

  # Deduplicate: Remove groups that contain the same m/z sets
  out <- out[!duplicated(lapply(out, function(df) df$mz))]

  # Final Step: Re-number Groups to ensure continuous sequence (1, 2, 3...)
  for (j in seq_along(out)) {
    out[[j]]$Group <- j
  }

  # Name the list elements by parent m/z for reference
  names(out) <- sapply(out, function(df) as.character(round(df$mz[1], 4)))
  
  return(out)
}



# mz_isotope_flatten <- function(input_mz_obj, isotope_list, method = max, cores = 4){
#  pblapply(input_mz_obj, function(isotope_list, x, method = method){
#    if x$mz %in% names(isotope_list){
#      x <- apply(select(input_mz_obj[input_mz_obj$mz %in% x,], -mz),2, method)
#  })
# }

# TODO FLATTEN?