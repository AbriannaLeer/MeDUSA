###For the statistics page phenotype groups have to be deduced from the metadata, this helper function accomplishes this.###

pheno_filter <- function(input_mzobj,
                         filter_value,
                         filter_name = "phenotype",
                         exclude = FALSE,
                         keep_mz = TRUE) {
  
  # Extract data
  data <- if (is.data.frame(input_mzobj)) {
    input_mzobj
  } else if (is.list(input_mzobj) && "raw" %in% names(input_mzobj)) {
    val <- input_mzobj$raw
    if (is.function(val)) val() else val
  } else {
    stop("input_mzobj must be a dataframe or list with $raw, got: ",
         paste(class(input_mzobj), collapse = ", "))
  }
  
  # Detect format
  is_wide <- !"sample_name" %in% names(data)
  
  if (is_wide) {
    # --- WIDE FORMAT: filter columns by name ---
    sample_cols <- setdiff(names(data), "mz")
    
    if (exclude) {
      keep_cols <- sample_cols[!grepl(filter_value, sample_cols, ignore.case = TRUE)]
    } else {
      keep_cols <- sample_cols[grepl(filter_value, sample_cols, ignore.case = TRUE)]
    }
    
    if (length(keep_cols) == 0) {
      stop("No samples matched filter: ", filter_value,
           "\nColumn names available: ", paste(sample_cols, collapse = ", "))
    }
    
    if (keep_mz && "mz" %in% names(data)) {
      return(data[, c("mz", keep_cols), drop = FALSE])
    } else {
      return(data[, keep_cols, drop = FALSE])
    }
    
  } else {
    # --- LONG FORMAT: filter rows by phenotype column ---
    data[[filter_name]] <- sub("_[0-9]+$", "", data$sample_name)
    
    if (exclude) {
      meta_filtered <- dplyr::filter(data, !(!!sym(filter_name) %in% filter_value))
    } else {
      meta_filtered <- dplyr::filter(data, !!sym(filter_name) %in% filter_value)
    }
    
    if (nrow(meta_filtered) == 0) {
      stop("No samples matched filter: ", paste(filter_value, collapse = ", "),
           "\nAvailable values: ", paste(unique(data[[filter_name]]), collapse = ", "))
    }
    
    out <- meta_filtered
    if (keep_mz && "mz" %in% names(out)) {
      out <- dplyr::select(out, mz, dplyr::everything())
    }
    return(out)
  }
}