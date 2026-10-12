build_metadata <- function(sample_names) {
  type <- dplyr::case_when(
    grepl("blank", sample_names) ~ "blank",
    grepl("media", sample_names) ~ "media",
    grepl("qc",    sample_names) ~ "qc",
    TRUE                         ~ "cell"
  )
  phenotype <- dplyr::case_when(
    grepl("hek", sample_names) ~ "hek",
    grepl("hep", sample_names) ~ "hep",
    TRUE                       ~ type
  )
  data.frame(
    sample_name  = sample_names,
    type         = type,
    phenotype    = phenotype,
    filtered_out = FALSE,
    stringsAsFactors = FALSE
  )
}
