#' Download and process compound databases for MeDUSA
#'
#' Databases are stored in tools::R_user_dir("MeDUSA", "data")
#' which resolves to a platform-appropriate directory:
#'   Windows: %LOCALAPPDATA%/MeDUSA/data
#'   macOS:   ~/Library/Application Support/MeDUSA/data
#'   Linux:   ~/.local/share/MeDUSA/data

db_setup <- function(db_dir = NULL) {
  if (is.null(db_dir)) {
    db_dir <- tools::R_user_dir("MeDUSA", "data")
  }

  dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  cat("Database directory:", db_dir, "\n\n")

  setup_hmdb(db_dir)
  setup_lipids(db_dir)

  cat("\nAll databases ready in:", db_dir, "\n")
  invisible(db_dir)
}


# --- HMDB Setup -----------------------------------------------------------

setup_hmdb <- function(db_dir) {
  hmdb_rdata <- file.path(db_dir, "hmdb_xml.Rdata")
  hmdb_xml <- file.path(db_dir, "hmdb_simple.xml")

  if (file.exists(hmdb_rdata)) {
    cat("HMDB database already exists, skipping.\n")
    return(invisible(NULL))
  }

  # Check if pre-built Rdata exists in the project root
  local_rdata <- "hmdb_xml.Rdata"
  if (file.exists(local_rdata)) {
    cat("Copying pre-built HMDB database from project...\n")
    file.copy(local_rdata, hmdb_rdata)
    cat("HMDB setup complete.\n")
    return(invisible(NULL))
  }

  cat("Downloading HMDB database (this is a large file)...\n")
  hmdb_zip <- file.path(db_dir, "hmdb_metabolites.zip")
  download.file(
    "https://hmdb.ca/system/downloads/current/hmdb_metabolites.zip",
    hmdb_zip,
    mode = "wb"
  )

  cat("Extracting HMDB...\n")
  unzip(hmdb_zip, exdir = db_dir)
  hmdb_full_xml <- file.path(db_dir, "hmdb_metabolites.xml")

  # Filter the large XML to only the tags MeDUSA needs
  cat("Filtering HMDB XML...\n")
  con_in <- file(hmdb_full_xml, "r")
  con_out <- file(hmdb_xml, "w")
  pattern <- paste0(
    "</?hmdb( xml.*)?>|</?metabolite>|",
    "^\\s\\s<accession>|^\\s\\s<name>|",
    "<monisotopic_molecular_weight>"
  )

  while (length(chunk <- readLines(con_in, n = 5000, warn = FALSE)) > 0) {
    matches <- chunk[grepl(pattern, chunk)]
    if (length(matches) > 0) writeLines(matches, con_out)
  }
  close(con_in)
  close(con_out)

  # Create Rdata for faster loading (~15 min, but only needed once)
  if (requireNamespace("XML", quietly = TRUE)) {
    cat("Converting HMDB to Rdata for faster loading (this may take ~15 minutes)...\n")
    hmdb <- XML::xmlToDataFrame(hmdb_xml)
    hmdb$monisotopic_molecular_weight <- as.numeric(hmdb$monisotopic_molecular_weight)
    hmdb <- hmdb[!is.na(hmdb$monisotopic_molecular_weight), ]
    save(hmdb, file = hmdb_rdata)
    cat("Rdata version created.\n")
  } else {
    cat("Note: XML package not yet available. hmdb_simple.xml created but Rdata\n")
    cat("      conversion skipped. It will be parsed on first use (slower).\n")
  }

  unlink(hmdb_zip)
  unlink(hmdb_full_xml)
  cat("HMDB setup complete.\n")
}


# --- LIPID MAPS Setup -----------------------------------------------------

setup_lipids <- function(db_dir) {
  lipids_csv <- file.path(db_dir, "lipids_simple.csv")

  if (file.exists(lipids_csv)) {
    cat("LIPID MAPS database already exists, skipping.\n")
    return(invisible(NULL))
  }

  cat("Downloading LIPID MAPS database...\n")
  lipids_zip <- file.path(db_dir, "lipids.zip")
  download.file(
    "https://lipidmaps.org/files/?file=LMSD&ext=sdf.zip",
    lipids_zip,
    mode = "wb"
  )

  cat("Extracting LIPID MAPS...\n")
  unzip(lipids_zip, exdir = db_dir)
  lipids_sdf <- file.path(db_dir, "structures.sdf")

  cat("Processing LIPID MAPS SDF...\n")
  lines <- readLines(lipids_sdf, warn = FALSE)

  id_markers <- which(lines == "> <LM_ID>")
  mass_markers <- which(lines == "> <EXACT_MASS>")

  ids <- trimws(lines[id_markers + 1])
  masses <- trimws(lines[mass_markers + 1])

  # Pair by record: both fields should appear in each record (delimiter: $$$$)
  n <- min(length(ids), length(masses))
  lipids_df <- data.frame(
    IDENTITY = gsub(",", "-", ids[seq_len(n)]),
    EXACT_MASS = masses[seq_len(n)],
    stringsAsFactors = FALSE
  )

  write.csv(lipids_df, lipids_csv, row.names = FALSE)

  unlink(lipids_zip)
  unlink(lipids_sdf)
  cat("LIPID MAPS setup complete.\n")
}


# Auto-run when executed directly: Rscript scripts/db_setup.R
if (sys.nframe() == 0L) {
  db_setup()
}
