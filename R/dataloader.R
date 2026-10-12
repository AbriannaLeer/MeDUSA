dataloader_module <- function(file_reactive) {
  file_input <- reactive({
    if (is.function(file_reactive)) {
      file_reactive()
    } else {
      file_reactive
    }
  })

  raw <- reactive({
    req(file_input(), file_input()$datapath)

    df <- read.csv(file_input()$datapath,
      sep = "\t", check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (ncol(df) <= 1) {
      df <- read.csv(file_input()$datapath,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    has_mz <- "mz" %in% colnames(df)
    has_intensity <- "intensity" %in% colnames(df)
    has_sample_name <- "sample_name" %in% colnames(df)
    has_long <- has_mz && has_intensity && has_sample_name
    has_sample_cols <- length(setdiff(colnames(df), "mz")) > 0
    has_wide <- has_mz && !has_long && has_sample_cols

    validate(
      need(has_mz, "File must have a column named 'mz'"),
      need(!xor(has_intensity, has_sample_name),
        "Long format requires both 'intensity' and 'sample_name' columns"
      ),
      need(
        has_long || has_wide,
        "File must be long format (mz, intensity, sample_name) or wide mz_obj (mz + sample columns)"
      )
    )

    df
  })

  format <- reactive({
    req(raw())
    df <- raw()
    if (all(c("mz", "intensity", "sample_name") %in% colnames(df))) {
      "long"
    } else {
      "wide"
    }
  })

  long <- reactive({
    req(raw())
    df <- raw()

    if (format() == "long") {
      return(df)
    }

    df_long <- tidyr::pivot_longer(
      df,
      cols = -mz,
      names_to = "sample_name",
      values_to = "intensity"
    )
    df_long$mz <- as.numeric(df_long$mz)
    df_long$intensity <- as.numeric(df_long$intensity)
    df_long
  })

  wide <- reactive({
    req(raw())
    df <- raw()

    if (format() == "wide") {
      return(df)
    }

    df_long <- df
    if ("feature" %in% colnames(df_long)) {
      df_long <- df_long |>
        dplyr::group_by(feature, sample_name) |>
        dplyr::summarise(
          intensity = mean(intensity, na.rm = TRUE),
          mz = mean(mz, na.rm = TRUE),
          .groups = "drop"
        )
    }

    df_long <- df_long |>
      dplyr::mutate(
        mz = as.numeric(mz),
        intensity = as.numeric(intensity)
      ) |>
      dplyr::select(mz, intensity, sample_name)

    wide <- tidyr::pivot_wider(
      df_long,
      names_from = sample_name,
      values_from = intensity,
      values_fill = 0,
      values_fn = mean
    ) |>
      dplyr::arrange(mz) |>
      dplyr::mutate(mz = as.numeric(mz))

    as.data.frame(wide)
  })

  sample_groups <- reactive({
    req(raw())
    if (format() == "long") {
      names <- trimws(unique(long()$sample_name))
    } else {
      names <- trimws(setdiff(colnames(wide()), "mz"))
    }
    sort(unique(sub("_\\d+.*$", "", names)))
  })

  list(
    raw = raw,
    long = long,
    wide = wide,
    format = format,
    sample_groups = sample_groups
  )
}
