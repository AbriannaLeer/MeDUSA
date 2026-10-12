# =========================
# UI
# =========================

visualization_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Analysis Hub", class = "btn-info", width = "100%"),
      br(), br(),
      
       h4("1. Data"),
      fileInput(
        ns("custom_upload"),
        "Custom Mz-table upload",accept = ".csv"
      ),
      tags$small("Full data from previous step loaded by default, add additional custom m/z tables if desired."),
      
      hr(),
      uiOutput(ns("polarity_ui")),
      hr(),
      uiOutput(ns("analysis_type_ui")),
      hr(),
      selectInput(
        ns("combine_method"),
        "Choose method of combining tables:",
        choices = c(
          "union" = "union",
          "intersection" = "intersection",
          "consensus > 1" = "c1",
          "consensus > 2" = "c2"
        )
      ),
      
      h4("2. Visualization"),
      
      tabsetPanel(
        id = ns("param_tabs"),
        
        tabPanel(
          "PCA",
          br(),
          helpText("Principal Component Analysis: colour samples by phenotype group."),
          textInput(ns("pca_title"), "Plot Title", value = "PCA"),
          textInput(
            ns("pca_blacklist"),
            "Sample blacklist (comma-separated)",
            value       = "",
            placeholder = "e.g. sumr_blank_001, sumr_qc_004"
          ),
          br(),
          actionButton(ns("run_pca"), "Run PCA", class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput(ns("pca_run_status")),
          shinyjs::disabled(
            downloadButton(ns("download_pca"), "Download PCA Plot (.png)")
          )
        ),
        
        tabPanel(
          "Heatmap",
          br(),
          helpText("Heatmap of intensities across all m/z values and samples, annotated by phenotype group."),
          textInput(ns("heatmap_title"), "Plot Title", value = "Heatmap of intensities"),
          numericInput(ns("heatmap_label_mz"),     "m/z label size (0 = hide)",    value = 2, min = 0, max = 20, step = 1),
          numericInput(ns("heatmap_label_sample"), "Sample label size (0 = hide)", value = 1, min = 0, max = 20, step = 1),
          checkboxInput(ns("heatmap_cluster_row"), "Cluster rows (m/z)",           value = FALSE),
          checkboxInput(ns("heatmap_cluster_col"), "Cluster columns (samples)",    value = FALSE),
          br(),
          actionButton(ns("run_heatmap"), "Run Heatmap", class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput(ns("heatmap_run_status")),
          shinyjs::disabled(
            downloadButton(ns("download_heatmap"), "Download Heatmap (.png)")
          )
        )
      ),
      
      hr(),
      
      actionButton(ns("go_next_to_name"), "Go to Identification", class = "btn-info", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        id = ns("output_tabs"),
        
        tabPanel(
          "PCA",
          br(),
          shinyjs::hidden(
            div(
              id = ns("pca_progress_wrap"),
              tags$label(id = ns("pca_progress_label"), ""),
              div(
                style = "width:100%; background:#e9ecef; border-radius:4px; margin-bottom:12px;",
                div(
                  id    = ns("pca_progress_bar"),
                  style = "width:0%; height:22px; background:#0d6efd; border-radius:4px;
                           transition:width 0.3s ease; text-align:center; color:white;
                           font-size:13px; line-height:22px;"
                )
              )
            )
          ),
          uiOutput(ns("pca_ui"))
        ),
        
        tabPanel(
          "Heatmap",
          br(),
          shinyjs::hidden(
            div(
              id = ns("heatmap_progress_wrap"),
              tags$label(id = ns("heatmap_progress_label"), ""),
              div(
                style = "width:100%; background:#e9ecef; border-radius:4px; margin-bottom:12px;",
                div(
                  id    = ns("heatmap_progress_bar"),
                  style = "width:0%; height:22px; background:#0d6efd; border-radius:4px;
                           transition:width 0.3s ease; text-align:center; color:white;
                           font-size:13px; line-height:22px;"
                )
              )
            )
          ),
          uiOutput(ns("heatmap_ui"))
        )
      )
    )
  )
}


# =========================
# SERVER
# =========================

visualization_page_server <- function(id, previous_result, previous_long, metadata = reactive(NULL), polarity, group, welch_mz,fold_mz,anova_mz,rf_mz) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    mz_welch <- reactive({ tryCatch(welch_mz(), error = function(e) NULL) })
    mz_fold  <- reactive({ tryCatch(fold_mz(),  error = function(e) NULL) })
    mz_anova <- reactive({ tryCatch(anova_mz(), error = function(e) NULL) })
    mz_rf    <- reactive({ tryCatch(rf_mz(),    error = function(e) NULL) })
    
    welch_thresh <- reactiveVal(0.05)
    fold_thresh  <- reactiveVal(0.6)
    selected_analyses <- reactiveVal("Full m/z")

    pca_trigger     <- reactiveVal(0)
    heatmap_trigger <- reactiveVal(0)


    # ---- progress bar helpers -----------------------------------------------
    # Inject the shared animated loading bar (below the run button) with the
    # current stage label. pct/bar_id/label_id are kept for call compatibility.
    set_progress <- function(bar_id, label_id, wrap_id, pct, label) {
      status_id <- sub("_progress_wrap$", "_run_status", wrap_id)
      shinyjs::html(id = session$ns(status_id),
        html = loading_bar_html(label), asis = TRUE)
    }

    hide_progress <- function(wrap_id) {
      status_id <- sub("_progress_wrap$", "_run_status", wrap_id)
      shinyjs::html(id = session$ns(status_id), html = "", asis = TRUE)
    }
  
    active_meta <- metadata
    custom_analyses <- reactiveVal(character())
    custom_data <- reactiveValues()
    custom_counter <- reactiveVal(0)
    
    
    ###Upload handling###
    observeEvent(input$custom_upload, {
      
      current <- custom_analyses()
      
      new_name <- paste0("custom_", length(current) + 1)
      
      custom_analyses(c(current, new_name))
      
    })
    
    observeEvent(input$custom_upload, {
      
      req(input$custom_upload)
      
      df <- read.csv(input$custom_upload$datapath)
      
      id <- custom_counter() + 1
      custom_counter(id)
      
      name <- paste0("custom_data_", id)
    
      custom_data[[name]] <- df
      
    })
    
    ###Mz table select UI###
    
    output$analysis_type_ui <- renderUI({
      
      base <- tryCatch(previous_result(), error = function(e) NULL)
      
      make_label <- function(name, df) {
        n <- if (!is.null(df)) nrow(df) else 0
        paste0(name, " (", n, " m/z)")
      }
      
      choices <- setNames("Full m/z", make_label("Full m/z", base))
      
      tables <- tryCatch(all_mz_tables(), error = function(e) NULL)
      
      if (!is.null(mz_welch()))  choices <- c(choices, setNames("Welch m/z", make_label("Welch m/z", tables[["Welch m/z"]])))
      if (!is.null(mz_fold()))   choices <- c(choices, setNames("Fold m/z",  make_label("Fold m/z",  tables[["Fold m/z"]])))
      if (!is.null(mz_anova()))  choices <- c(choices, setNames("ANOVA m/z",         make_label("ANOVA m/z",         mz_anova())))
      if (!is.null(mz_rf()))     choices <- c(choices, setNames("Random Forest m/z", make_label("Random Forest m/z", mz_rf())))
      
      tagList(
        checkboxGroupInput(
          ns("analysis_types"),
          "M/z tables to include:",
          choices  = c(choices, custom_analyses()),
          selected = selected_analyses()
        ),
        conditionalPanel(
          condition = sprintf("input['%s'].indexOf('Welch m/z') !== -1", ns("analysis_types")),
          numericInput(ns("welch_p_threshold"), "Welch p-value threshold", value = welch_thresh(), min = 0, max = 1, step = 0.01)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'].indexOf('Fold m/z') !== -1", ns("analysis_types")),
          numericInput(ns("fold_threshold"), "Fold change threshold (absolute)", value = fold_thresh(), min = 0, step = 0.1)
        ),
        tags$small("Execute analysis methods to be able to select their returned m/z tables here.")
      )
    })
    
    ###Polarity handling###
    output$polarity_ui <- renderUI({
      req(polarity())
      radioButtons(
        ns("polarity"),
        "Sample polarity:",
        choices  = c("Positive", "Negative"),
        selected = polarity()
      )
    })
    
    observeEvent(input$polarity, {
      polarity(input$polarity)
    }, ignoreInit = TRUE)
    
    # Helper: preserves column names to pass the polarity guesser
    filter_to_base <- function(base, filter_df) {
      if (is.null(filter_df) || is.null(base)) return(NULL)
      base[as.character(base$mz) %in% as.character(filter_df$mz), ]
    }
    
    all_mz_tables <- reactive({
      base <- previous_result()
      
      tables <- list("Full m/z" = base)
      
      if (!is.null(mz_welch())) {
        welch_filtered <- mz_welch()[mz_welch()$p < welch_thresh(), ]
        tables[["Welch m/z"]] <- filter_to_base(base, welch_filtered)
      }
      
      if (!is.null(mz_fold())) {
        fold_filtered <- mz_fold()[abs(mz_fold()$fold) > fold_thresh(), ]
        tables[["Fold m/z"]] <- filter_to_base(base, fold_filtered)
      }
      if (!is.null(mz_anova()))  tables[["ANOVA m/z"]]         <- filter_to_base(base, mz_anova())
      if (!is.null(mz_rf()))     tables[["Random Forest m/z"]] <- filter_to_base(base, mz_rf())
      
      for (nm in custom_analyses()) {
        key <- paste0("custom_data_", which(custom_analyses() == nm))
        if (!is.null(custom_data[[key]])) tables[[nm]] <- filter_to_base(base, custom_data[[key]])
      }
      
      tables
    }) 
    
    ###Combining selected Mz tables###
    combined_mz <- reactive({
      
      selected  <- input$analysis_types
      method    <- input$combine_method
      tables    <- all_mz_tables()
      base      <- tryCatch(previous_result(), error = function(e) NULL)
      
      # Nothing selected or no base → fall back to previous_result
      if (is.null(selected) || length(selected) == 0 || is.null(base)) return(base)
      
      chosen <- tables[intersect(selected, names(tables))]
      chosen <- Filter(Negate(is.null), chosen)
      
      if (length(chosen) == 0) return(base)
      if (length(chosen) == 1) return(chosen[[1]])
      
      mz_sets <- lapply(chosen, function(df) as.character(df$mz))
      
      #Combine Method
      keep_mz <- switch(method,
                        
                        "union" = Reduce(union, mz_sets),
                        
                        "intersection" = Reduce(intersect, mz_sets),
                        
                        "c1" = {   # present in more than 1 table
                          all_mz  <- unique(unlist(mz_sets))
                          counts  <- sapply(all_mz, function(m) sum(sapply(mz_sets, function(s) m %in% s)))
                          all_mz[counts > 1]
                        },
                        
                        "c2" = {   # present in more than 2 tables
                          all_mz  <- unique(unlist(mz_sets))
                          counts  <- sapply(all_mz, function(m) sum(sapply(mz_sets, function(s) m %in% s)))
                          all_mz[counts > 2]
                        }
      )
      
      if (length(keep_mz) == 0) {
        return(NULL)
      }
      
      # Filter the FULL m/z table (base) to the winning mz set.
      # Using the base table guarantees consistent intensity columns across all samples.
      base_filtered <- base[as.character(base$mz) %in% keep_mz, ]
      
      base_filtered
    })
  
    
    ###Threshold update observer###
    observeEvent(input$welch_p_threshold, { welch_thresh(input$welch_p_threshold) })
    observeEvent(input$fold_threshold,    { fold_thresh(input$fold_threshold) })
    observeEvent(input$analysis_types, {
      selected_analyses(input$analysis_types)
    })

    ###Blacklist helper function###
    parse_blacklist <- function(txt) {
      if (is.null(txt) || nchar(trimws(txt)) == 0) return(c())
      trimws(strsplit(txt, ",")[[1]])
    }
    # =========================================================================
    # PCA
    # =========================================================================
    
    pca_plot_obj <- reactiveVal(NULL)
    
    observeEvent(input$run_pca, {
      shinyjs::html(id = session$ns("pca_run_status"),
        html = loading_bar_html("Running PCA..."), asis = TRUE)
      shinyjs::delay(150, pca_trigger(pca_trigger() + 1))
    })

    observeEvent(pca_trigger(), {
      req(pca_trigger() > 0)
      data <- tryCatch(combined_mz(), error = function(e) NULL)
      meta <- tryCatch(active_meta(), error = function(e) NULL)

      pca_plot_obj(NULL)

      if (is.null(data)) {
        hide_progress("pca_progress_wrap")
        showNotification(
          "No m/z values survived the combine method — try 'union' or a looser consensus.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (nrow(data) == 0) {
        hide_progress("pca_progress_wrap")
        showNotification(
          "The selected m/z table is empty — no m/z values matched the base table. Try 'union' or check your m/z format.",
          type = "error"
        )
        return()
      }

      if (is.null(meta)) {
        hide_progress("pca_progress_wrap")
        showNotification(
          "Metadata is required for PCA grouping. Please upload metadata on the Analysis Hub",
          type = "error"
        )
        return()
      }
      
      bl       <- parse_blacklist(input$pca_blacklist)
      title    <- if (nchar(trimws(input$pca_title))    > 0) trimws(input$pca_title)    else "PCA"
      qual_col <- tryCatch(group(), error = function(e) NULL)
      
      if (is.null(qual_col) || !nzchar(trimws(qual_col))) {
        qual_col <- "phenotype"
      }
      
      pca_plot_obj(NULL)
      shinyjs::disable("run_pca")

      tryCatch({
      set_progress("pca_progress_bar", "pca_progress_label", "pca_progress_wrap",
                   20, "Building matrix\u2026")
      rownames(data) <- data$mz
      t_mz_obj <- scale(t(dplyr::select(data, -mz)))
      pca_bare_names <- sub("^(Positive|Negative)_", "", rownames(t_mz_obj))
      t_mz_obj <- t_mz_obj[!pca_bare_names %in% bl, , drop = FALSE]
      
      set_progress("pca_progress_bar", "pca_progress_label", "pca_progress_wrap",
                   40, "Attaching group labels\u2026")
      
      meta_fixed <- tryCatch(
        local.meta_polarity_fixer(data, meta),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      
      if (is.null(meta_fixed)) {
        shinyjs::enable("run_pca")
        hide_progress("pca_progress_wrap")
        return()
      }
      
      if (!qual_col %in% colnames(meta_fixed)) {
        showNotification(
          sprintf(
            "Column '%s' not found in metadata. Available: %s",
            qual_col,
            paste(colnames(meta_fixed), collapse = ", ")
          ),
          type = "error"
        )
        shinyjs::enable("run_pca")
        hide_progress("pca_progress_wrap")
        return()
      }
      
      pol_short <- tryCatch(
        local.mz_polarity_guesser(data, pos_return = "pos", neg_return = "neg"),
        error = function(e) "pos"
      )
      
      orig_rownames <- rownames(t_mz_obj)
      short_rownames <- sub(
        "^(Positive|Negative)_",
        paste0(pol_short, "_"),
        orig_rownames
      )
      
      rownames(t_mz_obj) <- short_rownames
      
      meta_col <- tibble::remove_rownames(meta_fixed) |>
        tibble::column_to_rownames("sample_name")
      
      t_mz_obj <- merge(
        x = t_mz_obj,
        y = subset(meta_col, select = qual_col),
        by = 0,
        all.x = TRUE
      )
      
      na_groups <- is.na(t_mz_obj[[qual_col]])
      
      if (any(na_groups)) {
        showNotification(
          sprintf(
            "%d sample(s) had no metadata match and will be unlabelled: %s",
            sum(na_groups),
            paste(t_mz_obj$Row.names[na_groups], collapse = ", ")
          ),
          type = "warning",
          duration = 8
        )
        
        t_mz_obj[[qual_col]][na_groups] <- "unknown"
      }
      
      rownames(t_mz_obj) <- paste(
        t_mz_obj$Row.names,
        t_mz_obj[[qual_col]],
        sep = "&"
      )
      
      pca_input <- subset(
        t_mz_obj,
        select = -c(Row.names, get(qual_col))
      )
      
      set_progress("pca_progress_bar", "pca_progress_label", "pca_progress_wrap",
                   65, "Computing principal components\u2026")
      pca_res <- tryCatch(
        prcomp(pca_input),
        error = function(e) { 
          shinyjs::enable("run_pca")
          showNotification(conditionMessage(e), type = "error"); NULL }
      )
      if (is.null(pca_res)) { shinyjs::enable("run_pca"); hide_progress("pca_progress_wrap"); return() }
      var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)

      set_progress("pca_progress_bar", "pca_progress_label", "pca_progress_wrap",
                   88, "Building plot\u2026")
      pol <- polarity()
      plot_df_sep <- pca_res$x |>
        as.data.frame() |>
        tibble::rownames_to_column("sample_qual") |>
        tidyr::separate(sample_qual, c("sample", qual_col), sep = "&")
      plot_df_sep[[qual_col]] <- factor(plot_df_sep[[qual_col]],
                                        levels = sort(unique(plot_df_sep[[qual_col]])))
      n_groups <- length(levels(plot_df_sep[[qual_col]]))
      group_sizes <- table(plot_df_sep[[qual_col]])
      enough_for_ellipse <- sum(group_sizes >= 3) >= 2
      p <- ggplot2::ggplot(plot_df_sep, ggplot2::aes(x = PC1, y = PC2, colour = .data[[qual_col]])) +
        ggplot2::geom_point(size = 1.5, alpha = 0.7) +
        ggplot2::stat_ellipse(
          ggplot2::aes(colour = .data[[qual_col]]),
          type = "norm", linewidth = 0.6,
          data = if (enough_for_ellipse) plot_df_sep else plot_df_sep[0, ]
        ) +
        ggplot2::geom_point(
          data = dplyr::summarise(
            dplyr::group_by(plot_df_sep, dplyr::across(dplyr::all_of(qual_col))),
            PC1 = mean(PC1), PC2 = mean(PC2)
          ),
          ggplot2::aes(colour = .data[[qual_col]]),
          size = 3, shape = 3, stroke = 1.2
        ) +
        ggplot2::labs(
          title  = paste(title, pol),
          x      = paste0("PC1: ", round(var_explained[1] * 100, 1), "%"),
          y      = paste0("PC2: ", round(var_explained[2] * 100, 1), "%"),
          colour = qual_col
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title   = ggplot2::element_text(hjust = 0.5, face = "bold"),
          legend.title = ggplot2::element_text(face  = "bold")
        )
      
      set_progress("pca_progress_bar", "pca_progress_label", "pca_progress_wrap",
                   100, "Done \u2714")
      pca_plot_obj(p)
      shinyjs::enable("run_pca")
      shinyjs::enable("download_pca")
      updateTabsetPanel(session, "output_tabs", selected = "PCA")
      shinyjs::delay(400, hide_progress("pca_progress_wrap"))
      shinyjs::html(id = session$ns("pca_run_status"),
      html = loading_bar_done_html(), asis = TRUE)
      }, error = function(e) {
        shinyjs::enable("run_pca")
        hide_progress("pca_progress_wrap")
        showNotification(paste("Unexpected error:", conditionMessage(e)), type = "error")
      })
    })
    
    output$pca_ui <- renderUI({
      req(pca_plot_obj())
      tagList(
        h4("PCA Plot"),
        plotOutput(ns("pca_plot"), height = "500px")
      )
    })
    
    output$pca_plot <- renderPlot({
      req(pca_plot_obj())
      suppressWarnings(print(pca_plot_obj()))
    })
    
    output$download_pca <- downloadHandler(
      filename = function() paste0(trimws(input$pca_title), ".png"),
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = pca_plot_obj(),
                        device = "png", width = 8, height = 6, dpi = 300)
      }
    )
    
    # =========================================================================
    # Heatmap
    # =========================================================================
    
    heatmap_png_path <- reactiveVal(NULL)
    
    observeEvent(input$run_heatmap, {
      shinyjs::html(id = session$ns("heatmap_run_status"),
        html = loading_bar_html("Running Heatmap..."), asis = TRUE)
      shinyjs::delay(150, heatmap_trigger(heatmap_trigger() + 1))
    })

    observeEvent(heatmap_trigger(), {
      req(heatmap_trigger() > 0)
      data <- tryCatch(combined_mz(), error = function(e) NULL)
      meta <- tryCatch(active_meta(), error = function(e) NULL)

      heatmap_png_path(NULL)

      if (is.null(data)) {
        hide_progress("heatmap_progress_wrap")
        showNotification(
          "No m/z values survived the combine method — try 'union' or a looser consensus.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (nrow(data) == 0) {
        hide_progress("heatmap_progress_wrap")
        showNotification(
          "The selected m/z table is empty — no m/z values matched the base table. Try 'union' or check your m/z format.",
          type = "error"
        )
        return()
      }
      
      title      <- if (nchar(trimws(input$heatmap_title)) > 0) trimws(input$heatmap_title) else "Heatmap of intensities"
      label_size <- c(input$heatmap_label_mz, input$heatmap_label_sample)
      cluster    <- c("row" = input$heatmap_cluster_row, "col" = input$heatmap_cluster_col)
      
      heatmap_png_path(NULL)
      shinyjs::disable("run_heatmap")
      
      tryCatch({ 
      set_progress("heatmap_progress_bar", "heatmap_progress_label", "heatmap_progress_wrap",
                   20, "Building intensity matrix\u2026")
      plot_obj <- as.matrix(dplyr::select(data, -mz))
      rownames(plot_obj) <- data$mz
      
      set_progress("heatmap_progress_bar", "heatmap_progress_label", "heatmap_progress_wrap",
                   45, "Building phenotype annotation\u2026")
      annotation_int <- NULL
      if (!is.null(meta)) {
        pol_short <- tryCatch(
          local.mz_polarity_guesser(data, pos_return = "pos", neg_return = "neg"),
          error = function(e) NULL
        )
        if (!is.null(pol_short)) {
          current_samps <- colnames(plot_obj)
          match_idx     <- match(current_samps, paste0(pol_short, "_", meta$sample_name))
          matched       <- !is.na(match_idx)
          if (any(matched)) {
            ann_phenotype <- rep(NA_character_, length(current_samps))
            ann_phenotype[matched] <- meta[match_idx[matched], "phenotype"]
            annotation_int <- data.frame(
              row.names = current_samps,
              phenotype = ann_phenotype
            )
            if (!all(matched)) {
              showNotification(
                sprintf("%d sample(s) had no metadata match — shown without annotation.",
                        sum(!matched)),
                type = "warning", duration = 8
              )
            }
          }
        }
      }
      
      detail <- if (cluster["row"] || cluster["col"]) "Clustering\u2026" else "Preparing layout\u2026"
      set_progress("heatmap_progress_bar", "heatmap_progress_label", "heatmap_progress_wrap",
                   70, detail)
      
      set_progress("heatmap_progress_bar", "heatmap_progress_label", "heatmap_progress_wrap",
                   90, "Rendering to file\u2026")
      tmp    <- tempfile(fileext = ".png")
      result <- tryCatch({
        pheatmap::pheatmap(
          t(plot_obj),
          cluster_rows   = cluster["row"],
          cluster_cols   = cluster["col"],
          annotation_row = annotation_int,
          show_rownames  = label_size[2] > 0,
          show_colnames  = label_size[1] > 0,
          fontsize_row   = label_size[2],
          fontsize_col   = label_size[1],
          main           = title,
          filename       = tmp
        )
        TRUE
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        FALSE
      })
      
      if (!result) { shinyjs::enable("run_heatmap"); hide_progress("heatmap_progress_wrap"); return() }
      
      set_progress("heatmap_progress_bar", "heatmap_progress_label", "heatmap_progress_wrap",
                   100, "Done.")
      heatmap_png_path(tmp)
      shinyjs::enable("run_heatmap")
      shinyjs::enable("download_heatmap")
      updateTabsetPanel(session, "output_tabs", selected = "Heatmap")
      shinyjs::delay(400, hide_progress("heatmap_progress_wrap"))
      shinyjs::html(id = session$ns("heatmap_run_status"),
      html = loading_bar_done_html(), asis = TRUE)
        
      }, error = function(e) {                                          
        shinyjs::enable("run_heatmap")
        hide_progress("heatmap_progress_wrap")
        showNotification(paste("Unexpected error:", conditionMessage(e)), type = "error")
    })
    })
    
    output$heatmap_ui <- renderUI({
      req(heatmap_png_path())
      tagList(
        h4("Heatmap"),
        uiOutput(ns("heatmap_img_ui"))
      )
    })
    
    output$heatmap_img_ui <- renderUI({
      req(heatmap_png_path())
      img_path <- heatmap_png_path()
      if (!is.null(img_path) && file.exists(img_path)) {
        img_b64 <- base64enc::base64encode(img_path)
        tags$img(
          src   = paste0("data:image/png;base64,", img_b64),
          style = "max-width: 100%; height: auto;"
        )
      }
    })
    
    output$download_heatmap <- downloadHandler(
      filename = function() paste0(trimws(input$heatmap_title), ".png"),
      content  = function(file) {
        req(heatmap_png_path())
        file.copy(heatmap_png_path(), file)
      }
    )
    
    # =========================================================================
    # Navigation
    # =========================================================================
    
    go_back <- eventReactive(input$back_prev,       { TRUE })
    go_next <- eventReactive(input$go_next_to_name, { TRUE })
    
    return(list(
      go_back = go_back,
      go_next = go_next
    ))
  })
}
