# UI
classification_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Analysis Hub",class = "btn-info", width = "100%"),
      br(), br(),
      h4("1. Data"),
      tags$small("Data from previous post-processing step is loaded by default."),
      br(), br(),
      uiOutput(ns("data_status")),
      hr(),
      h4("2. Parameters"),
      numericInput(ns("cor_cutoff"),
        "Correlation cutoff (0–1)",
        value = 0.75, min = 0.01, max = 0.99, step = 0.05
      ),
      tags$small("Higher value = fewer features removed."),
      br(), br(),
      numericInput(ns("cores"), "Cores", value = 4, min = 1, max = 32, step = 1),
      checkboxInput(ns("run_plots"), "Generate diagnostic plots", value = TRUE),
      radioButtons(ns("polarity"), "Sample polarity",
        choices  = c("Positive", "Negative"),
        selected = "Positive"
      ),
      tags$small(
        style = "color: #888;",
        "Note: Classification typically takes several minutes on the example dataset; longer on larger datasets."
      ),
      hr(),
      actionButton(ns("run_step"), "Run Classification",
        class = "btn-primary", width = "100%"
      ),
      br(), br(),
      uiOutput(ns("progress_wrap")),
      uiOutput(ns("current_status")),
      br(),
      downloadButton(ns("download_imp_mz"), "Download Important m/z (.csv)"),
      br(), br(),
      downloadButton(ns("download_rf_model"), "Download RF Model (.Rdata)"),
      br(), br(),
      actionButton(ns("go_next_to_name"), "Go to Identification",
        class = "btn-info", width = "100%"
      )
    ),

    mainPanel(
      tabsetPanel(
        id = ns("rf_tabs"),
        selected = "Summary",

        tabPanel(
          "Summary",
          br(),
          h4("Classification Summary"),
          verbatimTextOutput(ns("rf_summary"))
        ),

        tabPanel(
          "Confusion Matrix",
          br(),
          verbatimTextOutput(ns("confusion_matrix"))
        ),

        tabPanel(
          "Important m/z",
          br(),
          h4("Important m/z (top 50, ranked by MeanDecreaseGini)"),
          DT::dataTableOutput(ns("imp_mz_table"))
        )
      )
    )
  )
}

##### SERVER ######################
classification_page_server <- function(id, previous_result,group) {
  moduleServer(id, function(input, output, session) {
    
    rf_result <- reactiveVal(NULL)
    run_text     <- reactiveVal("Idle")
    run_color    <- reactiveVal("black")
    status_text  <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    run_trigger  <- reactiveVal(0)

    session$onFlushed(function() {
      shinyjs::disable(session$ns("download_imp_mz"))
      shinyjs::disable(session$ns("download_rf_model"))
      shinyjs::disable(session$ns("go_next_to_name"))
    }, once = TRUE)

    
    ###Data handling###
    extract_previous_data <- function(value) {
      if (is.function(value)) value <- value()
      if (is.list(value) && "result_wide" %in% names(value)) {
        r <- value$result_wide
        if (is.function(r)) return(r())
        return(r)
      }
      if (is.list(value) && "data" %in% names(value)) return(value$data)
      value
    }

    extract_previous_metadata <- function(value) {
      if (is.function(value)) value <- value()
      if (is.list(value) && "meta" %in% names(value)) {
        m <- value$meta
        if (is.function(m)) return(m())
        return(m)
      }
      if (is.list(value) && "metadata" %in% names(value)) return(value$metadata)
      NULL
    }

    input_data <- reactive({
      req(previous_result)
      extract_previous_data(previous_result)
    })

    metadata_data <- reactive({
      req(previous_result)
      meta <- extract_previous_metadata(previous_result)
      validate(need(!is.null(meta), "No metadata found from the previous step."))
      meta
    })
    
    observe({
      if (!is.null(input_data())) {
        status_text("Previous Dataset Loaded ✅")
        status_color("green")
      } else {
        status_text("No Data Loaded ❌")
        status_color("red")
      }
    })

    ###Run logic###
    observeEvent(input$run_step, {
      shinyjs::disable(id = session$ns("run_step"), asis = TRUE)
      shinyjs::html(id = session$ns("progress_wrap"),
        html = loading_bar_html("Running classification..."), asis = TRUE)
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
    }, ignoreInit = TRUE)

    observeEvent(run_trigger(), {
        req(run_trigger() > 0)
        req(input_data(), metadata_data())
        
        mzLog      <- isolate(input_data())
        metadata   <- isolate(metadata_data())
        sel_col    <- isolate(group())
        cor_cutoff <- isolate(input$cor_cutoff)
        cores_val  <- isolate(input$cores)
        run_plots  <- isolate(input$run_plots)
        pol        <- if (isolate(input$polarity) == "Negative") "neg" else "pos"
        
        res <- tryCatch({
        
        validate(
          need(!is.null(sel_col) && length(sel_col) == 1,
               "No grouping column selected (sel_col is NULL).")
        )
        
        validate(
          need(sel_col %in% names(metadata),
               paste("Column not found in metadata:", sel_col)))
          
          # ---- 0. Determine the two phenotypes to classify
          bio_phenos <- setdiff(unique(metadata[[sel_col]]), c("blank", "media", "qc"))
          validate(need(length(bio_phenos) >= 2,
                        "Need at least two biological phenotypes for classification."))
          if (length(bio_phenos) > 2) {
            counts <- sort(
              table(metadata[[sel_col]][metadata[[sel_col]] %in% bio_phenos]),
              decreasing = TRUE
            )
            bio_phenos <- names(counts)[1:2]
          }
          cell_meta <- metadata[metadata[[sel_col]] %in% bio_phenos, , drop = FALSE]
          validate(need(nrow(cell_meta) >= 4,
                        "Not enough samples in the chosen phenotypes."))
          
          
          
          # ---- 1. Filter mzLog to those samples
          run_text("Step 1/4: Filtering samples to selected phenotypes… ⏳")
          run_color("orange")
          keep_samples   <- intersect(
            sub("^(pos|neg)_", "", setdiff(colnames(mzLog), "mz")),
            cell_meta$sample_name
          )
          mzLog_filtered <- mzLog[, c("mz", paste0(pol, "_", keep_samples)), drop = FALSE]
          cell_meta <- cell_meta[cell_meta$sample_name %in% keep_samples, , drop = FALSE]

          mzLog_cells    <- mztools_filter(
            mzLog_filtered, cell_meta,
            filter_value = bio_phenos,
            filter_name  = sel_col
          )
          
          # ---- 2. Drop non-finite feature rows
          run_text("Step 2/4: Cleaning non-finite features… ⏳")
          run_color("orange")
          feature_cols <- setdiff(colnames(mzLog_cells), "mz")
          finite_rows  <- apply(
            mzLog_cells[, feature_cols, drop = FALSE], 1,
            function(r) all(is.finite(r))
          )
          mzLog_cells <- mzLog_cells[finite_rows, , drop = FALSE]
          
          # ---- 3. Correlation filter
          run_text("Step 3/4: Correlation filter… ⏳")
          run_color("orange")
          rf_matrix   <- data.frame(t(dplyr::select(mzLog_cells, -mz)))
          feature_ids <- as.character(mzLog_cells$mz)
          if (ncol(rf_matrix) > 1) {
            cor_mat      <- cor(rf_matrix)
            high_cor_idx <- caret::findCorrelation(cor_mat, cutoff = cor_cutoff)
          } else {
            high_cor_idx <- integer(0)
          }
          keep_idx            <- setdiff(seq_along(feature_ids), high_cor_idx)
          rf_matrix           <- rf_matrix[, keep_idx, drop = FALSE]
          colnames(rf_matrix) <- feature_ids[keep_idx]
          cor_data            <- cbind(sample_name = rownames(rf_matrix), rf_matrix)
          validate(need(ncol(cor_data) > 1,
                        "No RF features remain after correlation filtering."))
          
          rf_meta <- cell_meta
          
          ##Hardcoded phenotype column name workaround##
          if (sel_col != "phenotype") {
            
            if ("phenotype" %in% names(rf_meta)) {
              rf_meta$rf_original_phenotype <- rf_meta$phenotype
            }
            
            rf_meta$phenotype <- rf_meta[[sel_col]]
          }
          
          
          # ---- 4. Feature selection (rfe)
          run_text("Step 4/4: Feature selection & RF training… ⏳")
          run_color("orange")
          n_features    <- ncol(cor_data) - 1L
          feat_size_seq <- unique(pmin(seq(10, 500, by = 10), n_features))
          rfe_obj <- tryCatch(
            mzlog_rf_select(
              cor_data, rf_meta,
              feat_size_seq = feat_size_seq,
              plot          = isTRUE(run_plots),
              cores         = cores_val
            ),
            error = function(e) stop(paste("Feature selection failed:", conditionMessage(e)))
          )
          
          # ---- 5. Train Random Forest
          rf_obj <- mzlog_rf(
            mzlog_obj = mzLog_cells,
            metadata  = rf_meta,
            rfe_obj   = rfe_obj,
            cores     = cores_val,
            plot      = isTRUE(run_plots)
          )
          
          run_text("")
          run_color("green")
          shinyjs::enable(id = session$ns("run_step"), asis = TRUE)
          shinyjs::hide(id   = session$ns("progress_wrap"),    asis = TRUE)
          shinyjs::html(id = session$ns("progress_wrap"),
            html = loading_bar_done_html(), asis = TRUE)
          shinyjs::enable(id = session$ns("download_imp_mz"),  asis = TRUE)
          shinyjs::enable(id = session$ns("download_rf_model"),asis = TRUE)
          shinyjs::enable(id = session$ns("go_next_to_name"),  asis = TRUE)
          updateTabsetPanel(session, "rf_tabs", selected = "Summary")
          
          list(
            cor_data = cor_data,
            rfe_obj  = rfe_obj,
            rf_obj   = rf_obj,
            polarity = pol,
            phenos   = bio_phenos
          )
          
        }, error = function(e) {
          run_text(paste("Error:", conditionMessage(e)))
          shinyjs::enable(id = session$ns("run_step"), asis = TRUE)
          run_color("red")
          shinyjs::html(id = session$ns("progress_wrap"), html = "", asis = TRUE)
          try(while (sink.number() > 0) sink(), silent = TRUE)
          try(while (!is.null(dev.list())) dev.off(), silent = TRUE)
          NULL
        })
        rf_result(res)
      })
    
    output$current_status <- renderUI({
      tags$span(
        style = paste0("color: ", run_color(), "; font-weight: bold;"),
        run_text()
      )
    })
    
  
    output$data_status <- renderUI({
      tags$span(
        style = paste0("color: ", status_color(), ";"),
        status_text()
      )
    })
    
    
    ###Result output handling###
    output$rf_summary <- renderText({
      req(rf_result())
      res <- rf_result()
      tryCatch({
        paste0(
          "Polarity              : ", res$polarity, "\n",
          "Phenotypes classified : ", paste(res$phenos, collapse = " vs "), "\n",
          "Features after cor    : ", ncol(res$cor_data) - 1, "\n",
          "rfe best subset       : ", res$rfe_obj$bestSubset, " features\n",
          "rfe accuracy          : ", round(max(res$rfe_obj$results$Accuracy, na.rm = TRUE), 4), "\n",
          "RF OOB error rate     : ", round(res$rf_obj$model$err.rate[nrow(res$rf_obj$model$err.rate), "OOB"], 4), "\n",
          "Important m/z returned: ", nrow(res$rf_obj$imp_mz), "\n"
        )
      }, error = function(e) {
        paste("Error displaying summary:", conditionMessage(e))
      })
    })

    output$confusion_matrix <- renderPrint({
      req(rf_result())
      res   <- rf_result()$rf_obj
      preds <- predict(res$model, newdata = dplyr::select(res$test, -phenotype))
      cm    <- caret::confusionMatrix(preds, as.factor(res$test$phenotype))
      print(cm)
    })

    output$imp_mz_table <- DT::renderDataTable({
      req(rf_result())
      DT::formatRound(
        DT::datatable(
          rf_result()$rf_obj$imp_mz,
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = FALSE
          )
        ),
        columns = "mz",
        digits = 4
      )
    })
    
    
    ###Download handling###
    
    output$download_imp_mz <- downloadHandler(
      filename = function() paste0("important_mz_", rf_result()$polarity, ".csv"),
      content  = function(file) {
        fwrite(rf_result()$rf_obj$imp_mz, file)
      }
    )

    output$download_rf_model <- downloadHandler(
      filename = function() paste0("rf_model_", rf_result()$polarity, ".Rdata"),
      content  = function(file) {
        rf_obj <- rf_result()$rf_obj
        save(rf_obj, file = file)
      }
    )
    
    rf_mz <- reactive({
      req(rf_result())
      imp <- rf_result()$rf_obj$imp_mz
      if (!"mz" %in% names(imp)) imp <- tibble::rownames_to_column(imp, "mz")
      imp
    })

    return(list(
      go_back = reactive(input$back_prev),
      go_next = reactive(input$go_next_to_name),
      rf_mz = rf_mz
        ))
  })
}