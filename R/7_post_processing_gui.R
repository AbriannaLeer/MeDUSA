###### UI #########

postpro_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Previous Step"),
      br(), br(),
      h4("1. Data"),
      fileInput(ns("input_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      tags$small("Data from previous step is loaded by default. You can overwrite it with an alternative filtered dataset if needed."),
      br(),br(),
      tags$small("This page requires metadata, upload metadata to the upload page and select the primary grouping column, if not done previously."),
      br(), br(),
      uiOutput(ns("data_status")),
      uiOutput(ns("metadata_status")),
      hr(),
      h4("2. Parameters"),
      radioButtons(
        ns("polarity"),
        "Sample polarity",
        choices = c("Positive", "Negative"),
        selected = "Positive"
      ),
      numericInput(ns("low_noise"), "Low noise bound", value = 10, min = 0, step = 1),
      numericInput(ns("high_noise"), "High noise bound", value = 5000, min = 0, step = 1),
      checkboxInput(ns("run_plots"), "Generate normalization and pivot plots", value = TRUE),
      hr(),
      actionButton(ns("run_step"), "Run Post-Processing",
                   class = "btn-primary", width = "100%"
      ),
      br(), br(),
      uiOutput(ns("progress_wrap")),
      uiOutput(ns("current_status")),
      br(),
      downloadButton(ns("download_mzlong"), "Download mzLong result (.csv)"),
      br(), br(),
      downloadButton(ns("download_mzlog"), "Download mzLog result (.csv)"),
      br(), br(),
      downloadButton(ns("download_metadata"), "Download processed metadata (.csv)"),
      br(),br(),
      actionButton(ns("go_next_to_name"), "Go to Next Step",
                   class = "btn-info", width = "100%"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("main_tabs"),
        selected = "Summary",
        tabPanel(
          "Summary",
          br(),
          h4("Post-Processing Summary"),
          verbatimTextOutput(ns("postpro_summary"))
        ),
        tabPanel(
          "mzLog preview",
          br(),
          DT::dataTableOutput(ns("mzlog_preview"))
        ),
        tabPanel(
          "mzLong preview",
          br(),
          DT::dataTableOutput(ns("mzlong_preview"))
        ),
        tabPanel(
          "Plots",
          uiOutput(ns("plots_ui"))
        )
      )
    )
  )
}

##### SERVER######################
postpro_page_server <- function(id, previous_result, prev_meta, polarity,group) {
  moduleServer(id, function(input, output, session) {
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    meta_text <- reactiveVal("No Metadata Loaded ❌")
    meta_color <- reactiveVal("red")
    run_text <- reactiveVal("Idle")
    run_color <- reactiveVal("black")
    run_trigger <- reactiveVal(0)

    session$onFlushed(function() {
      shinyjs::disable(session$ns("download_mzlong"))
      shinyjs::disable(session$ns("download_mzlog"))
      shinyjs::disable(session$ns("download_metadata"))
      shinyjs::disable(session$ns("go_next_to_name"))
    }, once = TRUE)
    
    
    
    extract_previous_data <- function(value) {
      if (is.function(value)) {
        value <- value()
      }
      
      if (is.list(value) && "data" %in% names(value)) {
        return(value$data)
      }
      
      value
    }
    
    ###Normalization###
    normalize_sample_polarity <- function(input_mz_obj, polarity_choice) {
      sample_cols <- setdiff(colnames(input_mz_obj), "mz")
      if (length(sample_cols) == 0) {
        return(input_mz_obj)
      }
      
      if (all(grepl("^(pos|neg)_", sample_cols))) {
        return(input_mz_obj)
      }
      
      prefix <- if (identical(polarity_choice, "Negative")) "neg" else "pos"
      renamed <- input_mz_obj
      colnames(renamed)[colnames(renamed) != "mz"] <- paste0(prefix, "_", sample_cols)
      renamed
    }
    
    normalize_metadata_names <- function(meta) {
      if (is.null(meta) || !"sample_name" %in% colnames(meta)) {
        return(meta)
      }
      
      meta$sample_name <- sub("^(pos|neg)_", "", meta$sample_name)
      meta
    }
    
    ###Upload handling###
    file_reactive <- reactive(input$input_file)
    uploaded_data <- dataloader_module(file_reactive)
    
    input_data <- reactive({
      if (!is.null(input$input_file)) {
        uploaded_data$raw()
      } else {
        req(previous_result)
        extract_previous_data(previous_result)
      }
    })
    observe({
      if (!is.null(input$input_file)) {
        status_text("Custom Dataset Loaded ✅")
        status_color("green")
      } else if (!is.null(extract_previous_data(previous_result))) {
        status_text("Previous Dataset Loaded ✅")
        status_color("green")
      } else {
        status_text("No Data Loaded ❌")
        status_color("red")
      }
    })
    
    observe({
      meta <- tryCatch(
        if (is.reactive(prev_meta)) prev_meta() else prev_meta,
        error = function(e) NULL
      )
      if (!is.null(meta)) {
        meta_text("Previous Metadata Loaded ✅")
        meta_color("green")
      } else {
        meta_text("No Metadata Loaded ❌")
        meta_color("red")
      }
    })
    
    ###Polarity Handling###
    observe({
      req(polarity())
      updateRadioButtons(session, "polarity", selected = polarity())
    })
    
    observeEvent(input$polarity, {
      polarity(input$polarity)
    }, ignoreInit = TRUE)
    
    ###Step logic###
    observeEvent(input$run_step, {
      shinyjs::html(id = session$ns("progress_wrap"),
        html = loading_bar_html("Running post-processing..."), asis = TRUE)
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
    }, ignoreInit = TRUE)
    
    processed_result <- eventReactive(run_trigger(), {
      req(run_trigger() > 0)
      dat        <- isolate(input_data())
      meta       <- isolate(prev_meta())
      sel_col    <- isolate(group())
      low_noise  <- isolate(input$low_noise)
      high_noise <- isolate(input$high_noise)
      run_plots  <- isolate(input$run_plots)
      polarity   <- isolate(polarity())
      
      req(dat, meta)
      validate(
        need(input$low_noise < input$high_noise, "Low noise must be smaller than high noise")
      )
      shinyjs::disable(id = session$ns("run_step"), asis = TRUE)
      run_text("Running post-processing...")
      run_color("orange")
      
      numeric_cols <- vapply(dat, is.numeric, logical(1))
      dat_clean <- dat[, numeric_cols | names(dat) == "mz", drop = FALSE]
      dat_clean <- normalize_sample_polarity(dat_clean, polarity)
      
      meta <- meta[meta$type != "blank", , drop = FALSE]
      meta <- normalize_metadata_names(meta)
      
      ###Hardcoded phenotype workaround###
      if (!is.null(sel_col) && sel_col != "phenotype") {
        
        if ("phenotype" %in% names(meta)) {
          meta$rf_original_phenotype <- meta$phenotype
        }
        
        meta$phenotype <- meta[[sel_col]]
      }
      
      tryCatch({
        post <- mz_post_magic(
          input_mz_obj = dat_clean,
          metadata = meta,
          noise = c(low_noise,high_noise),
          plot = FALSE
        )
        
        meta <- meta[meta$sample_name %in% sub("^(pos|neg)_", "", colnames(post$mzLog)), , drop = FALSE]
        
        run_text("")
        run_color("green")
        
        list(
          data = post$mzLong,
          mzLong = post$mzLong,
          mzLog = post$mzLog,
          metadata = meta,
          normalized_data = dat_clean
          
          
        )
      }, error = function(e) {
        shinyjs::enable(id = session$ns("run_step"), asis = TRUE)
        run_text(paste("Error:", conditionMessage(e)))
        run_color("red")
        shinyjs::html(id = session$ns("progress_wrap"), html = "", asis = TRUE)
        NULL
      })
    })
    
    observeEvent(processed_result(), {
      req(processed_result())
      shinyjs::html(id = session$ns("progress_wrap"),
                    html = loading_bar_done_html(), asis = TRUE)
      shinyjs::enable(id = session$ns("download_mzlong"), asis = TRUE)
      shinyjs::enable(id = session$ns("download_mzlog"), asis = TRUE)
      shinyjs::enable(id = session$ns("download_metadata"), asis = TRUE)
      shinyjs::enable(id = session$ns("go_next_to_name"), asis = TRUE)
      shinyjs::enable(id = session$ns("run_step"), asis = TRUE)
      updateTabsetPanel(session, "main_tabs", selected = "Summary")
    })
    
    ###Status bars###
    
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
    
    output$metadata_status <- renderUI({
      tags$span(
        style = paste0("color: ", meta_color(), ";"),
        meta_text()
      )
    })
    
    ###Result handling###
    output$postpro_summary <- renderPrint({
      req(processed_result())
      
      res <- processed_result()
      dat <- isolate(input_data())
      
      cat("Input rows:", nrow(dat), "\n")
      cat("Input sample columns:", ncol(dat) - 1, "\n")
      cat("Metadata rows:", nrow(res$metadata), "\n")
      cat("mzLog rows:", nrow(res$mzLog), "\n")
      cat("mzLong rows:", nrow(res$mzLong), "\n")
      cat("Polarity:", polarity(), "\n")
      cat("Zero intensities before:", sum(dat[, colnames(dat) != "mz"] == 0, na.rm = TRUE), "\n")
      cat("Zero intensities after:", sum(res$mzLog[, colnames(res$mzLog) != "mz"] == 0, na.rm = TRUE), "\n")
    })
    
    output$mzlog_preview <- DT::renderDataTable({
      req(processed_result())
      DT::formatRound(
        DT::datatable(
          processed_result()$mzLog,
          rownames = FALSE,
          options = list(
            pageLength = 15,
            scrollX = TRUE
          )
        ),
        columns = "mz",
        digits = 4
      )
    })
    
    output$mzlong_preview <- DT::renderDataTable({
      req(processed_result())
      DT::formatRound(
        DT::datatable(
          processed_result()$mzLong,
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
    
    ###Plot output###
    output$plots_ui <- renderUI({
      req(processed_result())
      
      if (!isTRUE(input$run_plots)) {
        return(
          tags$p(
            style = "color: grey;",
            "Plot generation was disabled for this run."
          )
        )
      }
      
      tagList(
        br(),
        h4("Normalization"),
        plotOutput(session$ns("normalization_plot")),
        
        br(),
        h4("Pivot Scatter"),
        plotOutput(session$ns("pivot_scatter")),
        
        br(),
        h4("Pivot Boxplot"),
        plotOutput(session$ns("pivot_box"))
      )
    })
    
    output$normalization_plot <- renderPlot({
      req(processed_result())
      
      dat <- processed_result()$mzLog
      
      result <- quotNorm(t(dplyr::select(dat, -mz)))
      
      dilution <- data.frame(
        normal.dilution = result$dilution
      )
      
      dilution$sample <- rownames(dilution)
      dilution$sample <- sub("^(pos|neg)_", "", dilution$sample)
      
      join_by <- c("sample")
      names(join_by) <- "sample_name"
      
      dilution <- dplyr::left_join(
        processed_result()$metadata,
        dilution,
        by = join_by
      )
      
      ggpubr::ggboxplot(
        dilution,
        x = "phenotype",
        y = "normal.dilution",
        ylab. = "Dilution Factor",
        xlab. = "Phenotype",
        title = "Normalized Dilution Factor per Phenotype",
        subtitle = polarity(),
        legend = "none"
      )
    })
    
    ###Scatterplot - pivot
    output$pivot_scatter <- renderPlot({
      req(processed_result())
      
      ggpubr::ggscatter(
        processed_result()$mzLong,
        x = "sample_name",
        y = "intensity",
        xlab = "Sample_Name",
        ylab = "Intensity",
        shape = 20,
        size = 1,
        color = "intensity",
        title = paste(
          "PivotLonger:",
          polarity()
        ),
        rotate = TRUE
      ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    })
    
    ###boxplot - pivot
    output$pivot_box <- renderPlot({
      req(processed_result())
      
      ggpubr::ggboxplot(
        processed_result()$mzLong,
        x = "sample_name",
        y = "intensity",
        xlab = "Sample_Name",
        ylab = "Intensity",
        outlier.shape = 3,
        outlier.size = 0.1,
        title = paste(
          "PivotLonger box:",
          polarity()
        ),
        legend = "none",
        rotate = TRUE
      ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    })
    
    ###Download handling###
    output$download_mzlog <- downloadHandler(
      filename = function() "postprocessed_mzLog.csv",
      content = function(file) {
        req(processed_result())
        fwrite(processed_result()$mzLog, file)
      }
    )
    output$download_mzlong <- downloadHandler(
      filename = function() "postprocessed_mzLong.csv",
      content = function(file) {
        req(processed_result())
        fwrite(processed_result()$mzLong, file)
      }
    )
    
    output$download_metadata <- downloadHandler(
      filename = function() "postprocessed_metadata.csv",
      content = function(file) {
        req(processed_result())
        fwrite(processed_result()$metadata, file)
      }
    )
    
    return(list(
      go_back = reactive(input$back_prev),
      result_wide = reactive({
        req(processed_result())
        processed_result()$mzLog
      }),
      result_long = reactive({
        req(processed_result())
        processed_result()$mzLong
      }),
      meta = reactive({
        req(processed_result())
        processed_result()$metadata
      }),
      go_next = reactive(input$go_next_to_name)
    ))
  })
}
