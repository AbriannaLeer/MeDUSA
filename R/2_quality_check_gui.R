# ══════════════════════════════════════════════════════════════════════════
# Step 1 — Upload & Quality Check
# ══════════════════════════════════════════════════════════════════════════

long_to_mz_obj <- function(df) {
  if ("feature" %in% colnames(df)) {
    df <- df %>%
      dplyr::group_by(feature, sample_name) %>%
      dplyr::summarise(
        intensity = mean(intensity, na.rm = TRUE),
        mz        = mean(mz, na.rm = TRUE),
        .groups   = "drop"
      )
  }

  wide <- tidyr::pivot_wider(
    df[, c("mz", "intensity", "sample_name")],
    names_from   = "sample_name",
    values_from  = "intensity",
    values_fill  = 0,
    values_fn    = mean
  )

  wide <- wide %>%
    dplyr::arrange(mz) %>%
    dplyr::mutate(mz = as.numeric(mz))

  as.data.frame(wide)
}

is_long_format <- function(df) {
  all(c("intensity", "sample_name") %in% colnames(df))
}


# ══════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════
main_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

      
      h4("1. Upload Data"),
      fileInput(ns("input_file"),
        "DIMS dataset (.csv)",
        accept = ".csv"
      ),
      tags$small("Required columns: 'mz', 'intensity', 'sample_name'"),
      br(), br(),
      
      uiOutput(ns("data_status_ui")),
      hr(),
      
      fileInput(ns("meta_file"),
        "Metadata file (.csv)",
        accept = ".csv"
      ),
      tags$small("Required columns: 'sample_name', 'type' and any grouping column"),
      br(),br(),
    
      hr(),
      uiOutput(ns("meta_data_status_ui")),
      br(),
      uiOutput(ns("metadata_summary_ui")),
      br(),
      uiOutput(ns("grouping_selector_ui")),
      
      h4("2. Parameters"),
      radioButtons(
        ns("polarity"),
        "Sample polarity",
        choices = c("Positive", "Negative"),
        selected = "Positive"
      ),
      
      numericInput(ns("cores"),
        "CPU cores for parallel computation",
        value = 1, min = 1, max = 16, step = 1
      ),
      hr(),
      actionButton(ns("run_step"), "Run Quality Check",
        class = "btn-primary", width = "100%"
      ),
      br(), br(),

      uiOutput(ns("run_status_text")),
      
      hr(),
      downloadButton(ns("download_metrics"), "Download metrics (.csv)"),
      br(), br(),
      downloadButton(ns("download_mzobj"), "Download mz_obj wide (.csv)"),
      br(), br(),

      actionButton(ns("go_next_to_name"), "Go to Next Step",
        class = "btn-info", width = "100%"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("main_tabs"),
        tabPanel(
          "Data Preview",
          br(),
          uiOutput(ns("data_preview_ui"))
        ),
        tabPanel(
          "Metrics Table",
          br(),
          p(
            style = "color:#555;",
            "Quality metrics computed per sample."
          ),
          br(),
          DT::dataTableOutput(ns("metrics_table"))
          
        ),
        tabPanel(
          "Quality Plots",
          br(),
          fluidRow(
            column(
              4,
              selectInput(
                ns("plot_metric"),
                "Metric to display:",
                choices = c(
                  "median_mz", "min_mz", "max_mz",
                  "n_peaks", "peaks_1k", "peaks_10k", "peaks_100k"
                ),
                selected = "n_peaks"
              )
            ),
            column(
              4, br(),
              actionButton(ns("prev_metric"), "Prev", class = "btn-sm btn-default"),
              actionButton(ns("next_metric"), "Next", class = "btn-sm btn-default")
            )
          ),
          plotOutput(ns("quality_plot"), height = "460px")
        ),
        tabPanel(
          "Metadata Check",
          br(),
          uiOutput(ns("meta_check_ui"))
        )
      )
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════
main_page_server <- function(id,polarity) {
  moduleServer(id, function(input, output, session) {
    all_metrics <- c(
      "median_mz", "min_mz", "max_mz",
      "n_peaks", "peaks_1k", "peaks_10k", "peaks_100k"
    )
    
    session$onFlushed(function() {
      shinyjs::disable(session$ns("download_metrics"))
      shinyjs::disable(session$ns("download_mzobj"))
      shinyjs::disable(session$ns("download_plot"))
      shinyjs::disable(session$ns("go_next_to_name"))
    }, once = TRUE)
    
    run_trigger <- reactiveVal(0)
    metadata <- reactiveVal(NULL)
    
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    meta_text <- reactiveVal("No Metadata Loaded ❌")
    meta_color <- reactiveVal("red")
    
    file_reactive <- reactive(input$input_file)
    data <- dataloader_module(file_reactive)
    raw <- data$raw

    ##Create mz_object##
    mz_obj <- reactive({
      req(raw(), run_trigger() > 0)  
      df <- raw()
      if (is_long_format(df)) {
        long_to_mz_obj(df)
      } else {
        df
      }
    }) |> bindCache(input$input_file$name, input$input_file$size)
    
    ##Upload handling##
    observe({
      req(input$input_file)
      
      tryCatch(
        {
          df <- raw()
          
          if (is_long_format(df)) {
            n_samples <- length(unique(df$sample_name))
            
            n_features <- if ("feature" %in% colnames(df)) {
              length(unique(df$feature))
            } else {
              length(unique(df$mz))
            }
          } else {
            n_samples <- ncol(df) - 1
            n_features <- nrow(df)
          }
          
          status_text(
            paste0(
              "Dataset Loaded ✅ (",
              n_samples,
              " samples, ",
              n_features,
              " features)"
            )
          )
          
          status_color("green")
        },
        
        error = function(e) {
          status_text(
            paste("Load Error ❌", conditionMessage(e))
          )
          
          status_color("red")
        }
      )
    })

    output$data_status_ui <- renderUI({
      tags$span(
        style = paste0("color:", status_color(), ";"),
        status_text()
      )
    })
    
    ##Metadata upload handling##
    observe({
      req(input$meta_file)
      tryCatch({
        
        meta_df <- read.csv(
          input$meta_file$datapath,
          stringsAsFactors = FALSE
        )
        
        ##Check metadata##
        required_cols <- c("sample_name", "type")
        
        missing_cols <- setdiff(required_cols, colnames(meta_df))
        
        if (length(missing_cols) > 0) {
          stop(
            paste(
              "Missing required column(s):",
              paste(missing_cols, collapse = ", ")
            )
          )
        }
        
        
        metadata(meta_df)
        
        meta_text("Metadata Loaded ✅")
        meta_color("green")
        
      }, error = function(e) {
        
        metadata(NULL)
        
        meta_text(
          paste("Metadata Error ❌", conditionMessage(e))
        )
        meta_color("red")
      })
    })
    
    output$meta_data_status_ui <- renderUI({
      tags$span(
        style = paste0("color:", meta_color(), ";"),
        meta_text()
      )
    })
    ##Global polarity picker##
    observe({
      updateRadioButtons(session, "polarity", selected = polarity())
    })
    
    observeEvent(input$polarity, {
      polarity(input$polarity)
    }, ignoreInit = TRUE)
    
    ###Metadata preview --Sidebar
    output$metadata_summary_ui <- renderUI({
      req(metadata())
      
      meta <- metadata()
      
      cols <- colnames(meta)
      
      summary_tags <- lapply(cols, function(col) {
        
        vals <- unique(meta[[col]])
        vals <- vals[!is.na(vals)]
        
        preview <- paste(head(vals, 5), collapse = ", ")
        
        if (length(vals) > 5) {
          preview <- paste0(preview, ", ...")
        }
        
        tags$div(
          style = "
        margin-bottom:8px;
        padding-bottom:6px;
        border-bottom:1px solid #eee;
      ",
          tags$b(col),
          tags$br(),
          tags$small(
            style = "color:#666;",
            preview
          )
        )
      })
      
      tags$div(
        style = "
      border:1px solid #ddd;
      border-radius:6px;
      padding:10px;
      background:#fafafa;
      max-height:250px;
      overflow-y:auto;
      font-size:13px;
    ",
        tags$b("Metadata Columns"),
        tags$hr(style = "margin-top:5px;margin-bottom:10px;"),
        summary_tags
      )
    })
    
    ###Metadata column selector --Sidebar
    output$grouping_selector_ui <- renderUI({
      req(metadata())
      
      meta <- metadata()
      
      candidate_cols <- colnames(meta)
      
      candidate_cols <- setdiff(
        candidate_cols,
        c("sample_name", "filtered_out")
      )
      
      selectInput(
        session$ns("grouping_column"),
        "Primary grouping column",
        choices = candidate_cols,
        selected = if ("phenotype" %in% candidate_cols)
          "phenotype"
        else
          candidate_cols[1]
      )
    })
    
    samples <- reactive({
      req(raw())
      unique(raw()$sample_name)
    })
    
    ##Data preview##
    output$data_preview_ui <- renderUI({
      if (is.null(input$input_file)) {
        return(tags$p(style = "color:#888;", "Upload a file to see a preview."))
      }

      req(raw())
      df <- raw()

      tagList(
        h5("Raw file — first 10 rows:"),
        renderTable(head(df, 10), striped = TRUE, bordered = TRUE, digits = 4)
      )
    })
    
    ##Run status indicator##
    observeEvent(input$run_step, {
      
       shinyjs::disable("run_step")
       shinyjs::html(id = session$ns("run_status_text"),
        html = loading_bar_html("Applying quality check..."), asis = TRUE)
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
    })
    
    
    ##Execute step logic##
    result <- eventReactive(run_trigger(), {
      req(run_trigger() > 0)
      cores_val <- isolate(input$cores)
      req(mz_obj())
      
      tryCatch({
        wide <- mz_obj()
        metrics <- mz_quality_metrics(wide, cores = cores_val)
        list(data = wide, metrics = metrics)
      }, error = function(e) {
        shinyjs::enable(session$ns("run_step"))
        shinyjs::html(
          id = session$ns("run_status_text"),
          html = paste0("<span style='color: red;'>Error: ", conditionMessage(e), "</span>"),
          asis = TRUE
        )
        NULL
      })
    })

    observeEvent(result(), {
      session$onFlushed(function() {
        shinyjs::html(id = session$ns("run_status_text"),
          html = loading_bar_done_html(), asis = TRUE)
        
        shinyjs::enable(session$ns("run_step"))
        shinyjs::enable(session$ns("download_metrics"))
        shinyjs::enable(session$ns("download_mzobj"))
        shinyjs::enable(session$ns("download_plot"))
        shinyjs::enable(session$ns("go_next_to_name"))
      }, once = TRUE)
    })
    
    ###Result visualization logic###
    observeEvent(input$next_metric, {
      idx <- which(all_metrics == input$plot_metric)
      updateSelectInput(session, "plot_metric",
        selected = if (idx < length(all_metrics)) all_metrics[idx + 1] else all_metrics[1]
      )
    })

    observeEvent(input$prev_metric, {
      idx <- which(all_metrics == input$plot_metric)
      updateSelectInput(session, "plot_metric",
        selected = if (idx > 1) all_metrics[idx - 1] else all_metrics[length(all_metrics)]
      )
    })

    output$metrics_table <- DT::renderDataTable({
      req(result())
      result()$metrics
    }, options = list(pageLength = 15, dom = "ltip"), rownames = FALSE)

    output$quality_plot <- renderPlot({
      req(result(), input$plot_metric)

      mz_metrics <- result()$metrics
      focus <- input$plot_metric
      finite_vals <- mz_metrics[[focus]][is.finite(mz_metrics[[focus]])]

      p <- ggpubr::ggbarplot(
        mz_metrics,
        x     = "name",
        y     = focus,
        title = focus,
        fill  = focus,
        color = focus,
        xlab  = "Sample",
        ylab  = focus
      ) +
        ggpubr::rotate(ylim = c(min(finite_vals), max(finite_vals))) +
        ggplot2::scale_x_discrete(
          label = sapply(
            strsplit(mz_metrics$name, "_"),
            function(x) paste(x[-2:-1], collapse = ".")
          ),
          guide = ggplot2::guide_axis(n.dodge = 3)
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.y     = ggplot2::element_text(size = ggplot2::rel(0.6)),
          plot.title      = ggplot2::element_text(face = "bold", size = 14)
        )

      print(p)
    })
    
    meta_check_result <- eventReactive(run_trigger(), {
      req(run_trigger() > 0, mz_obj(), metadata())
      polarity_val <- isolate(polarity())
      tryCatch(
        {
          wide <- mz_obj()
          polarity_prefix <- tolower(substr(polarity_val, 1, 3))
          colnames(wide)[-1] <- paste0(polarity_prefix, "_", colnames(wide)[-1])
          
          mz_quality_meta_check(wide, metadata())
          list(ok = TRUE, errors = NULL)
        },
        error = function(e) {
          list(ok = FALSE, errors = conditionMessage(e))
        }
      )
    })
    
    output$meta_check_ui <- renderUI({
      if (is.null(metadata()) || is.null(mz_obj())) {
        return(tags$p(style = "color:#888;", "Upload both a dataset and metadata file to run the check."))
      }
      
      res <- meta_check_result()
      
      if (res$ok) {
        tags$div(
          style = "border:1px solid #c3e6cb; border-radius:6px; padding:14px 18px; background:#d4edda;",
          tags$span(style = "color:#155724; font-weight:600;", "✅ Metadata check passed"),
          tags$p(style = "color:#155724; margin:6px 0 0;",
                 "All samples and required columns are present and consistent.")
        )
      } else {
        error_lines <- strsplit(res$errors, "\n")[[1]]
        error_lines <- error_lines[nchar(trimws(error_lines)) > 0]
        
        tags$div(
          style = "border:1px solid #f5c6cb; border-radius:6px; padding:14px 18px; background:#f8d7da;",
          tags$span(style = "color:#721c24; font-weight:600;", "❌ Metadata check failed"),
          tags$ul(
            style = "color:#721c24; margin:8px 0 0; padding-left:18px;",
            lapply(error_lines, function(line) tags$li(line))
          )
        )
      }
    })
    
    ###Download handling###
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("quality_plot_", input$plot_metric, ".png")
      },
      content = function(file) {
        req(result(), input$plot_metric)

        mz_metrics <- result()$metrics
        focus <- input$plot_metric
        finite_vals <- mz_metrics[[focus]][is.finite(mz_metrics[[focus]])]

        p <- ggpubr::ggbarplot(
          mz_metrics,
          x = "name", y = focus,
          title = focus,
          fill = focus, color = focus
        ) +
          ggpubr::rotate(ylim = c(min(finite_vals), max(finite_vals))) +
          ggplot2::theme(legend.position = "none")

        ggplot2::ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
      }
    )

    output$download_metrics <- downloadHandler(
      filename = function() "quality_metrics.csv",
      content = function(file) {
        fwrite(result()$metrics, file)
      }
    )
    
    output$download_mzobj <- downloadHandler(
      filename = function() "mz_obj_wide.csv",
      content = function(file) {
        fwrite(result()$data, file)
      }
    )
    
    return(list(
      go_back = reactive(NULL),
      result  = reactive({
        req(result())
        result()$data
      }),
      meta = metadata,
      group = reactive({
        req(input$grouping_column)
        input$grouping_column
      }),
      full_long = raw,
      sample_names = reactive({ unique(raw()$sample_name) }),
      go_next = reactive(input$go_next_to_name)
    ))
  })
}
