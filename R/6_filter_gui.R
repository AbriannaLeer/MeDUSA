# UI Part
filter_page_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Previous Step"),
      br(), br(),
      h4("1. Data"),
      fileInput(ns("input_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      tags$small("Data from previous step loaded by default."),
      uiOutput(ns("data_status")),
      hr(),
      h4("2. Parameters"),
      numericInput(ns("min_intensity"), "Min Intensity Threshold",
        value = 5000, min = 0
      ),
      numericInput(ns("missingness_threshold"), "Missingness (fraction)",
        value = 0.1, min = 0, max = 1, step = 0.05
      ),
      selectInput(ns("blacklist_type"), "Blacklist",
                   choices = c(
                     "None" = "none",
                     "Default" = "default",
                     "Custom List" = "custom",
                     "Upload CSV" = "csv"
                   ),
                   selected = "default"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'custom'", ns("blacklist_type")),
        textInput(ns("custom_blacklist"), "Enter m/z values (comma-separated)",
                  placeholder = "e.g. 100.111, 105.122, 107,233"
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'csv'", ns("blacklist_type")),
        fileInput(ns("blacklist_file"), "Upload Blacklist CSV", accept = ".csv")
      ),
      hr(),
      actionButton(ns("run_step"), "Run Noise Filtering",
        class = "btn-primary", width = "100%"
      ),
      br(), br(),
      uiOutput(ns("current_status")),
      downloadButton(ns("download"), "Download result (.csv)"),
      br(), br(),
      actionButton(ns("go_next_to_name"), "Go to Next Step",
        class = "btn-info", width = "100%"
      )
    ),
    mainPanel(
      h4("Filtering Summary"),
      verbatimTextOutput(ns("filter_summary")),
      hr(),
      h4("Data Preview (Transposed)"),
      # Column explanations for the transposed layout
      wellPanel(
        h5("Table Layout Explanation:"),
        tags$ul(
          tags$li(tags$b("Sample_ID:"), " Rows represent individual samples."),
          tags$li(tags$b("mz_XXXX:"), " Columns represent m/z features."),
          tags$li(tags$b("Values:"), " Intensity of the feature in that sample.")
        ),
        tags$small("Note: Table is transposed for easier vertical scrolling across many samples.")
      ),
      
      DT::dataTableOutput(ns("filter_table"))
    )
  )
}

####### SERVER ######################
filter_page_server <- function(id, previous_result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    run_trigger <- reactiveVal(0)

    session$onFlushed(function() {
      shinyjs::disable(ns("download"))
      shinyjs::disable(ns("go_next_to_name"))
    }, once = TRUE)

    # Inherit data from Step 5 or manual upload
    input_data <- reactive({
      if (!is.null(input$input_file)) {
        return(read.csv(input$input_file$datapath, check.names = FALSE))
      } else {
        req(previous_result)
        # Extract the 'data' element from Step 5's list output
        step5_output <- if(is.function(previous_result)) previous_result() else previous_result
        
        if (is.list(step5_output) && "data" %in% names(step5_output)) {
          return(step5_output$data)
        }
        return(step5_output)
      }
    })

    # Update status indicator
    observe({
      if (!is.null(input$input_file) || !is.null(previous_result())) {
        status_text("Data Ready ✅")
        status_color("green")
      }
    })

    # Show the animated loading bar, then defer the heavy filtering by one
    # cycle so the bar is painted before R blocks.
    observeEvent(input$run_step, {
      shinyjs::html("current_status",
        loading_bar_html("Cleaning data and filtering noise..."))
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
    })

    # Main filtering logic
    result_df <- eventReactive(run_trigger(), {
      req(run_trigger() > 0)
      dat <- isolate(input_data())
      req(dat)
      shinyjs::disable("run_step")
      shinyjs::html("current_status", "Cleaning data and filtering noise...")

      # Sanitize data: ensure only numeric columns are processed
      numeric_cols <- sapply(dat, is.numeric)
      dat_clean <- dat[, numeric_cols, drop = FALSE]

      m_int <- input$min_intensity
      m_thr <- input$missingness_threshold
      bl <- switch(input$blacklist_type,
                   "none"    = NULL,
                   "default" = TRUE,
                   "custom"  = as.numeric(trimws(strsplit(input$custom_blacklist, ",")[[1]])),
                   "csv"     = input$blacklist_file$datapath
      )
      

      tryCatch({
        # Call core filtering function
        filtered <- mz_filter_magic(
          input_mz_obj = dat_clean,
          min_intensity = m_int,
          missingness_threshold = m_thr,
          blacklist = bl
        )
        
        shinyjs::enable("run_step")
        shinyjs::enable("download")
        shinyjs::enable("go_next_to_name")
        shinyjs::html("current_status", loading_bar_done_html())
        return(filtered)
        
      }, error = function(e) {
        shinyjs::enable("run_step")
        shinyjs::html("current_status", paste("Error:", e$message))
        return(NULL)
      })
    })

    # summary report
    output$filter_summary <- renderPrint({
      req(result_df())
      cat("Rows (features) before:", nrow(isolate(input_data())), "\n")
      cat("Rows (features) after: ", nrow(result_df()), "\n")
      if (nrow(result_df()) == 0) {
        cat("\n⚠️ WARNING: No features remaining after filtering!\n")
        cat("Consider rerunning with different thresholds.\n")
      }
    })

    output$filter_table <- DT::renderDataTable({
      req(result_df())
      df <- result_df()
      if (nrow(df) == 0) return(NULL)
      
      # Remove the preview cap, show all features
      mz_labels <- paste0("mz_", df$mz)
      
      df_transposed <- as.data.frame(t(df[, -1, drop = FALSE]))
      colnames(df_transposed) <- mz_labels
      df_transposed <- cbind(Sample_ID = rownames(df_transposed), df_transposed)
      
      DT::datatable(
        df_transposed,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          dom = "ltip",
          scrollX = TRUE
        )
      )
    })

    # Download filtered CSV
    output$download <- downloadHandler(
      filename = function() "filtered_results.csv",
      content = function(file) fwrite(result_df(), file)
    )

    output$data_status <- renderUI({
      tags$span(style = paste0("color: ", status_color(), ";"), status_text())
    })

    return(list(
      go_back = reactive(input$back_prev),
      result = result_df,
      go_next = reactive(input$go_next_to_name)
    ))
  })
}