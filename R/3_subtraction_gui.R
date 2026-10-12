# UI -------------------------------------------------------------

subtraction_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Previous Step"),
      br(), br(),
      h4("1. Data"),
      fileInput(ns("dims_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      tags$small("Data and metadata from previous step loaded by default, overwrite with alternative dataset if desired."),
      br(),br(),
      tags$small("This page requires metadata, upload a metadata to the previous page if it has not been passed on."),
      hr(),
      uiOutput(ns("subtraction_data_status")),
      uiOutput(ns("subtraction_meta_status")),
      h4("2. Assign groups"),
      tags$small(
        style = "color: #888;",
        "Column options auto-detected from sample names"
      ),
      checkboxGroupInput(ns("blank_groups"), "Subtract", choices = NULL),
      uiOutput(ns("cell_groups_ui")),
      hr(),
      h4("3. Parameters"),
      radioButtons(
        ns("subtraction_mode"), "Subtraction method",
        choices  = c("Blank aggregation", "Blank and Media Max"),
        selected = "Blank aggregation"
      ),
      hr(),
      
      # Blank aggregation controls
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Blank aggregation'", ns("subtraction_mode")),
        h5("Blank aggregation"),
        selectInput(ns("method"), "Aggregation method",
                    choices  = c("mean", "median"),
                    selected = "mean"
        ),
        numericInput(ns("threshold"), "Threshold",
                     value = 0, min = 0, step = 0.5
        )
      ),
      
      # Blank and Media Max controls
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Blank and Media Max'", ns("subtraction_mode")),
        h5("Blank and Media Max"),
        selectInput(ns("bm_method"), "Aggregation method",
                    choices  = c("mean", "median"),
                    selected = "mean"
        ),
        numericInput(ns("bm_threshold"), "Threshold",
                     value = 0, min = 0, step = 0.5
        )
      ),
      hr(),
      actionButton(ns("subtraction_run"), "Run Subtraction",
                   class = "btn-primary", width = "100%"
      ),
      br(), br(),
      uiOutput(ns("subtraction_status")),
      downloadButton(ns("subtraction_download"), "Download result (.csv)"),
      br(), br(),
      actionButton(ns("go_next_to_mass_defect"), "Go to Next Step",
                   class = "btn-info", width = "100%"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("main_tabs"),
        tabPanel(
          "Preview",
          tableOutput(ns("preview_raw")),
          DT::dataTableOutput(ns("sample_summary"))
        ),
        tabPanel(
          "Split check",
          verbatimTextOutput(ns("cell_cols")),
          verbatimTextOutput(ns("blank_cols"))
        ),
        tabPanel(
          "Result",
          verbatimTextOutput(ns("summary")),
          plotOutput(ns("plot")),
          DT::dataTableOutput(ns("result_table")),
          br()
        )
      )
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════

subtraction_server <- function(id, previous_result = NULL, quality_check_result = NULL,
                               meta = NULL, group = NULL, full_long = NULL, sample_names = NULL) {
  moduleServer(id, function(input, output, session) {
    
    status_text  <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    
    meta_status_text  <- reactiveVal("No Metadata Loaded ❌")
    meta_status_color <- reactiveVal("red")
    
    
    run_trigger <- reactiveVal(0)

    session$onFlushed(function() {
      shinyjs::disable(session$ns("subtraction_download"))
      shinyjs::disable(session$ns("go_next_to_mass_defect"))
    }, once = TRUE)
    
    ### Upload handling ###
    data     <- dataloader_module(file_reactive = reactive({ input$dims_file }))
    raw_long <- data$long
    raw_wide <- data$wide
    
    prev_data <- reactive({
      prev <- previous_result
      if (is.null(prev)) prev <- quality_check_result
      if (is.null(prev)) return(NULL)
      if (is.function(prev)) prev() else prev
    })
    
    is_long_format <- function(df) {
      all(c("mz", "intensity", "sample_name") %in% colnames(df))
    }
    
    to_wide <- function(df_long) {
      if (exists("long_to_mz_obj", mode = "function")) {
        return(long_to_mz_obj(df_long))
      }
      df_long <- df_long |>
        dplyr::mutate(
          mz        = as.numeric(mz),
          intensity = as.numeric(intensity)
        ) |>
        dplyr::select(mz, intensity, sample_name)
      
      tidyr::pivot_wider(
        df_long,
        names_from   = sample_name,
        values_from  = intensity,
        values_fill  = 0,
        values_fn    = mean
      ) |>
        dplyr::arrange(mz) |>
        dplyr::mutate(mz = as.numeric(mz)) |>
        as.data.frame()
    }
    
    input_long <- reactive({
      if (!is.null(input$dims_file)) return(raw_long())
      fl <- if (is.function(full_long)) full_long() else full_long
      if (!is.null(fl)) return(fl)
      prev <- prev_data()
      if (is.null(prev)) return(NULL)
      if (is_long_format(prev)) return(prev)
      tidyr::pivot_longer(prev, cols = -mz, names_to = "sample_name", values_to = "intensity")
    })
    
    input_wide <- reactive({
      if (!is.null(input$dims_file)) return(raw_wide())
      prev <- prev_data()
      if (is.null(prev)) return(NULL)
      if (is_long_format(prev)) return(to_wide(prev))
      prev
    })
    
    ### Detect groups from dataset ###
    sample_groups <- reactive({
      names <- if (!is.null(sample_names) && is.null(input$dims_file)) {
        sn <- if (is.function(sample_names)) sample_names() else sample_names
        trimws(sn)
      } else {
        req(input_long())
        trimws(unique(input_long()$sample_name))
      }
      meta_df   <- if (is.function(meta)) meta() else meta
      group_col <- if (is.function(group)) group() else group
      if (!is.null(meta_df) && !is.null(group_col) && group_col %in% colnames(meta_df)) {
        lookup  <- setNames(meta_df[[group_col]], meta_df$sample_name)
        sort(unique(lookup[names][!is.na(lookup[names])]))
      } else {
        sort(unique(sub("_\\d+.*$", "", names)))
      }
    })
    
    observe({
      req(sample_groups())
      groups     <- sample_groups()
      auto_blank <- groups[grepl("blank|media|qc", groups, ignore.case = TRUE)]
      updateCheckboxGroupInput(session, "blank_groups", choices = groups, selected = auto_blank)
    })
    
    ### Subtract/Keep group UI — Sidebar ###
    output$cell_groups_ui <- renderUI({
      groups      <- sample_groups()
      keep_groups <- setdiff(groups, input$blank_groups)
      
      tagList(
        tags$label("Keep"),
        tags$small(
          style = "color: #888; display: block; margin-bottom: 4px;",
          "Auto-selects options not chosen in Subtract."
        ),
        tags$div(
          style = "pointer-events: none; accent-color: black;",
          checkboxGroupInput(session$ns("cell_groups"), NULL,
                             choices  = groups,
                             selected = keep_groups
          )
        )
      )
    })
    
    observe({
      if (!is.null(meta())) {
        meta_status_text("Metadata Loaded ✅")
        meta_status_color("green")
      } else {
        meta_status_text("No Metadata Loaded ❌")
        meta_status_color("red")
      }
    })
    
    observe({
      if (!is.null(input$dims_file)) {
        status_text("Custom Dataset Loaded ✅")
        status_color("green")
      } else if (!is.null(quality_check_result())) {
        status_text("Dataset Loaded ✅")
        status_color("green")
      } else {
        status_text("No Data Loaded ❌")
        status_color("red")
      }
    })
    
    ### Run logic ###
    observeEvent(input$subtraction_run, {
      shinyjs::disable(id = session$ns("subtraction_run"), asis = TRUE)
      shinyjs::html("subtraction_status",
        loading_bar_html("Applying Subtraction..."), add = FALSE)
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
    })

    observeEvent(result(), {
      req(result())
      shinyjs::html("subtraction_status", loading_bar_done_html(), add = FALSE)
      shinyjs::enable(id = session$ns("subtraction_download"), asis = TRUE)
      shinyjs::enable(id = session$ns("go_next_to_mass_defect"), asis = TRUE)
      shinyjs::enable(session$ns("subtraction_run"),                 asis = TRUE)
      updateTabsetPanel(session, "main_tabs", selected = "Result")
    })
    
    output$preview_raw <- renderTable({
      req(input_long())
      head(input_long(), 10)
    })
    
    output$sample_summary <- DT::renderDataTable({
      req(input_long())
      lookup <- sample_group_lookup()
      input_long() |>
        mutate(group = lookup[sample_name]) |>
        group_by(group) |>
        summarise(
          n_samples  = n_distinct(sample_name),
          n_features = n(),
          .groups    = "drop"
        )
    },
    rownames = FALSE,
    options  = list(pageLength = 10, dom = "t"))
    
    mz_wide <- reactive({
      req(input_wide())
      input_wide()
    })
    
    sample_group_lookup <- reactive({
      df_long   <- input_long()
      meta_df   <- if (is.function(meta)) meta() else meta
      group_col <- if (is.function(group)) group() else group
      
      if (!is.null(meta_df) && !is.null(group_col) && group_col %in% colnames(meta_df)) {
        setNames(meta_df[[group_col]], meta_df$sample_name)
      } else {
        samples <- trimws(unique(df_long$sample_name))
        setNames(sub("_\\d+.*$", "", samples), samples)
      }
    })
    
    sample_mz_obj <- reactive({
      req(mz_wide(), length(input$cell_groups) > 0)
      lookup <- sample_group_lookup()
      keep   <- colnames(mz_wide()) == "mz" | lookup[colnames(mz_wide())] %in% input$cell_groups
      mz_wide()[, keep]
    })
    
    subtract_mz_obj <- reactive({
      req(mz_wide(), length(input$blank_groups) > 0)
      lookup <- sample_group_lookup()
      keep   <- colnames(mz_wide()) == "mz" | lookup[colnames(mz_wide())] %in% input$blank_groups
      mz_wide()[, keep]
    })
    
    output$cell_cols <- renderPrint({
      req(sample_mz_obj())
      cols <- colnames(dplyr::select(sample_mz_obj(), -mz))
      if (length(cols) == 0) cat("(none — select at least one cell group)") else cat(cols, sep = "\n")
    })
    
    output$blank_cols <- renderPrint({
      req(subtract_mz_obj())
      cols <- colnames(dplyr::select(subtract_mz_obj(), -mz))
      if (length(cols) == 0) cat("(none — select at least one blank group)") else cat(cols, sep = "\n")
    })

    ###Result Logic###

    result <- eventReactive(run_trigger(), {
      req(run_trigger() > 0)
      validate(
        need(length(input$cell_groups) > 0,  "Select at least one cell group"),
        need(length(input$blank_groups) > 0,  "Select at least one blank group"),
        need(ncol(sample_mz_obj()) > 1,       "No cell columns found"),
        need(ncol(subtract_mz_obj()) > 1,     "No blank columns found")
      )
      
      tryCatch({
        sub_df <- subtract_mz_obj()
        
        # Blank and Media Max
        if (input$subtraction_mode == "Blank and Media Max") {
          method_fun <- switch(input$bm_method, "mean" = mean, "median" = median)
          
          cols   <- colnames(sub_df)[colnames(sub_df) != "mz"]
          lookup <- sample_group_lookup()
          groups <- lookup[cols]
          
          is_blank <- grepl("blank", groups, ignore.case = TRUE)
          is_media <- grepl("media", groups, ignore.case = TRUE)
          
          validate(
            need(any(is_blank), "No 'blank' samples detected in subtract group"),
            need(any(is_media), "No 'media' samples detected in subtract group")
          )
          
          blank_df <- sub_df[, c("mz", cols[is_blank]), drop = FALSE]
          media_df <- sub_df[, c("mz", cols[is_media]), drop = FALSE]
          
          blank_val <- apply(dplyr::select(blank_df, -mz), 1, method_fun)
          media_val <- apply(dplyr::select(media_df, -mz), 1, method_fun)
          
          combined    <- pmax(blank_val, media_val)
          combined_df <- data.frame(mz = sub_df$mz, combined = combined)
          
          return(
            mz_subtraction(
              sample_mz_obj   = sample_mz_obj(),
              subtract_mz_obj = combined_df,
              method          = function(x) x,
              threshold       = input$bm_threshold
            )
          )
        }
        
        # Blank aggregation (default)
        method_fun <- switch(input$method, "mean" = mean, "median" = median)
        
        mz_subtraction(
          sample_mz_obj   = sample_mz_obj(),
          subtract_mz_obj = sub_df,
          method          = method_fun,
          threshold       = input$threshold
        )
        
      }, error = function(e) {
        shinyjs::enable(session$ns("subtraction_run"))
        shinyjs::html(
          "subtraction_status",
          paste0("<span style='color:red;'>Error: ", conditionMessage(e), "</span>"),
          asis = TRUE
        )
        NULL
      })
    })
    
    output$summary <- renderPrint({
      req(result())
      n_before <- nrow(sample_mz_obj())
      n_after  <- nrow(result())
      cat("m/z before :", n_before, "\n")
      cat("m/z after  :", n_after, "\n")
      cat("m/z removed:", n_before - n_after, "\n")
      cat("% kept     :", round(100 * n_after / n_before, 1), "%\n")
    })
    
    output$plot <- renderPlot({
      req(result())
      samp    <- sample_mz_obj()
      plot_df <- data.frame(
        mz             = samp$mz,
        mean_intensity = rowMeans(dplyr::select(samp, -mz)),
        status         = ifelse(samp$mz %in% result()$mz, "Survived", "Removed")
      )
      ggplot(plot_df, aes(x = mz, y = log2(mean_intensity + 1), colour = status)) +
        geom_point(size = 0.8, alpha = 0.6) +
        scale_colour_manual(values = c("Removed" = "#e74c3c", "Survived" = "#2ecc71")) +
        labs(
          title  = "m/z survival",
          x      = "m/z",
          y      = "log2(mean intensity)",
          colour = NULL
        ) +
        theme_bw(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "top")
    })
    
    output$result_table <- DT::renderDataTable({
      req(result())
      result()
    },
    rownames = FALSE,
    options  = list(
      pageLength = 15,
      dom        = "ltip",
      scrollX    = TRUE
    ))
    
    ### Download Handling ###
    output$subtraction_download <- downloadHandler(
      filename = function() "mz_subtraction.csv",
      content = function(file) {
       fwrite(result(), file)
      }
    )
    
    output$subtraction_data_status <- renderUI({
      tags$span(
        style = paste0("color: ", status_color(), ";"),
        status_text()
      )
    })
    
    output$subtraction_meta_status <- renderUI({
      tags$span(
        style = paste0("color: ", meta_status_color(), ";"),
        meta_status_text()
      )
    })
    
    return(list(
      result  = result,
      go_next = reactive(input$go_next_to_mass_defect),
      go_back = reactive(input$back_prev),
      keep    = reactive(input$cell_groups)
    ))
  })
}