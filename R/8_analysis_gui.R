# =========================
# UI
# =========================

analysis_page_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("analysis_ui")),
    div(
      id = ns("file_section"),
      tags$p(class = "small",
             "Data and metadata from previous step loaded by default, overwrite with alternative pre-processed dataset if desired."
      ),
      tags$p(class = "small","Wide format -- Used for most analysis methods:"),
      fileInput(ns("input_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      tags$p(class = "small","Long format -- Used for ANOVA method:"),
      fileInput(ns("input_long"), "OVERWRITE long form datatset (.csv)", accept = ".csv"),
      tags$p(class = "small","Meta Data -- Used for PCA, Heatmap and ANOVA:"),
      fileInput(ns("input_meta"), "OVERWRITE metadata (.csv)", accept = ".csv"),
      
      uiOutput(ns("data_status")),
      br(),
      h4("Select Grouping Column"),
      tags$p(class = "small","Previously selected grouping column passed on by default. Choose grouping column after uploading custom data or update this setting if desired here."),
      uiOutput(ns("group_col_ui")),
      
    ),
    tags$style(HTML("
.card-box {
  border: 1px solid #ddd;
  border-radius: 12px;
  padding: 20px;
  text-align: center;
  box-shadow: 2px 2px 10px rgba(0,0,0,0.08);
  background: white;
  transition: 0.2s;
  cursor: pointer;

  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
}

.card-box:hover {
  transform: translateY(-3px);
  box-shadow: 2px 4px 14px rgba(0,0,0,0.15);
}

.card-container {
  display: flex;
  flex-wrap: wrap;
  gap: 15px;
}

.card-item {
  flex: 1 1 calc(33.333% - 15px);
  display: flex;
}

.card-item-half {
  flex: 1 1 calc(50% - 15px);
  display: flex;
}

@media (max-width: 768px) {
  .card-item,
  .card-item-half {
    flex: 1 1 100%;
  }}
  
body {
  padding-bottom: 100px;

}
"))
  )
}


# =========================
# SERVER
# =========================

analysis_page_server <- function(id,previous_result,previous_long,prev_meta,keep,group,polarity){
  
  moduleServer(id, function(input, output, session) {
    analysis_mode <- reactiveVal("hub")
    analysis_data <- reactiveVal(NULL)
    analysis_long <- reactiveVal(NULL)
    analysis_meta <- reactiveVal(NULL)
    
    welch_mz <- reactiveVal(NULL)
    fold_mz  <- reactiveVal(NULL)
    anova_mz <- reactiveVal(NULL)
    rf_mz    <- reactiveVal(NULL)
    
    current_group <- reactiveVal(NULL)
    custom_meta_uploaded <- reactiveVal(FALSE)

    
    observe({
      req(input$group_col)
      current_group(input$group_col)
    })|> bindEvent(input$group_col, ignoreInit = TRUE)
    
    
    ###Sub-page definition###
    vis_result    <- visualization_page_server("vis", analysis_data,analysis_long,analysis_meta,polarity,
                                               reactive(current_group()), 
                                               welch_mz = reactive(welch_mz()),
                                               fold_mz  = reactive(fold_mz()),
                                               anova_mz = reactive(anova_mz()),
                                               rf_mz    = reactive(rf_mz()))
    stats_result  <- statistics_page_server("stats", analysis_data, analysis_long, analysis_meta, keep,
                                            reactive(current_group()))
    class_result <- classification_page_server("class", previous_result = reactive({
      list(
        result_wide = analysis_data(),
        meta        = analysis_meta()
      )
    }),  reactive(current_group()))
    shortcut_result <- shortcut_page_server("shortcut", analysis_data)
    
    ###Mz table handling###
    observe({
      val <- tryCatch(stats_result$welch_mz(), error = function(e) NULL)
      if (!is.null(val)) welch_mz(val)
    })
    
    observe({
      val <- tryCatch(stats_result$fold_mz(), error = function(e) NULL)
      if (!is.null(val)) fold_mz(val)
    })
    
    observe({
      val <- tryCatch(stats_result$anova_mz(), error = function(e) NULL)
      if (!is.null(val)) anova_mz(val)
    })
    
    observe({
      val <- tryCatch(class_result$rf_mz(), error = function(e) NULL)
      if (!is.null(val)) rf_mz(val)
    })
    
    
    ###Card UI###
    output$analysis_ui <- renderUI({
      if (analysis_mode() == "hub") {
        tagList(
          h2("MeDUSA Analysis"),
          div(
            class = "card-container",
            div(
              class = "card-item",
              div(
                class = "card-box",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("go_vis")),
                h3("Visualization"),
                p("PCA, heatmap")
              )
            ),
            div(
              class = "card-item",
              div(
                class = "card-box",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("go_stats")),
                h3("Statistics"),
                p("Welch t-test, fold change, volcano plot, ANOVA")
              )
            ),
            div(
              class = "card-item",
              div(
                class = "card-box",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("go_class")),
                h3("Classification"),
                p("Random forest")
              )
            )
          ),
          br(),
          h3("Other:"),
          div(
            class = "card-container",
            div(
              class = "card-item-half",
              div(
                class = "card-box",
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", session$ns("go_id")),
                h3("Go to Identification"),
                p("Finish analysis")
              )
            )
          ),
          h3("Overwrite Data?"),
        )
      } else if (analysis_mode() == "vis") {
        visualization_page_ui(session$ns("vis"))
      } else if (analysis_mode() == "stats") {
        statistics_page_ui(session$ns("stats"))
      } else if (analysis_mode() == "class") {
        classification_page_ui(session$ns("class"))
      } else if (analysis_mode() == "shortcut") {
        shortcut_page_ui(session$ns("shortcut"))
      }
    })
    
    ### Set Group Logic ###
    ##Active metadata##
    current_meta <- reactive({
      meta <- if (!is.null(analysis_meta())) {
        analysis_meta()
      } else {
        tryCatch(
          if (is.reactive(prev_meta)) prev_meta() else prev_meta,
          error = function(e) NULL
        )
      }
      
      if (is.null(meta) || !is.data.frame(meta)) return(NULL)
      meta
    })
    
    output$group_col_ui <- renderUI({
      custom_meta_uploaded()
      tryCatch({
        meta <- current_meta()
        if (is.null(meta)) return(NULL)
        choices <- setdiff(names(meta), c("sample_name", "filtered_out"))
        selected_val <- if (custom_meta_uploaded()) {
          ""
        } else {
          g <- tryCatch(if (is.reactive(group)) group() else group, error = function(e) NULL)
          if (!is.null(g) && g %in% choices) g else choices[1]
        }
        
        selectInput(session$ns("group_col"), "Primary grouping column:",
                    choices  = c("Select a column" = "", choices),
                    selected = selected_val
        )
      }, error = function(e) {
        message("group_col_ui error: ", e$message)
        NULL
      })
    })
    

    ###Subpage Navigation###
    observeEvent(input$go_vis, {
      analysis_mode("vis")
    })

    observeEvent(input$go_stats, {
      analysis_mode("stats")
    })

    observeEvent(input$go_class, {
      analysis_mode("class")
    })

    observeEvent(input$go_shortcut, {
      analysis_mode("shortcut")
    })
    
    ###Upload handling###
    file_reactive <- reactive(input$input_file)
    loaded_data <- dataloader_module(file_reactive)
    raw_data <- loaded_data$raw
    
    file_reactive_long <- reactive(input$input_long)
    loaded_data_long <- dataloader_module(file_reactive_long)
    raw_data_long <- loaded_data_long$raw
    
    observeEvent(input$input_meta, {
      req(input$input_meta)
      tryCatch({
        meta <- read.csv(input$input_meta$datapath, stringsAsFactors = FALSE)
        analysis_meta(meta)
        custom_meta_uploaded(TRUE)
      }, error = function(e) {
        message("meta load error: ", e$message)
      })
    })
    
    
    
    observeEvent(input$input_file, {
      analysis_data(raw_data())
    })
    observeEvent(input$input_long, {
      analysis_long(raw_data_long())
    })
    
    observeEvent(previous_result(), {
      if (is.null(analysis_data())) {
        analysis_data(previous_result())
        analysis_long(previous_long())
        analysis_meta(if (is.reactive(prev_meta)) prev_meta() else prev_meta)
      }
    }, once = TRUE)
    

    
    output$data_status <- renderUI({
      
      file_loaded <- !is.null(input$input_file)

      prev <- tryCatch(previous_result(), error = function(e) NULL)
      prev_loaded <- !is.null(prev)

      if (file_loaded) {
        span("Custom Dataset(s) Loaded ✅", style = "color:green;")
      } else if (prev_loaded) {
        span("Previous Dataset(s) Loaded ✅", style = "color:green;")
      } else {
        span("No Data Loaded ❌", style = "color:red;")
      }
    })

    observe({
      if (analysis_mode() == "hub") {
        shinyjs::show(id = session$ns("file_section"), asis = TRUE)
      } else {
        shinyjs::hide(id = session$ns("file_section"), asis = TRUE)
      }
    })

    ### To and from Navigation
    observeEvent(vis_result$go_back(), {
      analysis_mode("hub")
    })
    observeEvent(stats_result$go_back(), {
      analysis_mode("hub")
    })
    observeEvent(class_result$go_back(), {
      analysis_mode("hub")
    })
    observeEvent(shortcut_result$go_back(), {
      analysis_mode("hub")
    })

    go_next_trigger <- reactiveVal(NULL)

    observeEvent(input$go_id, {
      go_next_trigger(Sys.time())
    })
    observeEvent(vis_result$go_next(), {
      go_next_trigger(Sys.time())
    })
    observeEvent(stats_result$go_next(), {
      go_next_trigger(Sys.time())
    })
    observeEvent(class_result$go_next(), {
      go_next_trigger(Sys.time())
    })
    observeEvent(shortcut_result$go_next(), {
      go_next_trigger(Sys.time())
    })

    return(list(
      go_back = reactive(NULL),
      go_next = go_next_trigger,
      data = analysis_data,
      data_long = analysis_long,
      welch_mz = welch_mz,
      fold_mz = fold_mz,
      anova_mz = anova_mz,
      rf_mz = rf_mz,
      reset = function() analysis_mode("hub")
    ))
  })
}
