#########UI##########

tagging_page_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Previous Step"),
      br(), br(),

      h4("1. Data"),
      fileInput(ns("input_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      uiOutput(ns("data_status")),
      hr(),

      h4("2. Hunter Parameters"),
      numericInput(ns("iso_target"), "Isotope Mass Target",
                   value = 1.0034, step = 0.0001),
      numericInput(ns("iso_iter"), "Iterations (Max Isotopes)",
                   value = 5, min = 1, max = 20),
      numericInput(ns("tol_ppm"), "Tolerance (ppm)",
                   value = 5, min = 0.1),

      hr(),



      actionButton(ns("run_step"), "Run Isotope Hunter",
                   class = "btn-primary", width = "100%"),
      br(), br(),

      uiOutput(ns("current_status")),

      br(), br(),

      downloadButton(ns("download_report"), "Download Isotope Report (.csv)"),
      br(), br(),

      actionButton(ns("go_next_to_name"), "Skip to Next Step",
                   class = "btn-info", width = "100%")
    ),

    mainPanel(
      h3("Isotope Groups Found"),

      wellPanel(
        h5("Column Definitions:"),
        tags$ul(
          tags$li(tags$b("Group:"), " Counting index for the identified isotopic cluster."),
          tags$li(tags$b("mz:"), " The measured mass-to-charge ratio of the feature."),
          tags$li(tags$b("diff_to_prev:"), " Mass difference from the immediately preceding peak."),
          tags$li(tags$b("total_dist:"), " Cumulative distance from monoisotopic parent.")
        )
      ),

      verbatimTextOutput(ns("summary_text")),
      DT::dataTableOutput(ns("isotope_preview"))
    )
  )
}

##############SERVER################
tagging_page_server <- function(id, previous_result) {
  moduleServer(id, function(input, output, session) {
    
    cached_data <- reactiveVal(NULL)
    
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")

    session$onFlushed(function() {
      shinyjs::disable(session$ns("download_report"))
    }, once = TRUE)

    # -------------------------
    # DATA INPUT
    # -------------------------

    observe({
      if (!is.null(input$input_file)) {
        cached_data(read.csv(input$input_file$datapath, check.names = FALSE))
      } else {
        res <- if (is.function(previous_result)) previous_result() else previous_result
        if (is.list(res) && "data" %in% names(res)) {
          cached_data(res$data)
        } else {
          cached_data(res)
        }
      }
    })

    observe({
      if (!is.null(input$input_file) || !is.null(previous_result())) {
        status_text("Data Ready ✅")
        status_color("green")
      }
    })
    # -------------------------
    # MAIN COMPUTATION
    # -------------------------


    result <- eventReactive(input$run_step, {

      data_snapshot <- cached_data()

      iso_target_val <- isolate(input$iso_target)
      iso_iter_val   <- isolate(input$iso_iter)
      ppm_val        <- isolate(input$tol_ppm) / 1e6

      req(data_snapshot)

      shinyjs::disable("run_step")
      shinyjs::disable("go_next_to_name")

      shinyjs::html(
        "current_status",
        loading_bar_html("Hunting isotopes...")
      )

      tryCatch({
        iso_list <- mz_tag_isotope_hunter(
          input_mz   = data_snapshot,
          iso_target = iso_target_val,
          iso_iter   = iso_iter_val,
          tol_ppm    = ppm_val,
          cores      = 1
        )
        
        flat <- if (length(iso_list) > 0) {
          data.table::rbindlist(iso_list)
        } else {
          data.frame(Status = "No isotopes found.")
        }
       

        shinyjs::html("current_status", loading_bar_done_html())
        shinyjs::enable("download_report")
        shinyjs::enable("run_step")
        shinyjs::enable("go_next_to_name")

        updateActionButton(session, "go_next_to_name", label = "Go to Next Step")

        list(
          isotope_groups = iso_list,
          flat = flat,
          data = data_snapshot,
          target = iso_target_val
        )

      }, error = function(e) {

        shinyjs::enable("run_step")
        shinyjs::enable("go_next_to_name")

        shinyjs::html(
          "current_status",
          paste0("<span style='color:red;'>Error: ", conditionMessage(e), "</span>"),
          asis = TRUE
        )

        NULL
      })
    })

    # -------------------------
    # OUTPUTS
    # -------------------------
    output$summary_text <- renderPrint({
      req(result())

      groups <- result()$isotope_groups
      total_features <- sum(sapply(groups, nrow))

      cat("Target Mass Used:", result()$target, "\n")
      cat("Total Isotope Groups Found:", length(groups), "\n")
      cat("Total Isotopic Features Tagged:", total_features, "\n")
    })

    output$isotope_preview <- DT::renderDataTable({
      req(result())

      DT::datatable(
        result()$flat,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$download_report <- downloadHandler(
      filename = function() "isotope_detailed_report.csv",
      content = function(file) {
        req(result())
        data.table::fwrite(result()$flat, file)
      }
    )

    output$data_status <- renderUI({
      tags$span(style = paste0("color:", status_color(), ";"), status_text())
    })

    return(list(
      go_back = reactive(input$back_prev),
      result  = reactive({
        res <- tryCatch(result(), error = function(e) NULL)

        if (!is.null(res)) return(res)

        req(cached_data())

        list(
          isotope_groups = list(),
          flat = data.frame(Status = "Skipped"),
          data = cached_data(),
          target = NULL
        )
      }),
      go_next = reactive(input$go_next_to_name)
    ))
  })
}

