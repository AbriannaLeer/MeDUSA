  # UI
  
  identify_page_ui <- function(id) {
    shinyjs::useShinyjs()
    ns <- NS(id)
    
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("back_prev"), "Back to Previous Step"),
        br(), br(),
        h4("1. Data"),
        fileInput(ns("input_file"), "OVERWRITE wide dataset/MZ table (.csv)", accept = ".csv"),
        tags$small("Data from previous step loaded by default, overwrite with alternative dataset if desired."),
        br(),br(),
        fileInput(ns("input_long"), "OVERWRITE full dataset (.csv)", accept = ".csv"),
        tags$small("Overwrite original dataset here, used for filtering by feature."),
        br(), br(),
        uiOutput(ns("data_status")),
        
        h4("2. Parameters"),
        selectInput(ns("db_choice"), "Select Database", 
                    choices = c("HMDB", "Lipids", "Custom CSV")),
        
        textInput(ns("adducts"), "Adducts (comma separated)", value = "M+H"),
        numericInput(ns("tolerance"), "Tolerance (ppm)", value = 5, step = 1),
        checkboxInput(ns("use_feature"), "Filter Mz's by unique feature identity", value = FALSE),
        tags$small("Requires 'feature' column in passed in dataset", style = "color: grey; margin-top: -10px; display: block;"),
        
        conditionalPanel(
          condition = paste0("input['", ns("db_choice"), "'] == 'Custom CSV'"),
          fileInput(ns("custom_db_file"), "Upload Custom Database (.csv)", accept = ".csv"),
          textInput(ns("mz_colname"), "M/Z Column Name", value = "EXACT_MASS")
        ),
        
        br(),
        actionButton(ns("run_step"), "Run Identification",
                     class = "btn-primary", width = "100%"
        ),
        br(), br(),
        uiOutput(ns("current_status")),
        downloadButton(ns("download"), "Download result (.csv)"),
      ),
      
      mainPanel(
        h4("Identification Results"),
        dataTableOutput(ns("id_table"))
      )
    )
  }
  ##### SERVER######################
  identify_page_server <- function(id, previous_result,previous_long) {
    moduleServer(id, function(input, output, session) {
      status_text <- reactiveVal("No Data Loaded X")
      status_color <- reactiveVal("red")
      run_trigger <- reactiveVal(0)

      session$onFlushed(function() {
        shinyjs::disable(session$ns("download"))
        shinyjs::disable(session$ns("go_next_to_name"))
      }, once = TRUE)
      
      ###Upload handling###
      file_reactive <- reactive(input$input_file)
      data <- dataloader_module(file_reactive)
      
      raw <- reactive({
        if (!is.null(input$input_file)) {
          d <- data$raw()
          if (!is.null(d) && "mz" %in% names(d)) return(d)
        }
        
        pr <- previous_result()
        if (!is.null(pr) && "mz" %in% names(pr)) return(pr)
        
        return(NULL)
      })
      
      observe({
        if (!is.null(input$input_file)) {
          status_text("Custom Dataset Loaded")
          status_color("green")
        } else if (!is.null(previous_result())) {
          status_text("Previous Dataset Loaded")
          status_color("green")
        } else {
          status_text("No Data Loaded X")
          status_color("red")
        }
      })
      
      file_reactive_long <- reactive(input$input_long)
      data_long <- dataloader_module(file_reactive_long)
      
      long_data <- reactive({
        if (!is.null(input$input_long)) {
          d <- data_long$raw()
          if (!is.null(d)) return(d)
        }
        tryCatch(previous_long(), error = function(e) NULL)
      })
      
      observe({
        pl <- tryCatch(long_data(), error = function(e) NULL)
        if (!is.null(pl) && "feature" %in% names(pl)) {
          shinyjs::enable("use_feature")
          updateCheckboxInput(session, "use_feature", value = TRUE)
        } else {
          shinyjs::disable("use_feature")
          updateCheckboxInput(session, "use_feature", value = FALSE)
        }
      })
      
      ###Run logic###
      observeEvent(input$run_step, {
        shinyjs::html("current_status", loading_bar_html("Searching databases..."))
        shinyjs::delay(150, run_trigger(run_trigger() + 1))
        shinyjs::disable(id = session$ns("run_step"), asis = TRUE)
      })

      observeEvent(result(), {
        shinyjs::html("current_status", loading_bar_done_html())
        shinyjs::enable(id = session$ns("download"), asis = TRUE)
        shinyjs::enable(id = session$ns("go_next_to_name"), asis = TRUE)
        shinyjs::enable(id = session$ns("run_step"), asis = TRUE)
      })
      
      result <- eventReactive(run_trigger(), {
        req(run_trigger() > 0)
        current_data  <- isolate(raw())
        long_snap     <- isolate(tryCatch(long_data(), error = function(e) NULL))
        use_feature   <- isolate(input$use_feature)
        adducts_val   <- trimws(unlist(strsplit(isolate(input$adducts), ",")))
        tolerance_ppm <- isolate(input$tolerance)
        db_choice     <- isolate(input$db_choice)
        mz_colname    <- isolate(input$mz_colname)
        custom_file   <- isolate(input$custom_db_file)
        
        tolerance_val <- tolerance_ppm * 1e-6
        req(current_data)
        tryCatch({
        

        if (isTRUE(use_feature)) {
          req(!is.null(long_snap) && "feature" %in% names(long_snap) && "mz" %in% names(long_snap))
          
          mzs_to_identify <- long_snap %>%
            filter(mz %in% unique(current_data$mz)) %>%
            group_by(feature) %>%
            slice(1) %>%
            ungroup() %>%
            pull(mz) %>%    
            unique()
    
          
        } else {
          mzs_to_identify <- unique(current_data$mz)
        }
        
        
        id_results <- tryCatch({
          if (db_choice == "HMDB") {
            identify_hmdb(mzs = mzs_to_identify, 
                          adducts = adducts_val, 
                          tolerance = tolerance_val)
            
          } else if (db_choice == "Lipids") {
            identify_lipids(mzs = mzs_to_identify, 
                            adducts = adducts_val, 
                            tolerance = tolerance_val)
            
          } else if (db_choice == "Custom CSV") {
            req(custom_file)
            identify_from_csv(mzs = mzs_to_identify, 
                              adducts = adducts_val, 
                              csv_file = custom_file$datapath, 
                              mz_colname = mz_colname, 
                              tolerance = tolerance_val)
          }
        }, error = function(e) {
          data.frame(Error = paste("Identification failed:", e$message))
        })
        
        
        list(
          data = id_results
        )
        }, error = function(e) {
          shinyjs::enable(id = session$ns("run_step"),asis = TRUE )
          shinyjs::html(session$ns("current_status"), paste("Identification failed:", conditionMessage(e)),add = FALSE)
          showNotification(paste("Identification failed:", conditionMessage(e)),type = "error")
          NULL
        })
      })
      
      ###Result output handling###
      output$id_table <- renderDataTable({
        req(result()$data)
        result()$data
      }, options = list(scrollX = TRUE))
      
      ###Download handling###
      output$download <- downloadHandler(
        filename = function() "identification_results.csv",
        content = function(file) {
          fwrite(result()$data, file)
        }
      )
      
      output$data_status <- renderUI({
        tags$span(
          style = paste0("color: ", status_color(), ";"),
          status_text()
        )
      })
      
      return(list(
        go_back = reactive(input$back_prev),
        result = result,
        go_next = reactive(input$go_next_to_name)
      ))
    })
  }
  