############## UI #############

mass_defect_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_main"), "Back to Previous Step"),
      br(), br(),
      h4("1. Data"),
      fileInput(ns("sub_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      tags$small("Data from previous step loaded by default, overwrite with alternative subtracted dataset if desired."),
      br(), br(),
      uiOutput(ns("mass_defect_data_status")),
      br(),
      radioButtons(
        inputId = ns("pos_neg"),
        label = "Sample Polarity:",
        choices = c("Positive", "Negative"),
        selected = "Positive"
      ),
      hr(),
      h4("2. Parameters"),
      numericInput(ns("mass_defect_slope"), "Slope Parameter Value",
        value = 0.00112, min = -Inf, step = NA
      ),
      numericInput(ns("mass_defect_intersect"), "Intersect Parameter Value",
        value = 0.01953, min = -Inf, step = NA
      ),
      checkboxInput(ns("mass_defect_plot_show"), "Plot results", value = TRUE),
      hr(),
      actionButton(ns("run_mass_defect"), "Run Mass Defect Filter",
        class = "btn-primary", width = "100%"
      ),
      br(), br(),
      uiOutput(ns("mass_defect_status")),
      downloadButton(ns("mass_defect_download"), "Download result (.csv)"),
      br(), br(),
      actionButton(ns("go_next_to_isotope"), "Go to Next Step",
        class = "btn-info", width = "100%"
      )
    ),
    mainPanel(
      uiOutput(ns("mass_defect_plot_ui")),
      DT::dataTableOutput(ns("mass_defect_table"))
    )
  )
}

##### SERVER######################
mass_defect_server <- function(id, subtraction_result,polarity) {
  moduleServer(id, function(input, output, session) {
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    run_trigger <- reactiveVal(0)

    session$onFlushed(function() {
      shinyjs::disable(session$ns("mass_defect_download"))
      shinyjs::disable(session$ns("go_next_to_isotope"))
    }, once = TRUE)

    file_reactive <- reactive(input$sub_file)
    data_mass_defect <- dataloader_module(file_reactive)
    raw_mass_defect <- data_mass_defect$raw

    observe({
      if (!is.null(input$sub_file)) {
        status_text("Custom Dataset Loaded ✅")
        status_color("green")
      } else if (!is.null(subtraction_result())) {
        status_text("Subtraction Dataset Loaded ✅")
        status_color("green")
      } else {
        status_text("No Data Loaded ❌")
        status_color("red")
      }
    })

    observeEvent(input$run_mass_defect, {
      shinyjs::html("mass_defect_status",
        loading_bar_html("Applying Mass Defect Filter..."), add = FALSE)
      shinyjs::delay(150, run_trigger(run_trigger() + 1))
      
    })
    observeEvent(result_mass_defect(), {
      shinyjs::html("mass_defect_status", loading_bar_done_html(), add = FALSE)
      shinyjs::enable(id = session$ns("mass_defect_download"), asis = TRUE)
      shinyjs::enable(id = session$ns("go_next_to_isotope"), asis = TRUE)
      shinyjs::enable(session$ns("run_mass_defect"), asis = TRUE)
    })
    
    
    ###Global Polarity Handling###
    observeEvent(input$pos_neg, {
      polarity(input$pos_neg)
    }, ignoreInit = TRUE)
    
    observe({
      req(polarity())
      updateRadioButtons(session, "pos_neg", selected = polarity())
    })

    mass_defect_input <- reactive({
      if (!is.null(input$sub_file)) {
        raw_mass_defect()
      } else {
        req(subtraction_result())
        subtraction_result()
      }
    })
    ### STEP LOGIC###
    result_mass_defect <- eventReactive(run_trigger(), {
      req(run_trigger() > 0)
      req(polarity())
      shinyjs::disable(session$ns("run_mass_defect"), asis = TRUE)
      
      tryCatch({
        mass_defect_data <- mass_defect_input()
        
        POLARITY <- polarity()
        
        colnames(mass_defect_data)[ncol(mass_defect_data)] <-
          paste0(POLARITY, "_", colnames(mass_defect_data)[ncol(mass_defect_data)])
        
        mz_mass_defect(
          input_mz_obj = mass_defect_data,
          plot = FALSE,
          magicNumber1 = input$mass_defect_slope,
          magicNumber2 = input$mass_defect_intersect
        )
        
      }, error = function(e) {
        shinyjs::enable(session$ns("run_mass_defect"))
        shinyjs::html(session$ns("mass_defect_status"),
                      paste0("<span style='color:red;'>Error: ", conditionMessage(e), "</span>"),
                      asis = TRUE
        )
        NULL
      })
    })
    
    ### Result Handling ###
    output$mass_defect_plot_ui <- renderUI({
      req(result_mass_defect())
      if (isTRUE(input$mass_defect_plot_show)) {
        plotOutput(session$ns("mass_defect_plot"))
      } else {
        NULL
      }
    })

    output$mass_defect_plot <- renderPlot({
      req(result_mass_defect())
      df <- result_mass_defect()
      
      plot_df <- data.frame(mz = df$mz, MD = df$mz %% 1)
      
      ggpubr::ggscatter(
        plot_df,
        x = "mz",
        y = "MD",
        fill = "MD",
        color = "MD",
        xlab = "m/z",
        ylab = "MD",
        legend = "none"
      )
    })
    
    
    output$mass_defect_table <- DT::renderDataTable({
      req(result_mass_defect())
      result_mass_defect()
    },
    rownames = FALSE,
    options = list(
      pageLength = 15,
      scrollX = TRUE
    ))
    
    ###Download Handling###
    output$mass_defect_download <- downloadHandler(
      filename = function() "mass_defect.csv",
      content = function(file) {
        fwrite(result_mass_defect(), file)
      }
    )

    output$mass_defect_data_status <- renderUI({
      tags$span(
        style = paste0("color: ", status_color(), ";"),
        status_text()
      )
    })
    return(list(
      go_back = reactive(input$back_main),
      result = result_mass_defect,
      go_next = reactive(input$go_next_to_isotope)
    ))
  })
}
