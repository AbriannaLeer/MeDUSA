#UI

new_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  
  sidebarLayout(
    
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Previous Step"),
      br(), br(),
      
      h4("1. Data"),
      
      fileInput(ns("input_file"), "OVERWRITE dataset (.csv)", accept = ".csv"),
      
      tags$small("Data from previous step loaded by default, overwrite with alternative subtracted dataset if desired."),
      
      br(), br(),
      
      uiOutput(ns("data_status")),
      
      h4("2. Parameters"),
      
      uiOutput(ns("current_status")),
      
      actionButton(ns("run_step"), "Run Step",
                   class = "btn-primary", width = "100%"),
      
      br(), br(),
      
      downloadButton(ns("download"), "Download result (.csv)"),
      
      br(), br(),
      
      actionButton(ns("go_next_to_name"), "Go to Next Step",
                   class = "btn-info", width = "100%")
      
    ),
    #enable and add output
    
    mainPanel(
      #uiOutput(ns("mass_defect_plot_ui")),
      #tableOutput(ns("mass_defect_table"))
    )
  )
}

#####SERVER######################
new_page_server <- function(id,previous_result){
  moduleServer(id, function(input, output, session) {
    
    status_text <- reactiveVal("No Data Loaded ❌")
    status_color <- reactiveVal("red")
    
    session$onFlushed(function() {
      shinyjs::disable(session$ns("download"))
      shinyjs::disable(session$ns("go_next_to_name"))
    }, once = TRUE)
    
    file_reactive <- reactive(input$input_file)
    data<- dataloader_module(file_reactive)
    
    raw<- data$raw
    
    
    observe({
      if (!is.null(input$input_file)) {
        status_text("Custom Dataset Loaded ✅")
        status_color("green")
      } else if (!is.null(previous_result())) {
        status_text("X Dataset Loaded ✅")
        status_color("green")
      } else {
        status_text("No Data Loaded ❌")
        status_color("red")
      }
    })
    
    observeEvent(input$run_step, {
      shinyjs::html(session$ns("current_status"), "Applying Step...", add = FALSE)
    })
    observeEvent(result(), {
      shinyjs::html(session$ns("current_status"), "")
      shinyjs::enable(id = session$ns("download"), asis = TRUE)
      shinyjs::enable(id = session$ns("go_next_to_name"), asis = TRUE)
    })
    
    result <- eventReactive(input$run_step, {
      #req(input$pos_neg)
      
      #step_data <- mass_defect_input()
      
      ##PLACEHOLDER##
      list(
        data = raw()) 
      
      
      # mz_mass_defect(
      #   input_mz_obj = mass_defect_data,
      #   plot = input$mass_defect_plot_show,
      #   magicNumber1 = input$mass_defect_slope,
      #   magicNumber2 = input$mass_defect_intersect
      # )
    })
    
    
    
    output$download <- downloadHandler(
      filename = function() "curent_step.csv", ###CHANGE THIS########
      content = function(file) {
        write.csv(result()$data,file, row.names = FALSE)
      }
    )
    
    output$data_status <- renderUI({
      tags$span(style = paste0("color: ", status_color(), ";"),
                status_text())
    })
    return(list(
      go_back = reactive(input$back_prev),
      result = result,
      go_next = reactive(input$go_next_to_name)
    ))
  })
}
