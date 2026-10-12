# UI

shortcut_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Analysis Hub", class = "btn-info", width = "100%"),
      br(), br(),
      actionButton(ns("go_next_to_name"), "Go to Identify",
        class = "btn-info", width = "100%"
      )
    ),
    # enable and add output

    mainPanel(
      # uiOutput(ns("mass_defect_plot_ui")),
      # tableOutput(ns("mass_defect_table"))
    )
  )
}

##### SERVER######################
shortcut_page_server <- function(id, previous_result) {
  moduleServer(id, function(input, output, session) {
    result <- eventReactive(input$run_step, {
      # req(input$pos_neg)

      # step_data <- mass_defect_input()

      ## PLACEHOLDER##
      list(
        data = raw()
      )


      # mz_mass_defect(
      #   input_mz_obj = mass_defect_data,
      #   plot = input$mass_defect_plot_show,
      #   magicNumber1 = input$mass_defect_slope,
      #   magicNumber2 = input$mass_defect_intersect
      # )
    })


    go_back <- eventReactive(input$back_prev, {
      TRUE
    })
    go_next <- eventReactive(input$go_next_to_name, {
      TRUE
    })

    return(list(
      go_back = go_back,
      result = result,
      go_next = go_next
    ))
  })
}
