### UI ###

statistics_page_ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("back_prev"), "Back to Analysis Hub",class = "btn-info", width = "100%"),
      
      hr(),
      
      uiOutput(ns("sidebar_dynamic")),
      
      hr(),
      
      actionButton(ns("go_next_to_name"), "Go to Identification",
                   class = "btn-info", width = "100%"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = ns("stat_tabs"),
        tabPanel("Welch T-Test",
                 br(),
                 uiOutput(ns("welch_ui"))
        ),
        
        tabPanel("Fold Change",
                 br(),
                 uiOutput(ns("fold_ui"))
        ),
        
        tabPanel("Volcano Plot",
                 br(),
                 uiOutput(ns("volcano_ui"))
        ),
        
        tabPanel("ANOVA",
                 br(),
                 uiOutput(ns("anova_ui"))
        ),
        
      )
    )
  )
}

#####SERVER######################
statistics_page_server <- function(id,previous_result,previous_long,metadata,keep,group){
  moduleServer(id, function(input, output, session) {
    ###Result storage###
    result <- reactiveVal(NULL)
    fc_result  <- reactiveVal(NULL)
    anova_result <- reactiveVal(NULL)
    volcano_result <- reactiveVal(NULL)
    
    selected_groups <- reactiveVal(NULL)
    
    ###Group input###
    observe({
      req(keep())
      if (is.null(selected_groups())) selected_groups(keep())
    })
    
    observeEvent(input$stat_groups_select, {
      selected_groups(input$stat_groups_select)
    })
    
    stat_groups <- reactive({
      unique(metadata()[[group()]])
    })
    
    ###Set Data
    data_list <- reactive({
      
      req(previous_result())
      req(selected_groups())
      
      df <- previous_result()
      grp <- if (is.function(group)) group() else group
      
      lapply(selected_groups(), function(p) {
        
        pheno_filter(
          input_mzobj = df,   
          filter_value = p,
          filter_name  = grp,
          exclude = FALSE,
          keep_mz = TRUE
        )
        
      }) |> setNames(selected_groups())
      
    })|> bindCache(previous_result(),selected_groups(), if (is.function(group)) group() else group
    )
    
    
    ### Run logic Welch###
    observeEvent(input$run_welch, {
      if (length(selected_groups()) != 2) {
        showNotification("Welch T-test requires exactly 2 groups.", type = "error")
        return()
      }
      shinyjs::disable("run_welch")
      
      cores_val  <- isolate(input$welch_cores)
      adjust_val <- isolate(input$welch_adjust)
      
      shinyjs::html(
        "welch_run_status",
        loading_bar_html("Running Welch T-Test...")
      )
      
      tryCatch({
        data <- data_list()
        
        phenoA <- selected_groups()[1]
        phenoB <- selected_groups()[2]
        
        req(!is.null(data[[phenoA]]), !is.null(data[[phenoB]]))
        
        res <- mzlog_analysis_welch(
          phenoA_mz_obj = data[[phenoA]],
          phenoB_mz_obj = data[[phenoB]],
          adjust = adjust_val,
          cores = cores_val
        )
        
        result(res)
        shinyjs::html(
          "welch_run_status",
          loading_bar_done_html()
        )
      }, error = function(e) {
        shinyjs::enable("run_welch")
        showNotification(paste("Welch error:", conditionMessage(e)), type = "error")
      })
    })
    
    
    ###Run logic Fold###
    observeEvent(input$run_fc, {
      if (length(selected_groups()) != 2) {
        showNotification("Fold Change requires exactly 2 groups.", type = "error")
        return()
      }
      
      shinyjs::disable("run_fc")
      
      shinyjs::html(
        "fold_run_status",
        loading_bar_html("Calculating Fold Change...")
      )
      tryCatch({
        data <- data_list()
        
        phenoA <- selected_groups()[1]
        phenoB <- selected_groups()[2]
        
        req(!is.null(data[[phenoA]]), !is.null(data[[phenoB]]))
        
        fold_math_val <- if (input$fold_math == "mean") {
          "mean"
        } else if (input$fold_math == "median") {
          "median"
        } else {
          user_fun <- tryCatch(
            eval(parse(text = input$custom_function)),
            error = function(e) NULL
          )
          
          validate(need(!is.null(user_fun), "Invalid custom function"))
          
          function(x) {
            val <- tryCatch(user_fun(x), error = function(e) NA_real_)
            val <- suppressWarnings(as.numeric(val))
            
            if (length(val) == 0 || all(is.na(val))) return(NA_real_)
            if (length(val) != 1) val <- mean(val, na.rm = TRUE)
            as.numeric(val)
          }
        }
        
        res <- mzlog_analysis_fold(
          phenoA_mz_obj = data[[phenoA]],
          phenoB_mz_obj = data[[phenoB]],
          fold_math = fold_math_val
        )
        
        fc_result(res)
        shinyjs::html(
          "fold_run_status",
          loading_bar_done_html()
        )
      }, error = function(e) {
        shinyjs::enable("run_fc")
        showNotification(paste("Fold change error:", conditionMessage(e)), type = "error")
      })
    })
    
    
    ###ANOVA column select###
    observe({
      req(previous_long())
      req(input$stat_tabs == "ANOVA")  
      
      df <- previous_long()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      updateSelectInput(
        session,
        "anova_log_column",
        choices = numeric_cols,
        selected = if ("log" %in% numeric_cols) "log" else if ("intensity" %in% numeric_cols) "intensity" else numeric_cols[1]
      )
    })
    
    ###Run logic ANOVA###
    observeEvent(input$run_anova, {
      
      req(previous_long())
      if (length(selected_groups()) < 2) {
        showNotification("ANOVA requires at least 2 groups.", type = "error")
        return()}
      
      shinyjs::disable("run_anova")
      
      cores_val <- isolate(input$anova_cores)
      alpha_val <- isolate(input$anova_alpha)
      
      shinyjs::html(
        "anova_run_status",
        loading_bar_html("Running ANOVA...")
      )
      
      tryCatch({
        df_long <- previous_long()
        meta <- metadata()
        
        sel_col <- group()
        groups <- selected_groups()
        ## Hardcode phenotype name workaround ##
        if (!is.null(sel_col) && sel_col != "phenotype") {
          
          if ("phenotype" %in% names(meta)) {
            meta$rf_original_phenotype <- meta$phenotype
          }
          
          meta$phenotype <- meta[[sel_col]]
        }
        
        ## Log column creation for ANOVA ##
        req(input$anova_log_column)
        
        selected_col <- input$anova_log_column
        
        log_exists   <- "log" %in% names(df_long)
        log_selected <- selected_col == "log"
        log_checked  <- isTRUE(input$anova_log)
        
        
        reuse <- log_exists && log_selected && !log_checked
        
        if (log_exists && !reuse) {
          df_long$log_old <- df_long$log
        }
        
        value <- df_long[[selected_col]]
        
        if (!reuse && log_checked) {
          value <- log1p(value)
        }
        
        df_long$log <- value
        
        res <- mzlong_analysis_anova(
          input_mzlong_obj = df_long,
          metadata         = meta,
          phenotypes       = groups,
          p_cutoff         = alpha_val,
          cores            = cores_val
        )
        
        anova_result(res)
        shinyjs::html(
          "anova_run_status",
          loading_bar_done_html()
        )
      }, error = function(e) {
        shinyjs::enable("run_anova")
        showNotification(paste("ANOVA error:", conditionMessage(e)), type = "error")
      })
    })
    
    
    volcano_ready <- reactive({
      !is.null(tryCatch(result(), error = function(e) NULL)) &&
        !is.null(tryCatch(fc_result(), error = function(e) NULL))
    })
    
    ###Sidebar UI logic###
    output$sidebar_dynamic <- renderUI({
      tab <- input$stat_tabs
      ns <- session$ns
      
      switch(tab,
             "Welch T-Test" = tagList(
               helpText("Run the Welch T-test, a two group comparison method."),
               checkboxGroupInput(ns("stat_groups_select"), "Groups",
                                  choices  = stat_groups(),
                                  selected = selected_groups() %||% keep()
               ),
               numericInput(ns("welch_cores"), "Cores", value = 2, min = 1, max = 32, step = 1),
               selectInput(ns("welch_adjust"), "P-value Adjustment Method",
                           choices = c("FDR (BH)" = "fdr",
                                       "Bonferroni" = "bonferroni",
                                       "Holm" = "holm",
                                       "Hochberg" = "hochberg",
                                       "Hommel" = "hommel",
                                       "BY" = "BY",
                                       "None" = "none"),
                           selected = "fdr"),
               actionButton(ns("run_welch"), "Run Welch T-Test", class = "btn-primary", width = "100%"),
               br(),br(),
               uiOutput(ns("welch_run_status")),
               shinyjs::disabled(
                 downloadButton(ns("download_welch"), "Download mz Table")
               )
             ),
             
             "Fold Change" = tagList(
               helpText("Calculate the log fold difference, a two group comparison method."),
               checkboxGroupInput(ns("stat_groups_select"), "Groups",
                                  choices  = stat_groups(),
                                  selected = selected_groups() %||% keep()
               ),
               selectInput(ns("fold_math"),
                           "Fold method:",
                           choices = c("Mean" = "mean",
                                       "Median" = "median",
                                       "Custom" = "custom"),
                           selected = "mean"
               ),
               conditionalPanel(
                 condition = sprintf("input['%s'] == 'custom'", ns("fold_math")),
                 
                 textInput(
                   ns("custom_function"),
                   "Custom function:",
                   value = "function(x) mean(x * 2, na.rm = TRUE)"
                 )
               ),
               actionButton(ns("run_fc"), "Calculate Fold Change", class = "btn-primary", width = "100%"),
               br(),br(),
               uiOutput(ns("fold_run_status")),
               shinyjs::disabled(
                 downloadButton(ns("download_fold"),"Download mz Table"))
             ),
             
             "Volcano Plot" = tagList(
               helpText("Create a volcano plot for the comparison of two groups, requires the completion of the Welch T-test and Fold Change calculation."),
               uiOutput(ns("welch_status")),
               uiOutput(ns("fold_status")),
               br(),br(),
               if (volcano_ready()) {actionButton(ns("run_volcano"),"Plot Volcano",class = "btn-primary",width = "100%"
               )
               } else {
                 shinyjs::disabled(
                   actionButton(ns("run_volcano"),"Plot Volcano",class = "btn-primary",width = "100%"
                   )
                 )
               },
               br(),br(),
               uiOutput(ns("volcano_run_status")),
               shinyjs::disabled(
                 downloadButton(ns("download_volcano"),"Download Volcano Plot"))
             ),
             
             "ANOVA" = {
               df <- previous_long()
               has_log <- if (!is.null(df)) {"log" %in% names(df) }
               else {FALSE}
               
               tagList(
                 helpText("Group comparison method for 2+ groups"),
                 checkboxGroupInput(ns("stat_groups_select"), "Groups",
                                    choices  = stat_groups(),
                                    selected = selected_groups() %||% keep()
                 ),
                 selectInput(ns("anova_log_column"),"Column to log-transform:",choices = NULL),
                 checkboxInput(ns("anova_log"), "Apply log transform",value = !has_log),
                 numericInput(ns("anova_alpha"), "Significiance Threshold", value = 0.1, min = 0.01, max = 0.99, step = 0.01),
                 numericInput(ns("anova_cores"), "Cores", value = 1, min = 1, max = 32, step = 1),
                 actionButton(ns("run_anova"), "Run ANOVA", class = "btn-primary", width = "100%"),
                 br(),br(),
                 uiOutput(ns("anova_run_status")),
                 shinyjs::disabled(
                   downloadButton(ns("download_anova"),"Download Anova Summary Table"),
                   br(),br(),
                   downloadButton(ns("download_tukey"),"Download Tukey Post-Hoc Table"),
                   br(),br(),
                   downloadButton(ns("download_sig"),"Download Significant mz Values"))
               )}
      )
    })
    
    
    ###Result output handling###
    
    output$welch_ui <- renderUI({
      ns <- session$ns
      
      tagList(
        h4("Welch T-Test Results"),
        DT::dataTableOutput(ns("welch_table"))
      )
    })
    
    output$welch_table <- DT::renderDataTable({
      req(result())
      DT::formatRound(
        DT::datatable(result(), rownames = FALSE,
                      options = list(pageLength = 15, scrollX = TRUE)),
        columns = c("mz", "p"),
        digits = 4
      )
    })
    
    output$fold_ui <- renderUI({
      ns <- session$ns
      
      tagList(
        h4("Fold Change Results"),
        DT::dataTableOutput(ns("fold_table"))
      )
    })
    
    output$fold_table <- DT::renderDataTable({
      req(fc_result())
      DT::formatRound(
        DT::datatable(fc_result(), rownames = FALSE,
                      options = list(pageLength = 15, scrollX = TRUE)),
        columns = c("mz", "fold"),
        digits = 4
      )
    })
    
    output$volcano_ui <- renderUI({
      ns <- session$ns
      tagList(
        h4("Volcano Plot"),
        textInput(ns("volcano_title"), "Plot Title", value = "Volcano Plot"),
        br(),
        plotOutput(ns("volcano_plot")),
        br(),
        h4("Volcano Data"),
        DT::dataTableOutput(ns("volcano_table"))
      )
    })
    
    ###Run logic Volcano###
    observeEvent(input$run_volcano, {
      req(result(), fc_result())
      
      shinyjs::disable("run_volcano")
      
      shinyjs::html(
        "volcano_run_status",
        loading_bar_html("Building Volcano Plot...")
      )
      tryCatch({
        welch <- result()
        fold_change <- fc_result()
        
        df <- data.frame(
          mz   = welch$mz,
          p    = -log10(welch$p),
          fold = fold_change$fold,
          diff = rep("NONE", nrow(welch))
        )
        
        df$diff[df$fold >  0.6 & df$p > -log10(0.05)] <- "UP"
        df$diff[df$fold < -0.6 & df$p > -log10(0.05)] <- "DOWN"
        
        volcano_result(df)
        shinyjs::html(
          "volcano_run_status",
          loading_bar_done_html()
        )
      }, error = function(e) {
        shinyjs::enable("run_volcano")
        showNotification(paste("Volcano error:", conditionMessage(e)), type = "error")
      })
    })
    
    
    ###Volcano + Anova result output handling###
    volcano_plot_obj <- reactive({
      req(volcano_result())
      
      ggpubr::ggscatter(
        data    = volcano_result(),
        x       = "fold",
        y       = "p",
        title   = input$volcano_title,
        shape   = "diff",
        color   = "diff",
        palette = c("blue", "black", "red")
      ) +
        geom_vline(xintercept = c(-0.6, 0.6), col = "green") +
        geom_hline(yintercept = -log10(0.05), col = "green")
    })
    
    output$volcano_plot <- renderPlot({
      req(volcano_result())
      volcano_plot_obj()
    })
    
    output$volcano_table <- DT::renderDataTable({
      req(volcano_result())
      DT::formatRound(
        DT::datatable(volcano_result(), rownames = FALSE,
                      options = list(pageLength = 15, scrollX = FALSE)),
        columns = c("mz", "p", "fold"),
        digits = 4
      )
    })
    
    output$anova_ui <- renderUI({
      ns <- session$ns
      
      tabsetPanel(
        tabPanel("ANOVA Summary",
                 br(),
                 h4("ANOVA Results"),
                 DT::dataTableOutput(ns("anova_table"))
                 
        ),
        tabPanel("Significant m/z",
                 br(),
                 h4("Significant m/z Values"),
                 DT::dataTableOutput(ns("anova_sig_table"))
        ),
        tabPanel("Tukey Post-hoc",
                 br(),
                 h4("Tukey Post-hoc Results"),
                 DT::dataTableOutput(ns("anova_tukey"))
        )
      )
    })
    
    output$anova_table <- DT::renderDataTable({
      req(anova_result())
      DT::datatable(anova_result()$summary, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })
    
    output$anova_sig_table <- DT::renderDataTable({
      req(anova_result())
      DT::formatRound(
        DT::datatable(
          data.frame(mz = anova_result()$imp_mz),
          rownames = FALSE,
          options = list(pageLength = 15, scrollX = FALSE)
        ),
        columns = "mz",
        digits = 4
      )
    })
    
    output$anova_tukey <- DT::renderDataTable({
      req(anova_result())
      DT::datatable(anova_result()$tukey, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })
    
    ###Status indicators###
    output$welch_status <- renderUI({
      val <- tryCatch(result(), error = function(e) NULL)
      if (!is.null(val)) {
        tags$div("Welch T-test: ✔", style = "color: black; font-weight: bold;")
      } else {
        tags$div("Welch T-test: ❌", style = "color: black; font-weight: bold;")
      }
    })
    
    
    output$fold_status <- renderUI({
      val <- tryCatch(fc_result(), error = function(e) NULL)
      if (!is.null(val)) {
        tags$div("Fold Change: ✔", style = "color: black; font-weight: bold;")
      } else {
        tags$div("Fold Change: ❌", style = "color: black; font-weight: bold;")
      }
    })
    
    ###Download handling###
    
    output$download_welch <- downloadHandler(
      filename = function()"welch_mz_table.csv",
      content = function(file) {
        write.csv(result(),file,row.names = FALSE)
      }
    )
    
    output$download_fold <- downloadHandler(
      filename = function()"fold_mz_table.csv",
      content = function(file) {
        write.csv(fc_result(),file,row.names = FALSE)
      }
    )
    
    output$download_volcano <- downloadHandler(
      filename = function() {
        "volcano_plot.png"
      },
      
      content = function(file) {
        
        ggplot2::ggsave(
          filename = file,
          plot = volcano_plot_obj(),
          device = "png",
          width = 8,
          height = 6,
          dpi = 300
        )
      }
    )
    output$download_anova <- downloadHandler(
      filename = function()"anova_summary.csv",
      content = function(file) {
        fwrite(anova_result()$summary,file)
      }
    )
    output$download_tukey <- downloadHandler(
      filename = function()"anova_tukey.csv",
      content = function(file) {
        fwrite(anova_result()$tukey,file)
      }
    )
    output$download_sig <- downloadHandler(
      filename = function()"anova_sig_mz.csv",
      content = function(file) {
        fwrite(anova_result()$imp_mz,file)
      }
    )
    ###Button enabling###
    observeEvent(result(), {
      shinyjs::enable("download_welch")
      shinyjs::enable("run_welch")
    })
    observeEvent(fc_result(), {
      shinyjs::enable("download_fold")
      shinyjs::enable("run_fc")
    })
    
    observeEvent(volcano_result(), {
      shinyjs::enable("download_volcano")
      shinyjs::enable("run_volcano")
    })
    
    observeEvent(anova_result(), {
      shinyjs::enable("run_anova")
      shinyjs::enable("download_anova")
      shinyjs::enable("download_tukey")
      shinyjs::enable("download_sig")
    })
    
    ###Navigation###
    go_back <- eventReactive(input$back_prev, { TRUE })
    go_next <- eventReactive(input$go_next_to_name, { TRUE })
    
    return(list(
      go_back = go_back,
      welch_mz = result,
      fold_mz  = fc_result,
      anova_mz = reactive({ 
        res <- anova_result()$imp_mz
        if (is.null(res)) return(NULL)
        data.frame(mz = res)
      }),
      go_next = go_next
    ))
  })
}
