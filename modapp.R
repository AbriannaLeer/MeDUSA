library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyjs)
library(later)
library(DT)
library(data.table)
library(randomForest)

options(shiny.maxRequestSize = 1024 * 1024^2) # MAX upload set to 1 GB

# Load all MeDUSA functions
lapply(list.files("MeDUSA/R/", pattern = "\\.R$", full.names = TRUE), source)
lapply(list.files("R/", pattern = "\\.R$", full.names = TRUE), source)


# UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  tags$style(HTML("
  body {
    padding-bottom: 60px;
  }
  .nav-active {
      background-color: #0275d8 !important;
      color: white !important;}

      display: none;
    }
")),
  # Animated indeterminate loading bar (shared by pipeline steps) — kept in
  # its own <style> so it is parsed independently of the block above.
  tags$style(HTML("
  .qc-loading-wrap {
    position: relative;
    width: 100%;
    height: 28px;
    border-radius: 4px;
    overflow: hidden;
    margin: 4px 0 10px;
    background: #e6e6e6;
  }
  .qc-loading-bar {
    position: absolute;
    top: 0; right: 0; bottom: 0; left: 0;
    background: linear-gradient(90deg, #0275d8 25%, #5cb3f0 50%, #0275d8 75%);
    background-size: 200% 100%;
    animation: qc-indeterminate 1.4s linear infinite;
  }
  .qc-loading-bar.qc-done {
    background: #28a745;
    animation: none;
  }
  .qc-loading-text {
    position: absolute;
    top: 0; right: 0; bottom: 0; left: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 13px;
    font-weight: 600;
    color: #ffffff;
    text-shadow: 0 1px 2px rgba(0,0,0,0.35);
    z-index: 1;
  }
  @keyframes qc-indeterminate {
    0%   { background-position: 200% 0; }
    100% { background-position: -200% 0; }
  }
")),
  tags$script(HTML("
    $(document).ready(function() {
      $('#bottom_nav_wrapper').show();

      $(document).on('click', '.navbar-nav > li > a', function() {
        var tab = $(this).text().trim();
        if (tab === 'Pipeline') {
          $('#bottom_nav_wrapper').show();
        } else {
          $('#bottom_nav_wrapper').hide();
        }
      });
    });
  ")),
  navbarPage(
    title = "MeDUSA",

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 1 — Pipeline
    # ══════════════════════════════════════════════════════════════════════════

    tabPanel(
      "Pipeline",
      div(id = "main_page_div", main_page_ui("main_page")),
      div(id = "subtraction_page_div", subtraction_page_ui("subtraction_page"), style = "display:none;"),
      div(id = "mass_defect_page_div", mass_defect_page_ui("mass_defect_page"), style = "display: none;"),
      div(id = "tagging_page_div", tagging_page_ui("tagging_page"), style = "display: none;"),
      div(id = "filter_page_div", filter_page_ui("filter_page"), style = "display: none;"),
      div(id = "postpro_page_div", postpro_page_ui("postpro_page"), style = "display: none;"),
      div(id = "analysis_page_div", analysis_page_ui("analysis_page"), style = "display: none;"),
      div(id = "identify_page_div", identify_page_ui("identify_page"), style = "display: none;")
    ),

    # ══════════════════════════════════════════════════════════════════════════
    # TAB 2 — How it works
    # ══════════════════════════════════════════════════════════════════════════

    tabPanel(
      "How it works",
      how_it_works_ui("how_it_works")
    ),


    # ══════════════════════════════════════════════════════════════════════════
    # TAB 3 — About
    # ══════════════════════════════════════════════════════════════════════════
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 8, offset = 2,
          br(),
          h2("About MeDUSA"),
          p(tags$strong("MeDUSA"), " — Metabolomics of Direct-Infusion Untargeted
            Single-cell Analysis — is an R package and processing pipeline for
            analysing metabolomic data from individual cells measured by Direct
            Infusion Mass Spectrometry (DIMS)."),
          p(tags$b("The goal"),"for MeDUSA is to be a start-to-finish analysis package allowing 
          metabolomics researchers to focus on analytical content rather than R proficiency.
          MeDUSA handles the suggested workflow by Southam (Southam et al., 2017) from data extraction
          to filtering without a chromatogram, carrying the data through statistical analysis
          and identification of features for biological interpretation. 
          This toolset is aimed to be modular, customizable and user-friendly."),
          p("Currently you are viewing the",tags$b("GUI"),"created to make the pipeline even more accessible for researchers.
            Check out the How it works tab for an overview of functionalities within this software."),
          br()
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # Bottom Page Navigation
  # ══════════════════════════════════════════════════════════════════════════
  tags$div(id = "bottom_nav_wrapper", uiOutput("bottom_nav"))
)
# ══════════════════════════════════════════════════════════════════════════
# ══════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════
# ══════════════════════════════════════════════════════════════════════════
main_server <- function(input, output, session) {
  page <- reactiveVal("main_page")
  mode <- reactiveVal("preprocessing")
  polarity <- reactiveVal("Positive")
  
  post_processing_pages <- c("main_page","subtraction_page", "mass_defect_page","tagging_page","filter_page","postpro_page")
  analysis_pages <- c("analysis_page","identify_page")
  pages <- c(post_processing_pages, analysis_pages)
  buttons <- paste0("nav_", gsub("_page", "", pages))

  togglePage <- function(target) {
    for (i in seq_along(pages)) {
      page_id <- paste0(pages[i], "_div")
      btn_id <- buttons[i]

      if (pages[i] == target) {
        shinyjs::show(page_id)
        shinyjs::removeClass(btn_id, "btn-secondary")
        shinyjs::addClass(btn_id, "btn-primary")
      } else {
        shinyjs::hide(page_id)
        shinyjs::removeClass(btn_id, "btn-primary")
        shinyjs::addClass(btn_id, "btn-secondary")
      }
    }
    page(target)
  }
  
  #Server calls
  main <- main_page_server("main_page", polarity = polarity)
  subtraction <- subtraction_server("subtraction_page", quality_check_result = main$result,meta  = main$meta, group = main$group, full_long = main$full_long, sample_names = main$sample_names)
  mass_defect <- mass_defect_server("mass_defect_page", subtraction_result = subtraction$result,polarity = polarity)
  tagging <- tagging_page_server("tagging_page", previous_result = mass_defect$result)
  noise_filter <- filter_page_server("filter_page", previous_result = tagging$result)
  post_processing <- postpro_page_server("postpro_page", previous_result =noise_filter$result, prev_meta = main$meta, polarity = polarity, group = main$group)
  analysis <- analysis_page_server("analysis_page",previous_result = post_processing$result_wide,previous_long = post_processing$result_long,prev_meta = post_processing$meta,keep = subtraction$keep,group = main$group,polarity = polarity) 
  identify <- identify_page_server("identify_page", previous_result = analysis$data,previous_long = main$full_long)

  # Move between pages
  lapply(pages, function(page) {
    observeEvent(input[[paste0("nav_", gsub("_page", "", page))]], {
      togglePage(page)
    })
  })
  
  # List of Modules
  
  modules <- list(
    main_page = main,
    subtraction_page = subtraction,
    mass_defect_page = mass_defect,
    tagging_page = tagging,
    filter_page = noise_filter,
    postpro_page = post_processing,
    analysis_page = analysis,
    identify_page = identify
  )

  for (i in seq_along(pages)) {
    # NEXT button (go to next page)
    if (i < length(pages)) {
      local({
        current <- modules[[pages[i]]]
        next_page <- pages[i + 1]

        observeEvent(current$go_next(), {
          togglePage(next_page)
          
          if (next_page %in% analysis_pages)
            {mode("analysis")}
          
        })
      })
    }

    # BACK button (go to previous page)
    if (i > 1) {
      local({
        current <- modules[[pages[i]]]
        prev_page <- pages[i - 1]

        observeEvent(current$go_back(), {
          togglePage(prev_page)
          if (prev_page %in% post_processing_pages) {
            mode("preprocessing")
          }
        })
      })
    }
  }
  output$bottom_nav <- renderUI({
    tags$div(
      style = "position: fixed; bottom: 0; width: 100%; background: #f8f9fa;
             padding: 10px; border-top: 1px solid #ddd; text-align: center; z-index: 1000;",
      if (mode() == "preprocessing") {
        tagList(
          actionButton("nav_main", "1. Upload/Quality Check", class = "btn-sm btn-primary"),
          actionButton("nav_subtraction", "2. Subtraction", class = "btn-sm btn-secondary"),
          actionButton("nav_mass_defect", "3. Mass Defect Filtering", class = "btn-sm btn-secondary"),
          actionButton("nav_tagging", "4. Isotope Tagging", class = "btn-sm btn-secondary"),
          actionButton("nav_filter", "5. Noise Filtering", class = "btn-sm btn-secondary"),
          actionButton("nav_postpro", "6. Post-Processing", class = "btn-sm btn-secondary"),
          actionButton("skip_to_analysis", "Skip to Analysis →", class = "btn-sm btn-info")
        )
      } else {
        tagList(
          actionButton("back_to_pre", "← Back to Pre-processing", class = "btn-sm btn-info"),
          actionButton("nav_analysis", "Analysis", class = "btn-sm btn-primary"),
          actionButton("nav_identify", "Identification", class = "btn-sm btn-secondary")
        )
      }
    )
  })

  observeEvent(input$skip_to_analysis, {
    mode("analysis")
    togglePage("analysis_page")
    analysis$reset()
  })

  observeEvent(input$back_to_pre, {
    mode("preprocessing")
    togglePage("main_page")
  })
}
shinyApp(ui = ui, server = main_server)
