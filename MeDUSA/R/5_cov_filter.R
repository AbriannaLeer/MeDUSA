# *** Coefficient of Variance ------------------------------------------------
#' MZ-OBJ : Filter mz-obj based on cv and presence
#' 
#' @description:
#' Filtering of aligned samples is necessary to remove noise from the data 
#' before statistical analysis. This function removes rows which do not meet 
#' two thresholds set by the user. For each row the coefficient of variance is 
#' calculated, followed by determining the percentage of rows with an intensity
#' above 0.These two values are used as thresholds to select rows that are kept. 
#' The user can select to either keep all rows with an indication if the row meets 
#' the thresholds or only return the rows that reach the thresholds. The user
#' can also choose to plot the cv and presence against the cv.
#' 
#' Remove rows which have a coefficent of variance higher than |threshold_cv|% and 
#' a presence higher than |threshold_presence|% in samples.
#' 
#' @param input_mz_obj \cr
#'    Dataframe: input mz object with columns mz followed by samples with 
#'                intensities
#' @param min_samples \cr
#'    Numeric: Minimum number of samples per row to have an intensity above 0 in 
#'             percentage (10% is 10). The coefficient is not calculated for 
#'             rows that do not meet the min_samples.
#' @param threshold_cv \cr
#'    Numeric: Threshold for minimum coefficient of variance for a row. 
#'             |threshold_cv|% 
#' @param threshold_presence \cr
#'    Numeric: Threshold for minimum samples with intensity for a row. Needs to 
#'            be equal or more than min_samples. |threshold_presence|%
#' @param keep \cr
#'    Logical: If set to TRUE, original input_mz_object is kept, with added sd, 
#'             mean, cov, presence and included columns. If set
#'             to FALSE, only the rows that meet the thresholds threshold_cv and 
#'             threshold_presence are kept which will not cause issues for later
#'             steps.
#' @param plot \cr
#'    Logical: If set to TRUE, the cv and presence will be plotted for each mz.
#' @param title: \cr
#'    String: Sets the title for the plot and name when saving. When plot is set 
#'    to FALSE, this parameter will be ignored. 
#'  
#'  
#' dependencies: dplyr, plotly
#' @return Returns an mz-object. 
#' @export
mz_filter_cv <- function(input_mz_obj, min_samples = 10, threshold_cv = 50, 
                         threshold_presence = 10, keep = FALSE, plot = TRUE, 
                         title = "Cv_presence_vs_mz") {
  
  if (min_samples > threshold_presence) {
    print(paste("Warning: Minimum presence not possible with set minimum samples.
                \n Minimum samples set to threshold_presence."))
    min_samples = threshold_presence
  }
  
  min_samples <- (ncol(input_mz_obj %>% dplyr::select(-mz))) * (min_samples / 100) 
  print(paste("INFO: Minimum number of samples with intensity used: ", 
              round(min_samples)))
  
  cov <- input_mz_obj %>%
    dplyr::mutate(
      # Calculate standard deviation
      sd = apply(dplyr::select(., -mz), 1, function(x) { 
        if (length(x[x != 0]) < round(min_samples)) { return(NA) } else {
          sd(x, na.rm = TRUE) }
      }),
      
      # Calculate mean
      mean = apply(dplyr::select(., -mz), 1, function(y) {
        if (length(y[y != 0]) < round(min_samples)) { return(NA) } else {
          mean(y, na.rm = TRUE)}
      }),
      
      # Calculate cv
      cov = ifelse(is.na(sd) | is.na(mean), NA, (sd / mean) * 100),
      
      # calculate presence
      presence = (rowSums(dplyr::select(., -mz) != 0) / ncol(dplyr::select(., -mz))) * 100, 
      
      # set keep/remove
      included = ifelse(is.na(cov), "nc", ifelse(as.numeric(cov) > threshold_cv & 
                                                   as.numeric(presence) >
                                                   threshold_presence, "keep", "remove"))
    ) %>%
    dplyr::mutate(
      sd = ifelse(is.na(sd), "nc", sd),
      mean = ifelse(is.na(mean), "nc", mean),
      cov = ifelse(is.na(cov), "nc", cov)
    ) %>%
    # change location of certain columns
    dplyr::relocate(mz, sd, mean, cov, presence, included, .before = dplyr::everything())
  
  
  if (plot) {
    fig <- plot_cv(cov, threshold_presence, threshold_cv, title)
    print(fig)   # shown in rstudio viewer (in current session)
    # save the plot as a file
    htmlwidgets::saveWidget(fig, file.path(local.output_dir(), paste0(title, ".html"))) 
  }
  
  if (!keep) { # filter only rows that meet thresholds
    cov <- cov %>% dplyr::filter(included == "keep") %>%
      dplyr::select(-included, -mean, -cov, -sd, -presence)  # now returns only returns  mz and samples
    
    local.mz_log_removed_rows(input_mz_obj, cov, "MeDUSA::mz_filter_cv")
  } 
  
  cov
}

# *** CV plot ------------------------------------------------------------
#' MZ-obj-cv: create CV vs presence vs mz
#' 
#' @description
#' The output of mz_filter_cv is plotted. The cv and presence are plotted against
#' the mz and colored based on the thresholds min presence and min cv. If both 
#' thresholds are reached, the points will be colored green,if one of the 
#' tresholds is reached the points are colored orange and if both 
#' of the thresholds are not reached the cv and presence will be colored red.
#' The plot will be saved as an .rds file.
#' 
#' 
#' @param mz_obj_cv \cr
#'  Dataframe: mzobj with cv and presence
#' @param threshold_cv \cr
#'  Numeric: treshold for calculated coeffient of variance in percentage.
#' @param threshold_presence \cr
#'  Numeric: treshold for minimum number of samples to have an intensity above
#'           0 in percentage.
#' @param title \cr
#'  string: A string used as the title of the plot and the name for the save file.
#'
#' dependencies: plotly, dplyr
#' 
#' @export
plot_cv <- function(mz_obj_cv, threshold_presence, threshold_cv,  title = title) {
  print("INFO: Generating html plot.")
  df_cov <- mz_obj_cv %>% filter(cov != "nc") %>%
    dplyr::mutate(cov = as.numeric(cov))
  
  
  # plot
  fig <- plotly::plot_ly()
  
  
  # # main data both thresholds met
  fig <- fig %>%
    plotly::add_markers(
      data = df_cov %>%
        dplyr::filter(cov > threshold_cv & presence > threshold_presence),
      x = ~mz, y = ~cov, 
      opacity = 0.7,
      name = "Keep", 
      legendgroup = "keep",
      marker = list(color = "green", symbol = "circle"),
      text = ~paste("Cv: ", cov),  
      hoverinfo = "text") %>%
    plotly::add_markers(data = df_cov %>%
                          dplyr::filter(cov > threshold_cv & presence > threshold_presence),
                        x = ~mz, y = ~presence, 
                        opacity = 0.7,
                        legendgroup = "keep",
                        showlegend = FALSE,
                        marker = list(color = "green", symbol = "triangle-up"),
                        yaxis = "y2",
                        text = ~paste("Presence: ", presence),  
                        hoverinfo = "text") %>%
    plotly::add_segments(data = df_cov %>%
                           dplyr::filter(cov > threshold_cv & presence > threshold_presence),
                         x = ~mz, xend = ~mz,
                         y = ~cov, yend = ~presence,
                         line = list(color = 'darkgrey', dash = 'dot', width = 1),
                         opacity = 0.5,
                         legendgroup = "keep",
                         showlegend = FALSE,  # Hide this trace from the legend
                         hoverinfo = "skip"
    ) 
  
  
  # 1 threshold met 
  fig <- fig %>%
    plotly::add_markers(data = df_cov %>%
                          dplyr::filter((cov > threshold_cv & presence <= threshold_presence) 
                                        | (cov <= threshold_cv & presence > threshold_presence)),
                        x = ~mz, y = ~cov, 
                        opacity = 0.6,
                        name = "Remove 1", 
                        legendgroup = "Remove 1",
                        marker = list(color = "orange", symbol = "circle"),
                        text = ~paste("Cv: ", cov), 
                        hoverinfo = "text") %>%
    plotly::add_markers(data = df_cov %>%
                          dplyr::filter((cov > threshold_cv & presence <= threshold_presence)
                                        | (cov <= threshold_cv & presence > threshold_presence)),
                        x = ~mz, y = ~presence, 
                        opacity = 0.6,
                        legendgroup = "Remove 1",
                        showlegend = FALSE,
                        marker = list(color = "orange", symbol = "triangle-up"),
                        yaxis = "y2",
                        text = ~paste("Presence: ", presence),  
                        hoverinfo = "text") %>%
    plotly::add_segments(data = df_cov %>% 
                           dplyr::filter((cov > threshold_cv & presence <= threshold_presence)
                                         | (cov <= threshold_cv & presence > threshold_presence)),
                         x = ~mz, xend = ~mz,
                         y = ~cov, yend = ~presence,
                         line = list(color = 'darkgrey', dash = 'dot', width = 1),
                         opacity = 0.5,
                         legendgroup = "Remove 1",
                         showlegend = FALSE, 
                         hoverinfo = "skip"
    ) 
  
  # 2 thresholds not met group 
  fig <- fig %>%
    plotly::add_markers(data = df_cov %>%
                          dplyr::filter(cov < threshold_cv & presence <= threshold_presence),
                        x = ~mz, y = ~cov, 
                        opacity = 0.4,
                        name = "Remove 2", 
                        legendgroup = "Remove 2",
                        marker = list(color = "red", symbol = "circle"),
                        text = ~paste("Cv: ", cov),  
                        hoverinfo = "text") %>%
    plotly::add_markers(data = df_cov %>%
                          dplyr::filter(cov < threshold_cv & presence <= threshold_presence),
                        x = ~mz, y = ~presence, 
                        opacity = 0.4,
                        legendgroup = "Remove 2",
                        showlegend = FALSE,
                        marker = list(color = "red", symbol = "triangle-up"),
                        yaxis = "y2",
                        text = ~paste("Presence: ", presence),  
                        hoverinfo = "text") %>%
    plotly::add_segments(data = df_cov %>%
                           dplyr::filter(cov < threshold_cv & presence <= threshold_presence),
                         x = ~mz, xend = ~mz,
                         y = ~cov, yend = ~presence,
                         line = list(color = 'darkgrey', dash = 'dot', width = 1),
                         opacity = 0.5,
                         legendgroup = "Remove 2",
                         showlegend = FALSE,
                         hoverinfo = "skip"
    ) 
  
  fig <- fig %>%
    # Add a dummy point for Cov in the legend (no actual data displayed)
    plotly::add_markers(x = c(-1), y = c(-1),
                        name = "Cv", 
                        legendgroup = "shapes",
                        marker = list(color = "black", symbol = "circle", opacity = 0),  
                        showlegend = TRUE, 
                        hoverinfo = "skip",
                        visible = "legendonly") %>%
    
    # Add a dummy point for Missing in the legend (no actual data displayed)
    plotly::add_markers(x = c(-1), y = c(-1),
                        name = "Presence", 
                        legendgroup = "shapes",
                        marker = list(color = "black", symbol = "triangle-up", opacity = 0), 
                        showlegend = TRUE, hoverinfo = "skip",
                        visible = "legendonly") 
  
  # add points for threshold line
  fig <- fig %>% 
    plotly::add_trace(
      type = "scatter",
      mode = "lines",
      x = seq(min(df_cov$mz), max(df_cov$mz), by = 0.001),  
      y = seq(threshold_cv, threshold_cv, by = 0.001),                      
      line = list(
        color = "black",
        dash = "dash",
        width = 1
      ),
      showlegend = FALSE,  
      text = paste("Minimum Cv = ", threshold_cv),  
      hoverinfo = "text"
    ) %>%
    plotly::add_trace(
      type = "scatter",
      mode = "lines",
      x = seq(min(df_cov$mz), max(df_cov$mz), by = 0.001),  
      y = seq(threshold_presence, threshold_presence, by = 0.001),                    
      line = list(
        color = "black",
        dash = "dash",
        width = 1
      ),
      showlegend = FALSE,  # 
      text = paste("Minimum presence = ", threshold_presence),  
      hoverinfo = "text"
    )
  
  
  fig <- fig %>%
    plotly::layout(
      hovermode = "closest",
      title = list(text = "m/z vs Coefficient of Variation and Presence", y = 0.95),
      xaxis = list(
        title = "m/z",
        range = c(min(df_cov$mz), max(df_cov$mz))
      ),
      yaxis = list(
        title = "Coefficient of Variation (%)",
        range = c(0, (max(df_cov$cov) + 50))
      ),
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        title = list(text = "Presence (%)"),
        range = c(0, (max(df_cov$cov) + 50)),  # Set fixed range for secondary y-axis
        
        tickvals = c(0, 50, 100)
      ),
      legend = list(
        traceorder = "grouped",  # Group legend entries by legendgroup
        x = 1.15, y = 0.95,          # Position of the legend
        title = list(text = "Legend"),
        bgcolor = "rgba(255,255,255,0)",  # Transparent background to overlay shapes
        bordercolor = "black", 
        borderwidth = 1,
        itemclick = "skip",  # Disable legend item click
        itemdoubleclick = "skip"  # Disable legend item double-click
      ))
  # remove parts of mode bar   
  fig <- fig %>% 
    plotly::config(
      modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", 
                                 "sendDataToCloud", "lasso2d", "select2d"),
      displayModeBar = TRUE 
    )
  
  # save plot
  
  output_dir <- local.output_dir()
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("created directory:", output_dir)
  }
  
  file_name <- paste0(output_dir, local.dir_sep(), title, ".rds")
  saveRDS(fig, file_name)
  
  fig
}


