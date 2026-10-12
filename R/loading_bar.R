# Shared animated indeterminate loading bar used across the pipeline steps.
# Pure-CSS animation (CSS lives in the app UI head in modapp.R) keeps the bar
# moving in the browser while R is busy computing, with the status label
# sitting inside it. Returns an HTML string to inject via shinyjs::html().
loading_bar_html <- function(label) {
  paste0(
    "<div class='qc-loading-wrap'>",
    "<div class='qc-loading-bar'></div>",
    "<div class='qc-loading-text'>", label, "</div>",
    "</div>"
  )
}

# Completed state of the bar: solid green, no animation, label inside.
# Shown when a step finishes successfully.
loading_bar_done_html <- function(label = "Complete ✅") {
  paste0(
    "<div class='qc-loading-wrap'>",
    "<div class='qc-loading-bar qc-done'></div>",
    "<div class='qc-loading-text'>", label, "</div>",
    "</div>"
  )
}
