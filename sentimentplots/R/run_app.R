#' Run the Shiny Application
#'
#' @export
run_app <- function() {
  appDir <- system.file("R", package = "sentimentplots")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `myShinyApp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

# roxygen2::roxygenize()
#
# # Install devtools if you haven't already
# #install.packages("devtools")
#
# # Build the package
# devtools::build()
#
# # Install the package
# devtools::install()  # Or devtools::install(".")

