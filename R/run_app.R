#' Run the Shiny Application
#'
#' @export
launch_sentimentplot <- function() {
  appDir <- system.file("shinyapp", package = "sentimentplots")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sentimentplots`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

