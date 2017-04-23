#' Runs EpiEstimApp
#'
#' This function runs EpiEstimApp
#' @param port The port to run on. Defaults to 3000.
#' @keywords run
#' @export
#' @examples
#' runEpiEstimApp()

runEpiEstimApp <- function(port=3000, ...) {
  appDir <- system.file("app", package="EpiEstimApp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `EpiEstimApp`.", call. = FALSE)
  }
  shiny::runApp(appDir, port=port, ...)
}
