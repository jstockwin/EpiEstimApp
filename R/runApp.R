#' Runs EpiEstimApp
#'
#' This function runs EpiEstimApp
#' @param ... Additional arguments (passed to shiny::runApp())
#' @keywords run
#' @export
#' @examples
#' \dontrun{
#' runEpiEstimApp()
#' }

runEpiEstimApp <- function(...) {
  appDir <- system.file("app", package="EpiEstimApp")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `EpiEstimApp`.", call. = FALSE)
  }
  shiny::runApp(appDir, ...)
}
