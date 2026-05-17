#' Launch Interactive DMSS Cost-Effectiveness Model
#'
#' @param ... Arguments passed to shiny::runApp.
#' @return Invisible NULL.
#' @export
#' @examples
#' \dontrun{ launch_app() }
launch_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("shiny is required to run the app. Install with: install.packages('shiny')")
  app_dir <- system.file("shiny-app", package = "huncMarkovDMSS")
  if (app_dir == "")
    stop("Shiny app not found. Please reinstall huncMarkovDMSS.", call. = FALSE)
  shiny::runApp(app_dir, ...)
}
