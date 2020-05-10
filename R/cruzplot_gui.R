#' Open CruzPlot
#'
#' Open the CruzPlot utility program, an R Shiny application
#'
#' @param ... Passed to \code{\link[shiny]{runApp}}
#'
#' @examples
#' \dontrun{
#' cruzplot_gui()
#' }
#'
#' @importFrom shiny runApp
#'
#' @export
cruzplot_gui <- function(...) {
  appDir <- system.file("shiny", package = "CruzPlot")
  if (appDir == "") {
    stop("There was an error opening CruzPlot; try re-installing 'CruzPlot'",
         call. = FALSE)
  }
  runApp(appDir, ...)
}
