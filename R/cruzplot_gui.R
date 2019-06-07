#' Open CruzPlot
#'
#' Open the CruzPlot utility program, an R Shiny application
#'
#' @param launch.browser Logical with default of \code{TRUE}; passed to \code{launch.browser}
#'   argument of \code{\link[shiny]{runApp}}
#'
#' @usage cruzplot_gui(launch.browser = TRUE)
#'
#' @importFrom shiny runApp
#'
#' @export
cruzplot_gui <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "CruzPlot")
  if (appDir == "") {
    stop("The CruzPlot GUI folder could not be found. Try re-installing 'CruzPlot'",
         call. = FALSE)
  }
  runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
