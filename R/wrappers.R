#' CruzPlot wrapper
#'
#' CruzPlot wrapper functions
#'
#' @name wrapper
#'
#' @param title see [shinydashboard::box()]
#' @param width see [shinydashboard::box()]
#' @param ... arguments passed directly to wrapped function
#'
#' @details
#' `cruz_box` is a wrapper function around [shinydashboard::box()]
#' with the following set arguments:
#' - status = "warning" 
#' - solidHeader = FALSE
#' - collapsible = TRUE
#'
#' @returns The output of the respective wrapped function
#'
#' @export
cruz_box <- function(title, width, ...) {
  shinydashboard::box(
    title = title, 
    width = width, 
    status = "warning", 
    solidHeader = FALSE, 
    collapsible = TRUE, 
    ...
  )
}