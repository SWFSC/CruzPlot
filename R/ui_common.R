#' Common UI blocks
#' 
#' Common UI blocks used by the CruzPlot app
#' 
#' @details
#' Blocks of UI that are used in multiple places throughout the app. 
#' 
#' For instance, instructions on how to manage selected inputs 
#' (via `ui_select_instructions`).
#' 
#' @returns each function returns a chunk of shiny UI
#' 
#' @export
ui_select_instructions <- function() {
  helpText(
    "To remove selected input(s): click the input(s) to remove, ",
    "and then click backspace or delete"
  )
}