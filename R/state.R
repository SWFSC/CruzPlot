#' CruzPlot load/save state
#'
#' CruzPlot functions for loading and saving app state
#'
#' @name state
#'
#' @param item a list; the output of a `save_widget` call
#' @param session R Shiny session object. 
#'   The session object passed to the relevant server function
#' @param id The id (character) of the input object, or the name within 
#'   the module's [shiny::reactiveValues()]
#' @param type the type of the given value. See details for options
#' @param value value of the object specified by `id`. 
#'   If `NULL`, then `input` must be specified
#' @param input R Shiny input object. 
#'   The input object passed to the relevant server function. 
#'   Ignored if `value` is not `NULL`. See Details for how object is used
#'
#' @details
#' These are helper functions for loading and saving app state. 
#' CruzPlot modules call `save_widget` on each value that needs to be saved
#' to create lists that include the id, type, and value. 
#' These values are saved in the CruzPlot RDATA file. 
#' The value can be passed in explicitly via the `value` argument; 
#' if `value` is `NULL`, then the value will either be 
#' extracted from the `input` argument  if `input` is not `NULL`, 
#' or extracted from the output of [shiny::getDefaultReactiveDomain()] 
#' if `input` is `NULL``. 
#' 
#' When the saved state is loaded back into a new CruzPlot session 
#' and passed to the module, the module either 
#' a) updates the appropriate reactive value (custom code within the module) 
#' or b) updates the relevant input using `update_widget`. 
#' 
#' The accepted `type` values, and their corresponding update calls: 
#' - text: [shiny::updateTextInput()]
#' - numeric: [shiny::updateNumericInput()]
#' - select: [shiny::updateSelectInput()]
#' - check: [shiny::updateCheckboxInput()]
#' - radio: [shiny::updateRadioButtons()]
#' - reactive: handled individually by module
#'
#' @returns See details
#' * `save_widget`: a list with three named elements: id, type, and value
#' * `update_widget`: nothing
#'
#' @export
save_widget <- function(id, type, value = NULL, input = NULL) {
  # Checks
  if (!is.character(id)) stop("id must be a character")
  
  if (is.null(value)) {
    if (is.null(input)) {
      value <- shiny::getDefaultReactiveDomain()$input[[id]]
    } else {
      value <- input[[id]]
    }
  }
  
  if (type %in% c("text", "select", "radio")) {
    if (!is.character(value)) stop("Given the type (", type, "), 'value' must be a character")
  } else if (type %in% c("numeric")) {
    if (!is.numeric(value)) stop("Given the type (", type, "), 'value' must be a numeric")
  } else if (type %in% c("check")) {
    if (!is.logical(value)) stop("Given the type (", type, "), 'value' must be a logical")
  } else if (type %in% c("reactive")) {
    # Nothing to check
  } else{
    stop("The given type (", type, ") is an unrecognized type")
  }

  # Return list
  list(
    id = id,
    type = type,
    value = value
  )
}


#' @name state
#' @export
update_widget <- function(item, session) {
  switch(
    item$type,
    "text" = updateTextInput(session, item$id, value = item$value),
    "numeric" = updateNumericInput(session, item$id, value = item$value),
    "select" = updateSelectInput(session, item$id, selected = item$value),
    "radio" = updateRadioButtons(session, item$id, selected = item$value),
    "check" = updateCheckboxInput(session, item$id, value = item$value)
  )
}
  