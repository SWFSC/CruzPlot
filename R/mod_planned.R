#' Planned transect module
#'
#' Shiny module for planned transects
#'
#' @name mod_planned
#'
#' @inheritParams mod_map_range
#' 
#' @details
#' This module handles...
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#' 
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file. 
#'   See [cruzplot_gui()] for more info. 
#' - `todo`: ...
#' 
#' @export
mod_planned_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
      title = "Planned Transects",
      tags$h5("todo")
  )
}


#' @name mod_planned
#' @export
mod_planned_server  <- function(id, load_state) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
    )

    # Load state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          cruz.list[[item$id]] <- item$value
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10)


    


    ###############################################################################
    ### Save values
    to_save <- reactive({
      list(
        # save_widget("ndas_plot", "check"),
        # save_widget("ndas.data", "reactive", cruz.list$ndas.data), 
        # save_widget("ndas.df", "reactive", cruz.list$ndas.df), 
        # save_widget("ndas.toplot", "reactive", cruz.list$ndas.toplot)
      )
    })

    ### Return values
    list(
      to_save = to_save
    )
  })
}