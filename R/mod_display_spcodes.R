#' Display species info module
#'
#' Shiny module for displaying loaded species codes info
#'
#' @name mod_display_spcodes
#'
#' @inheritParams mod_display_format
#' @param sp_codes reactive of a dataframe with loaded species codes. 
#'   For CruzPlot, this is from [mod_das_server()] 
#'
#' @details
#' This module displays the species code information loaded in 
#' the DAS module. 
#'
#' @returns The UI function returns a [shinydashboard::tabItem()] object. 
#' 
#' The server function returns nothing
#'
#' @export
mod_display_spcodes_ui <- function(id, tab_name) {
  ns <- NS(id)

  tabItem(
    tabName = tab_name,
    fluidRow(
      box(
        title = "Species Information", width = 12,status = "primary", solidHeader = TRUE, 
        radioButtons(
          ns("sp_type"), 
          "Select species codes to display",
          choices = list("Mammals" = 1, "Turtles" = 2, "All" = 3)
        ),
        textOutput(ns("sp_message")),
        conditionalPanel(
          condition = "input.sp_type == 1", ns = ns, 
          DT::dataTableOutput(ns("sp1"))
        ),
        conditionalPanel(
          condition = "input.sp_type == 2", ns = ns, 
          DT::dataTableOutput(ns("sp2"))
        ),
        conditionalPanel(
          condition = "input.sp_type == 3", ns = ns, 
          DT::dataTableOutput(ns("sp3"))
        )
      )
    )
  )
}


#' @name mod_display_spcodes
#' @export
mod_display_spcodes_server  <- function(id, sp_codes) {
  moduleServer(id, function(input, output, session) {
    output$sp_message <- renderText({
      validate(
        need(
          sp_codes(), 
          "Please load a species codes file in the 'Plot DAS Data - Data' section"
        )
      )
    })
    
    
    cruzSpeciesMammals <- reactive({
      sp.codes <- req(sp_codes())
      turtle.codes <- cruzSpeciesTurtles()$SpCode
      ind.mammals <- which(!(sp.codes$SpCode %in% turtle.codes))

      sp.codes[ind.mammals, ]
    })

    cruzSpeciesTurtles <- reactive({
      sp.codes <- req(sp_codes())
      # ind.turtles <- which(sp.codes$Code %in% turtle.codes)
      turtle.nona <- !is.na(sp.codes$CommonName)
      turtle.flag <- str_detect(str_to_lower(sp.codes$CommonName), "turtle")
      ind.turtles <- turtle.flag & turtle.nona

      sp.codes[ind.turtles, ]
    })


    sp.codes.names <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")

    output$sp1 <- DT::renderDataTable({ #Mammals
      sp.mammals <- cruzSpeciesMammals()
      names(sp.mammals) <- sp.codes.names
      sp.mammals
    })
    output$sp2 <- DT::renderDataTable({ #Turtles
      sp.turtles <- cruzSpeciesTurtles()
      names(sp.turtles) <- sp.codes.names
      sp.turtles
    })
    output$sp3 <- DT::renderDataTable({ #All
      sp.all <- req(sp_codes())
      names(sp.all) <- sp.codes.names
      sp.all
    })
  })
}