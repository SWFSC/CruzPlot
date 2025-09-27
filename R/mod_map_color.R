#' Map color module
#'
#' Shiny module for map color
#'
#' @name mod_map_color
#'
#' @inheritParams mod_map_range
#' @inheritParams mod_plot
#'
#' @details
#' This module handles the map colors, including land color to water color. 
#' It also allows the user to download and/or load a bathymetry file using 
#' [marmap::getNOAA.bathy()]
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#' 
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file. 
#'   See [cruzplot_gui()] for more info. 
#' - `color_lakes_rivers`: a reactive indicating if lakes and rivers 
#'   should be plotted
#' - `cruzMapColorLand`: a reactive function for the land color
#' - `cruzMapRivers`: a reactive function for plotting rivers and lakes
#' - `cruzMapColorWater`: a reactive function for plotting water color
#'
#' @export
mod_map_color_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Color",
    fluidRow(
      cruz_box(
        title = "Land", width = 5,
        checkboxInput(ns("color_land_all"), label = "Color all land", value = TRUE), 
        conditionalPanel(
          condition = "input.color_land_all", ns = ns, 
          selectInput(ns("color_land"), label = tags$h5("Land color"), 
                      choices = cruz.palette.color, selected = "bisque1")
        )
        # fluidRow(
        #   column(6, checkboxInput(ns("color_land_all"), label = "Color all land", value = TRUE)),
        #   column(
        #     width = 6,
        #     conditionalPanel(
        #       condition = "input.color_land_all", ns = ns, 
        #       selectInput(ns("color_land"), label = tags$h5("Land color"), 
        #                   choices = cruz.palette.color, selected = "bisque1")
        #     )
        #   )
        # )
      ), 
      cruz_box(
        title = "Water", width = 7,
        checkboxInput(ns("map_rivers"), label = "Color major lakes and rivers", value = FALSE),
        selectInput(ns("color_water"), label = tags$h5("Water (background) color"),
                    choices = cruz.palette.color, selected = "white"),
        radioButtons(ns("color_water_style"), label = tags$h5("Ocean color style"),
                      choices = list("Single color" = 1, "Depth (bathymetric) shading" = 2),
                      selected = 1),
        conditionalPanel(
          condition = "input.color_water_style==2", ns = ns, 
          helpText("Load a CSV file with exactly 3 columns: latitude, longitude, and depth"),
          fileInput(ns("depth_file"), tags$h5("Bathymetric CSV file"), accept = ".csv"),
          textOutput(ns("bathy_load_text")),
          tags$span(textOutput(ns("bathy_message_text")), style = "color: blue;")
        )
      )
    ),
    fluidRow(
      cruz_box(
        title = "Download bathymetric data", width = 12,
        helpText("Download bathymetric data from NOAA website (see the documentation for",
                  tags$a(href = "https://CRAN.R-project.org/package=marmap",
                        "marmap function 'getNOAA.bathy'"),
                  "for more details).",
                  "The coordinates of the downloaded data will be the same as the current map range.",
                  "After downloading, you must load the CSV file into CruzPlot in the 'Water: Ocean color style' section"),
        numericInput(
          ns("depth_res"), 
          tags$h5("Bathymetric data resolution, in minutes (range: 0-60)"),
          value = 10, 
          min = 0, 
          max = 60, 
          step = 5
        ),
        uiOutput(ns("depth_download_button")),
        uiOutput(ns("depth_download_message"))
      )
    )
  )
}


#' @name mod_map_color
#' @export
mod_map_color_server  <- function(id, load_state, map_range) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(map_range)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
      # Bathymetric data, converted to CSV file xyz coordinates
      bathy.xyz = NULL,      
      # Logical flag indicating if a bathy download failed
      bathy.download = FALSE
    )

    # Load map_color state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          cruz.list[[item$id]] <- item$value
          # stop("Invalid map_color state - please report as an issue")
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10) #, ignoreInit = TRUE)

    #--------------------------------------------------------------------------
    # Color

    ### River values, if selected
    cruzMapRivers <- reactive({
      if (input$map_rivers) {
        rivs.try <- try(mapdata::riversMapEnv, silent = TRUE)
        validate(
          need(rivs.try, "Error - please install the mapdata package to use rivers maps")
        )
        rivs <- map("mapdata::rivers", plot = FALSE)
        
        req(is.logical(map_range()$world2))
        if (map_range()$world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)

        rivs
        
      } else {
        NULL
      }
    })

    ### Land
    cruzMapColorLand <- reactive({
      ifelse(input$color_land_all == TRUE, input$color_land, "white")
    })

    ### Load bathymetry data
    output$bathy_load_text <- renderText(cruzMapBathyLoad())
    cruzMapBathyLoad <- eventReactive(input$depth_file, {
      req(input$depth_file)
      file.in <- input$depth_file

      cruz.list$bathy.xyz <- NULL
      bathy.xyz <- read.csv(file.in$datapath)

      validate(
        need(ncol(bathy.xyz) >= 3,
            "The bathymetric CSV file must contain at least 3 columns")
      )

      cruz.list$bathy.xyz <- bathy.xyz

      NULL
    })

    ### Get color value and bathymetry data for water color
    cruzMapColorWater <- reactive({
      if (input$color_water_style == 1) {
        bathy <- NULL

      } else { #if (input$color_water_style == 2) {
        bathy.xyz <- cruz.list$bathy.xyz
        validate(need(bathy.xyz, "Please load a CSV file with bathymetric data"))

        # Make sure lat/lon range matches world2 flag
        req(is.logical(map_range()$world2))
        world2 <- map_range()$world2
        bathy.xyz[[1]] <- if (world2) {
          ifelse(bathy.xyz[[1]] < 0, bathy.xyz[[1]] + 360, bathy.xyz[[1]])
        } else {
          ifelse(bathy.xyz[[1]] > 180, bathy.xyz[[1]] - 360, bathy.xyz[[1]])
        }

        # Trim, and check that depth file lat/lon spans any map range
        lon.range <- req(map_range()$lon.range)
        lat.range <- req(map_range()$lat.range)
        bathy.xyz.keep <- between(bathy.xyz[[1]], lon.range[1], lon.range[2]) &
          between(bathy.xyz[[2]], lat.range[1], lat.range[2])

        bathy <- try(
          marmap::as.bathy(bathy.xyz[bathy.xyz.keep, ]),
          silent = TRUE
        )

        validate(
          need(
            inherits(bathy, "bathy"),
            paste("Unable to convert the loaded CSV file into a bathy object;",
                  "see `maramp::as.bathy` for data format requirements")
          )
        )
        validate(
          need(
            length(bathy) > 0,
            "The loaded bathymetric data does not cover any of the current map area"
          )
        )
      }

      list(input$color_water, bathy)
    })

    ###############################################################################
    # Download bathymetric data

    ### Download button for downloading bathymetric file
    output$depth_download_button <- renderUI({
      v.val <- input$depth_res
      v.mess <- "Bathymetric data resolution must be a whole number between 0 and 60"

      validate(need(!is.na(v.val), v.mess))
      validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.mess))
      validate(need(between(v.val, 0, 60), v.mess))

      downloadButton(session$ns("depth_download"), "Download bathymetric file")
    })

    ### Message indicating if download using marmap::getNOAA.bathy failed
    output$depth_download_message <- renderUI({
      if (cruz.list$bathy.download) {
        validate(
          paste(
            "CruzPlot was not able to download data using marmap::getNOAA.bathy.",
            "Please check your internet connection, and try again.", 
            "If this problem persists and you are able to use the function", 
            "yourself, please report this as an issue."
          )
        )
      } else {
        NULL
      }
    })

    # ### 'Reset' message if user leaves the page
    # observe({
    #   input$tabs
    #   input$tabset1

    #   isolate(cruz.list$bathy.download <- FALSE)
    # })

    ### Download bathymetric file
    output$depth_download <- downloadHandler(
      filename = function() {
        lon.range <- req(map_range()$lon.range)
        lat.range <- req(map_range()$lat.range)
        res <- input$depth_res

        # Defaults maramp file name: "marmap_coord_-135;29;-117;52_res_10.csv"
        str_glue(
          "marmap_coord_{lon1};{lon2};{lat1};{lat2}_res_{res}.csv", 
          lon1 = lon.range[1], 
          lon2 = lon.range[2], 
          lat1 = lat.range[1], 
          lat2 = lat.range[2]
        )
      },

      content = function(file) {
        cruz.list$bathy.download <- FALSE
        ll <- req(map_range()$latlon_input)
        world2 <- map_range()$world2

        # getNOAA.bathy() operates on -180 to 180 scale; use user inputs not lon.range
        bathy <- try(marmap::getNOAA.bathy(
          lon1 = ll[1], lon2 = ll[2], lat1 = ll[3], lat2 = ll[4],
          resolution = input$depth_res, antimeridian = world2,
          keep = FALSE
        ), silent = TRUE)

        if (!isTruthy(bathy)) cruz.list$bathy.download <- TRUE
        validate(need(bathy, "Download failed"))

        write.csv(marmap::as.xyz(bathy), file = file, row.names = FALSE)
      }
    )


    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    # Prepare values to save app state
    to_save <- reactive({
      list(
        save_widget("color_style", "radio"),
        save_widget("color_land_all", "check"),
        save_widget("color_land", "select"),
        save_widget("map_rivers", "check"),
        save_widget("color_water", "select"),
        save_widget("color_water_style", "radio"), 
        save_widget("depth_res", "numeric"), 
        save_widget("bathy.xyz", "reactive", cruz.list$bathy.xyz)
      )
    })

    ### Return values
    list(
      to_save = to_save,
      cruzMapColorLand = cruzMapColorLand, 
      cruzMapRivers = cruzMapRivers, 
      cruzMapColorWater = cruzMapColorWater
    )
  })
}
