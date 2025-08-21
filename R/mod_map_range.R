#' Map range module
#'
#' Shiny module for map ranges
#'
#' @name mod_map_range
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param start_vals A named list of 5 starting values:
#'   left and right longitude (`lon_left` and `lon_right`),
#'   bottom and top latitude (`lat_bot` and `lat_top`),
#'   and map resolution (`res`)
#' @param app_state A [shiny::reactiveValues()] object that serves as the shared,
#'   central state for the entire application. This object is initialized in the
#'   main server and passed down to each module, enabling communication and
#'   synchronization between them. Any changes made to this object in one
#'   module will be immediately visible in all others.
#'   See details for keys specific to this function.
#' @param brush Either `NULL` (default), or the brush from [mod_plot()]
#'
#' @details
#' Additional details...
#'
#' @returns A list with the following elements:
#' * `map_range`: reactive values with relevant map_range elements
#'
#' @export
mod_map_range_ui <- function(
  id,
  start_vals = list(
    lon_left = -135,
    lon_right = -117,
    lat_bot = 29,
    lat_top = 52,
    res = 1
  )
) {
  ns <- NS(id)

  ### Set default values for map
  # start.ll <- data.frame(X = c(-135, -117, 29, 52, 1))

  # tagList(
  tabPanel(
    title = "Range",
    fluidRow(
      box(
        title = "Map range", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
        helpText(
          "For longitude values, please use the range -180 to 180.",
          "For instance, use left and right longitudes of 130 and -110,",
          "respectively, for a map of the northern Pacific.",
          tags$br(), tags$br(),
          "Click the 'Replot map' button after changing map range values,",
          "or if the map isn't properly sized in the window.",
          tags$br(), tags$br(),
          "In addition, users can automatically change the map range input values",
          "by clicking and holding to draw a box on map.",
          "Users still must click 'Replot map'.",
          "To clear the box, click within the plot, outside of the box."
        ),
        fluidRow( #Separate to keep input boxes in line even if labels spill over
          column(3, tags$h5("Left longitude")),
          column(3, tags$h5("Right longitude")),
          column(3, tags$h5("Bottom latitude")),
          column(3, tags$h5("Top latitude"))
        ),
        fluidRow(
          column(3, numericInput(ns("lon_left"), NULL, value = start_vals[["lon_left"]])),
          column(3, numericInput(ns("lon_right"), NULL, value = start_vals[["lon_right"]])),
          column(3, numericInput(ns("lat_bot"), NULL, value = start_vals[["lat_bot"]])),
          column(3, numericInput(ns("lat_top"), NULL, value = start_vals[["lat_top"]]))
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              ns("resolution"), label = tags$h5("Resolution"),
              choices = list("Low" = 1, "High" = 2), selected = start_vals[["res"]]
            )
          ),
          column(3, tags$br(), tags$br(), actionButton(ns("map_replot"), "Replot map"))
        ),
        # tags$span(htmlOutput(ns("map_range_message")), style = "color: red;"),
        tags$h5("Set the map range to a default study area and replot:"),
        actionButton(ns("map_replot_cce"), "CCE"),
        actionButton(ns("map_replot_cce2"), "Extended CCE"),
        actionButton(ns("map_replot_etp"), "ETP"),
        actionButton(ns("map_replot_hawaii"), "Hawaii"),
        actionButton(ns("map_replot_hawaiimain"), "Main Hawaiian Islands"),
        actionButton(ns("map_replot_marianas"), "Marianas")
      ),
      # box(
      #   title = "Scale bar", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
      #   checkboxInput("bar", "Plot scale bar", value = FALSE),
      #   conditionalPanel(
      #     condition = "input.bar",
      #     helpText(
      #       "Provide the coordinates for the left edge of the scale bar.",
      #       "The coordinates must have the same range as the map range coordinates."
      #     ),
      #     fluidRow(
      #       column(4, uiOutput("scale_lon_uiOut_numeric")),
      #       column(4, uiOutput("scale_lat_uiOut_numeric")),
      #       column(4, numericInput("scale_width", tags$h5("Width of bar"), value = 2, min = 1, max = 6, step = 1)),
      #     ),
      #     fluidRow(
      #       column(
      #         width = 4,
      #         radioButtons(
      #           "scale_units", tags$h5("Scale bar units"),
      #           choices = list("Kilometers" = 1, "Nautical miles" = 2),
      #           selected = 2
      #         )
      #       ),
      #       column(4, uiOutput("out_scale_len"))
      #     )
      #   )
      # ),
      # box(
      #   title = "Coastline", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
      #   checkboxInput("coast", label = "Use coastline file", value = FALSE),
      #   conditionalPanel(
      #     condition = "input.coast",
      #     helpText(
      #       "Map limits will automatically be updated to the extent of the",
      #       "coastline file. Note: CruzPlot can only process coastline files",
      #       "with points are between -180 and 0"
      #     ),
      #     fileInput("coast_file", label = tags$h5("Coastline file"), width = "50%")
      #   )
      # )
    )
  )
  # )
}


#' @name mod_map_range
#' @export
mod_map_range_server <- function(id, app_state, brush = NULL) {
  moduleServer(id, function(input, output, session) {
    map_range <- reactiveValues(
      lon.range = NULL,
      lat.range = NULL,
      world2 = NULL,
      map.name = list()
    )

    # ### Update widgets. Ok without if checks, because these app_state elements
    # # are only updated on the click
    # observeEvent(app_state$lon.range, {
    #   updateNumericInput(session, "lon_left", value = app_state$lon.range[1])
    #   updateNumericInput(session, "lon_right", value = app_state$lon.range[2])
    # })
    #
    # observeEvent(app_state$lat.range, {
    #   updateNumericInput(session, "lat_bot", value = app_state$lat.range[1])
    #   updateNumericInput(session, "lat_top", value = app_state$lat.range[2])
    # })
    #
    # observeEvent(app_state$map.name, {
    #   res <- if_else(str_detect(app_state$map.name[[1]], "Hires"), "2", "1")
    #   updateSelectInput(session, "resolution", selected = res)
    # })

    observe({
      app_state$lon_left <- input$lon_left
      app_state$lon_right <- input$lon_right
      app_state$lat_bot <- input$lat_bot
      app_state$lat_top <- input$lat_top
      app_state$res <- input$res
    })

    observeEvent(app_state$lon_left, {
      updateNumericInput(session, "lon_left", value = app_state$lon_left)
    })
    observeEvent(app_state$lon_right, {
      updateNumericInput(session, "lon_right", value = app_state$lon_right)
    })
    observeEvent(app_state$lat_bot, {
      updateNumericInput(session, "lat_bot", value = app_state$lat_bot)
    })
    observeEvent(app_state$lat_top, {
      updateNumericInput(session, "lat_top", value = app_state$lat_top)
    })
    observeEvent(app_state$res, {
      updateSelectInput(session, "res", selected = app_state$res)
    })


    # Countries to be removed for world2 map
    # http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
    remove <- c(
      "UK:Great Britain", "France", "Spain", "Algeria", "Mali",
      "Burkina Faso",      "Ghana", "Togo"
    )
    mapnames <- map("world2", plot = FALSE)$names
    mapnames.hires <- map("mapdata::world2Hires", plot = FALSE)$names
    regions.rm <- base::setdiff(mapnames, remove)
    regions.rm.hires <- base::setdiff(mapnames.hires, remove)

    ###############################################################################
    # TODO: brush needs to update input

    ###############################################################################
    # TODO: Move these functions into their own R file

    world2_calc <- function(lon1, lon2) {
      (lon2 < lon1) & (lon1 > 0) & (lon2 < 0)
    }

    map_name_calc <- function(world2, res) {
      # Calculate values that are stored in cruz.map.range#map.name\
      hires <- (res == 2)

      map.name <- if (world2) {
        if_else(hires, "mapdata::world2Hires", "world2")
      } else {
        if_else(hires, "mapdata::worldHires", "world")
      }

      reg.toplot <- if (world2 & hires) {
        regions.rm.hires
      } else if (world2 & !hires) {
        regions.rm
      } else {
        NULL
      }

      list(map.name, reg.toplot)
    }

    lon_range_world2 <- function(lon.range, world2) {
      lon1 <- lon.range[1]
      lon2 <- lon.range[2]

      if (world2) {
        lon1 <- ifelse(lon1 < 0, 360 + lon1, lon1)
        lon2 <- ifelse(lon2 < 0, 360 + lon2, lon2)
      }

      c(lon1, lon2)
    }


    # Update map range when default study area buttons are clicked
    default_range_set <- function(ll.vals, res) {
      world2 <- world2_calc(ll.vals[1], ll.vals[2])

      updateNumericInput(session, "lon_left", value = ll.vals[1])
      updateNumericInput(session, "lon_right", value = ll.vals[2])
      updateNumericInput(session, "lat_bot", value = ll.vals[3])
      updateNumericInput(session, "lat_top", value = ll.vals[4])

      lon.range <- lon_range_world2(c(ll.vals[1], ll.vals[2]), world2)

      map_range$lon.range <- lon.range
      map_range$lat.range <- c(ll.vals[3], ll.vals[4])
      map_range$world2 <- world2
      map_range$map.name <- map_name_calc(world2, res)
      # cruz.map.range$res <- "1"
    }

    # TODO: Make these part of the function input, to be able to customize
    ### CCE
    observeEvent(input$map_replot_cce, {
      ll.vals <- c(-135, -117, 29, 52)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)

    ### Expanded CCE
    observeEvent(input$map_replot_cce2, {
      ll.vals <- c(-135, -110, 27, 52)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)

    ### ETP
    observeEvent(input$map_replot_etp, {
      ll.vals <- c(-155, -75, -10, 50)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)

    ### Hawaii
    observeEvent(input$map_replot_hawaii, {
      ll.vals <- c(175, -150, 12, 35)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)

    ### Main Hawaiian Islands
    observeEvent(input$map_replot_hawaiimain, {
      ll.vals <- c(-162, -153, 17.5, 23.5)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)

    ### Marianas
    observeEvent(input$map_replot_marianas, {
      ll.vals <- c(140, 150, 10, 24)
      default_range_set(ll.vals, input$resolution)
    }, priority = 11)


    # ###############################################################################
    # # Update params as necessary
    # cruzMapParam <- reactive({
    #   app_state$lon.range
    #   app_state$lat.range
    #   param.unit <- par("usr")
    #   param.inch <- par("pin")
    #
    #   list(param.unit = param.unit, param.inch = param.inch)
    # })


    ###############################################################################
    # Series of steps/actions triggered by input$map_replot
    observeEvent(input$map_replot, {
      if (isTruthy(brush())) {
        # If the map range got a truthy brush object, then use it
        z <- brush()
        z.coords <- round(c(z$xmin, z$xmax, z$ymin, z$ymax), 1)
        lon.left <- if_else(z.coords[1] > 180, z.coords[1] - 360, z.coords[1])
        lon.right <- if_else(z.coords[2] > 180, z.coords[2] - 360, z.coords[2])
        lat.bot <- z.coords[3]
        lat.top <- z.coords[4]

        updateNumericInput(session, "lon_left", value = lon.left)
        updateNumericInput(session, "lon_right", value = lon.right)
        updateNumericInput(session, "lat_bot", value = lat.bot)
        updateNumericInput(session, "lat_top", value = lat.top)

        # Reset map brush, in case
        session$resetBrush(z$brushId)

      } else {
        # Otherwise use input widgets
        lon.left <- input$lon_left
        lon.right <- input$lon_right
        lat.bot <- input$lat_bot
        lat.top <- input$lat_top
      }

      # # Checks that inputs are numbers
      # map.range.message <- reactiveVal(NULL)
      # vals.bad <- c("", "-", "+", NA)
      # m1 <- if ((lon.min %in% vals.bad) | !between(lon.min, -180, 180))
      #   "The left longtiude must be a number between -180 and 180" else NULL
      # m2 <- if ((lon.max %in% vals.bad) | !between(lon.max, -180, 180))
      #   "The right longtiude must be a number between -180 and 180" else NULL
      # m3 <- if ((lat.min %in% vals.bad) | !between(lat.min, -90, 90))
      #   "The bottom latitude must be a number between -90 and 90" else NULL
      # m4 <- if ((lat.max %in% vals.bad) | !between(lat.max, -90, 90))
      #   "The top latitude must be a number between -90 and 90" else NULL
      #
      # m.all <- c(m1, m2, m3, m4)
      #
      # map.range.message(if (is.null(m.all)) m.all else paste(m.all, collapse = "<br/>"))
      # req(is.null(m.all))

      # Determine if world2 map should be used and thus if lons need to be rescaled
      world2 <- world2_calc(lon.left, lon.right)
      lon.range <- lon_range_world2(c(lon.left, lon.right), world2)

      # Save as reactive values
      map_range$lon.range <- lon.range
      map_range$lat.range <- c(lat.bot, lat.top)
      map_range$world2 <- world2
      map_range$map.name <- map_name_calc(world2, input$resolution)
    }, ignoreNULL = FALSE, priority = 9)


    # output$map_range_message <- renderUI({
    #   HTML(req(map.range.message()))
    # })

    ### Return values
    list(
      map_range = map_range
    )
  })
}
