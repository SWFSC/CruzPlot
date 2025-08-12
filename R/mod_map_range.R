#' Map range module
#'
#' Shiny module fro defining map ranges
#'
#' @name map_range
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#'
#' @export
mod_map_range_ui <- function(id) {
  ns <- NS(id)

  ### Set default values for map - keep this format in case they need to be user-provided
  start.ll <- data.frame(X = c(-135, -117, 29, 52, 1))
  start.tick <- list(interval = 5, lon = -135, lat = 30)

  tagList(
    tabPanel(
      title = "Range",
      fluidRow(
        box(
          title = "Map range", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
          helpText(
            "For longitude values, please use the range -180 to 180.",
            "For instance, use left and right longitudes of 130 and -110, respectively,",
            "for a map of the northern Pacific.", tags$br(),
            "Click the 'Replot map' button after changing map range values,",
            "or if the map isn't properly sized in the window.", tags$br(),
            "In addition, users can automatically change the map range input values",
            "by clicking and holding to draw a box on map, although users still must click 'Replot map'.",
            "To clear the box, click within the plot outside of the box."
            ),
          fluidRow( #Separate to keep input boxes in line even if labels spill over
            column(3, tags$h5("Left longitude")),
            column(3, tags$h5("Right longitude")),
            column(3, tags$h5("Bottom latitude")),
            column(3, tags$h5("Top latitude"))
          ),
          fluidRow(
            column(3, numericInput(ns("lon_left"), NULL, value = start.ll$X[1])),
            column(3, numericInput(ns("lon_right"), NULL, value = start.ll$X[2])),
            column(3, numericInput(ns("lat_bot"), NULL, value = start.ll$X[3])),
            column(3, numericInput(ns("lat_top"), NULL, value = start.ll$X[4]))
          ),
          fluidRow(
            column(
              width = 3,
              selectInput(
                ns("resolution"), label = tags$h5("Resolution"),
                choices = list("Low" = 1, "High" = 2), selected = start.ll$X[5]
              )
            ),
            column(3, tags$br(), tags$br(), actionButton(ns("map_replot"), "Replot map"))
          ),
          tags$span(htmlOutput(ns("map_range_message")), style = "color: red;"),
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
  )
}

#' @export
mod_map_range_server  <- function(id) {
  moduleServer(id, function(input, output, session) {
    cruz.map.range <- reactiveValues(
      lon.range = NULL,
      lat.range = NULL,
      world2 = NULL,
      map.name = list()
    )
    # cruz.map.range <- reactiveValues(
    #   lon.range = c(-135, -117),
    #   lat.range = NULL,
    #   world2 = c(29, 52),
    #   map.name = list("world", NULL)
    # )

    ###############################################################################
    # Update map range when default study area buttons are clicked
    world2_calc <- function(lon.min, lon.max) {
      (lon.max < lon.min) & (lon.min > 0) & (lon.max < 0)
    }

    default_range_set <- function(ll.vals, res) {
      world2 <- world2_calc(ll.vals[1], ll.vals[2])

      updateNumericInput(session, "lon_left", value = ll.vals[1])
      updateNumericInput(session, "lon_right", value = ll.vals[2])
      updateNumericInput(session, "lat_bot", value = ll.vals[3])
      updateNumericInput(session, "lat_top", value = ll.vals[4])

      cruz.map.range$lon.range <- c(ll.vals[1], if_else(world2, ll.vals[2] + 360, ll.vals[2]))
      cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
      cruz.map.range$world2 <- world2
      cruz.map.range$map.name <- list(
        if (world2) {
          if_else(res == 2, "world2Hires", "world2")
        } else {
          if_else(res == 2, "worldHires", "world")
        },
        if (world2) {if (res == 2) regions.rm.hires else regions.rm} else NULL
      )
    }

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


    ###############################################################################
    # Update params as necessary
    cruzMapParam <- reactive({
      cruz.map.range$lon.range
      cruz.map.range$lat.range
      param.unit <- par("usr")
      param.inch <- par("pin")

      list(param.unit = param.unit, param.inch = param.inch)
    })


    ###############################################################################
    # Use brush to fill inputs with new map range
    observeEvent(input$map_brush, {
      req(cruz.map.range$lon.range, cruz.map.range$lat.range)

      if (isTruthy(input$map_brush)) {
        z <- input$map_brush
        z.coords <- round(c(z$xmin, z$xmax, z$ymin, z$ymax), 1)
        lon.left <- if_else(z.coords[1] > 180, z.coords[1] - 360, z.coords[1])
        lon.right <- if_else(z.coords[2] > 180, z.coords[2] - 360, z.coords[2])

        updateNumericInput(session, "lon_left", value = lon.left)
        updateNumericInput(session, "lon_right", value = lon.right)
        updateNumericInput(session, "lat_bot", value = z.coords[3])
        updateNumericInput(session, "lat_top", value = z.coords[4])

      } else {
        lon.range <- cruz.map.range$lon.range
        lat.range <- cruz.map.range$lat.range

        lon.range <- if_else(lon.range > 180, lon.range - 360, lon.range)

        updateNumericInput(session, "lon_left", value = lon.range[1])
        updateNumericInput(session, "lon_right", value = lon.range[2])
        updateNumericInput(session, "lat_bot", value = lat.range[1])
        updateNumericInput(session, "lat_top", value = lat.range[2])
      }
    }, ignoreNULL = FALSE)


    ###############################################################################
    # Series of steps/actions triggered by input$map_replot
    map.range.message <- reactiveVal(NULL)

    observeEvent(input$map_replot, {
      lon.min <- input$lon_left
      lon.max <- input$lon_right
      lat.min <- input$lat_bot
      lat.max <- input$lat_top

      # # Checks that inputs are numbers
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
      world2 <- world2_calc(lon.min, lon.max)

      if (world2) {
        lon.min <- ifelse(lon.min < 0, 360 + lon.min, lon.min)
        lon.max <- ifelse(lon.max < 0, 360 + lon.max, lon.max)
      }

      # Get map name
      hires <- input$resolution == 2

      m <- if_else(hires, "Hires", "")
      m <- if_else(world2, paste0("world2", m), paste0("world", m))

      #regions.rm and regions.rm.hires are created in server file
      reg.toplot <- if (world2 & hires) {
        regions.rm.hires
      } else if (world2 & !hires) {
        regions.rm
      } else {
        NULL
      }

      # Save as reactive values
      cruz.map.range$lon.range <- c(lon.min, lon.max)
      cruz.map.range$lat.range <- c(lat.min, lat.max)
      cruz.map.range$world2 <- world2
      cruz.map.range$map.name <- list(m, reg.toplot)

      # Reset map brush, in case
      session$resetBrush("map_brush")
    }, ignoreNULL = FALSE, priority = 9)


    output$map_range_message <- renderUI({
      HTML(req(map.range.message()))
    })

    ### Return values
    list(
      cruz.map.range = cruz.map.range
    )
  })
}
