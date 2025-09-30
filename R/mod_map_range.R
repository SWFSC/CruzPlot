#' Map range module
#'
#' Shiny module for map range
#'
#' @name mod_map_range
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param start_vals A named list of 5 starting values:
#'   left and right longitude (`lon_left` and `lon_right`),
#'   bottom and top latitude (`lat_bot` and `lat_top`),
#'   and map resolution (`resolution`)
#' @param load_state A [shiny::reactiveValues()] object that allows for a 
#' saved 'app state' to be loaded for this module. The main function
#' saves the state, using the 'to_save' output list, and then loads
#' those values into the `load_state` object when a saved app state is loaded
#' @param brush Either `NULL` (default), or the brush from [mod_plot()]
#'
#' @details
#' This module handles strictly the map range: lat/lon boundaries, 
#' and map resolution. It provides some convenience action buttons.
#' Its returned values (the map range) are used by many other modules 
#' for setting bounds, etc.
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#' 
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file. 
#'   See [cruzplot_gui()] for more info. 
#' - `map_range`: a reactive, containing the values needed by [mod_plot_server()].
#'     Specifically: 
#'   - `lon.range`, a vector of the left and right longitudes, respectively;
#'   - `lat.range`, a vector of the bottom and top latitudes, respectively;
#'   - `world2`, a logical indicating if the map is using the world2 protocol
#'     (lons 0-360);
#'   - `map.name`, a list of 1) the map name, passed directly to the `database`
#'     argument of [maps::map()], and 2) the regions to plot,
#'     passed directly to the `regions` argument of [maps::map()]
#'   - `latlon_input`: the input lat/lon values, for functions that need the 
#'     map range in `[-180, 180]` coordinates
#'
#' @export
mod_map_range_ui <- function(
    id,
    start_vals = list(
      lon_left = -135,
      lon_right = -117,
      lat_bot = 29,
      lat_top = 52,
      resolution = 1
    )
) {
  ns <- NS(id)

  ### Set default values for map
  # start.ll <- data.frame(X = c(-135, -117, 29, 52, 1))

  tabPanel(
    title = "Range",
    fluidRow(
      cruz_box(
        title = "Map range", width = 12, 
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
      cruz_box(
        title = "Coastline", width = 12, 
        checkboxInput(ns("coast"), label = "Use coastline file", value = FALSE),
        conditionalPanel(
          condition = "input.coast", ns = ns, 
          helpText("Map limits will automatically be updated to the extent of the",
                    "coastline file. Note: the app can only process coastline files",
                    "with points between -180 and 0. "),
          fileInput("coast_file", label = tags$h5("Coastline file"), width = "50%"), 
          textOutput(ns("coast_load_text")),
        )
      )
    )
  )
}


#' @name mod_map_range
#' @export
mod_map_range_server <- function(id, load_state, brush = NULL) {
  moduleServer(id, function(input, output, session) {
    map_range <- reactiveValues(
      lon.range = NULL,
      lat.range = NULL,
      world2 = NULL,
      map.name = list()
    )

    ll_vals <- reactiveValues(
      lon_left = NULL,
      lon_right = NULL,
      lat_bot = NULL,
      lat_top = NULL,
      resolution = NULL
    )


    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          ll_vals[[item$id]] <- item$value
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10) #, ignoreInit = TRUE)


    ###############################################################################
    # Update inputs via brush

    observeEvent(brush(), {
      z <- req(brush())
      z.coords <- round(c(z$xmin, z$xmax, z$ymin, z$ymax), 1)
      lon.left <- if_else(z.coords[1] > 180, z.coords[1] - 360, z.coords[1])
      lon.right <- if_else(z.coords[2] > 180, z.coords[2] - 360, z.coords[2])
      lat.bot <- z.coords[3]
      lat.top <- z.coords[4]

      c(lon.left, lon.right, lat.bot, lat.top)


      updateNumericInput(session, "lon_left", value = lon.left)
      updateNumericInput(session, "lon_right", value = lon.right)
      updateNumericInput(session, "lat_bot", value = lat.bot)
      updateNumericInput(session, "lat_top", value = lat.top)

    }, ignoreNULL = TRUE)

    ###############################################################################
    # TODO: Move these functions into their own R file
    world2_calc <- function(lon1, lon2) {
      (lon2 < lon1) & (lon1 > 0) & (lon2 < 0)
    }

    map_name_calc <- function(world2, res) {
      # Calculate values that are stored in cruz.map.range#map.name\
      hires <- (res == 2)

      map.name <- if_else(
        world2, 
        if_else(hires, "mapdata::world2Hires", "world2"), 
        if_else(hires, "mapdata::worldHires", "world")
      )

      # Countries to be removed for world2 map
      # http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
      remove <- c(
        "UK:Great Britain", "France", "Spain", "Algeria", "Mali",
        "Burkina Faso", "Ghana", "Togo"
      )
      mapnames <- map("world2", plot = FALSE)$names
      mapnames.hires <- map("mapdata::world2Hires", plot = FALSE)$names
      # regions.rm <- base::setdiff(mapnames, remove)
      # regions.rm.hires <- base::setdiff(mapnames.hires, remove)

      reg.toplot <- if (world2 & hires) {
        base::setdiff(mapnames.hires, remove)
      } else if (world2 & !hires) {
        base::setdiff(mapnames, remove)
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
      # session$sendInputMessage("map_replot", list(value = input$map_replot + 1))

      ll_vals$lon_left <- ll.vals[1]
      ll_vals$lon_right <- ll.vals[2]
      ll_vals$lat_bot <-  ll.vals[3]
      ll_vals$lat_top <- ll.vals[4]

      # app_state$lon_left <- ll.vals[1]
      # app_state$lon_right <- ll.vals[2]
      # app_state$lat_bot <- ll.vals[3]
      # app_state$lat_top <- ll.vals[4]
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


    ###############################################################################
    # Series of steps/actions triggered by input$map_replot

    observeEvent(input$map_replot, {
      lon.left <- input$lon_left
      lon.right <- input$lon_right
      lat.bot <- input$lat_bot
      lat.top <- input$lat_top
      res <- input$resolution

      ll_vals$lon_left <- lon.left
      ll_vals$lon_right <- lon.right
      ll_vals$lat_bot <- lat.bot
      ll_vals$lat_top <- lat.top
      ll_vals$resolution <- res
    }, ignoreNULL = FALSE)

    # When ll_vals reactiveValues changes, then update the reactive output
    cruzMapRange <- reactive({
      lon.left <- req(ll_vals$lon_left)
      lon.right <- req(ll_vals$lon_right)
      lat.bot <- req(ll_vals$lat_bot)
      lat.top <- req(ll_vals$lat_top)
      res <- req(ll_vals$resolution)

      isolate({
        # Reset map brush, if necessary
        if (isTruthy(brush())) session$resetBrush(brush()$brushId)

        world2 <- world2_calc(lon.left, lon.right)
        lon.range <- lon_range_world2(c(lon.left, lon.right), world2)
        lat.range = c(lat.bot, lat.top)

        ### Validate
        stopifnot("world2 param is not a logical" = inherits(world2, "logical"))

        if (res == "2") {          
          x.try <- try(mapdata::worldHiresMapEnv, silent = TRUE)
          validate(
            need(x.try, "Error - please install the mapdata package to use hires maps")
          )
        }

        vals.bad <- c("", "_", "+", NA)
        validate( #lats
          need(
            all(!(lat.range %in% vals.bad) & between(lat.range, -90, 90)),
            "The latitudes must be a number between -90 and 90"
          )
        )

        if ((0 <= lon.range[1] & 0 <= lon.range[2]) || (lon.range[1] < 0 & lon.range[2] < 0))
          validate( #lons
            need(
              lon.range[1] < lon.range[2],
              paste(
                "Left longitude must be less than right longitude,",
                "unless left longitude is positive and right longitude",
                "is negative (Pacific-centered map)"
              )
            )
          )
        if (world2) {
          validate(
            need(
              all(!(lon.range %in% vals.bad) & between(lon.range, 0, 360)),
              "The longtiudes must be a number between -180 and 180"
            )
          )
        } else {
          validate(
            need(
              all(!(lon.range %in% vals.bad) & between(lon.range, -180, 180)),
              "The longtiudes must be a number between -180 and 180"
            )
          )
        }

        ll <- c(input$lon_left, input$lon_right, input$lat_bot, input$lat_top)

        # Save as reactive values
        list(
          lon.range = lon.range,
          lat.range = lat.range,
          world2 = world2,
          map.name = map_name_calc(world2, res), 
          latlon_input = ll
        )
      })
    })

    # latlon_input <- reactive(
    #   c(input$lon_left, input$lon_right, input$lat_bot, input$lat_top)
    # )


    ###########################################################################
    ### Coastline    
    coast_load <- eventReactive(input$coast_file, {
      req(input$coast_file)
      ll_vals$coastline <- NULL
      coastline <- read.csv(input$coast_file$datapath)

      validate(need(coastline, "Please load a valid CSV file"))
      validate(
        need(identical(names(coastline), c("lon", "lat")),
            paste("The coastline CSV file must have exactly", 
                  "two columns named 'lon' and 'lat', respectively"))
      )
      coastline <- rbind(c(NA, NA, NA), coastline, c(NA, NA, NA))

      ll_vals$coastline <- coastline

      NULL
    }, ignoreInit = TRUE)
    
    output$coast_load_text <- renderText(coast_load())
    
    observeEvent(ll_vals$coastline, {
      if (isTruthy(ll_vals$coastline)) {
        map.coastline <- ll_vals$coastline
        x <- map.coastline$lon[!is.na(map.coastline$lon)]
        y <- map.coastline$lat[!is.na(map.coastline$lat)]
        validate(
          need(length(x) == length(y),
              "Coastline lon and lat columns have difference number of non-NA values"),
          need(all(x >= -180) & all(x < 0),
              paste("CruzPlot can currently only handle coastline data with a", 
                    "longitude range of -180 to 0 degrees"))
        )

        # Update inputs
        updateNumericInput(session, "lon_left", value = min(x))
        updateNumericInput(session, "lon_right", value = max(x))
        updateNumericInput(session, "lat_bot", value = min(y))
        updateNumericInput(session, "lat_top", value = max(y))

        # Update reactiveValues
        ll_vals$lon_left <- min(x)
        ll_vals$lon_right <- max(x)
        ll_vals$lat_bot <- min(y)
        ll_vals$lat_top <- max(y)
      }
    })

    cruzMapCoastline <- reactive({
      if (input$coast) {
        validate(need(ll_vals$coastline, "Please load a valid coastline file"))
        ll_vals$coastline
      } else {
        NULL
      }
    })


    # output$map_range_message <- renderUI({
    #   HTML(req(map.range.message()))
    # })

    ###############################################################################
    ### Prepare values to save app state
    to_save <- reactive({
      list(
        save_widget("lon_left", "numeric"),
        save_widget("lon_right", "numeric"),
        save_widget("lat_bot", "numeric"),
        save_widget("lat_top", "numeric"),
        save_widget("resolution", "select"),
        save_widget("lon_left", "reactive", ll_vals$lon_left),
        save_widget("lon_right", "reactive", ll_vals$lon_right),
        save_widget("lat_bot", "reactive", ll_vals$lat_bot),
        save_widget("lat_top", "reactive", ll_vals$lat_top), 
        save_widget("coast", "check"),
        save_widget("coastline", "reactive", ll_vals$coastline)
      )
    })

    ### Return values
    list(
      to_save = to_save,
      config = cruzMapRange, 
      coastline = cruzMapCoastline
    )
  })
}
