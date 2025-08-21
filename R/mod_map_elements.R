#' Map elements module
#'
#' Shiny module for map elements, beyond the range
#'
#' @name mod_map_elements
#'
#' @inheritParams mod_map_range
#'
#' @details
#' Additional details...
#'
#' @returns An empty list
#'
#' @export
mod_map_elements_ui <- function(id) {
  ns <- NS(id)
  start.tick <- list(interval = 5, lon = -135, lat = 30)

  # tagList(
  tabPanel(
    title = "Elements",
    fluidRow(
      # Scale bar
      box(
        title = "Scale bar", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
        checkboxInput(ns("bar"), "Plot scale bar", value = FALSE),
        conditionalPanel(
          condition = "input.bar", ns = ns,
          helpText(
            "Provide the coordinates for the left edge of the scale bar.",
            "The coordinates must have the same range as the map range coordinates."
          ),
          fluidRow(
            column(4, uiOutput(ns("scale_lon_uiOut_numeric"))),
            column(4, uiOutput(ns("scale_lat_uiOut_numeric"))),
            column(4, numericInput(ns("scale_width"), tags$h5("Width of bar"), value = 2, min = 1, max = 6, step = 1)),
          ),
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("scale_units"), tags$h5("Scale bar units"),
                choices = list("Kilometers" = 1, "Nautical miles" = 2),
                selected = 2
              )
            ),
            column(4, uiOutput(ns("out_scale_len")))
          )
        )
      ),
      # Ticks & Labels
      box(
        title = "Ticks & Labels", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
        checkboxInput(ns("tick"), label = "Plot tick marks and/or their labels", value = TRUE),
        conditionalPanel(
          condition = "input.tick", ns = ns,
          fluidRow(
            box(
              title = "Tick Marks", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
              fluidRow(
                column(
                  width = 6,
                  checkboxInput(ns("tick_left"), label = "Left", value = TRUE),
                  checkboxInput(ns("tick_bot"), label = "Bottom", value = TRUE),
                  numericInput(
                    ns("tick_interval_major"), label = tags$h5("Degrees between each major tick"),
                    value = start.tick$interval, min = 0, max = 45, step = 5
                  ),
                  selectInput(
                    ns("tick_style"), label = tags$h5("Tick label style"),
                    choices = list("120" = 1, "120W" = 2, "120\u00B0" = 3, "120\u00B0W" = 4),
                    selected = 4
                  )
                ),
                column(
                  width = 6,
                  checkboxInput(ns("tick_right"), label = "Right", value = TRUE),
                  checkboxInput(ns("tick_top"), label = "Top", value = TRUE),
                  numericInput(
                    ns("tick_interval_minor"), label = tags$h5("Minor ticks between each major tick"),
                    value = 4, min = 0, max = 45, step = 1
                  ),
                  numericInput(
                    ns("tick_length"), label = tags$h5("Tick length"),
                    value = 1.0, min = 0, max = 2.5, step = 0.1
                  )
                )
              )
            ),
            box(
              title = "Labels", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
              fluidRow(
                column(
                  width = 6,
                  checkboxInput(ns("tick_left_lab"), "Left", value = TRUE),
                  checkboxInput(ns("tick_bot_lab"), "Bottom", value = TRUE),
                  numericInput(ns("label_lon_start"), tags$h5("Start longitude tick labels at"), value = as.character(start.tick$lon)),
                  selectInput(ns("label_tick_font"), tags$h5("Tick label font"), choices = font.family, selected = 1)
                ),
                column(
                  width = 6,
                  checkboxInput(ns("tick_right_lab"), "Right", value = TRUE),
                  checkboxInput(ns("tick_top_lab"), "Top", value = TRUE),
                  numericInput(ns("label_lat_start"), tags$h5("Start latitude tick labels at"), value = as.character(start.tick$lat)),
                  numericInput(ns("label_tick_size"), tags$h5("Tick label size"), value = 1.0, min = 0.1, max = 3, step = 0.1)
                )
              )
            )
          )
        )
      ),
      # Map Labels
      # Gridlines
      box(
        title = "Grid", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, height = 385,
        checkboxInput(ns("grid"), label = "Include grid lines at major tick marks", value = FALSE),
        conditionalPanel(
          condition = "input.grid", ns = ns,
          fluidRow(
            column(3, selectInput(ns("grid_col"), label = tags$h5("Line color"),
                                  choices = cruz.palette.color, selected = "black")),
            column(3, numericInput(ns("grid_lwd"), label = tags$h5("Line width"),
                                   value = 1, min = 1, max = 6, step = 1)),
            column(3, selectInput(ns("grid_lty"), label = tags$h5("Line type"),
                                  choices = cruz.line.type, selected = 1))
          )
        )
      )
    )
  )
  # )
}

# TODO:
# Make a map_elements reactive Value, to pass back and forth.
# ...


#' @name mod_map_elements
#' @export
mod_map_elements_server  <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    # #--------------------------------------------------------------------------
    # # Processing for Tick tab of Create and Save Map tab
    # #   update: major tick interval, start of longitude tick labels, start of latitude tick labels
    # #   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
    # #   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
    # #   cruzMapTickLon() returns labels for longitude tick marks
    # #   cruzMapTickLat() returns labels for latitude tick marks
    # #   cruzMapTickParam() returns list of tick length, font, and scale

    cruz.tick <- reactiveValues(
      tick_interval_major = NULL,
      label_lon_start = NULL,
      label_lat_start = NULL
    )

    observeEvent(input$tick_interval_major, {
      app_state$tick_interval_major <- input$tick_interval_major
      cruz.tick$tick_interval_major <- input$tick_interval_major
    })

    observeEvent(input$label_lon_start, {
      app_state$label_lon_start <- input$label_lon_start
      cruz.tick$label_lon_start <- input$label_lon_start
    })

    observeEvent(input$label_lat_start, {
      app_state$label_lat_start <- input$label_lat_start
      cruz.tick$label_lat_start <- input$label_lat_start
    })

    observe({
      print("obs")
      app_state$tick <- input$tick
      app_state$tick_left <- input$tick_left
      app_state$tick_right <- input$tick_right
      app_state$tick_bot <- input$tick_bot
      app_state$tick_top <- input$tick_top
      # app_state$tick_interval_major <- input$tick_interval_major
      app_state$tick_style <- input$tick_style
      app_state$tick_interval_minor <- input$tick_interval_minor
      app_state$tick_length <- input$tick_length

      app_state$tick_left_lab <- input$tick_left_lab
      app_state$tick_right_lab <- input$tick_right_lab
      app_state$tick_bot_lab <- input$tick_bot_lab
      app_state$tick_top_lab <- input$tick_top_lab
      # app_state$label_lon_start <- input$label_lon_start
      # app_state$label_lat_start <- input$label_lat_start
      app_state$label_tick_font <- input$label_tick_font
      app_state$label_tick_size <- input$label_tick_size
    })

    # TODO: observeEvent for the different inputs

    ###############################################################################
    #  Return list of longitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLon <- reactive({
      print("cruzMapIntervalLon")
      lon.range <- req(app_state$lon.range)
      lon.start <- req(cruz.tick$label_lon_start)
      tick.maj <- req(cruz.tick$tick_interval_major)
      tick.min <- input$tick_interval_minor

      if (app_state$world2) {
        req(all(lon.range > 0))
        lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)
      }

      req(lon.start <= lon.range[2])
      tick.lon <- list(label.loc = seq(lon.start, lon.range[2], by = tick.maj))
      temp.tick <- rev(seq(lon.start, lon.range[1], by = -tick.maj))
      tick.lon$maj <- sort(unique(c(tick.lon$label.loc, temp.tick)))
      tick.lon$min <- cruzTickMinor(
        deg.range = lon.range, maj.ticks = tick.lon$maj,
        tick.maj.interval = tick.maj, n=tick.min
      )

      tick.lon
    })

    # Return list of latitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLat <- reactive({
      print("cruzMapIntervalLat")
      lat.range <- req(app_state$lat.range)
      tick.maj <- req(cruz.tick$tick_interval_major)
      tick.min <- input$tick_interval_minor
      lat.start <- req(cruz.tick$label_lat_start)

      tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
      temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
      tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
      tick.lat$min <- cruzTickMinor(
        deg.range = lat.range, maj.ticks = tick.lat$maj,
        tick.maj.interval = tick.maj, n=tick.min
      )

      tick.lat
    })


    # ###############################################################################
    # # Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
    # #    if inputs change and are different from cruz.tick

    # observe({
    #   req(input$tick_interval_major)

    #   in.tick.interval.major <- input$tick_interval_major
    #   isolate({
    #     if (cruz.tick$tick.interval.major != in.tick.interval.major)
    #       cruz.tick$tick.interval.major <- in.tick.interval.major
    #   })
    # })

    # observe({
    #   req(input$label_lon_start)

    #   in.label.lon.start <- as.numeric(input$label_lon_start)
    #   isolate({
    #     if (cruz.tick$label.lon.start != in.label.lon.start)
    #       cruz.tick$label.lon.start <- in.label.lon.start
    #   })
    # })

    # observe({
    #   req(input$label_lat_start)

    #   in.label.lat.start <- as.numeric(input$label_lat_start)
    #   isolate({
    #     if (cruz.tick$label.lat.start != in.label.lat.start)
    #       cruz.tick$label.lat.start <- in.label.lat.start
    #   })
    # })

    ###############################################################################
    # Update inputs

    # Tick major interval
    observe({
      print("Tick major interval")
      lon.range <- app_state$lon.range
      lat.range <- app_state$lat.range
      tick.val <- cruzTickUpdate(lon.range, lat.range)

      updateNumericInput(session, "tick_interval_major", value = tick.val)
      app_state$tick_interval_major <- tick.val
      cruz.tick$tick_interval_major <- tick.val
    }, priority = 2)

    # Tick label longitude start
    observe({
      print("label longitude start")
      b <- app_state$tick_interval_major
      if (b != 0 && !is.na(b)) {
        lon.range <- app_state$lon.range
        lon.start <- cruzTickStart(lon.range, b)

        updateTextInput(session, "label_lon_start", value = paste(lon.start))
        app_state$label_lon_start <- lon.start
        cruz.tick$label_lon_start <- lon.start
      }
    }, priority = 1)

    # Tick label latitude start
    observe({
      b <- app_state$tick_interval_major
      if (b != 0 && !is.na(b)) {
        lat.range <- app_state$lat.range
        lat.start <- cruzTickStart(lat.range, b)

        updateTextInput(session, "label_lat_start", value = paste(lat.start))
        app_state$label_lat_start <- lat.start
        cruz.tick$label_lat_start <- lat.start
      }
    }, priority = 1)

    ###############################################################################
    # Reactive functions

    # Plot longitude tick marks
    cruzMapTickLonBool <- reactive({
      bot <- c(input$tick_bot, input$tick_bot_lab)
      top <- c(input$tick_top, input$tick_top_lab)
      list(bot = bot, top = top)
    })

    # Plot latitude tick marks
    cruzMapTickLatBool <- reactive({
      left <- c(input$tick_left, input$tick_left_lab)
      right <- c(input$tick_right, input$tick_right_lab)
      list(left = left, right = right)
    })

    # Plot longitude tick labels
    cruzMapTickLonLab <- reactive({
      print("cruzMapTickLonLab")
      tick.lab.loc <- cruzMapIntervalLon()$label.loc
      format <- input$tick_style

      tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
        i <- ifelse(i > 180, i - 360, i)
        i <- ifelse(i < -180, 360 - i, i)
        a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
        b <- ifelse(i < 0, "~W", "~E")
        b <- ifelse(a %in% c(0, 180), "", b)
        b <- ifelse((format == 2 || format == 4), b, "")
        l <- ifelse((format == 3 || format == 4), "*degree", "")
        paste(a, l, b, sep = "")
      }))

      tick.lab
    })

    # Plot latitude tick labels
    cruzMapTickLatLab <- reactive({
      print("cruzMapTickLatLab")
      tick.lab.loc <- cruzMapIntervalLat()$label.loc
      format <- input$tick_style

      tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
        a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
        b <- ifelse(i < 0, "~S", "~N")
        b <- ifelse(a %in% c(0, 90), "", b)
        b <- ifelse((format == 2 || format == 4), b, "")
        l <- ifelse((format == 3 || format == 4), "*degree", "")
        paste(a, l, b, sep = "")
      }))

      tick.lab
    })

    cruzMapTickParam <- reactive({
      print("cruzMapTickParam")
      tick.len <- input$tick_length
      lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
      lab.scale <- input$label_tick_size
      list(len = tick.len, font = lab.font, scale = lab.scale)
    })


    ###########################################################################
    # Add things to app_state that are needed by plot module
    observe({
      print("app-state")
      app_state$tick.lon.bool <- cruzMapTickLonBool()
      app_state$tick.lat.bool <- cruzMapTickLatBool()

      tick.lon <- cruzMapIntervalLon()
      tick.lon$label <- cruzMapTickLonLab()
      app_state$tick.lon <- tick.lon

      tick.lat <- cruzMapIntervalLat()
      tick.lat$label <- cruzMapTickLatLab()
      app_state$tick.lat <- tick.lat

      app_state$tick.param <- cruzMapTickParam()
    })


    #--------------------------------------------------------------------------
    ### Grid
    # Update app_state with new input values
    observe({
      app_state$grid <- input$grid
      app_state$grid_col <- input$grid_col
      app_state$grid_lwd <- input$grid_lwd
      app_state$grid_lty <- input$grid_lty
    })

    # Update input values if app_state changes, meaning a workspace was loaded
    observeEvent(app_state$grid, {
      updateCheckboxInput(session, "grid", value = app_state$grid)
    })
    observeEvent(app_state$grid_col, {
      updateSelectInput(session, "grid_col", selected = app_state$grid_col)
    })
    observeEvent(app_state$grid_lwd, {
      updateNumericInput(session, "grid_lwd", value = app_state$grid_lwd)
    })
    observeEvent(app_state$grid_lty, {
      updateSelectInput(session, "grid_lty", selected = app_state$grid_lty)
    })


    #--------------------------------------------------------------------------
    ### Scale bar
    # TODO next
    # ###############################################################################
    # ### Update reactiveValues cruz.scale if inputs change and are different
    # observe({
    #   req(input$scale_lon)
    #   isolate({
    #     if (!isTRUE(all.equal(cruz.scale$scale.lon, input$scale_lon)))
    #       cruz.scale$scale.lon <- input$scale_lon
    #   })
    # })

    # observe({
    #   req(input$scale_lat)
    #   isolate({
    #     if (!isTRUE(all.equal(cruz.scale$scale.lat, input$scale_lat)))
    #       cruz.scale$scale.lat <- input$scale_lat
    #   })
    # })

    # observe({
    #   req(input$scale_len)
    #   isolate({
    #     if (cruz.scale$scale.len != input$scale_len)
    #       cruz.scale$scale.len <- input$scale_len
    #   })
    # })


    # ###############################################################################
    # # Calculate new default scale bar lon/lat/len if map dimensions change
    # #    Update both reactiveVals and widgets

    # output$scale_lon_uiOut_numeric <- renderUI({
    #   numericInput("scale_lon", tags$h5("Longitude"), value = cruz.scale$scale.lon)
    # })

    # output$scale_lat_uiOut_numeric <- renderUI({
    #   numericInput("scale_lat", tags$h5("Latitude"), value = cruz.scale$scale.lat)
    # })

    # observeEvent(input$scale_units, {
    #   cruz.scale$scale.len <- if (input$scale_units == 1) {
    #     base::signif(cruz.scale$scale.len * 1.852, 2)
    #   } else {
    #     base::signif(cruz.scale$scale.len / 1.852, 2)
    #   }
    # })

    # output$out_scale_len <- renderUI({
    #   title.new <- ifelse(input$scale_units == 1, "Length in km", "Length in nmi")
    #   numericInput("scale_len", tags$h5(title.new), value = cruz.scale$scale.len)
    # })


    # ### Calculate scale bar default start position if map range changes
    # observe({
    #   lon.range <- cruz.map.range$lon.range
    #   lat.range <- cruz.map.range$lat.range

    #   isolate({
    #     x <- cruz.scale$scale.lon
    #     y <- cruz.scale$scale.lat

    #     if (!isTruthy(x) | !isTruthy(y)) {
    #       bar.in.range <- TRUE
    #     } else {
    #       x <- ifelse(cruz.map.range$world2, x + 360, x)
    #       bar.in.range <- between(x, lon.range[1], lon.range[2]) &
    #         between(y, lat.range[1], lat.range[2])
    #     }

    #     # If scale bar is off or bar is out of current map range
    #     if (!input$bar | !bar.in.range) {
    #       lon.diff <- abs(lon.range[2] - lon.range[1])
    #       lat.diff <- abs(lat.range[2] - lat.range[1])

    #       # Scale bar longitude start
    #       lon.new <- 0.1 * lon.diff + lon.range[1]
    #       lon.new <- ifelse(lon.new > 180, lon.new - 360, lon.new)

    #       # Scale bar latitude start
    #       lat.new <- 0.1 * lat.diff + lat.range[1]

    #       # Set reactiveValues
    #       cruz.scale$scale.lon <- lon.new
    #       cruz.scale$scale.lat <- lat.new
    #     }
    #   })
    # }, priority = 1) #Must run before observe() for bar length

    # ### After getting start position, get the default scale bar length
    # ###   Separate observe() so that lat/lon update isn't run if scale units change
    # ###   Only update length if scale bar is not alrady on
    # observe({
    #   if (!input$bar) {
    #     lon.range <- cruz.map.range$lon.range
    #     cruz.map.range$lat.range
    #     isolate({
    #       lon.pos <- cruz.scale$scale.lon
    #       lat.pos <- cruz.scale$scale.lat
    #       scale.units <- input$scale_units
    #     })

    #     # Scale bar length; suppressWarnings() for if world2
    #     lon.range.m <- suppressWarnings(geosphere::distVincentyEllipsoid(
    #       c(lon.range[1], lat.pos), c(lon.range[2], lat.pos)
    #     ))
    #     len.new.km <- lon.range.m * 0.2 / 1000

    #     # Scale units
    #     if (scale.units == 1) {
    #       len.new <- base::signif(len.new.km, 2)
    #       title.new <- "Length in km"
    #     } else if (scale.units == 2) {
    #       # nmi, 1nmi = 1.852km
    #       len.new <- base::signif((len.new.km / 1.852), 2)
    #       title.new <- "Length in nmi"
    #     }

    #     cruz.scale$scale.len <- len.new
    #   }
    # })


    # ###############################################################################
    # ### Put all scale bar values in list for plotting
    # cruzMapScaleBar <- reactive({
    #   isolate(world2 <- cruz.map.range$world2)
    #   scale.lon <- cruz.scale$scale.lon
    #   scale.lat <- cruz.scale$scale.lat
    #   scale.len <- cruz.scale$scale.len
    #   scale.lwd <- input$scale_width
    #   scale.units <- input$scale_units

    #   scale.units.str <- ifelse(scale.units == 1, "km", "nmi")

    #   # Determine length of scale bar in meters
    #   scale.len.m <- if (scale.units == 1) {
    #     scale.len * 1000
    #   } else if (scale.units == 2) {
    #     scale.len * 1.852 * 1000
    #   }

    #   scale.x1 <- ifelse((world2 && scale.lon < 0), scale.lon + 360, scale.lon)
    #   scale.x2 <- geosphere::destPoint(c(scale.lon, scale.lat), 90, scale.len.m)[1]
    #   scale.x2 <- ifelse((world2 && scale.x2 < 0), scale.x2 + 360, scale.x2)

    #   scale.y <- scale.lat

    #   list(
    #     x1 = scale.x1, x2 = scale.x2, y = scale.y,
    #     lwd = scale.lwd, len = scale.len, units.str = scale.units.str
    #   )
    # })



    #--------------------------------------------------------------------------
    ### Return values
    list()
  })
}
