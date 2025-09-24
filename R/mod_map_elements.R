#' Map elements module
#'
#' Shiny module for map elements
#'
#' @name mod_map_elements
#'
#' @inheritParams mod_map_range
#' @inheritParams mod_plot
#'
#' @details
#' Additional details...
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#' The server function returns a named list, as follows:
#' - 
#'
#' @export
mod_map_elements_ui <- function(id) {
  ns <- NS(id)
  start.tick <- list(interval = 5, lon = -135, lat = 30)

  # tagList(
  tabPanel(
    title = "Elements",
    fluidRow(
      # # Scale bar
      # box(
      #   title = "Scale bar", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
      #   checkboxInput(ns("bar"), "Plot scale bar", value = TRUE),
      #   conditionalPanel(
      #     condition = "input.bar", ns = ns,
      #     helpText(
      #       "Provide the coordinates for the left edge of the scale bar.",
      #       "The coordinates must have the same range as the map range coordinates."
      #     ),
      #     fluidRow(
      #       column(4, uiOutput(ns("scale_lon_uiOut_numeric"))),
      #       column(4, uiOutput(ns("scale_lat_uiOut_numeric"))),
      #       column(4, numericInput(ns("scale_width"), tags$h5("Width of bar"), value = 2, min = 1, max = 6, step = 1)),
      #     ),
      #     fluidRow(
      #       column(
      #         width = 4,
      #         radioButtons(
      #           ns("scale_units"), tags$h5("Scale bar units"),
      #           choices = list("Kilometers" = 1, "Nautical miles" = 2),
      #           selected = 2
      #         )
      #       ),
      #       column(4, uiOutput(ns("out_scale_len")))
      #     )
      #   )
      # ),
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
        checkboxInput(ns("grid"), label = "Include grid lines at major tick marks", value = TRUE),
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
mod_map_elements_server  <- function(id, load_state, map_range) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(map_range)
    )

    # TODO
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          stop("Invalid map_elements state - please report as an issue")
        } else {
          update_widget(item, session)
          # switch(
          #   item$type,
          #   "text" = updateTextInput(session, item$id, value = item$value),
          #   "numeric" = updateNumericInput(session, item$id, value = item$value),
          #   "select" = updateSelectInput(session, item$id, selected = item$value),
          #   "check" = updateCheckboxInput(session, item$id, value = item$value)
          # )
        }
      }
    }, priority = 1) #, ignoreInit = TRUE)

    # # Reactively save all input values to app_state
    # # Can't use reactiveValuesToList(input), because it triggers too soon
    # observe({
    #   app_state$bar <- input$bar
    #   app_state$grid <- input$grid
    #   app_state$grid_col <- input$grid_col
    #   app_state$grid_lty <- input$grid_lty
    #   app_state$grid_lwd <- input$grid_lwd
    #   app_state$label_lat_start <- input$label_lat_start
    #   app_state$label_lon_start <- input$label_lon_start
    #   app_state$label_tick_font <- input$label_tick_font
    #   app_state$label_tick_size <- input$label_tick_size
    #   app_state$scale_units <- input$scale_units
    #   app_state$scale_lon <- input$scale_lon
    #   app_state$scale_lat <- input$scale_lat
    #   app_state$scale_len <- input$scale_len
    #   app_state$scale_width <- input$scale_width
    #   app_state$tick <- input$tick
    #   app_state$tick_bot <- input$tick_bot
    #   app_state$tick_bot_lab <- input$tick_bot_lab
    #   app_state$tick_interval_major <- input$tick_interval_major
    #   app_state$tick_interval_minor <- input$tick_interval_minor
    #   app_state$tick_left <- input$tick_left
    #   app_state$tick_left_lab <- input$tick_left_lab
    #   app_state$tick_length <- input$tick_length
    #   app_state$tick_right <- input$tick_right
    #   app_state$tick_right_lab <- input$tick_right_lab
    #   app_state$tick_style <- input$tick_style
    #   app_state$tick_top <- input$tick_top
    #   app_state$tick_top_lab <- input$tick_top_lab
    # }, priority = 10)
    #
    # # When app_state changes, aka env loaded, update widgets
    # observeEvent(reactiveValuesToList(app_state), {
    #   input.list.names <- names(reactiveValuesToList(input))
    #
    #   input.checkbox <- c(
    #     "bar", "tick", "grid",
    #     "tick_left", "tick_right", "tick_bot", "tick_top",
    #     "tick_left_lab", "tick_right_lab", "tick_bot_lab", "tick_top_lab"
    #   )
    #   input.numeric <- c(
    #     "label_lat_start", "label_lon_start","label_tick_size",
    #     "tick_interval_major", "tick_interval_minor", "tick_length",
    #     "grid_lwd",
    #     "scale_lon", "scale_lat", "scale_width", "scale_len"
    #   )
    #   input.select <- c(
    #     "grid_col", "grid_lty", "label_tick_font", "tick_style"
    #   )
    #   input.radio <- c("scale_units")
    #
    #   for (i in input.list.names) {
    #     if (i %in% input.checkbox) {
    #       # browser()
    #       updateCheckboxInput(session, i, value = app_state[[i]])
    #     } else if (i %in% input.numeric) {
    #       # if (i == "tick_interval_major") browser()
    #       updateNumericInput(session, i, value = app_state[[i]])
    #     } else if (i %in% input.select) {
    #       updateSelectInput(session, i, selected = app_state[[i]])
    #     } else if (i %in% input.radio) {
    #       updateRadioButtons(session, i, selected = app_state[[i]])
    #     } else {
    #       stop("Input value not found:", i)
    #     }
    #   }
    # }, ignoreInit = TRUE, priority = 1)


    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    ### Ticks

    ############################################################################
    cruz.tick <- reactiveValues(
      tick.interval.major = NULL,
      label.lon.start = NULL,
      label.lat.start = NULL
    )

    # Processing for Tick piece
    #   update: major tick interval, start of longitude tick labels, start of latitude tick labels
    #   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
    #   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
    #   cruzMapTickLon() returns labels for longitude tick marks
    #   cruzMapTickLat() returns labels for latitude tick marks
    #   cruzMapTickParam() returns list of tick length, font, and scale


    ###############################################################################
    #  Return list of longitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLon <- reactive({
      lon.range <- req(map_range()$lon.range)
      tick.maj <- req(cruz.tick$tick.interval.major)
      tick.min <- input$tick_interval_minor
      lon.start <- req(cruz.tick$label.lon.start)

      if (map_range()$world2) {
        lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)
      }

      validate(
        need(lon.start <= lon.range[2], "Invalid tick lon start 2"),
        need(lon.start >= lon.range[1], "Invalid tick lon start 1")
      )
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
      lat.range <- req(map_range()$lat.range)
      tick.maj <- req(cruz.tick$tick.interval.major)
      tick.min <- input$tick_interval_minor
      lat.start <- req(cruz.tick$label.lat.start)

      tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
      temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
      tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
      tick.lat$min <- cruzTickMinor(
        deg.range = lat.range, maj.ticks = tick.lat$maj,
        tick.maj.interval = tick.maj, n=tick.min
      )

      tick.lat
    })


    ###############################################################################
    # Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
    #    if inputs change and are different from cruz.tick
    observeEvent(input$tick_interval_major, {
      if (req(cruz.tick$tick.interval.major) != input$tick_interval_major){
        cruz.tick$tick.interval.major <- input$tick_interval_major
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$label_lon_start, {
      if (req(cruz.tick$label.lon.start) != input$label_lon_start){
        cruz.tick$label.lon.start <-input$label_lon_start
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$label_lat_start, {
      if (req(cruz.tick$label.lat.start) != input$label_lat_start) {
        cruz.tick$label.lat.start <- input$label_lat_start
      }
    }, ignoreNULL = TRUE)

    ###############################################################################
    # Update inputs and reactiveValues cruz.tick when map ranges change

    # Tick major interval
    observe({
      lon.range <- req(map_range()$lon.range)
      lat.range <- req(map_range()$lat.range)
      tick.val <- cruzTickUpdate(lon.range, lat.range)

      updateNumericInput(session, "tick_interval_major", value = tick.val)
      # app_state$tick_interval_major <- tick.val
      cruz.tick$tick.interval.major <- tick.val
    }, priority = 2)

    # Tick label longitude start
    observe({
      b <- req(cruz.tick$tick.interval.major)
      if (b != 0 && !is.na(b)) {
        lon.range <- req(map_range()$lon.range)
        lon.start <- cruzTickStart(lon.range, b)

        updateNumericInput(session, "label_lon_start", value = lon.start)
        # app_state$label_lon_start <- lon.start
        cruz.tick$label.lon.start <- lon.start
      }
    }, priority = 1)

    # Tick label latitude start
    observe({
      b <- req(cruz.tick$tick.interval.major)
      if (b != 0 && !is.na(b)) {
        lat.range <- req(map_range()$lat.range)
        lat.start <- cruzTickStart(lat.range, b)

        updateNumericInput(session, "label_lat_start", value = lat.start)
        # app_state$label_lat_start <- lat.start
        cruz.tick$label.lat.start <- lat.start
      }
    }, priority = 1)

    ###############################################################################
    # Reactive functions, which get values for plotting

    # Longitude tick marks, boolean
    cruzMapTickLonBool <- reactive({
      bot <- c(input$tick_bot, input$tick_bot_lab)
      top <- c(input$tick_top, input$tick_top_lab)
      list(bot = bot, top = top)
    })

    # Latitude tick marks, boolean
    cruzMapTickLatBool <- reactive({
      left <- c(input$tick_left, input$tick_left_lab)
      right <- c(input$tick_right, input$tick_right_lab)
      list(left = left, right = right)
    })

    # # Longitude tick labels
    # cruzMapTickLonLab <- reactive({
    #   tick.lab.loc <- cruzMapIntervalLon()$label.loc
    #   format <- input$tick_style
    #
    #   tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    #     i <- ifelse(i > 180, i - 360, i)
    #     i <- ifelse(i < -180, 360 - i, i)
    #     a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    #     b <- ifelse(i < 0, "~W", "~E")
    #     b <- ifelse(a %in% c(0, 180), "", b)
    #     b <- ifelse((format == 2 || format == 4), b, "")
    #     l <- ifelse((format == 3 || format == 4), "*degree", "")
    #     paste(a, l, b, sep = "")
    #   }))
    #
    #   tick.lab
    # })

    # # Latitude tick labels
    # cruzMapTickLatLab <- reactive({
    #   tick.lab.loc <- cruzMapIntervalLat()$label.loc
    #   format <- input$tick_style
    #
    #   tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    #     a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    #     b <- ifelse(i < 0, "~S", "~N")
    #     b <- ifelse(a %in% c(0, 90), "", b)
    #     b <- ifelse((format == 2 || format == 4), b, "")
    #     l <- ifelse((format == 3 || format == 4), "*degree", "")
    #     paste(a, l, b, sep = "")
    #   }))
    #
    #   tick.lab
    # })

    # Tick parameters: length, font, size)
    cruzMapTickParam <- reactive({
      tick.len <- input$tick_length
      lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
      lab.scale <- input$label_tick_size
      list(len = tick.len, font = lab.font, scale = lab.scale)
    })

    # Logitude tick marks and labels
    cruzMapTickLon <- reactive({
      tick.lon <- cruzMapIntervalLon()
      # tick.lon$label <- cruzMapTickLonLab()

      # Labels
      tick.lab.loc <- tick.lon$label.loc
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
      tick.lon$label <- tick.lab

      tick.lon
    })

    # Latitude tick marks and labels
    cruzMapTickLat <- reactive({
      tick.lat <- cruzMapIntervalLat()
      # tick.lat$label <- cruzMapTickLatLab()

      # Labels
      tick.lab.loc <- tick.lat$label.loc
      format <- input$tick_style
      tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
        a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
        b <- ifelse(i < 0, "~S", "~N")
        b <- ifelse(a %in% c(0, 90), "", b)
        b <- ifelse((format == 2 || format == 4), b, "")
        l <- ifelse((format == 3 || format == 4), "*degree", "")
        paste(a, l, b, sep = "")
      }))
      tick.lat$label <- tick.lab

      tick.lat
    })


    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    ### Grid
    # Reactive to return
    cruzMapGrid <- reactive({
      list(
        col = input$grid_col,
        lwd = input$grid_lwd,
        lty = input$grid_lty
      )
    })


    # #--------------------------------------------------------------------------
    # #--------------------------------------------------------------------------
    # #--------------------------------------------------------------------------
    # ### Scale bar
    #
    # # Processing for Scale bar section of Map range tab of Create and Save Map tab
    # #   update: scale bar longitude, length, and latitude
    # #   cruzMapScaleBar() returns a list of the scale bar coordinates and parameters
    # cruz.scale <- reactiveValues(
    #   scale.lon = NULL,
    #   scale.lat = NULL,
    #   scale.len = NULL
    # )
    #
    #
    # ###############################################################################
    # ### Update reactiveValues cruz.scale if inputs change and are different
    # observeEvent(input$scale_lon, {
    #   if (!isTRUE(all.equal(cruz.scale$scale.lon, input$scale_lon))) {
    #     cruz.scale$scale.lon <- input$scale_lon
    #   }
    # })
    #
    # observeEvent(input$scale_lat, {
    #   if (!isTRUE(all.equal(cruz.scale$scale.lat, input$scale_lat))) {
    #     cruz.scale$scale.lat <- input$scale_lat
    #   }
    # }, ignoreNULL = TRUE)
    #
    # observeEvent(input$scale_len, {
    #   if (cruz.scale$scale.len != input$scale_len) {
    #     cruz.scale$scale.len <- input$scale_len
    #   }
    # }, ignoreNULL = TRUE)
    #
    #
    # ###############################################################################
    # # Calculate new default scale bar lon/lat/len if map dimensions change
    # #    Update both reactiveVals and widgets
    #
    # output$scale_lon_uiOut_numeric <- renderUI({
    #   numericInput(
    #     session$ns("scale_lon"),
    #     tags$h5("Longitude"),
    #     value = cruz.scale$scale.lon
    #   )
    # })
    #
    # output$scale_lat_uiOut_numeric <- renderUI({
    #   numericInput(
    #     session$ns("scale_lat"),
    #     tags$h5("Latitude"),
    #     value = cruz.scale$scale.lat
    #   )
    # })
    #
    # observeEvent(input$scale_units, {
    #   cruz.scale$scale.len <- if (input$scale_units == 1) {
    #     base::signif(cruz.scale$scale.len * 1.852, 2)
    #   } else {
    #     base::signif(cruz.scale$scale.len / 1.852, 2)
    #   }
    # })
    #
    # output$out_scale_len <- renderUI({
    #   title.new <- ifelse(input$scale_units == 1, "Length in km", "Length in nmi")
    #   numericInput(
    #     session$ns("scale_len"),
    #     tags$h5(title.new),
    #     value = cruz.scale$scale.len
    #   )
    # })
    #
    # # observe(print(cruz.scale$scale.lon))
    #
    #
    # ### Calculate scale bar default start position if map range changes
    # observe({
    #   # TODO
    #   lon.range <-  req(map_range()$lon.range)
    #   lat.range <- req(map_range()$lat.range)
    #
    #
    #   cruz.scale$scale.lon <- lon.range[1] + 2
    #   cruz.scale$scale.lat <- lat.range[1] + 2
    #
    #   # # browser()
    #   # isolate({
    #   #   print("here0")
    #   #   x <- cruz.scale$scale.lon
    #   #   y <- cruz.scale$scale.lat
    #   #
    #   #   if (!isTruthy(x) | !isTruthy(y)) {
    #   #     bar.in.range <- TRUE
    #   #   } else {
    #   #     x <- ifelse(map_range()$world2, x + 360, x)
    #   #     bar.in.range <- between(x, lon.range[1], lon.range[2]) &
    #   #       between(y, lat.range[1], lat.range[2])
    #   #   }
    #   #
    #   #   # If scale bar is off or bar is out of current map range
    #   #   print("here1")
    #   #   if (!input$bar | !bar.in.range) {
    #   #     lon.diff <- abs(lon.range[2] - lon.range[1])
    #   #     lat.diff <- abs(lat.range[2] - lat.range[1])
    #   #
    #   #     # Scale bar longitude start
    #   #     lon.new <- 0.1 * lon.diff + lon.range[1]
    #   #     lon.new <- ifelse(lon.new > 180, lon.new - 360, lon.new)
    #   #
    #   #     # Scale bar latitude start
    #   #     lat.new <- 0.1 * lat.diff + lat.range[1]
    #   #
    #   #     print("here2")
    #   #     # Set reactiveValues
    #   #     cruz.scale$scale.lon <- lon.new
    #   #     cruz.scale$scale.lat <- lat.new
    #   #   }
    #   # })
    # }, priority = 1) #Must run before observe() for bar length
    #
    # ### After getting start position, get the default scale bar length
    # ###   Separate observe() so that lat/lon update isn't run if scale units change
    # ###   Only update length if scale bar is not already on
    # observe({
    #   if (!input$bar) {
    #     lon.range <- req(map_range()$lon.range)
    #     req(map_range()$lat.range)
    #     isolate({
    #       lon.pos <- cruz.scale$scale.lon
    #       lat.pos <- cruz.scale$scale.lat
    #       scale.units <- input$scale_units
    #     })
    #
    #     # Scale bar length; suppressWarnings() for if world2
    #     lon.range.m <- suppressWarnings(geosphere::distVincentyEllipsoid(
    #       c(lon.range[1], lat.pos), c(lon.range[2], lat.pos)
    #     ))
    #     len.new.km <- lon.range.m * 0.2 / 1000
    #
    #     # Scale units
    #     if (scale.units == 1) {
    #       len.new <- base::signif(len.new.km, 2)
    #       title.new <- "Length in km"
    #     } else if (scale.units == 2) {
    #       # nmi, 1nmi = 1.852km
    #       len.new <- base::signif((len.new.km / 1.852), 2)
    #       title.new <- "Length in nmi"
    #     }
    #
    #     cruz.scale$scale.len <- len.new
    #   }
    # })
    #
    #
    # ###############################################################################
    # ### Put all scale bar values in list for plotting
    # cruzMapScaleBar <- reactive({
    #   isolate({
    #     req(is.logical(map_range()$world2))
    #     world2 <- map_range()$world2
    #   })
    #   scale.lon <- req(cruz.scale$scale.lon)
    #   scale.lat <- req(cruz.scale$scale.lat)
    #   scale.len <- req(cruz.scale$scale.len)
    #   scale.lwd <- input$scale_width
    #   scale.units <- input$scale_units
    #
    #   scale.units.str <- ifelse(scale.units == 1, "km", "nmi")
    #
    #   # Determine length of scale bar in meters
    #   scale.len.m <- if (scale.units == 1) {
    #     scale.len * 1000
    #   } else if (scale.units == 2) {
    #     scale.len * 1.852 * 1000
    #   }
    #
    #   scale.x1 <- ifelse((world2 && scale.lon < 0), scale.lon + 360, scale.lon)
    #   scale.x2 <- geosphere::destPoint(c(scale.lon, scale.lat), 90, scale.len.m)[1]
    #   scale.x2 <- ifelse((world2 && scale.x2 < 0), scale.x2 + 360, scale.x2)
    #
    #   scale.y <- scale.lat
    #
    #   list(
    #     x1 = scale.x1, x2 = scale.x2, y = scale.y,
    #     lwd = scale.lwd, len = scale.len, units.str = scale.units.str
    #   )
    # })

    #--------------------------------------------------------------------------
    to_save <- reactive({
      list(
        save_widget("grid", "check"),
        save_widget("grid_col", "select"),
        save_widget("grid_lty", "select"),
        save_widget("grid_lwd", "numeric"),
        save_widget("label_lat_start", "numeric"),
        save_widget("label_lon_start", "numeric"),
        save_widget("label_tick_font", "select"),
        save_widget("label_tick_size", "numeric"),
        # save_widget("bar", "checkbox"),
        # save_widget("scale_units", "reactive"),
        # save_widget("scale_lon", "reactive"),
        # save_widget("scale_lat", "reactive"),
        # save_widget("scale_len", "reactive"),
        # save_widget("scale_width", "reactive"),
        save_widget("tick", "check"),
        save_widget("tick_bot", "check"),
        save_widget("tick_bot_lab", "check"),
        save_widget("tick_top", "check"),
        save_widget("tick_top_lab", "check"),
        save_widget("tick_left", "check"),
        save_widget("tick_left_lab", "check"),
        save_widget("tick_right", "check"),
        save_widget("tick_right_lab", "check"),
        save_widget("tick_interval_major", "numeric"),
        save_widget("tick_interval_minor", "numeric"),
        save_widget("tick_length", "numeric"),
        save_widget("tick_style", "select")
      )
    })



    #--------------------------------------------------------------------------
    ### Return values
    list(
      to_save = to_save,
      tick_list = list(
        tick = reactive(input$tick),
        cruzMapTickLonBool = cruzMapTickLonBool,
        cruzMapTickLatBool = cruzMapTickLatBool,
        cruzMapTickLon = cruzMapTickLon,
        cruzMapTickLat = cruzMapTickLat,
        cruzMapTickParam = cruzMapTickParam
      ),
      # scale_bar_list = list(
      #   bar = reactive(input$bar),
      #   cruzMapScaleBar = cruzMapScaleBar
      # ),
      grid_list = list(
        grid = reactive(input$grid),
        cruzMapGrid = cruzMapGrid
      )
    )
  })
}
