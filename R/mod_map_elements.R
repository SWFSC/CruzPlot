#' Map elements
#'
#' Shiny module for map elements: scale bar, ticks, 
#' grid lines, title, and axis labels
#'
#' @name mod_map_elements
#'
#' @inheritParams mod_map_color
#'
#' @details
#' This modukle handles the map elements of scale bar, ticks and their labels, 
#' grid lines, map title, and axis labels. It uses the map range values
#' to generate default values, and validate tick and scale bar parameters
#'
#' @returns 
#' The UI function returns a [shiny::tagList()] object, 
#' which contains two [shiny::tabPanel()] objects. 
#' These objects are for map elements and labels, respectively
#' 
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file. 
#'   See [cruzplot_gui()] for more info. 
#' - `tick_list`: a list of the various reactives needed for plotting tick marks and labels
#' - `scale_bar_list`: a list of the various reactives needed for plotting the scale bar
#' - `grid_list`: a list of the various reactives needed for plotting grid lines
#' - `label_list`: a list of the various reactives needed for plotting map label
#'   (title and axis labels)
#'
#' @export
mod_map_elements_ui <- function(id) {
  ns <- NS(id)
  start.tick <- list(interval = 5, lon = -135, lat = 30)

  tagList(
    tabPanel(
      title = "Elements",
      fluidRow(
        # Scale bar
        cruz_box(
          title = "Scale bar", width = 12, 
          checkboxInput(ns("bar"), "Plot scale bar", value = FALSE),
          conditionalPanel(
            condition = "input.bar", ns = ns,
            helpText(
              "Provide the coordinates for the left edge of the scale bar.",
              "The coordinates must be within the map range coordinates.",
              "To automatically reset the scale bar for new map range coordiantes,",
              "check and uncheck the scale bar checkbox. "
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
        cruz_box(
          title = "Ticks & Labels", width = 12, 
          checkboxInput(ns("tick"), label = "Plot tick marks and/or their labels", value = TRUE),
          conditionalPanel(
            condition = "input.tick", ns = ns,
            fluidRow(
              box(
                title = "Tick Marks", solidHeader = FALSE, width = 6, collapsible = TRUE,
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
                title = "Labels", solidHeader = FALSE, width = 6, collapsible = TRUE,
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
        # Gridlines
        cruz_box(
          title = "Grid", width = 12, 
          checkboxInput(ns("grid"), label = "Include grid lines at major tick marks", value = TRUE),
          conditionalPanel(
            condition = "input.grid", ns = ns,
            fluidRow(
              column(3, selectInput(ns("grid_lty"), label = tags$h5("Line type"),
                                    choices = cruz.line.type, selected = 1)), 
              column(3, selectInput(ns("grid_col"), label = tags$h5("Line color"),
                                    choices = cruz.palette.color, selected = "black")),
              column(3, numericInput(ns("grid_lwd"), label = tags$h5("Line width"),
                                    value = 1, min = 1, max = 6, step = 1))              
            )
          )
        )
      )
    ), 
    # Map Labels
    tabPanel(
      title = "Labels",
      fluidRow(
        cruz_box(
          title = "Title", width = 6, 
          textInput(ns("label_title"), tags$h5("Map title"), value = ""),
          fluidRow(
            column(6, selectInput(ns("label_title_font"), label = tags$h5("Title font"), choices = font.family, selected = 1)),
            column(6, numericInput(ns("label_title_size"), label = tags$h5("Title size"), value = 1.5, min = 0.1, max = 3, step = 0.1))
          )
        ),
        cruz_box(
          title = "Axis labels", width = 6, 
          textInput(ns("label_axis_lon"), tags$h5("Longitude axis label"), value = ""),
          textInput(ns("label_axis_lat"), tags$h5("Latitude axis label"), value = ""),
          fluidRow(
            column(6, selectInput(ns("label_axis_font"), label = tags$h5("Axis label font"), choices = font.family, selected = 1)),
            column(6, numericInput(ns("label_axis_size"), label = tags$h5("Axis label size"), value = 1.2, min = 0.1, max = 3, step = 0.1))
          )
        )
      )
    )
  )
}


#' @name mod_map_elements
#' @export
mod_map_elements_server  <- function(id, load_state, map_range_config) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(map_range_config)
    )

    # Load map_elements state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          # # Only reactive values are scale bar
          cruz.scale[[item$id]] <- item$value
          # stop("Invalid map_elements state - please report as an issue")
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10) #, ignoreInit = TRUE)


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
    #   update: major tick interval, start of longitude and latitude tick labels
    #   cruzMapTickLonBool() returns boolean list of whether bottom
    #     and top tick marks and tick labels are drawn, respectively
    #   cruzMapTickLatBool() returns boolean list of whether left
    #     and right tick marks and tick labels are drawn, respectively
    #   cruzMapTickLon() returns labels for longitude tick marks
    #   cruzMapTickLat() returns labels for latitude tick marks
    #   cruzMapTickParam() returns list of tick length, font, and scale

    ###############################################################################
    #  Return list of longitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLon <- reactive({
      lon.range <- req(map_range_config()$lon.range)
      lon.start <- cruz.tick$label.lon.start
      tick.maj <- cruz.tick$tick.interval.major
      tick.min <- input$tick_interval_minor

      world2 <- map_range_config()$world2

      validate(
        need(cruz.tick$tick.interval.major, "Please enter a valid major tick interval value"),
        need(input$tick_interval_minor, "Please enter a valid minor tick interval value"),
        need(cruz.tick$tick.interval.major > 0, "Please enter a major tick interval value greater than zero"),
        need(input$tick_interval_minor >= 0, "Please enter a minor tick interval value greater than or equal to zero"),
        need(lon.start, "Please enter a valid longitude tick label start value"),
      )

      if (world2) {
        lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)
      }

      # Check that actual longitude values are valid given map rnages
      if (!world2) {
        validate(
          need(lon.range[1] <= as.numeric(lon.start),
               message = "Start of longitude tick labels must be after left longitude value"),
          need(lon.range[2] >= as.numeric(lon.start),
               message = "Start of longitude tick labels must be before right longitude value")
        )
      } else { #world2
        validate(
          need(as.numeric(lon.start) != 0,
               message = "Please use '180' rather than '0' for the start of longitude tick labels")
        )
        if (as.numeric(lon.start) < 0)
        {
          validate(
            need((as.numeric(lon.start) + 180) <= lon.range[2],
                 message = "Start of longitude tick labels must be before right longitude value")
          )
        }
        if (as.numeric(lon.start) > 0)
        {
          validate(
            need(lon.range[1] <= (as.numeric(lon.start)),
                 message = "Start of longitude tick labels must be after left longitude value")
          )
        }
      }

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
      lat.range <- req(map_range_config()$lat.range)
      tick.maj <- req(cruz.tick$tick.interval.major)
      tick.min <- input$tick_interval_minor
      lat.start <- cruz.tick$label.lat.start

      # Check that actual latitude values are valid given map ranges
      validate(
        need(cruz.tick$label.lat.start, "Please enter a valid latitude tick label start value"),
      )
      validate(
        need(lat.range[1] <= cruz.tick$label.lat.start,
             message = "Start of latitude tick labels must be greater than bottom latitude value"),
        need(lat.range[2] >= cruz.tick$label.lat.start,
             message = "Start of latitude tick labels must be less than top latitude value")
      )

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
      if (!isTRUE(all.equal(cruz.tick$tick.interval.major, input$tick_interval_major))) {
        cruz.tick$tick.interval.major <- input$tick_interval_major
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$label_lon_start, {
      # if (req(cruz.tick$label.lon.start) != input$label_lon_start){
      if (!isTRUE(all.equal(cruz.tick$label.lon.start, input$label_lon_start))) {
        cruz.tick$label.lon.start <-input$label_lon_start
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$label_lat_start, {
      # if (req(cruz.tick$label.lat.start) != input$label_lat_start) {
      if (!isTRUE(all.equal(cruz.tick$label.lat.start, input$label_lat_start))) {
        cruz.tick$label.lat.start <- input$label_lat_start
      }
    }, ignoreNULL = TRUE)

    ###############################################################################
    # Update inputs and reactiveValues cruz.tick when map ranges change

    # Tick major interval
    observe({
      lon.range <- req(map_range_config()$lon.range)
      lat.range <- req(map_range_config()$lat.range)
      tick.val <- cruzTickUpdate(lon.range, lat.range)

      updateNumericInput(session, "tick_interval_major", value = tick.val)
      cruz.tick$tick.interval.major <- tick.val
    }, priority = 2)

    # Tick label longitude start
    observe({
      b <- req(cruz.tick$tick.interval.major)
      if (b != 0 && !is.na(b)) {
        lon.range <- req(map_range_config()$lon.range)
        lon.start <- cruzTickStart(lon.range, b)

        updateNumericInput(session, "label_lon_start", value = lon.start)
        cruz.tick$label.lon.start <- lon.start
      }
    }, priority = 1)

    # Tick label latitude start
    observe({
      b <- req(cruz.tick$tick.interval.major)
      if (b != 0 && !is.na(b)) {
        lat.range <- req(map_range_config()$lat.range)
        lat.start <- cruzTickStart(lat.range, b)

        updateNumericInput(session, "label_lat_start", value = lat.start)
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

    # Tick parameters: length, font, size)
    cruzMapTickParam <- reactive({
      tick.len <- input$tick_length
      lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
      lab.scale <- input$label_tick_size

      validate(
        need(input$label_tick_size, "Please enter a valid tick label size value"),
        need(input$label_tick_size >= 0, "Please enter a tick label size value greater than or equal to zero"),
        need(input$tick_length, "Please enter a valid tick length value"),
        need(input$tick_length >= 0, "Please enter a tick length value greater than or equal to zero")
      )

      list(len = tick.len, font = lab.font, scale = lab.scale)
    })

    # Logitude tick marks and labels
    cruzMapTickLon <- reactive({
      tick.lon <- cruzMapIntervalLon()

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


    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    ### Scale bar

    # Processing for Scale bar section
    #   update: scale bar longitude, length, and latitude
    #   cruzMapScaleBar() returns a list of the scale bar coordinates and params

    # If these are NULL to start, then the scale bar must start off
    cruz.scale <- reactiveValues(
      scale.lon = NULL,
      scale.lat = NULL,
      scale.len = NULL
    )


    ###############################################################################
    ### Update reactiveValues cruz.scale if inputs change and are different
    observeEvent(input$scale_lon, {
      if (!isTRUE(all.equal(cruz.scale$scale.lon, input$scale_lon))) {
        cruz.scale$scale.lon <- input$scale_lon
      }
    })

    observeEvent(input$scale_lat, {
      if (!isTRUE(all.equal(cruz.scale$scale.lat, input$scale_lat))) {
        cruz.scale$scale.lat <- input$scale_lat
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$scale_len, {
      # if (cruz.scale$scale.len != input$scale_len) {
      if (!isTRUE(all.equal(cruz.scale$scale.len, input$scale_len))) {
        cruz.scale$scale.len <- input$scale_len
      }
    }, ignoreNULL = TRUE)


    ###############################################################################
    # Calculate new default scale bar lon/lat/len if map dimensions change
    output$scale_lon_uiOut_numeric <- renderUI({
      numericInput(
        session$ns("scale_lon"),
        tags$h5("Longitude"),
        value = cruz.scale$scale.lon
      )
    })

    output$scale_lat_uiOut_numeric <- renderUI({
      numericInput(
        session$ns("scale_lat"),
        tags$h5("Latitude"),
        value = cruz.scale$scale.lat
      )
    })

    observeEvent(input$scale_units, {
      cruz.scale$scale.len <- if (input$scale_units == 1) {
        base::signif(cruz.scale$scale.len * 1.852, 2)
      } else {
        base::signif(cruz.scale$scale.len / 1.852, 2)
      }
    })

    output$out_scale_len <- renderUI({
      title.new <- ifelse(input$scale_units == 1, "Length in km", "Length in nmi")
      numericInput(
        session$ns("scale_len"),
        tags$h5(title.new),
        value = cruz.scale$scale.len
      )
    })


    ### Calculate scale bar default start position if map range changes
    observe({
      lon.range <- req(map_range_config()$lon.range)
      lat.range <- req(map_range_config()$lat.range)

      isolate({
        x <- cruz.scale$scale.lon
        y <- cruz.scale$scale.lat

        if (!isTruthy(x) | !isTruthy(y)) {
          bar.in.range <- TRUE
        } else {
          x <- ifelse(map_range_config()$world2, x + 360, x)
          bar.in.range <- between(x, lon.range[1], lon.range[2]) &
            between(y, lat.range[1], lat.range[2])
        }

        # If scale bar is off or bar is out of current map range
        if (!input$bar | !bar.in.range) {
          lon.diff <- abs(lon.range[2] - lon.range[1])
          lat.diff <- abs(lat.range[2] - lat.range[1])

          # Scale bar longitude start
          lon.new <- 0.1 * lon.diff + lon.range[1]
          lon.new <- ifelse(lon.new > 180, lon.new - 360, lon.new)

          # Scale bar latitude start
          lat.new <- 0.1 * lat.diff + lat.range[1]

          # Set reactiveValues
          cruz.scale$scale.lon <- lon.new
          cruz.scale$scale.lat <- lat.new
        }
      })
    }, priority = 1) #Must run before observe() for bar length

    ### After getting start position, get the default scale bar length
    ###   Separate observe() so that lat/lon update isn't run if scale units change
    ###   Only update length if scale bar is not alrady on
    observe({
      if (!input$bar) {
        lon.range <- req(map_range_config()$lon.range)
        req(map_range_config()$lat.range)
        isolate({
          lon.pos <- cruz.scale$scale.lon
          lat.pos <- cruz.scale$scale.lat
          scale.units <- input$scale_units
        })

        # Scale bar length; suppressWarnings() for if world2
        lon.range.m <- suppressWarnings(geosphere::distVincentyEllipsoid(
          c(lon.range[1], lat.pos), c(lon.range[2], lat.pos)
        ))
        len.new.km <- lon.range.m * 0.2 / 1000

        # Scale units
        if (scale.units == 1) {
          len.new <- base::signif(len.new.km, 2)
          title.new <- "Length in km"
        } else if (scale.units == 2) {
          # nmi, 1nmi = 1.852km
          len.new <- base::signif((len.new.km / 1.852), 2)
          title.new <- "Length in nmi"
        } else {
          stop("Invalid scale units")
        }

        cruz.scale$scale.len <- len.new
      }
    })


    ###############################################################################
    ### Put all scale bar values in list for plotting
    cruzMapScaleBar <- reactive({
      isolate({
        req(is.logical(map_range_config()$world2))
        world2 <- map_range_config()$world2
      })

      validate(
        need(cruz.scale$scale.lon, "Please provide a valid scale bar longitude value"),
        need(cruz.scale$scale.lat, "Please provide a valid scale bar latitude value"),
        need(input$scale_width, "Please provide a valid scale bar width value"),
        need(cruz.scale$scale.len, "Please provide a valid scale bar length value"),
      )

      scale.lon <- cruz.scale$scale.lon
      scale.lat <- cruz.scale$scale.lat
      scale.len <- cruz.scale$scale.len
      scale.lwd <- input$scale_width
      scale.units <- input$scale_units

      scale.units.str <- ifelse(scale.units == 1, "km", "nmi")

      # Determine length of scale bar in meters
      scale.len.m <- if (scale.units == 1) {
        scale.len * 1000
      } else if (scale.units == 2) {
        scale.len * 1.852 * 1000
      }

      scale.x1 <- ifelse((world2 && scale.lon < 0), scale.lon + 360, scale.lon)
      scale.x2 <- geosphere::destPoint(c(scale.lon, scale.lat), 90, scale.len.m)[1]
      scale.x2 <- ifelse((world2 && scale.x2 < 0), scale.x2 + 360, scale.x2)

      scale.y <- scale.lat

      scale.bar <- list(
        x1 = scale.x1,
        x2 = scale.x2,
        y = scale.y,
        lwd = scale.lwd,
        len = scale.len,
        units.str = scale.units.str
      )

      # Validate
      lon.range <- map_range_config()$lon.range
      lat.range <- map_range_config()$lat.range

      validate(
        need(lon.range[1] <= scale.bar$x1,
             "Start of scale bar must be after left longitude value"),
        need(lon.range[2] >= scale.bar$x2,
             paste("End of scale bar must be before right longitude value -",
                   "please extend the map range or decrease the scale bar length")),
        need(lat.range[1] <= scale.bar$y,
             "Scale bar latitude must be greater than bottom latitude value"),
        need(lat.range[2] >= scale.bar$y,
             "Scale bar latitude must be less than top latitude value")
      )

      scale.bar
    })


    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    # Map labels
    # Return title label, font, and size
    cruzMapLabelTitle <- reactive({
      lab <- input$label_title
      fam <- font.family.vals[as.numeric(input$label_title_font)]
      cex <- input$label_title_size
      
      validate(
        need(!is.na(input$label_title_size), "Please enter a valid title size value"), 
        need(input$label_title_size > 0, "Please enter a title size greater than zero")
      )

      list(lab = lab, fam = fam, cex = cex)
    })

    #	Return axes labels (lon and lat), font, and size
    cruzMapLabelAxes <- reactive({
      lab.lon <- input$label_axis_lon
      lab.lat <- input$label_axis_lat
      fam <- font.family.vals[as.numeric(input$label_axis_font)]
      cex <- input$label_axis_size

      validate(
        need(!is.na(input$label_axis_size), "Please enter a valid axis label size value"), 
        need(input$label_axis_size > 0, "Please enter an axis label size greater than zero")
      )

      list(lab.lon = lab.lon, lab.lat = lab.lat, fam = fam, cex = cex)
    })


    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    #--------------------------------------------------------------------------
    # Prepare values to save app state
    to_save <- reactive({
      # Only save bar reactive values if bar is on
      bar.tosave <- if (input$bar) {
        list(          
          save_widget("scale_lon", "numeric"),
          save_widget("scale_lat", "numeric"),
          save_widget("scale_len", "numeric"),
          save_widget("scale.lon", "reactive", cruz.scale$scale.lon),
          save_widget("scale.lat", "reactive", cruz.scale$scale.lat),
          save_widget("scale.len", "reactive", cruz.scale$scale.len)
        )
      } else {
        NULL
      }

      y <- c(bar.tosave, list(
        save_widget("label_lat_start", "numeric"),
        save_widget("label_lon_start", "numeric"),
        save_widget("label_tick_font", "select"),
        save_widget("label_tick_size", "numeric"),
        save_widget("bar", "check"),
        save_widget("scale_width", "numeric"), 
        save_widget("scale_units", "radio"),
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
        save_widget("tick_style", "select"), 
        save_widget("label_title", "text"), 
        save_widget("label_title_font", "select"), 
        save_widget("label_title_size", "numeric"), 
        save_widget("label_axis_lon", "text"), 
        save_widget("label_axis_lat", "text"), 
        save_widget("label_axis_font", "select"), 
        save_widget("label_axis_size", "numeric"), 
        save_widget("grid", "check"),
        save_widget("grid_col", "select"),
        save_widget("grid_lty", "select"),
        save_widget("grid_lwd", "numeric")
      ))
    })

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
      scale_bar_list = list(
        bar = reactive(input$bar),
        cruzMapScaleBar = cruzMapScaleBar
      ),
      grid_list = list(
        grid = reactive(input$grid),
        cruzMapGrid = cruzMapGrid
      ), 
      label_list = list(
        cruzMapLabelTitle = cruzMapLabelTitle, 
        cruzMapLabelAxes = cruzMapLabelAxes
      )
    )
  })
}
