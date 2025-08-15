#' Map elements module
#'
#' Shiny module for map elements, beyond the range
#'
#' @name mod_map_elements
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#'
#' @details
#' Additional details...
#'
#' @returns The server function returns a list of...
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
      )
      # Map Labels
      # Gridlines
    )
  )
  # )
}

# TODO:
# Make a map_elements reactive Value, to pass back and forth. 
# ...


#' @name mod_map_elements
#' @export
mod_map_elements_server  <- function(id, cruz.map.range) {
  moduleServer(id, function(input, output, session) {

    ###############################################################################
    ###############################################################################
    # Processing for Tick tab of Create and Save Map tab
    #   update: major tick interval, start of longitude tick labels, start of latitude tick labels
    #   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
    #   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
    #   cruzMapTickLon() returns labels for longitude tick marks
    #   cruzMapTickLat() returns labels for latitude tick marks
    #   cruzMapTickParam() returns list of tick length, font, and scale

    cruz.tick <- reactiveValues(

    )


    ###############################################################################
    #  Return list of longitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLon <- reactive({
      lon.range <- cruz.map.range$lon.range
      tick.maj <- cruz.tick$tick.interval.major
      tick.min <- input$tick_interval_minor
      lon.start <- cruz.tick$label.lon.start

      if (cruz.map.range$world2) lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)

      tick.lon <- list(label.loc = seq(lon.start, lon.range[2], by = tick.maj))
      temp.tick <- rev(seq(lon.start, lon.range[1], by = -tick.maj))
      tick.lon$maj <- sort(unique(c(tick.lon$label.loc, temp.tick)))
      tick.lon$min <- cruzTickMinor(deg.range = lon.range, maj.ticks = tick.lon$maj,
                                    tick.maj.interval = tick.maj, n=tick.min)

      tick.lon
    })

    # Return list of latitude values of major tick marks/grid lines and minor tick marks
    cruzMapIntervalLat <- reactive({
      lat.range <- cruz.map.range$lat.range
      tick.maj <- cruz.tick$tick.interval.major
      tick.min <- input$tick_interval_minor
      lat.start <- cruz.tick$label.lat.start

      tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
      temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
      tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
      tick.lat$min <- cruzTickMinor(deg.range = lat.range, maj.ticks = tick.lat$maj,
                                    tick.maj.interval = tick.maj, n=tick.min)

      tick.lat
    })


    ###############################################################################
    # Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
    #    if inputs change and are different from cruz.tick

    observe({
      req(input$tick_interval_major)

      in.tick.interval.major <- input$tick_interval_major
      isolate({
        if (cruz.tick$tick.interval.major != in.tick.interval.major)
          cruz.tick$tick.interval.major <- in.tick.interval.major
      })
    })

    observe({
      req(input$label_lon_start)

      in.label.lon.start <- as.numeric(input$label_lon_start)
      isolate({
        if (cruz.tick$label.lon.start != in.label.lon.start)
          cruz.tick$label.lon.start <- in.label.lon.start
      })
    })

    observe({
      req(input$label_lat_start)

      in.label.lat.start <- as.numeric(input$label_lat_start)
      isolate({
        if (cruz.tick$label.lat.start != in.label.lat.start)
          cruz.tick$label.lat.start <- in.label.lat.start
      })
    })

    ###############################################################################
    # Update inputs

    # Tick major interval
    observe({
      lon.range <- cruz.map.range$lon.range
      lat.range <- cruz.map.range$lat.range
      tick.val <- cruzTickUpdate(lon.range, lat.range)

      updateNumericInput(session, "tick.interval.major", value = tick.val)
      cruz.tick$tick.interval.major <- tick.val
    }, priority = 2)

    # Tick label longitude start
    observe({
      b <- cruz.tick$tick.interval.major
      if (b != 0 && !is.na(b)) {
        lon.range <- cruz.map.range$lon.range
        lon.start <- cruzTickStart(lon.range, b)

        updateTextInput(session, "label.lon.start", value = paste(lon.start))
        cruz.tick$label.lon.start <- lon.start
      }
    }, priority = 1)

    # Tick label latitude start
    observe({
      b <- cruz.tick$tick.interval.major
      if (b != 0 && !is.na(b)) {
        lat.range <- cruz.map.range$lat.range
        lat.start <- cruzTickStart(lat.range, b)

        updateTextInput(session, "label.lat.start", value = paste(lat.start))
        cruz.tick$label.lat.start <- lat.start
      }
    }, priority = 1)

    ###############################################################################
    # Reactive functions

    # Plot longtiude tick marks
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

    # Plot longtiude tick labels
    cruzMapTickLonLab <- reactive({
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
      tick.len <- input$tick_length
      lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
      lab.scale <- input$label_tick_size
      list(len = tick.len, font = lab.font, scale = lab.scale)
    })
    
    ### Return values
    list()
  })
}
