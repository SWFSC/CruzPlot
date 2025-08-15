#' PLot module
#'
#' Shiny module for creating the crzplot plot
#'
#' @name mod_plot
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param enable_brush boolean indicating if the plotOutput should include
#'   `brush = ns("map_brush")`
#' @param height reactive representing the height of the plot, in pixels.
#'   Passed directly to [shiny::renderPlot()]
#' @param cruz.map.range reactiveValues object; intended to be the
#'   first element of the list output of [mod_map_range_server()]
#' @param res Plot resolution. Passed directly to [shiny::renderPlot()]
#'
#' @details
#' Additional details...
#'
#' @returns
#' `mod_plot_ui` returns the plot UI, here simply a [shiny::plotOutput()] object
#' 
#' `mod_plot_server` returns a named list:
#' * 'brush': a reactive of `input$map_brush`. 
#'   If `enable_brush` is `FALSE`, then this value will be `NULL`
#'
#' @export
mod_plot_ui <- function(id, enable_brush = FALSE) {
  ns <- NS(id)

  tagList(
    if (enable_brush) {
      plotOutput(ns("plot1"), height = "auto", brush = ns("map_brush"))
    } else {
      plotOutput(ns("plot1"), height = "auto")
    }
  )
}


#' @name mod_plot
#' @export
mod_plot_server  <- function(
    id,
    height,
    cruz.map.range,
    res = 72
) {
  moduleServer(id, function(input, output, session) {
    plotMap <- reactive({
      # function() {
      # The on.exit call causes the coordinates, eg from a click or brush event,
      #   to not be scaled to the data space, aka their range is 0-1.
      #   This is ok because the only par calls in CruzPlot are around legend
      #   calls for the sake of the font family.
      # oldpar <- par(no.readonly = TRUE)
      # on.exit(par(oldpar))

      lon.range <- req(cruz.map.range$lon.range)
      lat.range <- req(cruz.map.range$lat.range)
      world2 <- cruz.map.range$world2
      stopifnot("world2 param is not a logical" = inherits(world2, "logical"))

      vals.bad <- c("", "-", "+", NA)
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

      map.name <- cruz.map.range$map.name
      map(map.name[[1]], regions = map.name[[2]],
          xlim = lon.range[1:2], ylim = lat.range[1:2],
          fill = TRUE, col = "yellow",
          # add = TRUE
      )
      # }
    })

    output$plot1 <- renderPlot({
      # plotMap()()
      plotMap()
    }, height = height, units = "px", res = res)


    ### Return
    list(
      brush = reactive(input$map_brush)
    )
  })
}
