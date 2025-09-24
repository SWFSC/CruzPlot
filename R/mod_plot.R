#' Plot module
#'
#' Shiny module for creating the crzplot plot
#'
#' @name mod_plot
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param enable_brush boolean indicating if the plotOutput should include
#'   `brush = ns("map_brush")`
#' @param map_range output of [mod_map_range_server()]
#' @param map_elements output of [mod_map_elements_server()]
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
    map_range,
    map_elements
) {
  moduleServer(id, function(input, output, session) {

    ###########################################################################
    plotMap <- reactive({
      # The on.exit call causes the coordinates, eg from a click or brush event,
      #   to not be scaled to the data space, aka their range is 0-1.
      #   This is ok because the only par calls in CruzPlot are around legend
      #   calls for the sake of the font family.
      # oldpar <- par(no.readonly = TRUE)
      # on.exit(par(oldpar))



      #------------------------------------------------------------------------
      ### Map range
      lon.range <- req(map_range()$lon.range)
      lat.range <- req(map_range()$lat.range)
      req(is.logical(map_range()$world2))
      world2 <- map_range()$world2

      map.name <- map_range()$map.name
      map(map.name[[1]], regions = map.name[[2]],
          xlim = lon.range[1:2], ylim = lat.range[1:2],
          fill = TRUE, col = "yellow",
          # add = TRUE
      )

      #------------------------------------------------------------------------
      # Map elements

      ### Tick marks and labels
      tick_list <- map_elements$tick_list

      if (tick_list$tick()) {
        # Assigned inside here, so validate are only triggered when relevant
        tick.lon.bool <- tick_list$cruzMapTickLonBool()
        tick.lat.bool <- tick_list$cruzMapTickLatBool()
        tick.lon <- tick_list$cruzMapTickLon()
        tick.lat <- tick_list$cruzMapTickLat()
        tick.param <- tick_list$cruzMapTickParam()

        # Draw major and minor tick marks
        if (tick.lon.bool$bot[1]) {
          axis(1, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
               family = tick.param$font)
          axis(1, at = tick.lon$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") *0.4*tick.param$len)
        }
        if (tick.lat.bool$left[1]) {
          axis(2, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
               family = tick.param$font)
          axis(2, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") * 0.4*tick.param$len)
        }
        if (tick.lon.bool$top[1]) {
          axis(3, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
               family = tick.param$font)
          axis(3, at = tick.lon$min, labels = FALSE, lwd = 0,  lwd.ticks = 1,
               tcl = par("tcl") * 0.4*tick.param$len)
        }
        if (tick.lat.bool$right[1]) {
          axis(4, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
               family = tick.param$font)
          axis(4, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
               tcl = par("tcl") * 0.4*tick.param$len)
        }

        # Draw tick labels
        if (tick.lon.bool$bot[2])
          axis(1, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
               cex.axis = tick.param$scale, family = tick.param$font)
        if (tick.lat.bool$left[2])
          axis(2, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
               las = 1, cex.axis = tick.param$scale, family = tick.param$font)
        if (tick.lon.bool$top[2])
          axis(3, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
               cex.axis = tick.param$scale, family = tick.param$font)
        if (tick.lat.bool$right[2])
          axis(4, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
               las = 1, cex.axis = tick.param$scale, family = tick.param$font)
      }

      ### Grid
      grid_list <- map_elements$grid_list
      if (grid_list$grid()) {
        grid.param <- grid_list$cruzMapGrid()
        tick.lon <- tick_list$cruzMapTickLon()
        tick.lat <- tick_list$cruzMapTickLat()

        abline(
          v = tick.lon$maj,
          col = grid.param$col,
          lwd = grid.param$lwd,
          lty = as.numeric(grid.param$lty)
        )
        abline(
          h = tick.lat$maj,
          col = grid.param$col,
          lwd = grid.param$lwd,
          lty = as.numeric(grid.param$lty)
        )
      }

      ### Scale bar
      scale_bar_list = map_elements$scale_bar_list
      if (scale_bar_list$bar()) {
        scale.bar <- scale_bar_list$cruzMapScaleBar()

        lines(
          c(scale.bar$x1, scale.bar$x2),
          c(scale.bar$y, scale.bar$y),
          lwd = scale.bar$lwd
        )
        text(
          mean(c(scale.bar$x1, scale.bar$x2)),
          scale.bar$y - 0.04 * abs(lat.range[2]-lat.range[1]),
          paste(scale.bar$len, scale.bar$units.str)
        )
      }


      ### ...
    })

    output$plot1 <- renderPlot({
      plotMap()
    }, height = height, units = "px", res = 72)


    ### Return
    list(
      brush = reactive(input$map_brush)
    )
  })
}
