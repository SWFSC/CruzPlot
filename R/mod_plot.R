#' Plot module
#'
#' Shiny module for creating the CruzPlot map/plot
#'
#' @name mod_plot
#'
#' @param id character used to specify namespace, see [shiny::NS()]
#' @param enable_brush boolean indicating if the plotOutput should include
#'   `brush = ns("map_brush")`
#' @param height a reactive indicating the map height, in pixels
#' @param map_range a list; the output of [mod_map_range_server()]
#' @param map_elements a list; the output of [mod_map_elements_server()]
#' @param map_color a list; the output of [mod_map_color_server()]
#' @param nondas a list; the output of [mod_nondas_server()]
#'
#' @details
#' This module takes in map ranges/elements/etc, 
#' as well as other data to plot. 
#' It does not perform any validation; 
#' validation is expected to happen in individual modules, 
#' where the values are generated. 
#' 
#' It also provides functionality for downloading the plot.
#'
#' @returns `mod_plot_ui` returns a [shiny::tagList()] with a 
#' [shinydashboard::tabBox()] of the [shiny::plotOutput()] object, 
#' and the UI to save the plot. 
#'
#' `mod_plot_server` returns a named list with the following elements:
#' * 'brush': a reactive of `input$map_brush`.
#'   If `enable_brush` is `FALSE`, then this value will be `NULL`
#'
#' @export
mod_plot_ui <- function(id, enable_brush = FALSE) {
  ns <- NS(id)

  tagList(
    tabBox(
      width = 6, 
      tabPanel(
        title = "Display",
        if (enable_brush) {
          plotOutput(ns("plotmap"), height = "auto", brush = ns("map_brush"))
        } else {
          plotOutput(ns("plotmap"), height = "auto")
        }
      ), 
      tabPanel(
        title = "Save",
        fluidRow(
          cruz_box(
            title = "Save map", width = 12, 
            fluidRow(
              column(3, radioButtons(ns("download_format"), label = tags$h5("File format"),
                                      choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                      selected = 3)),
              column(
                width = 8,
                fluidRow(
                  column(6, radioButtons(ns("download_dim"), tags$h5("File dimensions"),
                                          choices = list("Use dimensions of plot window" = 1, "Specify dimensions" = 2),
                                          selected = 1)),
                  column(6, numericInput(ns("download_res"), tags$h5("Resolution (ppi)"),
                                          value = 300, step = 50, min = 0))
                ),
                conditionalPanel(
                  condition = "input.download_dim == 1", ns = ns, 
                  helpText("Downloaded map will have the same dimensions as the displayed map")
                ),
                conditionalPanel(
                  condition = "input.download_dim == 2", ns = ns, 
                  fluidRow(
                    column(6, numericInput(ns("download_width"), tags$h5("File width (inches)"),
                                            value = 10, step = 1, min = 0)),
                    column(6, numericInput(ns("download_height"), tags$h5("File height (inches)"),
                                            value = 10, step = 1, min = 0))
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.download_format != 1", ns = ns, 
              checkboxInput(ns("background_transparent"), "Make plot background transparent",
                            value = FALSE)
            ),
            uiOutput(ns("downloadMap_button"))
          )
        )
      )
    )
  )
}


#' @name mod_plot
#' @export
mod_plot_server  <- function(
    id,
    height,
    map_range,
    map_elements, 
    map_color, 
    nondas
) {
  moduleServer(id, function(input, output, session) {
    lon_range_req <- reactive(req(map_range$config()$lon.range))
    lat_range_req <- reactive(req(map_range$config()$lat.range))
    world2_req <- reactive({
      world2 <- map_range$config()$world2
      req(is.logical(world2))
      world2
    })
    plot.res <- 72

    ###########################################################################
    plotMap <- reactive({
      function() {
        # The on.exit call causes the coordinates, eg from a click or brush event,
        #   to not be scaled to the data space, aka their range is 0-1.
        #   This is ok because the only par calls in CruzPlot are around legend
        #   calls for the sake of the font family.
        # oldpar <- par(no.readonly = TRUE)
        # on.exit(par(oldpar))

        #------------------------------------------------------------------------
        ### Map range and labels
        lon.range <- lon_range_req()
        lat.range <- lat_range_req()
        world2 <- world2_req()

        map.name <- map_range$config()$map.name
        map.water.col <- map_color$cruzMapColorWater()
        map.land.col <- map_color$cruzMapColorLand()
        
        title.info <- map_elements$label_list$cruzMapLabelTitle()
        axes.info <- map_elements$label_list$cruzMapLabelAxes()

        #------------------------------------------------------------------------
        ### Window
        mar1 <- ifelse(nchar(axes.info$lab.lon) > 0, 7, 3)
        mar2 <- ifelse(nchar(axes.info$lab.lat) > 0, 7, 5)
        mar3 <- ifelse(nchar(title.info$lab)    > 0, 7, 2)

        x.try <- try(map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
                        mar = c(mar1, mar2, mar3, 4)),
                    silent = TRUE)
        validate(need(x.try, "Error - there must be some land in the map area"))

        x.1 <- map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
                  mar = c(mar1, mar2, mar3, 4))

        param.unit <- par("usr")
        param.inch <- par("pin")
        param <- param.unit

        #------------------------------------------------------------------------
        # Water and Land

        ### Water
        rect(param[1], param[3], param[2], param[4], col = map.water.col[[1]])

        # Depth
        map.depth <- map.water.col[[2]]
        if (isTruthy(map.depth)) {
          plot(
            map.depth, image = TRUE, land = TRUE, add = TRUE,
            axes = FALSE, xlab = NA, ylab = NA, lwd = 0.0,
            bpal = list(
              c(0, max(map.depth), "grey"), 
              c(min(map.depth), 0, bathy.col)
            )
          )
        }

        ### Land
        coastline <- map_range$coastline()
        if (!is.null(coastline)) {
          # Coastline
          polygon(x = coastline$lon, y = coastline$lat, col = map.land.col)
          lines(x = coastline$lon, y = coastline$lat)
        } else {
          # Default from maps package
          map(map.name[[1]], regions = map.name[[2]],
              xlim = lon.range[1:2], ylim = lat.range[1:2],
              fill = TRUE, col = map.land.col, add = TRUE)
        }

        ### Rivers and Lakes
        map.rivers <- map_color$cruzMapRivers()
        if (!is.null(map.rivers))
          map(map_color$cruzMapRivers(), col = map.water.col[[1]], add = TRUE)

        graphics::box()


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
            axis(
              1, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
              family = tick.param$font
            )
            axis(
              1, at = tick.lon$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") *0.4*tick.param$len
            )
          }
          if (tick.lat.bool$left[1]) {
            axis(
              2, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
              family = tick.param$font
            )
            axis(
              2, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") * 0.4*tick.param$len
            )
          }
          if (tick.lon.bool$top[1]) {
            axis(
              3, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
              family = tick.param$font
            )
            axis(
              3, at = tick.lon$min, labels = FALSE, lwd = 0,  lwd.ticks = 1,
              tcl = par("tcl") * 0.4*tick.param$len
            )
          }
          if (tick.lat.bool$right[1]) {
            axis(
              4, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale,
              family = tick.param$font
            )
            axis(
              4, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
              tcl = par("tcl") * 0.4*tick.param$len
            )
          }

          # Draw tick labels
          if (tick.lon.bool$bot[2])
            axis(
              1, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
              cex.axis = tick.param$scale, family = tick.param$font
            )
          if (tick.lat.bool$left[2])
            axis(
              2, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
              las = 1, cex.axis = tick.param$scale, family = tick.param$font
            )
          if (tick.lon.bool$top[2])
            axis(
              3, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
              cex.axis = tick.param$scale, family = tick.param$font
            )
          if (tick.lat.bool$right[2])
            axis(
              4, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
              las = 1, cex.axis = tick.param$scale, family = tick.param$font
            )
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

        ### Map labels - assigned at the top, because needed for map boundaries
        # Title
        if (!is.null(title.info$lab)) {
          title(main = title.info$lab, line = 3, family = title.info$fam,
                cex.main = title.info$cex)
        }

        # Longitude axis
        if (!is.null(axes.info$lab.lon)) {
          title(xlab = axes.info$lab.lon, family = axes.info$fam,
                cex.lab = axes.info$cex)
        }
        # Latitude axis
        if (!is.null(axes.info$lab.lat)) {
          title(ylab = axes.info$lab.lat, family = axes.info$fam,
                cex.lab = axes.info$cex, line = 4)
        }

        ### TODO: planned transects


        #----------------------------------------------------------------------
        ### Non-DAS data
        if (nondas$ndas_plot()) {
          data.ndas <- nondas$cruzNonDas()

          # Plot lines
          data.ndas.l <- data.ndas[[1]]
          if (length(data.ndas.l) > 0) {
            for(i in seq_along(data.ndas.l)) {
              data.ndas.l.curr <- data.ndas.l[[i]]
              lines(x = data.ndas.l.curr$x, y = data.ndas.l.curr$y,
                    lty = data.ndas.l.curr$type, col = data.ndas.l.curr$col,
                    lwd = data.ndas.l.curr$lwd)
            }
          }

          # Plot points
          data.ndas.p <- data.ndas[[2]]
          if (length(data.ndas.p) > 0) {
            for(j in seq_along(data.ndas.p)) {
              data.ndas.p.curr <- data.ndas.p[[j]]
              points(x = data.ndas.p.curr$x, y = data.ndas.p.curr$y,
                    pch = data.ndas.p.curr$type, col = data.ndas.p.curr$col,
                    cex = data.ndas.p.curr$cex, lwd = data.ndas.p.curr$lwd)
            }
          }
        }
      }
    })

    output$plotmap <- renderPlot({
      plotMap()()
    }, height = height, units = "px", res = plot.res)


    ###########################################################################
    # Download
    ### Render download button, with checks
    output$downloadMap_button <- renderUI({
      # Resolution
      v.val <- input$download_res
      v.message <- "Resolution must be a whole number greater than zero"
      validate(need(!is.na(v.val), v.message))
      validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
      validate(need(v.val > 0, v.message))

      # Plot dimensions
      if (input$download_dim == 2) {
        # Plot width
        v.val <- input$download_width
        v.message <- "Plot width must be greater than zero"
        validate(need(!is.na(v.val), v.message))
        # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
        validate(need(v.val > 0, v.message))

        # Plot height
        v.val <- input$download_height
        v.message <- "Plot height must be greater than zero"
        validate(need(!is.na(v.val), v.message))
        # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message)
        validate(need(v.val > 0, v.message))
      }

      # Button
      downloadButton(session$ns("downloadMap"), label = "Download map")
    })


    ### Download map
    output$downloadMap <- downloadHandler(
      filename = function() {
        ext <- switch(
          input$download_format, "1" = "jpeg", "2" = "pdf", "3" = "png"
        )

        str_glue(
          "CruzPlot_{lon1}_{lon2}_{lat1}_{lat2}.{ext}", 
          lon1 = lon_range_req()[1], 
          lon2 = lon_range_req()[2], 
          lat1 = lat_range_req()[1], 
          lat2 = lat_range_req()[2]
        )
      },

      content = function(file) {
        # Get file dimension values
        file.res <- input$download_res

        if (input$download_dim == 1) {
          nsid <- session$ns("plotmap")
          file.width <- session$clientData[[str_glue("output_{nsid}_width")]] / plot.res
          file.height <- session$clientData[[str_glue("output_{nsid}_height")]] / plot.res
          

        } else if (input$download_dim == 2) {
          file.width <- input$download_width
          file.height <- input$download_height
        }

        plot.bg <- if_else(input$background_transparent, "transparent", "white")

        # Save map
        if (input$download_format == 1) {
          jpeg(file, width = file.width, height = file.height, units = "in",
              res = file.res)
          plotMap()()
          dev.off()
        } else if (input$download_format == 2) {
          pdf(file, width = file.width, height = file.height, onefile = FALSE,
              bg = plot.bg)
          plotMap()()
          dev.off()
        } else if (input$download_format == 3) {
          png(file, width = file.width, height = file.height, units = "in",
              res = file.res, bg = plot.bg)
          plotMap()()
          dev.off()
        }
      }
    )


    ###########################################################################
    ### Return
    list(
      brush = reactive(input$map_brush)
    )
  })
}
