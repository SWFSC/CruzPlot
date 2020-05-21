# Set values for subsequent draw code

###############################################################################
# draw_setVals - Set map param values and call reactive functions for drawMap.R

#------------------------------------------------------------------------------
# Map range
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range
world2 <- cruz.map.range$world2
stopifnot("world2 param is not a logical" = inherits(world2, "logical"))

validate( #lats
  need(-90 <= lat.range[1] & lat.range[1]<= 90,
       "The bottom latitude must be a number between -90 and 90"),
  need(-90 <= lat.range[2] & lat.range[2]<= 90,
       "The top latitude must be a number between -90 and 90")
)
if ((0 <= lon.range[1] & 0 <= lon.range[2]) || (lon.range[1] < 0 & lon.range[2] < 0))
  validate( #lons
    need(lon.range[1] < lon.range[2],
         message = paste("Left longitude must be less than right longitude,",
                         "unless left longitude s positive and right longitude",
                         "is negative (Pacific-centered map)"))
  )
if (world2) {
  validate(
    need(0 <= lon.range[1] & lon.range[1]<= 360,
         "The left longtiude must be a number between -180 and 180"),
    need(0 <= lon.range[2] & lon.range[2]<= 360,
         "The right longtiude must be a number between -180 and 180")
  )
} else { #!cruz.map.range$world2
  validate(
    need(-180 <= lon.range[1] & lon.range[1]<= 180,
         "The left longtiude must be a number between -180 and 180"),
    need(-180 <= lon.range[2] & lon.range[2]<= 180,
         "The right longtiude must be a number between -180 and 180")
  )
}


#------------------------------------------------------------------------------
# Other map paramters - not tick/grid
map.name <- cruz.map.range$map.name
map.water.col <- cruzMapColorWater()
map.land.col <- cruzMapColorLand()
if (input$color_lakes_rivers) map.river <- cruzMapRiver()

map.coastline <- NULL
if (input$coast & !is.null(cruz.list$coastline))
  map.coastline <- cruz.list$coastline

if (input$bar) {
  scale.bar <- cruzMapScaleBar()

  validate(
    need(lon.range[1] <= scale.bar$x1,
         message = "Start of scale bar must be after left longitude value"),
    need(lon.range[2] >= (scale.bar$x2),
         message = "End of scale bar must be before right longitude value"),
    need(lat.range[1] <= scale.bar$y,
         message = "Scale bar latitude must be greater than bottom latitude value"),
    need(lat.range[2] >= scale.bar$y,
         message = "Scale bar latitude must be less than top latitude value")
  )
}

validate(
  need(!is.na(input$label_title_size),
       "Please enter a valid title size value") %then%
    need(!is.na(input$label_axis_size),
         "Please enter a valid axis label size value") %then%
    need(input$label_title_size > 0,
         "Please enter a title size greater than zero") %then%
    need(input$label_axis_size > 0,
         "Please enter an axis label size greater than zero")
)
title.info <- cruzMapLabelTitle()
axes.info <- cruzMapLabelAxes()


#------------------------------------------------------------------------------
# Other map paramters - tick/grid
if (input$tick || input$grid) {
  # Error checks
  validate( #Check that tick intervals are possibly valid
    need(!is.na(cruz.tick$tick.interval.major),
         "Please enter a valid major tick interval value") %then%
      need(!is.na(input$tick_interval_minor),
           "Please enter a valid minor tick interval value") %then%
      need(cruz.tick$tick.interval.major > 0,
           "Please enter a major tick interval value greater than zero") %then%
      need(input$tick_interval_minor >= 0,
           paste("Please enter a minor tick interval value",
                 "greater than or equal to zero"))
  )
  validate( #Check that tick label size is a valid entry
    need(!is.na(input$label_tick_size),
         "Please enter a valid tick label size value") %then%
      need(input$label_tick_size >= 0,
           paste("Please enter a tick label size value",
                 "greater than or equal to zero"))
  )
  if (!world2) { #Check that actual longitude values are valid given map rnages
    validate(
      need(lon.range[1] <= as.numeric(cruz.tick$label.lon.start),
           message = "Start of longitude tick labels must be after left longitude value"),
      need(lon.range[2] >= as.numeric(cruz.tick$label.lon.start),
           message = "Start of longitude tick labels must be before right longitude value")
    )
  } else { #world2
    validate(
      need(as.numeric(cruz.tick$label.lon.start) != 0,
           message = "Please use '180' rather than '0' for the start of longitude tick labels")
    )
    if (as.numeric(cruz.tick$label.lon.start) < 0)
    {
      validate(
        need((as.numeric(cruz.tick$label.lon.start) + 180) <= lon.range[2],
             message = "Start of longitude tick labels must be before right longitude value")
      )
    }
    if (as.numeric(cruz.tick$label.lon.start) > 0)
    {
      validate(
        need(lon.range[1] <= (as.numeric(cruz.tick$label.lon.start)),
             message = "Start of longitude tick labels must be after left longitude value")
      )
    }
  }
  validate( #Check that actual latitude values are valid given map rnages
    need(lat.range[1] <= cruz.tick$label.lat.start,
         message = "Start of latitude tick labels must be greater than bottom latitude value"),
    need(lat.range[2] >= cruz.tick$label.lat.start,
         message = "Start of latitude tick labels must be less than top latitude value"),
    need(!is.na(input$tick_length),
         message = "Please enter a valid tick length value")
  )

  # Assignments
  tick.lon <- cruzMapIntervalLon()
  tick.lat <- cruzMapIntervalLat()
}

if (input$tick) {
  tick.lon.bool <- cruzMapTickLonBool()
  tick.lat.bool <- cruzMapTickLatBool()
  tick.lon$label <- cruzMapTickLonLab()
  tick.lat$label <- cruzMapTickLatLab()
  tick.param <- cruzMapTickParam()
}

if (input$grid) grid.param <- cruzMapGrid()


#------------------------------------------------------------------------------
### Planned transects
if (input$planned_transects_plot) {
  validate(
    need(input$planned_transects_toplot,
         "Please select at least one class of planned transects to plot")
  )
  #So that renderUI()'s can catch up
  req(input$planned_transects_color, input$planned_transects_lty)

  # Get user inputs
  pltrans <- cruz.list$planned.transects
  pltrans.which <- as.numeric(input$planned_transects_toplot)
  pltrans.which2 <- as.numeric(input$planned_transects_toplot2)
  pltrans.colors <- input$planned_transects_color
  pltrans.lty <- as.numeric(input$planned_transects_lty)
  pltrans.lwd <- input$planned_transects_lwd

  # Process user inputs
  if (length(pltrans.colors) == 1) {
    pltrans.colors <- rep(pltrans.colors, length(pltrans.which))
  }

  validate(
    need(length(pltrans.colors) == length(pltrans.which),
         paste("The number of selected planned transect colors must either be",
               "1 or equal to than the number of selected planned transects"))
  )

  pltrans.class1 <- planned_transects_class1()[pltrans.which]
  names(pltrans.colors) <- pltrans.class1

  pltrans <- dplyr::filter(pltrans, class1 %in% pltrans.class1)

  if (anyNA(planned_transects_class2())) {
    # Class 2 was not specified
    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)

      lapply(unique(x$num), function(k) {
        x <- dplyr::filter(x, num == k)
        if (nrow(x) == 0) {
          NULL
        } else if (nrow(x) == 1){
          validate(need(FALSE, "Error in planned transect processing"))
        } else {
          list(x$lon, x$lat, unname(pltrans.colors[as.character(i)]), pltrans.lty)
        }
      })
    })

  } else {
    # Class 2 was specified
    validate(
      need(pltrans.which2,
           "Please select at least one class 2 type to plot")
    )

    pltrans.class2 <- planned_transects_class2()[pltrans.which2]
    pltrans <- dplyr::filter(pltrans, class2 %in% pltrans.class2)

    if (length(pltrans.lty) == 1) {
      pltrans.lty <- rep(pltrans.lty, length(pltrans.class2))
    }
    validate(
      need(length(pltrans.lty) == length(pltrans.class2),
           paste("The number of selecetd planned transect line types must either be",
                 "1 or equal to than the number unique class 2 values"))
    )
    names(pltrans.lty) <- pltrans.class2

    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)

      lapply(pltrans.class2, function(j) {
        x <- dplyr::filter(x, class2 == j)

        lapply(unique(x$num), function(k) {
          x <- dplyr::filter(x, num == k)
          if (nrow(x) == 0) {
            NULL
          } else if (nrow(x) == 1){
            validate(need(FALSE, "Error in planned transect processing"))
          } else {
            list(x$lon, x$lat, unname(pltrans.colors[as.character(i)]), unname(pltrans.lty[as.character(j)]))
          }
        })
      })
    })
  }
}


###############################################################################
# Set data values and call reactive functions for drawData.R

#------------------------------------------------------------------------------
### Non-DAS
data.ndas <- if (input$ndas_plot) cruzNonDas() else NULL

#------------------------------------------------------------------------------
### DAS
if (isTruthy(cruz.list$das.data)) {
  req(input$das_sight_dateRange, input$das_effort_dateRange)

  #### TODO Add validate() checks for lat/long info
  # Sightings
  if (input$das_sightings) {
    # Error checks are in cruzDasSight... functions
    das.sight <- cruzDasSightRange()$das.sight
    sight.type <- cruzDasSightRange()$sight.type
    das.sight.pt <- cruzDasSightSymbol()$pt.df
    if (input$das_legend) das.sight.legend <- cruzDasSightLegend()
  }

  # Effort
  if (as.numeric(input$das_effort) != 1) {
    das.eff.lines <- cruzDasEffort()

    # data.effort <- data.effort.list$data.effort
    # eff.ndx.R <- data.effort.list$ndx.R
    # eff.ndx.E <- data.effort.list$ndx.E
    # eff.bft <- data.effort.list$data.effort$Bft[eff.ndx.R]

    eff.col <- cruzDasEffortLines()$eff.col
    eff.lwd <- cruzDasEffortLines()$eff.lwd
    if (input$eff_legend) data.eff.legend <- cruzDasEffortLegend()


    # Adjust data.effort$Lon points as needed for world/world2
    validate(
      need(!(any(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
                   is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon))),
           "Some of the lat/long data for the effort is 'NA'")
    )

    if (world2) {
      # If world2 then convert lons to 0 to 360 range
      #   assumes there won't be any weird 0/360 overlap business
      das.eff.lines$st_lon <- ifelse(
        das.eff.lines$st_lon < 0, das.eff.lines$st_lon + 360, das.eff.lines$st_lon
      )
      das.eff.lines$end_lon <- ifelse(
        das.eff.lines$end_lon < 0, das.eff.lines$end_lon + 360, das.eff.lines$end_lon
      )

    } else { #!world2
      # If not world2, convert to -180 to 180 range
      #   then see if there's any weird Pacific overlap business
      lon.fix1 <- ifelse(das.eff.lines$st_lon > 180, das.eff.lines$st_lon - 360, das.eff.lines$st_lon)
      lon.fix2 <- ifelse(das.eff.lines$end_lon > 180, das.eff.lines$end_lon - 360, das.eff.lines$end_lon)

      # Semi-arbitrary cutoffs to determine if effort lines are in the Pacific rather than Atlantic
      # TODO: ???
      if (!(all(lon.fix1 < 0) | all(lon.fix1 > 0)) & any(lon.fix1 > 130) & any(lon.fix1 < -100)) {
        lon.fix1 <- ifelse(lon.fix1 > 0, lon.fix1 - 360, lon.fix1)
      }
      if (!(all(lon.fix2 < 0) | all(lon.fix2 > 0)) & any(lon.fix2 > 130) & any(lon.fix2 < -100)) {
        lon.fix2 <- ifelse(lon.fix2 > 0, lon.fix2 - 360, lon.fix2)
      }

      das.eff.lines$st_lon <- lon.fix1
      das.eff.lines$end_lon <- lon.fix2
    }
  }
}

###############################################################################
