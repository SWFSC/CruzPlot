# Processing for Scale bar section of Map range tab of Create and Save Map tab
#   update: scale bar longitude, length, and latitude
#   cruzMapScaleBar() returns a list of the scale bar coordinates and parameters


###############################################################################
### Update reactiveValues cruz.scale if inputs change and are different
observe({
  req(input$scale_lon)
  isolate({
    if (!isTRUE(all.equal(cruz.scale$scale.lon, input$scale_lon)))
      cruz.scale$scale.lon <- input$scale_lon
  })
})

observe({
  req(input$scale_lat)
  isolate({
    if (!isTRUE(all.equal(cruz.scale$scale.lat, input$scale_lat)))
      cruz.scale$scale.lat <- input$scale_lat
  })
})

observe({
  req(input$scale_len)
  isolate({
    if (cruz.scale$scale.len != input$scale_len)
      cruz.scale$scale.len <- input$scale_len
  })
})


###############################################################################
# Calculate new default scale bar lon/lat/len if map dimensions change
#    Update both reactiveVals and widgets

output$scale_lon_uiOut_numeric <- renderUI({
  numericInput("scale_lon", tags$h5("Longitude"), value = cruz.scale$scale.lon)
})

output$scale_lat_uiOut_numeric <- renderUI({
  numericInput("scale_lat", tags$h5("Latitude"), value = cruz.scale$scale.lat)
})

output$out_scale_len <- renderUI({
  title.new <- ifelse(input$scale_units == 1, "Length in km", "Length in nmi")
  numericInput("scale_len", tags$h5(title.new), value = cruz.scale$scale.len)
})


### Calculate scale bar default start position if map range changes
observe({
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

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

  NULL
}, priority = 1) # Must run before length observe()

### After getting start position, get the default scale bar length
###   Separate observe() so that lat/lon update isn't run if scale units chagne
observe({
  # cruzMapScaleUpdateLength <- reactive({

  lon.range <- cruz.map.range$lon.range
  lon.pos <- cruz.scale$scale.lon
  lat.pos <- cruz.scale$scale.lat
  scale.units <- input$scale_units

  # Scale bar length; suppressWarnings() for if world2
  lon.range.m <- suppressWarnings(distVincentyEllipsoid(
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
  }

  cruz.scale$scale.len <- len.new

  # list(title.new, len.new)
})


###############################################################################
### Put all scale bar values in list for plotting
cruzMapScaleBar <- reactive({
  isolate(world2 <- cruz.map.range$world2)
  scale.lon <- cruz.scale$scale.lon
  scale.lat <- cruz.scale$scale.lat
  scale.len <- cruz.scale$scale.len
  scale.lwd <- input$scale_width
  scale.units <- input$scale_units

  scale.units.str <- ifelse(scale.units == 1, "km", "nmi")

  # Determine length of scale bar in meters
  if (scale.units == 1) scale.len.m <- scale.len * 1000
  if (scale.units == 2) scale.len.m <- scale.len * 1.852 * 1000

  scale.x1 <- ifelse((world2 && scale.lon < 0), scale.lon + 360, scale.lon)
  scale.x2 <- destPoint(c(scale.lon, scale.lat), 90, scale.len.m)[1]
  scale.x2 <- ifelse((world2 && scale.x2 < 0), scale.x2 + 360, scale.x2)

  scale.y <- scale.lat

  # browser()

  list(
    x1 = scale.x1, x2 = scale.x2, y = scale.y,
    lwd = scale.lwd, len = scale.len, units.str = scale.units.str
  )
})
