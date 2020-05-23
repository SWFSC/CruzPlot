# Processing for Map range section of Range tab of Create and Save Map tab

###############################################################################
# Update map range when default study area buttons are clicked

### CCE
observeEvent(input$map_replot_cce, {
  ll.vals <- c(-135, -117, 29, 52)
  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], ll.vals[2])
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- FALSE
  cruz.map.range$map.name <- list(
    ifelse(input$resolution == 2, "worldHires", "world"), NULL
  )
}, priority = 11)

### Expanded CCE
observeEvent(input$map_replot_cce2, {
  ll.vals <- c(-135, -110, 27, 52)
  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], ll.vals[2])
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- FALSE
  cruz.map.range$map.name <- list(
    ifelse(input$resolution == 2, "worldHires", "world"), NULL
  )
}, priority = 11)

### ETP
observeEvent(input$map_replot_etp, {
  ll.vals <- c(-155, -75, -10, 50)
  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], ll.vals[2])
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- FALSE
  cruz.map.range$map.name <- list(
    ifelse(input$resolution == 2, "worldHires", "world"), NULL
  )
}, priority = 11)

### Hawaii
observeEvent(input$map_replot_hawaii, {
  ll.vals <- c(175, -150, 12, 35)
  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], ll.vals[2] + 360)
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- TRUE
  cruz.map.range$map.name <- list(
    ifelse(input$resolution == 2, "world2Hires", "world2"),
    if (input$resolution == 2) regions.rm.hires else regions.rm
  )
}, priority = 11)

### Marianas
observeEvent(input$map_replot_marianas, {
  ll.vals <- c(140, 150, 10, 24)
  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], ll.vals[2])
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- FALSE
  cruz.map.range$map.name <- list(
    ifelse(input$resolution == 2, "worldHires", "world"), NULL
  )
}, priority = 11)


###############################################################################
# Update params as necessary
cruzMapParam <- reactive({
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  param.unit <- par("usr")
  param.inch <- par("pin")

  list(param.unit = param.unit, param.inch = param.inch)
})


###############################################################################
# Series of steps/actions triggered by replot_reac()

#------------------------------------------------------------------------------
### Set lon reactiveValues
observeEvent(input$map_replot, {
  lon.min <- input$lon_left
  lon.max <- input$lon_right
  lat.min <- input$lat_bot
  lat.max <- input$lat_top

  # Checks that inputs are numbers
  vals.bad <- c("", "-", "+", NA)
  validate(
    need(!(lon.min %in% vals.bad),
         "The left longtiude must be a number between -180 and 180"),
    need(!(lon.max %in% vals.bad),
         "The right longtiude must be a number between -180 and 180"),
    need(!(lat.min %in% vals.bad),
         "The bottom latitude must be a number between -90 and 90"),
    need(!(lat.max %in% vals.bad),
         "The top latitude must be a number between -90 and 90")
  )

  # Determine if world2 map should be used and thus if lons need to be rescaled
  world2 <- (lon.max < lon.min) && (lon.min > 0) && (lon.max < 0)

    if (world2) {
    lon.min <- ifelse(lon.min < 0, 360 + lon.min, lon.min)
    lon.max <- ifelse(lon.max < 0, 360 + lon.max, lon.max)
  }

  # Get map name
  hires <- input$resolution == 2

  m <- ifelse(hires, "Hires", "")
  m <- ifelse(world2, paste0("world2", m), paste0("world", m))

  #regions.rm and regions.rm.hires are created in server file
  reg.toplot <- if (world2 & hires) {
    regions.rm.hires
  } else if (world2 & !hires) {
    regions.rm
  } else {
    NULL
  }

  # Save as reactive values
  cruz.map.range$lon.range <- c(lon.min, lon.max)
  cruz.map.range$lat.range <- c(lat.min, lat.max)
  cruz.map.range$world2 <- world2
  cruz.map.range$map.name <- list(m, reg.toplot)
}, ignoreNULL = FALSE, priority = 10)

###############################################################################
