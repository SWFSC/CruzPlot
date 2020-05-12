# Processing for Map range section of Range tab of Create and Save Map tab

###############################################################################
# Return list of map name and regions to be plotted (if world2 map)
observeEvent(input$map.replot, {
  world2 <- cruz.map.range$world2
  hires <- input$resolution == 2

  m <- ifelse(hires, "Hires", "")
  m <- ifelse(world2, paste("world2", m, sep = ""), paste("world", m, sep = ""))

  reg.toplot <- NULL
  if(world2) {
    #regions.rm and regions.rm.hires are created in server file
    if(hires) reg.toplot <- regions.rm.hires
    else reg.toplot <- regions.rm
  }

  cruz.map.range$map.name <- list(m, reg.toplot)
}, ignoreNULL = FALSE, priority = 8)


###############################################################################
# Update params as necessary
cruzMapParam <- reactive({
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  param.unit <- par("usr")
  param.inch <- par("pin")

  list(param.unit = param.unit, param.inch = param.inch)
})


### Set lon reactiveValues
observeEvent(input$map.replot, {
  lon.min <- input$lon.left
  lon.max <- input$lon.right
  # lon.min <- suppressWarnings(as.numeric(input$lon.left))
  # lon.max <- suppressWarnings(as.numeric(input$lon.right))

  # Checks that inputs are numbers
  #    Numerical checks done in errorMapRange.R so messages are dispalyed
  validate(
    need(!(lon.min %in% c("", "-", "+", NA)),
         message = "Please ensure that -180 <= left longitude <= 180"),
    need(!(lon.max %in% c("", "-", "+", NA)),
         message = "Please ensure that -180 <= right longitude <= 180")
  )

  # Determine if world2 map should be used and thus if lons need to be rescaled
  world2 <- (lon.max < lon.min) && (lon.min > 0) && (lon.max < 0)

  if(world2) {
    lon.min <- ifelse(lon.min < 0, 360 + lon.min, lon.min)
    lon.max <- ifelse(lon.max < 0, 360 + lon.max, lon.max)
  }

  cruz.map.range$lon.range <- c(lon.min, lon.max)
}, ignoreNULL = FALSE, priority = 10)


### Set lat reactiveValues
observeEvent(input$map.replot, {
  lat.min <- input$lat.bot
  lat.max <- input$lat.top
  # lat.min <- suppressWarnings(as.numeric(input$lat.bot))
  # lat.max <- suppressWarnings(as.numeric(input$lat.top))

  # Checks that inputs are numbers
  #    Numerical checks done in errorMapRange.R so messages are dispalyed
  validate(
    need(!(lat.min %in% c("", "-", "+", NA)),
         message = "Please ensure that -90 <= bottom latitude <= 90"),
    need(!(lat.max %in% c("", "-", "+", NA)),
         message = "Please ensure that -90 <= top latitude <= 90")
  )

  cruz.map.range$lat.range <- c(lat.min, lat.max)
}, ignoreNULL = FALSE, priority = 10)


### Set world2 reactiveValue
observeEvent(input$map.replot, {
  lon.min <- input$lon.left
  lon.max <- input$lon.right

  # Determine if world or world2 map should be used
  world2 <- (lon.max < lon.min) && (lon.min > 0) && (lon.max < 0)

  cruz.map.range$world2 <- world2
}, ignoreNULL = FALSE, priority = 9)

###############################################################################

