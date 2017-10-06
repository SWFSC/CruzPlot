# cruzMapInterval for cruzPlot by Sam Woodman
#   cruzMapIntervalLon() returns list of longitude values of major tick marks/grid lines and minor tick marks
#   cruzMapIntervalLat() returns list of latitude values of major tick marks/grid lines and minor tick marks


cruzMapIntervalLon <- reactive({
  lon.range <- cruz.map.range$lon.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick.interval.minor
  lon.start <- cruz.tick$label.lon.start
  
  if(cruz.map.range$world2) lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)
  
  tick.lon <- list(label.loc = seq(lon.start, lon.range[2], by = tick.maj))
  temp.tick <- rev(seq(lon.start, lon.range[1], by = -tick.maj))
  tick.lon$maj <- sort(unique(c(tick.lon$label.loc, temp.tick)))
  tick.lon$min <- cruzTickMinor(deg.range = lon.range, maj.ticks = tick.lon$maj, 
                                tick.maj.interval = tick.maj, n=tick.min)
  
  return(tick.lon)
})

cruzMapIntervalLat <- reactive({
  lat.range <- cruz.map.range$lat.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick.interval.minor
  lat.start <- cruz.tick$label.lat.start
  
  tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
  temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
  tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
  tick.lat$min <- cruzTickMinor(deg.range = lat.range, maj.ticks = tick.lat$maj, 
                                tick.maj.interval = tick.maj, n=tick.min)
  
  return(tick.lat)
})