# cruzMapTick for CruzPlot by Sam Woodman
#   update: major tick interval, start of longitude tick labels, start of latitude tick labels
#   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
#   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
#   cruzMapTickLon() returns labels for longitude tick marks
#   cruzMapTickLat() returns labels for latitude tick marks
#   cruzMapTickParam() returns list of tick length, font, and scale


###############################################################################
# Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
#    if inputs change and are different from cruz.tick

observe({
  req(input$tick.interval.major)

  in.tick.interval.major <- input$tick.interval.major
  isolate({
    if(cruz.tick$tick.interval.major != in.tick.interval.major)
      cruz.tick$tick.interval.major <- in.tick.interval.major
  })
})

observe({
  req(input$label.lon.start)
  validate(need(!is.na(input$label.lon.start), "This is never printed"))

  in.label.lon.start <- as.numeric(input$label.lon.start)
  isolate({
    if(cruz.tick$label.lon.start != in.label.lon.start)
      cruz.tick$label.lon.start <- in.label.lon.start
  })
})

observe({
  req(input$label.lat.start)
  validate(need(!is.na(input$label.lat.start), "This is never printed"))

  in.label.lat.start <- as.numeric(input$label.lat.start)
  isolate({
    if(cruz.tick$label.lat.start != in.label.lat.start)
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
  if(b != 0 && !is.na(b)) {
    lon.range <- cruz.map.range$lon.range
    lon.start <- cruzTickStart(lon.range, b)

    updateTextInput(session, "label.lon.start", value = paste(lon.start))
    cruz.tick$label.lon.start <- lon.start
  }
}, priority = 1)

# Tick label latitude start
observe({
  b <- cruz.tick$tick.interval.major
  if(b != 0 && !is.na(b)) {
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
  bot <- c(input$tick.bot, input$tick.bot.lab)
  top <- c(input$tick.top, input$tick.top.lab)
  return(list(bot = bot, top = top))
})

# Plot latitude tick marks
cruzMapTickLatBool <- reactive({
  left <- c(input$tick.left, input$tick.left.lab)
  right <- c(input$tick.right, input$tick.right.lab)
  return(list(left = left, right = right))
})

# Plot longtiude tick labels
cruzMapTickLonLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLon()$label.loc
  format <- input$tick.style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    i <- ifelse(i > 180, i - 360, i)
    i <- ifelse(i < -180, 360 - i, i)
    a <- ifelse(i < 0, -1 * i, i)
    b <- ifelse(i < 0, "~W", "~E")
    b <- ifelse(a %in% c(0, 180), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  return(tick.lab)
})

# Plot latitude tick labels
cruzMapTickLatLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLat()$label.loc
  format <- input$tick.style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    a <- ifelse(i < 0, -1 * i, i)
    b <- ifelse(i < 0, "~S", "~N")
    b <- ifelse(a %in% c(0, 90), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  return(tick.lab)
})

cruzMapTickParam <- reactive({
  tick.len <- input$tick.length
  lab.font <- font.family.vals[as.numeric(input$label.tick.font)]
  lab.scale <- input$label.tick.size
  return(list(len = tick.len, font = lab.font, scale = lab.scale))
})
