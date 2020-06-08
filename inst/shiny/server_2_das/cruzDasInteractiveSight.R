# cruzDasInteractiveSight for CruzPlot

### Interactive sighting click
observeEvent(input$sight_click, {
  click.curr <- c(input$sight_click$x, input$sight_click$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      sight$click <- c(sight$click, list(click.curr))
      sight$lab <- c(sight$lab, close.info[3])
    })
    sight$miss <- FALSE
  } else{
    sight$miss <- TRUE
  }
})


### Hover to display sighitng information
observeEvent(input$sight_hover, {
  sight$hover <- c(input$sight_hover$x, input$sight_hover$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(sight$hover.lab <- close.info[3])
    sight$hover.miss <- FALSE
  } else {
    sight$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_sight_interactive_reset_last, {
  sight$click <- if (length(sight$click) == 1) NULL else head(sight$click, -1)
  sight$lab <- if (length(sight$lab) == 1) NULL else head(sight$lab, -1)
  sight$miss <- FALSE

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_sight_interactive_reset_all, {
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})
