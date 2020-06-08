# cruzDasInteractiveEffort for CruzPlot

### Click to add labels to map
observeEvent(input$effort_click, {
  click.curr <- c(input$effort_click$x, input$effort_click$y)
  das.effort <- cruzDasEffortFilter()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    click.curr, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      effort$click <- c(effort$click, list(click.curr))
      effort$lab <- c(effort$lab, close.info[3])
    })
    effort$miss <- FALSE
  } else {
    effort$miss <- TRUE
  }
})


### Hover to see R and E lat/lon coordinates
observeEvent(input$effort_hover, {
  effort$hover <- c(input$effort_hover$x, input$effort_hover$y)
  das.effort <- cruzDasEffortFilter()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    effort$hover, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(effort$hover.lab <- close.info[3])
    effort$hover.miss <- FALSE
  } else {
    effort$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_effort_interactive_reset_last, {
  effort$click <- if (length(effort$click) == 1) NULL else head(effort$click, -1)
  effort$lab <- if (length(effort$lab) == 1) NULL else head(effort$lab, -1)
  effort$miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_effort_interactive_reset_all, {
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})
