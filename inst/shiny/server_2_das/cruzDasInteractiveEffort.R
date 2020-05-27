# cruzDasInteractiveEffort for CruzPlot

### Click to add labels to map
observeEvent(input$effort_click, {
  effort$click <- c(effort$click, input$effort_click$x, input$effort_click$y)
  effort$miss <- FALSE

  len <- length(effort$click)
  curr <- c(effort$click[len-1], effort$click[len])
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
  if(dist.inch <= 0.2) {
    isolate(effort$lab <- c(effort$lab, close.info[3]))
  } else {
    effort$click <- effort$click[1:(length(effort$click)-2)]
    effort$miss <- TRUE
  }
})


### Hover to see R and E lat/lon coordinates
observeEvent(input$effort_hover, {
  effort$hover <- c(input$effort_hover$x, input$effort_hover$y)
  effort$hover.miss <- FALSE
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
  } else {
    effort$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_effort_interactive_reset_last, {
  effort$click <- if (length(effort$click) == 2) NULL else effort$click[1:(length(effort$click)-2)]
  effort$lab <- if (length(effort$lab) == 1) NULL else effort$lab[1:(length(effort$lab)-1)]
  effort$miss <- FALSE
})


### Remove all points
observeEvent(input$das_effort_interactive_reset_all, {
  effort$click <- NULL
  effort$lab <- NULL
  effort$miss <- FALSE
})


# ### Make sure interactively labeled effort lines are still present
# cruzDasInteractiveEffortCheck <- reactive({
#   data.effort <- cruzDasEffortFilter()
#
#   # Only need to do checks when data.sight changes
#   isolate(effort.lab <- effort$lab)
#
#   if(!is.null(effort.lab)) {
#     #     keep.click <- sapply(1:length(effort.lab), function(j) any(sapply(1:length(data.effort[,1]), function(i) all(effort.lab[j,] %in% data.effort[i,]))))
#     #     isolate(effort$click <- effort$click[keep.click])
#     #     isolate(effort$lab <- effort$lab[keep.pt])
#   }
# })
