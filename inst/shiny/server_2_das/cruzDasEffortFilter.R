# cruzDasEffortFilter for CruzPlot
#   cruzDasEffortFilterBeaufort() returns effort filtered by beaufort  - not used: filtering by Bft in cruzDasEffortDetailed
#   cruzDasEffortFilterDate() returns effort filtered by date
#   cruzDasEffortFilterCruise() returns effort filtered by cruise number
#   cruzDasEffortFilter() returns filtered effort data


###############################################################################
observeEvent(input$das_sightings, {
  if(!input$das_sightings) {
    updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
  }
})


###############################################################################
# Top-level function for filtering sighting data
cruzDasEffortFilter <- reactive({
  data.effort <- cruzDasEffortSimpDet()

  num.keep1 <- cruzDasEffortFilterBeaufort()
  num.keep2 <- cruzDasEffortFilterDate()
  num.keep3 <- cruzDasEffortFilterCruise()

  num.keep <- unique(c(num.keep1, num.keep2, num.keep3))
  num.keep <- num.keep[(num.keep %in% num.keep1) & (num.keep %in% num.keep2) &
                         (num.keep %in% num.keep3)]

  data.effort.filt <- data.effort[num.keep,]

  return(data.effort.filt)
})


###############################################################################
# Individual filters filter for indicies of data.effort to keep

### Beaufort filter
cruzDasEffortFilterBeaufort <- reactive ({
  filter.same <- input$das_effort_filter_same #same as sighting data
  data.effort <- cruzDasEffortSimpDet()

  eff.bft.min <- ifelse(filter.same,
                        input$das.sight.minBeau, input$das.effort.minBeau)
  eff.bft.max <- ifelse(filter.same,
                        input$das.sight.maxBeau, input$das.effort.maxBeau)

  # End of eff segment has next bft level, so just filter by start bft
  ndx.start <- seq(1, nrow(data.effort), by = 2)

  ndx.keep <- lapply(data.effort$Bft[ndx.start], function(i) {
    x <- (i >= eff.bft.min) & (i <= eff.bft.max)
    c(x, x) # Keep or remove start and end of effort
  })
  ndx.keep <- which(unlist(ndx.keep))

  return(ndx.keep)
})


### Date Filter
cruzDasEffortFilterDate <- reactive({
  filter.same <- input$das_effort_filter_same #same as sighting data
  data.effort <- cruzDasEffortSimpDet()


  if(filter.same) eff.date.range <- input$das.sight.dateRange
  else eff.date.range <- input$das.effort.dateRange

  current.date <- substring(data.effort$Date, 1, 10)
  diff.min <- as.numeric(difftime(current.date, eff.date.range[1],
                                  units = "days"))
  diff.max <- as.numeric(difftime(current.date, eff.date.range[2],
                                  units = "days")) - 1
  date.keep <- (diff.min >= 0 ) & (diff.max <= 0)

  ndx.keep <- which(date.keep)

  validate(
    need(length(ndx.keep) != 0,
         message = "No effort lines match the given date filters")
  )

  return(ndx.keep)
})


### Cruise number filter
cruzDasEffortFilterCruise <- reactive ({
  filter.same <- input$das_effort_filter_same #same as sighting data
  data.effort <- cruzDasEffortSimpDet()

  if(filter.same) cruise.nums.in <- input$das.sight.cruiseNum
  else cruise.nums.in <- input$das.effort.cruiseNum

  ndx.keep <- seq(1:length(data.effort[,1]))
  if(cruise.nums.in != "") {
    cruise.nums <- as.numeric(unlist(strsplit(cruise.nums.in, split = ", ")))
    cruise.present <- cruise.nums %in% data.effort$Cruise
    cruise.absent.num <- cruise.nums[!cruise.present]

    validate(
      need(all(cruise.present),
           paste("Based on given filter parameters,",
                 "no effort lines found for cruise number",
                 cruise.absent.num))
    )
    ndx.keep <- which(data.effort$Cruise %in% cruise.nums)
  }

  return(ndx.keep)
})


###############################################################################
