# cruzDasEffort for CruzPlot
#   cruzDasEffortClosePass() returns closing/passing effort data with effort segments split by V events
#   cruzDasEffortSimplified() returns simplified effort data, from each R to E event
#   cruzDasEffortDetailed() returns detailed effort data, filtered by standard/non-standard/fine effort types
#   cruzDasEffortSimpDet() returns siplified or detailed effort data, as selected
#   cruzDasEffort() returns selected and filtered effort data
#   cruzDasEffortLines() returns effort line color and line width


# Code sections are in 'chronological' order

###############################################################################
# Filter effort data by Closing/Passing and split effort segments by Beaufort

### Closing/Passing filter
cruzDasEffortClosePass <- reactive({
  data.all <- cruz.list$das.data
  req(data.all)
  type.effort.1 <- input$das.effort.closePass

  ndx.B <- which(data.all$Event == "B")
  ndx.R <- which(data.all$Event == "R")
  ndx.E <- which(data.all$Event == "E")
  ndx.V <- which(data.all$Event == "V")
  # data.eff <- data.all[sort(c(ndx.B, ndx.R, ndx.E, ndx.V)),]
  #### TODO make it so that simplified effort ignores V events
  # Functions should only require info they need, e.g. in case V events have issues Simplified Effort can still plot

  validate(
    need(length(ndx.R) == length(ndx.E),
         paste("There are not an equal number of 'R' and 'E' events",
               "in the provided .DAS file")) %then%
      need(length(type.effort.1) != 0,
           "Please choose 1 or more of Closing/Passing effort types")
  )

  ## Filter effort by Closing / Passing
  if(length(type.effort.1) != 2) {
    ## New code
    if(type.effort.1 == "C") { # C and P are only options
      type.effort.1 <- c("C", "c")
    } else {
      type.effort.1 <- c("P", "p")
    }

    data.all$EffType1[is.na(data.all$EffType1)] <- "NA"
    data.cp <- data.all[data.all$EffType1 %in% type.effort.1,]

    # Use row.names to get indices for data.all for easier debugging
    ndx.B <- as.numeric(row.names(data.cp[data.cp$Event == "B",]))
    ndx.R <- as.numeric(row.names(data.cp[data.cp$Event == "R",]))
    ndx.E <- as.numeric(row.names(data.cp[data.cp$Event == "E",]))
    ndx.V <- as.numeric(row.names(data.cp[data.cp$Event == "V",]))
  }

  validate(
    need(length(ndx.B) != 0,
         paste("No effort lines match the given closing/passing",
               "effort type parameters")),
    need(length(ndx.R) == length(ndx.E),
         paste("Post closing/passing filter, there are not an equal number",
               "of 'R' and 'E' events -> close/pass filter error"))
  )

  #####################################
  ## Divide R to E effort segements into R to V, V to V, ..., V to E segments

  # Get V records that are between some R and E event pair
  ndx.V.inc <- mapply(function(i, j) {
    c(ndx.V[ndx.V > i & ndx.V < j])
  }, ndx.R, ndx.E, SIMPLIFY = FALSE) # Guarantees output to be a list
  ndx.V.inc <- as.numeric(unlist(ndx.V.inc))

  # Split effort segments by V events, aka when Beaufort changes
  ndx.R.inc <- ndx.V.inc # All R events are immediately followed by a V event
  j.sapply <- sort(c(ndx.E, ndx.V.inc))
  ndx.E.inc <- unlist(sapply(ndx.R.inc, function(i,j) min(j[j > i]), j = j.sapply))

  data.effort <- data.all[sort(c(ndx.R.inc, ndx.E.inc)), ]

  validate(
    need(nrow(data.effort) %% 2 == 0, "Effort splitting error at line 82")
  )

  return(list(data.all = data.all, data.effort = data.effort,
              B = ndx.B, R = ndx.R, E = ndx.E, V = ndx.V))
})


###############################################################################
# Get simplified effort or detailed effort

### Simplified effort - defined by Jeff Moore as all R -> E segments July 2017
# Thus no filtering done here
cruzDasEffortSimplified <- reactive({
  data.list <- cruzDasEffortClosePass()
  data.effort <- data.list$data.effort

  validate(
    need(length(data.effort[,1]) != 0, # Should never happen
         "No effort lines match the given simplified effort type parameters")
  )

  return(data.effort)
})


### Detailed effort - effort filtered by standard/non-standard/fine
cruzDasEffortDetailed <- reactive({
  data.list <- cruzDasEffortClosePass()
  data.effort <- data.list$data.effort

  # Standard/Non-standard/Fine Filter
  #   Std/Non-std/Fine: 'S'/'N'/'F'
  type.effort.2 <- input$das_effort_snf
  validate(
    need(length(type.effort.2) != 0,
         message = "For Detailed effort please choose 1 or more of Standard/Non-standard/Fine effort types")
  )

  ndx.efftype2 <- which(data.effort$EffType2 %in% type.effort.2)
  validate(
    need(length(ndx.efftype2) != 0,
         message = "No effort lines match the given Standard/Non-standard/Fine effort type(s)")
  )

  data.effort <- data.effort[ndx.efftype2,]
  validate(
    need(length(data.effort[,1]) != 0,
         message = "No effort lines match the given detailed effort type parameters")
  )

  return(data.effort)
})


###############################################################################
# Get simplified or detailed effort data
### cruzDasEffortSimpDet() called by effort filter functions
cruzDasEffortSimpDet <- reactive({
  validate(
    need(!(!input$das_sightings && input$das_effort != 1 && input$das_effort_filter_same),
         message = "Cannot use sighting filters for effort if sightings are not plotted")
  )

  effort.type <- input$das_effort
  data.effort <- NULL

  if(effort.type == 2) data.effort <- cruzDasEffortSimplified()
  if(effort.type == 3) data.effort <- cruzDasEffortDetailed()

  validate(
    need(length(data.effort[,1]) %% 2 == 0,
         message = "Data is not an even length, might be due to abnormality in data")
  )

  data.effort
})


###############################################################################
# Functions called in drawData_setVals.R

### Final effort function - gets filtered data from cruzDasEffortFilter()
cruzDasEffort <- reactive({
  # effort.type <- as.numeric(input$das_effort)
  data.effort <- cruzDasEffortFilter()

  # if(effort.type == 2 || effort.type == 3) {
  data.effort.len <- length(data.effort[,1])
  ndx.R <- seq(1, data.effort.len, by = 2)
  ndx.E <- seq(2, data.effort.len, by = 2)
  # }
  # else {
  #   ndx.R <- NULL
  #   ndx.E <- NULL
  # }

  return(list(data.effort = data.effort, ndx.R = ndx.R, ndx.E = ndx.E))
})

### Get effort plotting colors and line widths
cruzDasEffortLines <- reactive({
  ## If simplified effort, simple results
  if(input$das_effort == 2) {
    eff.col <- input$das.effort.simp.col
    eff.lwd <- input$das.effort.simp.lwd
  }

  ## If detailed effort, not as simple
  if(input$das_effort == 3) {
    data.list <- cruzDasEffort()
    data.effort <- data.list$data.effort
    ndx.R <- data.list$ndx.R

    # Color code by Beaufort
    if(input$das_effort_det_byBft) {
      bft <- data.effort$Bft[ndx.R]
      bft.cols <- c("darkblue", "dodgerblue2", "forestgreen", "greenyellow",
                    "orange", "darkorange3", "red", "red", "red", "red")
      eff.col <- bft.cols[bft]
      eff.lwd <- 2
    }

    # Color code by SNF, not Beaufort
    if(!input$das_effort_det_byBft) {
      snf <- data.effort$EffType2[ndx.R]

      eff.col <- sapply(snf, function(i) {
        switch(i,
               "S" = input$das.effort.det.col.s,
               "N" = input$das.effort.det.col.n,
               "F" = input$das.effort.det.col.f)
      })
      eff.lwd <- sapply(snf, function(i) {
        switch(i,
               "S" = input$das.effort.det.lwd.s,
               "N" = input$das.effort.det.lwd.n,
               "F" = input$das.effort.det.lwd.f)
      })
    }
  }

  return(list(eff.col = eff.col, eff.lwd = eff.lwd))
})

###############################################################################
