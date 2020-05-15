# cruzDasSightFilter for CruzPlot
#   cruzDasSightFilterEffort() returns row numbers of data.sight that satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns row numbers of data.sight that satisfy the beaufort filter
#   cruzDasSightFilterDate() returns row numbers of data.sight that satisfy the date filter
#   cruzDasSightFilterCruise() returns row numbers of data.sight entries from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns row numbers of data.sight entries within the given truncation distance
#   cruiseDasSightFilter() returns list of data frame containing data for selected species that satisfy the filters,
#     counts for each selected species, and selected species codes

###############################################################################
### Top-level function for filtering
cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightSpecies()

  das.sight  <- data.list$das.sight
  sight.type <- data.list$sight.type
  sp.codes   <- data.list$sp.codes

  num.keep1 <- cruzDasSightFilterEffort()
  num.keep2 <- cruzDasSightFilterBeaufort()
  num.keep3 <- cruzDasSightFilterDate()
  num.keep4 <- cruzDasSightFilterCruise()
  num.keep5 <- cruzDasSightFilterTrunc()

  num.keep <- sort(unique(num.keep1, num.keep2, num.keep3, num.keep4, num.keep5))
  das.sight <- das.sight %>% slice(num.keep)

  browser()
  # If plotting selected mammals, check that all selected still have sightings
  if(sight.type == 1 && input$das_sighting_code_1_all == 2) {
    temp.all <- sapply(sp.codes, function(i) i %in% data.sight$Data5 || i %in% data.sight$Data6 ||
                         i %in% data.sight$Data7 || i %in% data.sight$Data8)
    sp.code.false <- sp.codes[which(!temp.all)]
    validate(
      need(all(temp.all),
           message = paste("Species with code", sp.code.false,
                           "does not have any sightings that match the given filters"))
    )
  }

  # If plotting selected turtles, check that all selected still have sightings
  if(sight.type == 2 && input$das_sighting_code_2_all == 2) {
    temp.all <- sapply(sp.codes, function(i) i %in% data.sight$Data2)
    sp.code.false <- sp.codes[which(!temp.all)]
    validate(
      need(all(temp.all),
           message = paste("Species with code", sp.code.false,
                           "does not have any sightings that match the given filters"))
    )
  }

  list(das.sight = das.sight, sight.type = sight.type, sp.codes = sp.codes)
})


###############################################################################
### Helper functions that filter sightings data by single factor

#------------------------------------------------------------------------------
# On/off effort
cruzDasSightFilterEffort <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight
  effort.val <- switch(
    as.numeric(input$das_sightings_effort), c(0, 1), 1, 0
  )
  ndx.keep <- which(as.numeric(das.sight$OnEffort) %in% effort.val)

  validate(
    need(ndx.keep, "No sightings match the given on/off effort filters")
  )

  ndx.keep
})

#------------------------------------------------------------------------------
# Beaufort
cruzDasSightFilterBeaufort <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight
  bft.min <- as.numeric(input$das_sight_minBft)
  bft.max <- as.numeric(input$das_sight_maxBft)

  validate(
    need(input$das_sight_minBft <= input$das_sight_maxBft,
         "Minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  ndx.keep <- which(between(das.sight$Bft, bft.min, bft.max))

  validate(
    need(ndx.keep, "No sightings match the given Beaufort filters")
  )

  ndx.keep
})

#------------------------------------------------------------------------------
# Dates
cruzDasSightFilterDate <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight
  date.vals <- input$das_sight_dateRange

  validate(
    need(input$das_sight_dateRange[1] <= input$das_sight_dateRange[2],
         "Minimum date must be less than or equal to maximum date")
  )

  ndx.keep <- which(between(
    as.Date(das.sight$DateTime), date.vals[1], date.vals[2]
  ))

  validate(
    need(ndx.keep, "No sightings match the given date filter")
  )

  ndx.keep
})

#------------------------------------------------------------------------------
# Cruise numbers
cruzDasSightFilterCruise <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight

  cruise.vals <- if (identical(input$das_sight_cruiseNum, "")) {
    unique(das.sight$Cruise)
  } else {
    as.numeric(unlist(strsplit(input$das_sight_cruiseNum, split = ",")))
  }

  ndx.keep <- which(das.sight$Cruise %in% cruise.vals)

  validate(
    need(ndx.keep, "No sightings match the given Cruise number filter")
  )

  ndx.keep
})

#------------------------------------------------------------------------------
# Perpendicular distance truncation
cruzDasSightFilterTrunc <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight

  pdist.val <- ifelse(
    input$das_sight_trunc_units == 1,
    input$das_sight_trunc, input$das_sight_trunc * 1.852
  )

  if (is.na(pdist.val)) {
    1:nrow(das.sight)

  } else {
    ndx.keep <- which(das.sight$PerpDistKm <= pdist.val)

    validate(
      need(ndx.keep,
           "No sightings match the given truncation (perpendicular distance) filter")
    )

    ndx.keep
  }
})

###############################################################################
