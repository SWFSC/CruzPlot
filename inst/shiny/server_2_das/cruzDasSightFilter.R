# cruzDasSightFilter for CruzPlot - step 2 of processing species data
#   cruiseDasSightFilter() pulls individual filters together

#   cruzDasSightFilterEffort() returns row numbers of data.sight that satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns row numbers of data.sight that satisfy the beaufort filter
#   cruzDasSightFilterDate() returns row numbers of data.sight that satisfy the date filter
#   cruzDasSightFilterCruise() returns row numbers of data.sight entries from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns row numbers of data.sight entries within the given truncation distance


###############################################################################
### Top-level function for filtering
cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightSpecies()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  num.keep1 <- cruzDasSightFilterEffort()
  num.keep2 <- cruzDasSightFilterBeaufort()
  num.keep3 <- cruzDasSightFilterDate()
  num.keep4 <- cruzDasSightFilterCruise()
  num.keep5 <- cruzDasSightFilterTrunc()

  num.keep.all <- 1:nrow(das.sight)
  num.keep <- num.keep.all[num.keep.all %in% num.keep1 &
                             num.keep.all %in% num.keep2 &
                             num.keep.all %in% num.keep3 &
                             num.keep.all %in% num.keep4 &
                             num.keep.all %in% num.keep5]
  das.sight.filt <- das.sight %>% slice(num.keep)

  # If plotting selected mammals, check that all selected still have sightings
  if (sp.selection) {
    sp.codes.none <- base::setdiff(sp.codes, das.sight.filt$Sp)
    validate(
      need(length(sp.codes.none) == 0,
           paste("The following species code(s) does (do) not",
                 "have any sightings that match the given filters:",
                 paste(sp.codes.none, collapse = ", ")))
    )
  }

  list(das.sight = das.sight.filt, sight.type = sight.type, sp.codes = sp.codes,
       sp.selection = sp.selection)
})


###############################################################################
### Helper functions that filter sightings data by single property

.func_sight_filt_validate <- function(x, x.txt) {
  validate(
    need(x, paste("No sightings match the given", x.txt, "filters"))
  )

  x
}

#------------------------------------------------------------------------------
# On/off effort
cruzDasSightFilterEffort <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight
  effort.val <- switch(
    as.numeric(input$das_sightings_effort), c(0, 1), 1, 0
  )
  ndx.keep <- which(as.numeric(das.sight$OnEffort) %in% effort.val)

  # validate(
  #   need(ndx.keep, "No sightings match the given on/off effort filters")
  # )
  #
  # ndx.keep
  .func_sight_filt_validate(ndx.keep, "on/off effort")
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

  # validate(
  #   need(ndx.keep, "No sightings match the given Beaufort filters")
  # )
  #
  # ndx.keep
  .func_sight_filt_validate(ndx.keep, "Beaufort")
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

  # validate(
  #   need(ndx.keep, "No sightings match the given date filter")
  # )
  #
  # ndx.keep
  .func_sight_filt_validate(ndx.keep, "date")
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

  # validate(
  #   need(ndx.keep, "No sightings match the given Cruise number filter")
  # )
  #
  # ndx.keep
  .func_sight_filt_validate(ndx.keep, "cruise number")
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

    # validate(
    #   need(ndx.keep,
    #        "No sightings match the given truncation (perpendicular distance) filter")
    # )
    #
    # ndx.keep
    .func_sight_filt_validate(ndx.keep, "truncation (perpendicular distance)")
  }
})

###############################################################################
