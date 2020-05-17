# cruzDasSightFilter for CruzPlot - step 2 of processing species data
#   cruiseDasSightFilter() pulls individual filters together

#   cruzDasSightFilterEffort() returns a logical indicating which rows satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns a logical indicating which rows satisfy the beaufort filter
#   cruzDasSightFilterDate() returns a logical indicating which rows satisfy the date filter
#   cruzDasSightFilterCruise() returns a logical indicating which rows are from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns a logical indicating which rows are within the given truncation distance


###############################################################################
### Top-level function for filtering
cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightSpecies()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  keep1 <- cruzDasSightFilterEffort()
  keep2 <- cruzDasSightFilterBeaufort()
  keep3 <- cruzDasSightFilterDate()
  keep4 <- cruzDasSightFilterCruise()
  keep5 <- cruzDasSightFilterTrunc()

  num.keep <- which(keep1 & keep2 & keep3 & keep4 & keep5)
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
    need(any(x), paste("No sightings match the given", x.txt, "filters"))
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

  keep <- as.numeric(das.sight$OnEffort) %in% effort.val
  .func_sight_filt_validate(keep, "on/off effort")
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

  keep <- between(das.sight$Bft, bft.min, bft.max)
  .func_sight_filt_validate(keep, "Beaufort")
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

  keep <- between(
    as.Date(das.sight$DateTime), date.vals[1], date.vals[2]
  )
  .func_sight_filt_validate(keep, "date")
})

#------------------------------------------------------------------------------
# Cruise numbers
cruzDasSightFilterCruise <- reactive({
  das.sight <- cruzDasSightSpecies()$das.sight

  cruise.vals <- if (identical(input$das_sight_cruiseNum, "")) {
    unique(das.sight$Cruise)
  } else {
    as.numeric(unlist(strsplit(input$das_sight_cruiseNum, split = ", ")))
  }

  validate(
    need(all(cruise.vals %in% das.sight$Cruise),
         paste("There are no selected sightings in the following cruises:",
               paste(base::setdiff(cruise.vals, das.sight$Cruise), collapse = ", ")))
  )

  keep <- das.sight$Cruise %in% cruise.vals
  .func_sight_filt_validate(keep, "cruise number")
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
    keep <- das.sight$PerpDistKm <= pdist.val
    validate(
      need(any(keep),
           "There are no selected sightings within the given truncation distance")
    )
    .func_sight_filt_validate(keep, "truncation (perpendicular distance)")
  }
})

###############################################################################
