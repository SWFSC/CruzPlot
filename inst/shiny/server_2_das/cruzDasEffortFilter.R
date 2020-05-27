# cruzDasEffortFilter for CruzPlot - file 2 of effort processing
#   cruzDasEffortFilter() returns filtered effort data


###############################################################################
observeEvent(input$das_sightings, {
  if (!input$das_sightings) {
    updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
  }
})


###############################################################################
# Top-level function for filtering sighting data
cruzDasEffortFilter <- reactive({
  das.eff.lines <- cruzDasEffortEvent()

  keep1 <- cruzDasEffortFilterMode()
  keep2 <- cruzDasEffortFilterEfftype()
  keep3 <- if (input$das_effort == 3) cruzDasEffortFilterBeaufort() else TRUE
  keep4 <- cruzDasEffortFilterDate()
  keep5 <- cruzDasEffortFilterCruise()

  num.keep <- keep1 & keep2 & keep3 & keep4 & keep5
  das.eff.lines.filt <- das.eff.lines[num.keep, ]

  validate(
    need(nrow(das.eff.lines.filt) > 0,
         "No effort lines match the provided filters")
  )

  cruz.list$das.eff.filt <- das.eff.lines.filt

  das.eff.lines.filt
})


###############################################################################
# Individual filters filter for indicies of data.effort to keep

#------------------------------------------------------------------------------
.func_eff_filt_validate <- function(x, x.txt) {
  if (anyNA(x)) warning(paste("some", x.txt, "filter values were NA"))

  validate(
    need(any(x), paste("No effort lines match the given", x.txt, "filters"))
  )
  x
}


#------------------------------------------------------------------------------
### Closing/passing mode filter
cruzDasEffortFilterMode <- reactive ({
  das.eff.lines <- cruzDasEffortEvent()

  keep <- das.eff.lines$Mode %in% input$das_effort_cp
  .func_eff_filt_validate(keep, "mode (closing/passing)")
})


#------------------------------------------------------------------------------
### S/N/F effort type filter
cruzDasEffortFilterEfftype <- reactive ({
  das.eff.lines <- cruzDasEffortEvent()

  keep <- das.eff.lines$EffType %in% input$das_effort_snf
  .func_eff_filt_validate(keep, "effort type (standard/non-standard/fine)")
})


#------------------------------------------------------------------------------
### Beaufort filter
cruzDasEffortFilterBeaufortVal <- reactive({
  # Separate function to be used in Legend
  if (input$das_effort_filter_same) {
    eff.bft.min <- as.numeric(input$das_sight_minBft)
    eff.bft.max <- as.numeric(input$das_sight_maxBft)
  } else {
    eff.bft.min <- as.numeric(input$das_effort_minBft)
    eff.bft.max <- as.numeric(input$das_effort_maxBft)
  }

  c(eff.bft.min, eff.bft.max)
})

cruzDasEffortFilterBeaufort <- reactive ({
  das.eff.lines <- cruzDasEffortEvent()
  bft.vals <- cruzDasEffortFilterBeaufortVal()

  keep <- between(das.eff.lines$Bft, bft.vals[1], bft.vals[2])
  keep[is.na(keep)] <- FALSE
  .func_eff_filt_validate(keep, "Beaufort")
})


#------------------------------------------------------------------------------
### Date Filter
cruzDasEffortFilterDate <- reactive({
  das.eff.lines <- cruzDasEffortEvent()

  eff.date.vals <- if (input$das_effort_filter_same) {
    input$das_sight_dateRange
  } else {
    input$das_effort_dateRange
  }

  validate(
    need(eff.date.vals[1] <= eff.date.vals[2],
         "Minimum date must be less than or equal to maximum date")
  )

  keep <- between(
    as.Date(das.eff.lines$DateTime), eff.date.vals[1], eff.date.vals[2]
  )
  .func_eff_filt_validate(keep, "date")
})


#------------------------------------------------------------------------------
### Cruise number filter
cruzDasEffortFilterCruise <- reactive ({
  das.eff.lines <- cruzDasEffortEvent()

  if (input$das_effort_filter_same) {
    if (is.null(input$das_sight_cruiseNum)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_sight_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_sight_filt_validate(keep, "cruise number") }

  } else {
    if (is.null(input$das_effort_cruise)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_effort_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_sight_filt_validate(keep, "cruise number")}
  }
})


###############################################################################
