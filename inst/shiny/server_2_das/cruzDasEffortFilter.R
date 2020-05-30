# cruzDasEffortFilter for CruzPlot - file 2 of effort processing
#   cruzDasEffortFilter() returns filtered effort data


###############################################################################
observeEvent(input$das_sightings, {
  if (!input$das_sightings) {
    updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
  }
})


###############################################################################
# Top-level function for filtering effort line data
cruzDasEffortFilter <- reactive({
  das.eff.lines <- cruzDasEffortEvent()

  if (input$das_effort_filter_same) {
    validate(
      need(input$das_sightings,
           "Sightings must be plotted to use the sighting filters for effort lines")
    )
  }

  ### Collect logical vectors
  keep1 <- cruzDasEffortFilterMode()
  keep2 <- cruzDasEffortFilterEfftype()
  keep3 <- if (input$das_effort == 3) cruzDasEffortFilterBeaufort() else TRUE
  keep4 <- cruzDasEffortFilterDate()
  keep5 <- cruzDasEffortFilterCruise()

  keep.all <- keep1 & keep2 & keep3 & keep4 & keep5
  keep.all.na <- which(is.na(keep.all))

  ### Show modal if any filter values are NA
  # Can be this simple b/c (NA & F) output is (F)
  if (length(keep.all.na) > 0) {
    table.out <- das.eff.lines %>%
      slice(keep.all.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise)) %>%
      select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
             # File = file_das, #Can take up too much space
             `Line number` = line_num)
    if (input$das_effort != 3) table.out <- table.out %>% select(Bft)

    txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following effort", txt.out, "at least one NA filter value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      easyClose = TRUE,
      footer = "Click anywhere or press any button to close this notice",
      size = "l"
    ))
  }

  keep.all[is.na(keep.all)] <- FALSE
  das.eff.lines.filt <- das.eff.lines[keep.all, ]

  ### Final checks and return
  validate(
    need(sum(is.na(das.eff.lines.filt$Event)) == 0,
         "Error in CruzPlot effort filtering - please report this as an issue") %then%
      need(nrow(das.eff.lines.filt) > 0,
           "No effort lines match the provided filters")
  )

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

  validate(
    need(eff.bft.min <= eff.bft.max,
         "Effort filter: minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  c(eff.bft.min, eff.bft.max)
})

cruzDasEffortFilterBeaufort <- reactive ({
  das.eff.lines <- cruzDasEffortEvent()
  bft.vals <- cruzDasEffortFilterBeaufortVal()

  keep <- if (identical(bft.vals, c(0, 9))) {
    TRUE
  } else {
    between(das.eff.lines$Bft, bft.vals[1], bft.vals[2])
  }
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
         "Effort filter: minimum date must be less than or equal to maximum date")
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
