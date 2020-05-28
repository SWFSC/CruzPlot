# Initialize interactive reactive values, and reset them when appropriate

###############################################################################
sight <- reactiveValues(
  click = NULL,
  hover = NULL,
  hover.lab = NULL,
  lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)

effort <- reactiveValues(
  click = NULL,
  hover = NULL,
  lab = NULL,
  hover.lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)


###############################################################################
### When changed to a new page or tab: 1) Turn plot to not-interactive,
###   2) reset hover info, and 3) remove miss labels
observe({
  input$tabs
  input$tabset2

  updateRadioButtons(session, "das_sight_interactive", selected = 1)
  updateRadioButtons(session, "das_effort_interactive", selected = 1)

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})


###############################################################################
# Reset as needed

### If DAS file or map range changes, reset interactive everything
observe({
  cruz.list$das.data
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  cruz.map.range$world2

  # Reset all things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If interactive selection changes, remove hover and miss
observeEvent(input$das_sight_interactive, {
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})
observeEvent(input$das_effort_interactive, {
  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If sightings selections change, reset interactive sighting things
observe({
  # Sightings to plot and filters
  input$das_sighting_type
  input$das_sighting_code_1_all
  input$das_sighting_code_2_all
  input$das_sighting_events
  input$das_sighting_code_1
  input$das_sighting_code_2

  input$das_sight_effort
  input$das_sight_cp
  input$das_sight_snf
  input$das_sight_minBft
  input$das_sight_maxBft
  input$das_sight_dateRange
  input$das_sight_cruise
  input$das_sight_trunc

  # Reset sighting things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})


### If effort selections change, reset interactive effort things
observe({
  # Effort to plot and filters
  input$das_effort
  input$das_effort_cp
  input$das_effort_snf

  input$das_effort_filter_same
  input$das_effort_minBft
  input$das_effort_maxBft
  input$das_effort_dateRange
  input$das_effort_cruise

  # Reset effort things
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

###############################################################################
