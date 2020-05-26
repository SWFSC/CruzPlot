# cruzDasInteractive for CruzPlot
#   Handles events realted to labelling sightings interactively
#   cruzInteractiveSight handles interactive labels for sightings
#   cruzInteractiveEffort handles interactive labels for effort


sight <- reactiveValues(
  click = NULL,
  hover = NULL,
  lab = NULL,
  miss = FALSE
)

effort <- reactiveValues(
  click = NULL,
  hover = NULL,
  lab = NULL,
  hover.lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)

# If DAS file changes, reset interactive everything
observeEvent(cruz.list$das.data, {
  # Remove all points
  sight$click <- NULL
  sight$hover <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE

  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

# If sightings selections change, reset interactive sighting things
observe({
  # Sightings to plot
  input$das_sighting_type
  input$das_sighting_code_1_all
  input$das_sighting_code_2_all
  input$das_sighting_events
  input$das_sighting_code_1
  input$das_sighting_code_2

  # Filters
  input$das_sight_effort
  input$das_sight_cp
  input$das_sight_snf
  input$das_sight_minBft
  input$das_sight_maxBft
  input$das_sight_dateRange
  input$das_sight_cruise
  input$das_sight_trunc

  sight$click <- NULL
  sight$hover <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
})

# If effort selections change, reset interactive effort things
observe({
  # Effort to plot
  input$das_effort
  input$das_effort_cp
  input$das_effort_snf

  # Filters
  input$das_effort_filter_same
  input$das_effort_minBft
  input$das_effort_maxBft
  input$das_effort_dateRange
  input$das_effort_cruise

  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})
