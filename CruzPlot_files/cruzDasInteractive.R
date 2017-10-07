# cruzDasInteractive for CruzPlot by Sam Woodman
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
  miss = FALSE
)

# If .DAS file changes, reset interactive everything
observeEvent(input$das.file, { 
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
  input$das_sighting_code_1_al2
  input$das.sighting.code.1
  input$das.sighting.code.2
  
  # Filters
  input$das.sightings.effort
  input$das.sight.minBeau
  input$das.sight.maxBeau
  input$das.sight.dateRange
  input$das.sight.cruiseNum
  input$das.sight.cruiseNum
  
  sight$click <- NULL
  sight$hover <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
})

# If effort selections change, reset interactive effort things
observe({
  # Effort to plot
  input$das_effort
  input$das.effort.closePass
  input$effort.std
  
  # Filters
  input$das_effort_filter_same
  input$das.effort.minBeau
  input$das.effort.maxBeau
  input$das.effort.dateRange
  input$das.effort.cruiseNum
  
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})
