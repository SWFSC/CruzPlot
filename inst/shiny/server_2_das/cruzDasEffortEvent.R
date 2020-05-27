# cruzDasEffort for CruzPlot - file 1 of effort processing
#   cruzDasEffortEvent() returns selected and filtered effort data
#   Do filter stuff in cruzDasEffortFilter()

###############################################################################
cruzDasEffortEvent <- reactive({
  req(input$das_effort != 0)
  das.proc <- req(cruz.list$das.data)
  cruz.list$das.eff.filt <- NULL

  eff.events <- if (input$das_effort == 2) c("R", "E") else c("R", "V", "E")

  das.eff <- das.proc %>%
    filter(.data$OnEffort | .data$Event == "E",
           .data$Event %in% eff.events)

  validate(
    need(sum(das.eff$Event == "R") == sum(das.eff$Event == "E"),
         "There are not an equal number of R and E events in the data") %then%
      need(all((which(das.eff$Event == "E") - which(das.eff$Event == "R")) > 0),
           "R and E events do not properly alternate"),
    need(identical(tail(das.eff$Event, 1), "E"),
         "The DAS data effort must end with an E event")
  )

  # For simplified effort, we don't need Beaufort values
  # For detailed effort, what to do about R-V 'segments'?
  # Each continuous effort section will end with an E event
  das.eff.lines <- das.eff %>%
    mutate(st_lat = .data$Lat,
           st_lon = .data$Lon,
           end_lat = c(.data$Lat[-1], NA),
           end_lon = c(.data$Lon[-1], NA)) %>%
    filter(.data$Event != "E")

  das.eff.lines
})

###############################################################################
