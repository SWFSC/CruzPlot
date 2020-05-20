# cruzDasEffort for CruzPlot
#   cruzDasEffortEvent() returns selected and filtered effort data
#   Do filter stuff in cruzDasEffortFilter()
#   cruzDasEffort() returns selected and filtered effort data
#   cruzDasEffortLines() returns effort line color and line width


# Code sections are in 'chronological' order

###############################################################################
cruzDasEffortEvent <- reactive({
  req(input$das_effort != 0)
  das.proc <- req(cruz.list$das.data)

  ndx.R <- which(das.proc$Event == "R")
  ndx.E <- which(das.proc$Event == "E")
  validate(
    need(length(ndx.R) == length(ndx.E),
         "There are not an equal number of R and E events in the data") %then%
      need(all((ndx.E - ndx.R) > 0),
           "R and E events do not alternate")
  )

  eff.events <- if (input$das_effort == 2) c("R", "E") else c("R", "V", "E")

  das.eff <- das.proc %>%
    filter(.data$OnEffort | .data$Event == "E",
           .data$Event %in% eff.events)

  validate(
    need(identical(tail(das.eff$Event, 1), "E"),
         "The DAS data effort must end with an E event")
  )

  # For simplified effort, we don't need Beaufort values
  # For detailed effort, what to do about R-V 'segments'?
  # browser()
  das.eff.lines <- das.eff %>%
    # select(Event, Lat, Lon, DateTime, Bft) %>%
    mutate(st_lat = .data$Lat,
           st_lon = .data$Lon,
           end_lat = c(.data$Lat[-1], NA),
           end_lon = c(.data$Lon[-1], NA)) %>%
    filter(.data$Event != "E") # effort data will end with an E event

  # event.na <- das.eff.lines$Event[is.na(das.eff.lines$Bft)]
  # list(das.eff.lines, setdiff(event.na, "R"))

  das.eff.lines
})


###############################################################################
# Functions called in draw_setVals.R

### Final effort function - gets filtered data from cruzDasEffortFilter()
cruzDasEffort <- reactive({
  # effort.type <- as.numeric(input$das_effort)
  das.eff.lines <- cruzDasEffortFilter()

  das.eff.lines.nona <- das.eff.lines %>%
    filter(!is.na(das.eff.lines$st_lat),
           !is.na(das.eff.lines$end_lat),
           !is.na(das.eff.lines$st_lon),
           !is.na(das.eff.lines$end_lon))

  if (input$das_effort == 3)
    das.eff.lines.nona <- das.eff.lines.nona %>%
    filter(!is.na(.data$Bft))

  validate(
    need(nrow(das.eff.lines.nona) > 0,
         "No effort lines that match the filters have non-NA values")
  )

  das.eff.lines.nona
})

### Get effort plotting colors and line widths
cruzDasEffortLines <- reactive({
  if (input$das_effort == 2) {
    ## If simplified effort, simple results
    eff.col <- input$das_effort_simp_col
    eff.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    ## If detailed effort, not as simple
    das.eff.lines <- cruzDasEffort()

    # Color code by Bft or SNF
    if (input$das_effort_det_byBft) {
      bft.cols <- c("darkblue", "dodgerblue2", "forestgreen", "greenyellow",
                    "orange", "darkorange3", "red", "red", "red", "red")
      eff.col <- bft.cols[das.eff.lines$Bft + 1]
      eff.lwd <- 2

    } else {
      eff.col <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_col_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_col_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_col_f
      )
      eff.lwd <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_lwd_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_lwd_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_lwd_f
      )
    }
  }

  list(eff.col = eff.col, eff.lwd = eff.lwd)
})

###############################################################################
