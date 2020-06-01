# File 3 of processing effort

###############################################################################
# Functions called in draw_setVals.R

### Final effort function - gets filtered data from cruzDasEffortFilter()
cruzDasEffortRange <- reactive({
  das.eff.lines <- cruzDasEffortFilter()

  # Check for any effort lines with NA coordinates
  ll.na <- sum(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$st_lon) |
                 is.na(das.eff.lines$end_lat) | is.na(das.eff.lines$end_lon))
  validate(
    need(ll.na == 0,
         "Error processing effort line positions - please report this as an issue")
  )

  # Remove any effort lines with both st and end points outside map range
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  das.eff.lines.range <- das.eff.lines %>%
    filter(between(.data$st_lat, lat.range[1], lat.range[2]),
           between(.data$st_lon, lon.range[1], lon.range[2]),
           between(.data$end_lat, lat.range[1], lat.range[2]),
           between(.data$end_lon, lon.range[1], lon.range[2]))

  validate(
    need(nrow(das.eff.lines.range) > 0,
         "No effort lines are completely within the map boundaries")
  )

  # Return
  das.eff.lines.range
})



### Get effort plotting colors and line widths
cruzDasEffortParams <- reactive({
  if (input$das_effort == 2) {
    ## If simplified effort, simple results
    eff.col <- input$das_effort_simp_col
    eff.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    ## If detailed effort, not as simple
    das.eff.lines <- cruzDasEffortRange()

    # Color code by Bft or SNF
    if (input$das_effort_det_byBft) {
      bft.cols <- input$das_effort_det_bft_col
      bft.col.num <- cruzDasEffortFilterBeaufortVal()[2] + 1
      validate(
        need(length(bft.cols) >= bft.col.num,
             paste("Please choose at least", bft.col.num,
                   "colors, one for each possible Beaufort value between 0",
                   "and the specified maximum Beaufort"))
      )

      eff.col <- bft.cols[das.eff.lines$Bft + 1]
      eff.lwd <- input$das_effort_det_bft_lwd

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
