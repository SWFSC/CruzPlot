# File 3 of processing effort

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

  cruz.list$das.eff.filt <- das.eff.lines.nona

  das.eff.lines.nona
})

### Get effort plotting colors and line widths
cruzDasEffortParams <- reactive({
  if (input$das_effort == 2) {
    ## If simplified effort, simple results
    eff.col <- input$das_effort_simp_col
    eff.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    ## If detailed effort, not as simple
    das.eff.lines <- cruzDasEffort()

    # Color code by Bft or SNF
    if (input$das_effort_det_byBft) {
      bft.cols <- input$das_effort_det_bft_col
      validate(
        need(length(bft.cols) == 10,
             "Please choose exactly 10 colors, one for each possible Beaufort value")
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
