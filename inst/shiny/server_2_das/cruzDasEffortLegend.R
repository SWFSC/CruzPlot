# eturns parameters for effort legend
cruzDasEffortLegend <- reactive({

  eff.leg.pos <- input$eff_legend_pos
  if (eff.leg.pos == 1) {
    validate(
      need(!is.na(input$eff_legend_lat), "Please enter a valid effort legend latitude value"),
      need(!is.na(input$eff_legend_lon), "Please enter a valid effort legend longitude value")
    )
    eff.leg.x = input$eff_legend_lon
    eff.leg.y = input$eff_legend_lat

  } else {
    eff.leg.x <- eff.leg.pos
    eff.leg.y <- NULL
  }

  bft.range <- cruzDasEffortFilterBeaufortVal()
  bft.which <- (bft.range[1]:bft.range[2]) + 1

  eff.leg.title <- if (input$eff_legend_title == "") NULL else input$eff_legend_title
  eff.leg.lab <- (0:9)[bft.which]
  eff.leg.col <- input$das_effort_det_bft[bft.which]
  eff.leg.lwd <- 2

  eff.leg.bty <- ifelse(input$eff_legend_boxCol == 1, "n", "o")
  eff.leg.box.col <- ifelse(input$eff_legend_boxCol == 2, NA, "black")
  eff.leg.box.lwd <- ifelse(input$eff_legend_boxCol == 2, 0, 1)
  eff.leg.box.cex <- input$eff_legend_textSize


  font.fam <- font.family.vals[as.numeric(input$eff_legend_font)]


  list(
    eff.leg.x = eff.leg.x, eff.leg.y = eff.leg.y,
    eff.leg.title = eff.leg.title, eff.leg.lab = eff.leg.lab,
    eff.leg.col = eff.leg.col, eff.leg.lwd = eff.leg.lwd,
    eff.leg.bty = eff.leg.bty, eff.leg.box.col = eff.leg.box.col,
    eff.leg.box.lwd = eff.leg.box.lwd,
    eff.leg.box.cex = eff.leg.box.cex, font.fam = font.fam
  )
})
