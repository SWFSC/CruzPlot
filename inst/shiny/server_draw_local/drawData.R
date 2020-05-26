# drawData for CruzPlot
#   Plots selected sightings, legends, and effort lines from DAS file
#   Plots non-DAS data (lines or points)


### Plot non-DAS data
if (isTruthy(data.ndas)) {
  # Plot lines
  data.ndas.l <- data.ndas[[1]]
  if (length(data.ndas.l) > 0) {
    for(i in seq_along(data.ndas.l)) {
      data.ndas.l.curr <- data.ndas.l[[i]]
      lines(x = data.ndas.l.curr$x, y = data.ndas.l.curr$y,
            lty = data.ndas.l.curr$type, col = data.ndas.l.curr$col,
            lwd = data.ndas.l.curr$lwd)
    }
  }

  # Plot points
  data.ndas.p <- data.ndas[[2]]
  if (length(data.ndas.p) > 0) {
    for(j in seq_along(data.ndas.p)) {
      data.ndas.p.curr <- data.ndas.p[[j]]
      points(x = data.ndas.p.curr$x, y = data.ndas.p.curr$y,
             pch = data.ndas.p.curr$type, col = data.ndas.p.curr$col,
             cex = data.ndas.p.curr$cex, lwd = data.ndas.p.curr$lwd)
    }
  }
}


### Plot DAS data
if (isTruthy(cruz.list$das.data)) {
  ## Plot effort segments
  if (input$das_effort != "1")
    segments(x0 = das.eff.lines$st_lon, x1 = das.eff.lines$end_lon,
             y0 = das.eff.lines$st_lat, y1 = das.eff.lines$end_lat,
             col = eff.col, lwd = eff.lwd)


  ## Plot sighting points and legend
  if (input$das_sightings) {
    points(das.sight.pt$Lon, das.sight.pt$Lat,
           pch = das.sight.pt$pch, col = das.sight.pt$col,
           cex = das.sight.pt$cex, lwd = das.sight.pt$lwd)

    if (input$das_legend & sight.type < 3) {
      op <- par(family = das.sight.legend$font.fam)
      legend(x = das.sight.legend$leg.x,
             y = das.sight.legend$leg.y,
             legend = das.sight.legend$leg.lab,
             title = das.sight.legend$leg.title,
             pch = das.sight.legend$leg.pch,
             col = das.sight.legend$leg.col,
             pt.cex = das.sight.legend$leg.cex,
             pt.lwd = das.sight.legend$leg.lwd,
             bty = das.sight.legend$leg.bty,
             box.col = das.sight.legend$leg.box.col,
             box.lwd = das.sight.legend$leg.box.lwd,
             cex = das.sight.legend$leg.box.cex,
             bg = "white")
      par(op)
    }
  }


  ## Plot legend for effort by bft
  if (input$eff_legend & input$das_effort_det_byBft & input$das_effort == "3") {
    op <- par(family = data.eff.legend$font.fam)
    legend(x = data.eff.legend$eff.leg.x,
           y = data.eff.legend$eff.leg.y,
           title = data.eff.legend$eff.leg.title,
           legend = data.eff.legend$eff.leg.lab,
           lwd = data.eff.legend$eff.leg.lwd,
           col = data.eff.legend$eff.leg.col,
           bty = data.eff.legend$eff.leg.bty,
           box.col = data.eff.legend$eff.leg.box.col,
           box.lwd = data.eff.legend$eff.leg.box.lwd,
           cex = data.eff.legend$eff.leg.box.cex,
           bg = "white")
    par(op)
  }
}

graphics::box() # Added in case legend takes out some of the map border
