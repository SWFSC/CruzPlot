### drawMap for CruzPlot
## Creates window and then plots water and land (map)
## Plots scale bar, tick marks/labels, map labels, and grid lines as specified

###############################################################################
# Range and map window triggers for redrawing the map
input$map.replot
input$map.size


###############################################################################
#------------------------------------------------------------------------------
### Window
mar1 <- ifelse(nchar(axes.info$lab.lon) > 0, 7, 3)
mar2 <- ifelse(nchar(axes.info$lab.lat) > 0, 7, 5)
mar3 <- ifelse(nchar(title.info$lab)    > 0, 7, 2)

map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
    mar = c(mar1, mar2, mar3, 4))
x.1 <- map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2],
           mar = c(mar1, mar2, mar3, 4))
param <- cruzMapParam()$param.unit

#------------------------------------------------------------------------------
### Water
rect(param[1], param[3], param[2], param[4], col = map.water.col[[1]])

# Depth
if (map.water.col[[2]][1] != 0)
  plot(map.water.col[[2]], image = TRUE, land = TRUE,
       lwd = 0.0, axes = FALSE, xlab = NA, ylab = NA,
       bpal = list(c(0, max(map.water.col[[2]]), "grey"),
                   c(min(map.water.col[[2]]), 0, bathy.col)))

#------------------------------------------------------------------------------
### Land
# Coastline
if (input$coast) {
  validate(
    need(!is.null(map.coastline),
         message = "Please input a valid coastline file")
  )

  polygon(x = map.coastline$lon, y = map.coastline$lat, col = map.land.col)
  lines(x = map.coastline$lon, y = map.coastline$lat)
}

# Map from map package
if (!input$coast) {
  map(map.name[[1]], regions = map.name[[2]],
      xlim = lon.range[1:2], ylim = lat.range[1:2],
      fill = TRUE, col = map.land.col, add = TRUE)
}

#------------------------------------------------------------------------------
### Rivers and Lakes
if (input$color_lakes_rivers) map(map.river, col = map.water.col[[1]],
                                  add = TRUE)

graphics::box()

#------------------------------------------------------------------------------
### Tick marks and labels
if (input$tick) {
  # Draw major and minor tick marks
  if (tick.lon.bool$bot[1]) {
    axis(1, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
    axis(1, at = tick.lon$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *0.4*tick.param$len)
  }
  if (tick.lat.bool$left[1]) {
    axis(2, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
    axis(2, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }
  if (tick.lon.bool$top[1]) {
    axis(3, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
    axis(3, at = tick.lon$min, labels = FALSE, lwd = 0,  lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }
  if (tick.lat.bool$right[1]) {
    axis(4, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
    axis(4, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1,
         tcl = par("tcl") * 0.4*tick.param$len)
  }

  # Draw tick labels
  if (tick.lon.bool$bot[2])
    axis(1, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
         cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lat.bool$left[2])
    axis(2, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
         las = 1, cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lon.bool$top[2])
    axis(3, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE,
         cex.axis = tick.param$scale, family = tick.param$font)
  if (tick.lat.bool$right[2])
    axis(4, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE,
         las = 1, cex.axis = tick.param$scale, family = tick.param$font)
}

#------------------------------------------------------------------------------
### Grid lines
if (input$grid) {
  abline(v = tick.lon$maj, col = grid.param$col, lwd = grid.param$lwd,
         lty = as.numeric(grid.param$lty))
  abline(h = tick.lat$maj, col = grid.param$col, lwd = grid.param$lwd,
         lty = as.numeric(grid.param$lty))
}

#------------------------------------------------------------------------------
### Scale bar
if (input$bar) {
  lines(c(scale.bar$x1, scale.bar$x2), c(scale.bar$y, scale.bar$y),
        lwd = scale.bar$lwd)
  text(mean(c(scale.bar$x1, scale.bar$x2)),
       scale.bar$y-0.04*abs(lat.range[2]-lat.range[1]),
       paste(scale.bar$len, scale.bar$units.str))
}

#------------------------------------------------------------------------------
### Labels
# Title
if (!is.null(title.info$lab)) {
  title(main = title.info$lab, line = 3, family = title.info$fam,
        cex.main = title.info$cex)
}

# Longitude axis
if (!is.null(axes.info$lab.lon)) {
  title(xlab = axes.info$lab.lon, family = axes.info$fam,
        cex.lab = axes.info$cex)
}
# Latitude axis
if (!is.null(axes.info$lab.lat)) {
  title(ylab = axes.info$lab.lat, family = axes.info$fam,
        cex.lab = axes.info$cex, line = 4)
}

#------------------------------------------------------------------------------
### Transect lines
# Not in drawData since this isn't DAS data
if (input$planned_transects_plot) {
  if (anyNA(planned_transects_class2())) {
    # No class2
    for (i in pltrans.list) {
      for (k in i) {
        graphics::lines(
          x = k[[1]], y = k[[2]], col = k[[3]], lty = k[[4]], lwd = pltrans.lwd
        )
      }
    }

  } else {
    # Yes class 2
    for (i in pltrans.list) {
      for (j in i) {
        for (k in j) {
          graphics::lines(
            x = k[[1]], y = k[[2]], col = k[[3]], lty = k[[4]], lwd = pltrans.lwd
          )
        }
      }
    }
  }
}

###############################################################################
