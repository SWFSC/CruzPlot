### drawMap for CruzPlot by Sam Woodman
## Creates window and then plots water and land (map) 
## Plots scale bar, tick marks/labels, map labels, and grid lines as specified

input$map.replot

### Window
mar1 <- ifelse(nchar(axes.info$lab.lon)>0,7,3)
mar2 <- ifelse(nchar(axes.info$lab.lat)>0,7,5)
mar3 <- ifelse(nchar(title.info$lab)>0,7,2)
# x <<- try(map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2], 
#               mar = c(mar1,mar2,mar3,4)))
map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2], 
    mar = c(mar1,mar2,mar3,4))
x1 <- map(map.name[[1]], xlim = lon.range[1:2], ylim = lat.range[1:2], 
          mar = c(mar1,mar2,mar3,4))
param <- cruzMapParam()$param.unit

### Water
rect(param[1], param[3], param[2], param[4], col = map.water.col[[1]])

# Depth
if(map.water.col[[2]][1] != 0)
  plot(map.water.col[[2]], image = TRUE, land = TRUE, 
       lwd = 0.0, axes = FALSE, xlab = NA, ylab = NA,
       bpal = list(c(0, max(map.water.col[[2]]), "grey"), 
                   c(min(map.water.col[[2]]), 0, bathy.col)))

### Land
# Coastline
if(input$coast) {
  validate(
    need(!is.null(map.coastline),
         message = "Please input a valid coastline file")
  )
  
  polygon(x = map.coastline$lon, y = map.coastline$lat, col = map.land.col)
  lines(x = map.coastline$lon, y = map.coastline$lat)
}

# Map from map package
if(!input$coast) {
  map(map.name[[1]], regions = map.name[[2]], 
      xlim = lon.range[1:2], ylim = lat.range[1:2], 
      fill = TRUE, col = map.land.col, add = TRUE)
}

### Rivers and Lakes
if(input$color_lakes_rivers) map(map.river, col = map.water.col[[1]], 
                                 add = TRUE)

graphics::box()

### Tick marks and labels
if(input$tick) source('CruzPlot Files/drawMapTick.R', local=TRUE, echo=FALSE)

### Grid lines
if(input$grid) {
  abline(v = tick.lon$maj, col = grid.param$col, lwd = grid.param$lwd, 
         lty = as.numeric(grid.param$lty))
  abline(h = tick.lat$maj, col = grid.param$col, lwd = grid.param$lwd, 
         lty = as.numeric(grid.param$lty))
}

### Scale bar
if(input$bar) {
  lines(c(scale.bar$x1, scale.bar$x2), c(scale.bar$y, scale.bar$y), 
        lwd = scale.bar$lwd)
  text(mean(c(scale.bar$x1, scale.bar$x2)), 
       scale.bar$y-0.04*abs(lat.range[2]-lat.range[1]), 
       paste(scale.bar$len, scale.bar$units.str))
}

### Labels
# Title
if(!is.null(title.info$lab)) title(main = title.info$lab, line = 3,
                                   family = title.info$fam,
                                   cex.main = title.info$cex)

# Longitude axis
if(!is.null(axes.info$lab.lon)) title(xlab = axes.info$lab.lon, 
                                      family = axes.info$fam,
                                      cex.lab = axes.info$cex)
# Latitude axis
if(!is.null(axes.info$lab.lat)) title(ylab = axes.info$lab.lat, 
                                      family = axes.info$fam,
                                      cex.lab = axes.info$cex, line = 4)

### Transect lines 
#      Not in drawData since this isn't DAS data
if(input$planned_transects_plot) {
  segments(x0 = transects.toplot$lon1, y0 = transects.toplot$lat1, 
           x1 = transects.toplot$lon2, y1 = transects.toplot$lat2, 
           col = transects.cols, lwd = transects.lw)
}
