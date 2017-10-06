### drawMap_setVals
## Set values and call reactive functions for drawMap.R

### Call reactive functions and set values
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range
world2 <- cruz.map.range$world2
source('CruzPlot Files/errorMapRange.R', local=TRUE, echo=FALSE)

map.name <- cruz.map.range$map.name
map.water.col <- cruzMapColorWater()
map.land.col <- cruzMapColorLand()
if(input$color_lakes_rivers) map.river <- cruzMapRiver()

map.coastline <- NULL
if(input$coast & !is.null(cruz.list$coastline)) 
  map.coastline <- cruz.list$coastline

if(input$bar) {
  scale.bar <- cruzMapScaleBar()
  source('CruzPlot Files/errorScaleBar.R', local=TRUE, echo=FALSE)
}

if(input$tick || input$grid) {
  source('CruzPlot Files/errorTick.R', local=TRUE, echo=FALSE)
  tick.lon <- cruzMapIntervalLon()
  tick.lat <- cruzMapIntervalLat()
}
if(input$tick) {
  tick.lon.bool <- cruzMapTickLonBool()
  tick.lat.bool <- cruzMapTickLatBool()
  tick.lon$label <- cruzMapTickLonLab()
  tick.lat$label <- cruzMapTickLatLab()
  tick.param <- cruzMapTickParam()
}
if(input$grid) {
  grid.param <- cruzMapGrid()
}

source('CruzPlot Files/errorLabel.R', local=TRUE, echo=FALSE)
title.info <- cruzMapLabelTitle()
axes.info <- cruzMapLabelAxes()

# Planned transects
if(input$planned_transects_plot) {
  transects.all <- cruz.list$planned.transects
  transects.cols <- input$planned_transects_color
  transects.lw <- str_split(gsub(" ", "", input$planned_transects_lw), ",")
  transects.lw <- suppressWarnings(as.numeric(unlist(transects.lw)))
  transects.toplot.names.which <- as.numeric(input$planned_transects_toplot)
  
  validate(
    need(!is.null(transects.all), "Please load planned transects"), 
    need(!anyNA(transects.lw), "Please enter valid transect line widths")
    )
  validate(
    need(length(transects.cols) <= length(transects.toplot.names.which), 
         paste("The number of planned transect colors selected is greater", 
               "than the number of planned transects selected")), 
    need(length(transects.lw) <= length(transects.toplot.names.which), 
         paste("The number of planned transect line widths is greater", 
               "than the number of planned transects selected"))
  )
  
  transects.toplot.names  <- transects.names()[transects.toplot.names.which]
  transects.toplot <- transects.all[transects.all$name %in% transects.toplot.names,]
}
