### drawMap_setVals
## Set values and call reactive functions for drawMap.R

### Call reactive functions and set values
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range
world2 <- cruz.map.range$world2
source("errorMapRange.R", local = TRUE, chdir = TRUE)

map.name <- cruz.map.range$map.name
map.water.col <- cruzMapColorWater()
map.land.col <- cruzMapColorLand()
if (input$color_lakes_rivers) map.river <- cruzMapRiver()

map.coastline <- NULL
if (input$coast & !is.null(cruz.list$coastline)) 
  map.coastline <- cruz.list$coastline

if (input$bar) {
  scale.bar <- cruzMapScaleBar()
  source("errorScaleBar.R", local = TRUE, chdir = TRUE)
}

if (input$tick || input$grid) {
  source("errorTick.R", local = TRUE, chdir = TRUE)
  tick.lon <- cruzMapIntervalLon()
  tick.lat <- cruzMapIntervalLat()
}
if (input$tick) {
  tick.lon.bool <- cruzMapTickLonBool()
  tick.lat.bool <- cruzMapTickLatBool()
  tick.lon$label <- cruzMapTickLonLab()
  tick.lat$label <- cruzMapTickLatLab()
  tick.param <- cruzMapTickParam()
}
if (input$grid) {
  grid.param <- cruzMapGrid()
}

source("errorLabel.R", local = TRUE, chdir = TRUE)
title.info <- cruzMapLabelTitle()
axes.info <- cruzMapLabelAxes()

# Planned transects
if (input$planned_transects_plot) {
  validate(
    need(input$planned_transects_toplot, 
         "Please select at least one class of planned transects to plot")
  )
  #So that renderUI()'s can catch up
  req(input$planned_transects_color, input$planned_transects_lty) 
  
  # Get user inputs
  pltrans <- cruz.list$planned.transects
  pltrans.which <- as.numeric(input$planned_transects_toplot)
  pltrans.which2 <- as.numeric(input$planned_transects_toplot2)
  pltrans.colors <- input$planned_transects_color
  pltrans.lty <- as.numeric(input$planned_transects_lty)
  pltrans.lwd <- input$planned_transects_lwd
  
  # Process user inputs
  if (length(pltrans.colors) == 1) {
    pltrans.colors <- rep(pltrans.colors, length(pltrans.which))
  }
  
  validate(
    need(length(pltrans.colors) == length(pltrans.which), 
         paste("The number of selected planned transect colors must either be", 
               "1 or equal to than the number of selected planned transects"))
  )
  
  pltrans.class1 <- planned_transects_class1()[pltrans.which]
  names(pltrans.colors) <- pltrans.class1
  
  pltrans <- dplyr::filter(pltrans, class1 %in% pltrans.class1)
  
  if (anyNA(planned_transects_class2())) {
    # Class 2 was not specified
    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)
      
      lapply(unique(x$num), function(k) {
        x <- dplyr::filter(x, num == k)
        if (nrow(x) == 0) {
          NULL
        } else if (nrow(x) == 1){
          validate(need(FALSE, "Error in planned transect processing"))
        } else {
          list(x$lon, x$lat, unname(pltrans.colors[as.character(i)]), pltrans.lty)
        }
      })
    })
    
  } else {
    # Class 2 was specified
    validate(
      need(pltrans.which2, 
           "Please select at least one class 2 type to plot")
    )
    
    pltrans.class2 <- planned_transects_class2()[pltrans.which2]
    pltrans <- dplyr::filter(pltrans, class2 %in% pltrans.class2)
    
    if (length(pltrans.lty) == 1) {
      pltrans.lty <- rep(pltrans.lty, length(pltrans.class2))
    }
    validate(
      need(length(pltrans.lty) == length(pltrans.class2),
           paste("The number of selecetd planned transect line types must either be",
                 "1 or equal to than the number unique class 2 values"))
    )
    names(pltrans.lty) <- pltrans.class2
    
    pltrans.list <- lapply(pltrans.class1, function(i) {
      x <- dplyr::filter(pltrans, class1 == i)
      
      lapply(pltrans.class2, function(j) {
        x <- dplyr::filter(x, class2 == j)
        
        lapply(unique(x$num), function(k) {
          x <- dplyr::filter(x, num == k)
          if (nrow(x) == 0) {
            NULL
          } else if (nrow(x) == 1){
            validate(need(FALSE, "Error in planned transect processing"))
          } else {
            list(x$lon, x$lat, unname(pltrans.colors[as.character(i)]), unname(pltrans.lty[as.character(j)]))
          }
        })
      })
    })
  }
  
  
}
