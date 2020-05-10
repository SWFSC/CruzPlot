# cruzMapName for CruzPlot by Sam Woodman
#   cruzMapName() returns list of map name and regions to be plotted (if world2 map)


observeEvent(input$map.replot, {
  world2 <- cruz.map.range$world2
  hires <- input$resolution == 2
  
  m <- ifelse(hires, "Hires", "")
  m <- ifelse(world2, paste("world2", m, sep = ""), paste("world", m, sep = ""))
  
  reg.toplot <- NULL
  if(world2) {
    #regions.rm and regions.rm.hires are created in server file
    if(hires) reg.toplot <- regions.rm.hires 
    else reg.toplot <- regions.rm 
  }
  
  cruz.map.range$map.name <- list(m, reg.toplot)
}, ignoreNULL = FALSE, priority = 8)