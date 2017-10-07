# cruzMapRiver for CruzPlot by Sam Woodman
#   cruzMapRiver() returns river data, adjusted for world2 map if necessary


cruzMapRiver <- reactive({
  world2 <- cruz.map.range$world2
  rivs <- map("rivers", plot = F)
  if(world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)
  
  return(rivs)
})