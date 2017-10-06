# cruzMapGrid for CruzPlot by Sam Woodman
#   cruzMapGrid() returns grid line parameters

cruzMapGrid <- reactive({
  grid.col <- input$grid.line.color
  grid.lwd <- input$grid.line.width
  grid.lty <- input$grid.line.type
  
  return(list(col = grid.col, lwd = grid.lwd, lty = grid.lty))
})