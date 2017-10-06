# cruzDasEffortLegend() for CruzPlot 
#   Returns parameters for effort legend
#   Tim Gerrodette Oct 2015


cruzDasEffortLegend <- reactive({
  
  eff.leg.pos <- input$eff_legend_pos
  if(eff.leg.pos == 1) {
    eff.leg.x = as.numeric(input$eff.legend.lon)
    eff.leg.y = as.numeric(input$eff.legend.lat)
  } else {
    eff.leg.x <- eff.leg.pos
    eff.leg.y <- NULL
  }
  
  eff.leg.title <- "Effort by Beaufort"
  eff.leg.lab <- c(0:5, "6 - 9")
  eff.leg.col <- c("darkblue", "dodgerblue2", "forestgreen", "greenyellow", 
                   "orange", "darkorange3", "red", "red", "red", "red")
  eff.leg.lwd <- 2
  
  eff.leg.bty <- ifelse(input$eff.legend.boxCol == 1, "n", "o")
  eff.leg.box.col <- ifelse(input$eff.legend.boxCol == 2, NA, "black")
  eff.leg.box.lwd <- ifelse(input$eff.legend.boxCol == 2, 0, 1)
  eff.leg.box.cex <- input$eff.legend.textSize
  
  
  
  font.fam <- font.family[as.numeric(input$eff.legend.font)]
  
  
  return(list(eff.leg.x = eff.leg.x, eff.leg.y = eff.leg.y, 
              eff.leg.title = eff.leg.title, eff.leg.lab = eff.leg.lab, 
              eff.leg.col = eff.leg.col, eff.leg.lwd = eff.leg.lwd, 
              eff.leg.bty = eff.leg.bty, eff.leg.box.col = eff.leg.box.col, 
              eff.leg.box.lwd = eff.leg.box.lwd, 
              eff.leg.box.cex = eff.leg.box.cex, font.fam = font.fam))
})
