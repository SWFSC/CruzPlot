# cruzMapLabel for CruzPlot by Sam Woodman
#   cruzMapTitle() returns title name, font, and size
#	cruzMapAxes() returns axes name (lon and lat), font, and size


cruzMapLabelTitle <- reactive({
  lab <- input$label.title
  fam <- font.family[as.numeric(input$label.title.font)]
  cex <- input$label.title.size
  
  return(list(lab = lab, fam = fam, cex = cex))
})

cruzMapLabelAxes <- reactive({
  lab.lon <- input$label.axis.lon
  lab.lat <- input$label.axis.lat
  fam <- font.family[as.numeric(input$label.axis.font)]
  cex <- input$label.axis.size
  
  return(list(lab.lon = lab.lon, lab.lat = lab.lat, fam = fam, cex = cex))
})
