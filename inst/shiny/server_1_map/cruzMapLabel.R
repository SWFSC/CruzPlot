# Processing for Label tab of Create and Save Map tab

# Return title label, font, and size
cruzMapLabelTitle <- reactive({
  lab <- input$label.title
  fam <- font.family.vals[as.numeric(input$label.title.font)]
  cex <- input$label.title.size

  list(lab = lab, fam = fam, cex = cex)
})

#	Return axes labels (lon and lat), font, and size
cruzMapLabelAxes <- reactive({
  lab.lon <- input$label.axis.lon
  lab.lat <- input$label.axis.lat
  fam <- font.family.vals[as.numeric(input$label.axis.font)]
  cex <- input$label.axis.size

  list(lab.lon = lab.lon, lab.lat = lab.lat, fam = fam, cex = cex)
})
