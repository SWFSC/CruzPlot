# CruzPlot outputs

#------------------------------------------------------------------------------
# output$manual_pdf <- renderUI({
#   tags$iframe(style = "height:850px; width:100%", src = "CruzPlot_Manual_app.pdf")
# })


#------------------------------------------------------------------------------
### Planned transects outputs
output$planned_transects_text <- renderText({
  planned_transects()
})

output$planned_transects_remove_text <- renderText({
  planned_transects_remove()
})


#------------------------------------------------------------------------------
### Non-DAS outputs
output$cruzNonDasLoaded <- renderDataTable({
  df <- cruz.list$ndas.df
  req(df)
  row.names(df) <- 1:nrow(df)

  df
}, options = list(dom = 't'), rownames = TRUE)

output$cruzNonDasAdd_text <- renderText({
  cruzNonDasAdd()
})

output$cruzNonDasFile_LonLat_text <- renderText({
  cruzNonDasFile_LonLat()
  return("")
})

output$cruzNonDasRemove_text <- renderText({
  cruzNonDasRemove()
})


#------------------------------------------------------------------------------
### DAS tabular output
output$das_out_sight_table <- renderTable({ cruzDasOutSight_Table() })
output$cruzDasOutSight_Save_text <- renderText({ cruzDasOutSight_Save() })

output$das_out_effort_table <- renderTable({ cruzDasOutEffort_Table() })
output$cruzDasOutEffort_Save_text <- renderText({ cruzDasOutEffort_Save() })


#------------------------------------------------------------------------------
# Plot Map
output$plot1 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot2 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot3 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot4 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot5 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot6 <- renderPlot({
  plotMap()()
  plotInteractive()()
})
output$plot7 <- renderPlot({
  plotMap()()
  plotInteractive()()
})


# Plot Map pt 2
output$plot1.ui <- renderUI({
  plotOutput("plot1", height = map.height.ui())
})
output$plot2.ui <- renderUI({
  plotOutput("plot2", height = map.height.ui())
})
output$plot3.ui <- renderUI({
  plotOutput("plot3", height = map.height.ui(), click = "sight_click")
})
output$plot4.ui <- renderUI({
  plotOutput("plot4", height = map.height.ui(), hover  = "sight_hover")
})
output$plot5.ui <- renderUI({
  plotOutput("plot5", height = map.height.ui(), click = "effort_click")
})
output$plot6.ui <- renderUI({
  plotOutput("plot6", height = map.height.ui(), hover = "effort_hover")
})
output$plot7.ui <- renderUI({
  plotOutput("plot7", height = map.height.ui())
})


#------------------------------------------------------------------------------
# Display color/formatting options
# slight bug: display does not occur when window is resized
output$plotDisplay <- renderPlot({
  cruzDisplaySymbolProp()
})


#------------------------------------------------------------------------------
# Display mammals, turtles, and all species codes
output$sp1 <- renderDataTable({
  sp.mammals <- cruzSpeciesMammals()
  names(sp.mammals) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.mammals
})
output$sp2 <- renderDataTable({ # Turtles
  sp.turtles <- cruzSpeciesTurtles()
  names(sp.turtles) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.turtles
})
output$sp3 <- renderDataTable({ # All
  sp.all <- cruzSpecies()
  names(sp.all) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.all
})

#------------------------------------------------------------------------------
