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
  df <- req(cruz.list$ndas.df)
  row.names(df) <- 1:nrow(df)

  df
}, options = list(dom = 't'), rownames = TRUE)

output$cruzNonDasAdd_text <- renderText({
  cruzNonDasAdd()
})

output$cruzNonDasFile_LonLat_text <- renderText({
  cruzNonDasFile_LonLat()
  ""
})

output$cruzNonDasRemove_text <- renderText({
  cruzNonDasRemove()
})


#------------------------------------------------------------------------------
### DAS

# Loading DAS file(s)
output$das_file_load_text <- renderText(das_file_load())

output$das_loaded_text <- renderText({
  req(cruz.list$das.data)
  paste(
    "The following DAS files are loaded:",
    paste(cruz.list$das.data.name, collapse = ", ")
  )
})

# Tabular output
output$das_out_sight_table <- renderTable(cruzDasOutSight_Table())
output$cruzDasOutSight_Save_text <- renderText(cruzDasOutSight_Save())

output$das_out_effort_table <- renderTable(cruzDasOutEffort_Table())
output$cruzDasOutEffort_Save_text <- renderText(cruzDasOutEffort_Save())


#------------------------------------------------------------------------------
# Plot Map
output$plot1 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot2 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot3 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot4 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot5 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot6 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")
output$plot7 <- renderPlot({
  plotMap()()
  plotInteractive()()
}, height = map.height, units = "px")


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
