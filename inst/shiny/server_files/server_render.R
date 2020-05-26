# CruzPlot outputs

#------------------------------------------------------------------------------
# output$manual_pdf <- renderUI({
#   tags$iframe(style = "height:850px; width:100%", src = "CruzPlot_Manual_app.pdf")
# })


#------------------------------------------------------------------------------
output$bathy_load_text <- renderText(cruzMapBathyLoad())

output$bathy_message_text <- renderText({
  if (isTruthy(cruz.list$bathy.xyz)) {
    "A bathymetry file is loaded"
  } else {
    NULL
  }
})


#------------------------------------------------------------------------------
### Planned transects outputs
output$planned_transects_text <- renderText(planned_transects())

# output$planned_transects_remove_text <- renderText(planned_transects_remove())

output$planned_transects_message <- renderText({
  req(cruz.list$planned.transects)
  "A planned transects file is loaded"
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

# Loading SpCodes file
output$spcodes_user_read_text <- renderText(spcodes_user_read())
output$spcodes_default_read_text <- renderText(spcodes_default_read())

output$spcodes_message <- renderText({
  req(cruz.list$sp.codes.name)

  if (cruz.list$sp.codes.name == "default") {
    "The default SpCodes.dat file is loaded"
  } else {
    paste("The following user-provided species code file is loaded:",
          cruz.list$sp.codes.name)
  }
})


# Text in sightings tab about no SpCodes.data file
output$das_sight_spcodes_message <- renderText({
  validate(
    need(cruz.list$sp.codes,
         "You must load a species code file in the 'Data' window to plot sightings")
  )

  ""
})


# Text with notice(s) about sightings, e.g. NA lat/lons
output$das_sight_message_text <- renderText({
  das.sight <- cruz.list$das.sight.filt

  count.na <- sum(is.na(das.sight$Lat) | is.na(das.sight$Lon))

  if (count.na == 1) {
    paste(
      "There is", count.na,
      "sighting with NA lat/lon values that will not be plotted"
    )
  } else if (count.na > 1) {
    paste(
      "There are", count.na,
      "sightings with NA lat/lon values that will not be plotted"
    )
  } else {
    NULL
  }
})


# Text with notice about effort with NA lat/lon
output$das_effort_message1_text <- renderText({
  das.eff.lines <- req(cruz.list$das.eff.filt)

  ll.na <- sum(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
                 is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon))

  if (ll.na == 1) {
    paste(
      "There is", ll.na,
      "effort line with an NA lat/lon value that will not be plotted"
    )
  } else if (ll.na > 1) {
    paste(
      "There are", ll.na,
      "effort lines with NA lat/lon values that will not be plotted"
    )
  } else {
    NULL
  }
})

# Text with notice about effort with NA Bft
output$das_effort_message2_text <- renderText({
  das.eff.lines <- req(cruz.list$das.eff.filt)
  req(input$das_effort == 3)

  bft.na <- sum(is.na(das.eff.lines$Bft))

  if (bft.na == 1) {
    paste(
      "There is", bft.na,
      "effort line with an NA Beaufort value that will not be plotted"
    )
  } else if (bft.na > 1) {
    paste(
      "There are", bft.na,
      "effort lines with NA Beaufort values that will not be plotted"
    )
  } else {
    NULL
  }
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
