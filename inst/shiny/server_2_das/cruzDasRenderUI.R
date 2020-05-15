## renderUI()'s for Plot DAS Data tab


###############################################################################
### Codes for mammals and turtles
# renderUIs for mammal and turtle species
output$das.sighting.code.1_uiOut_select <- renderUI({
  sp.mammals <- cruzSpeciesMammals()
  sp.mammals.lab <- paste(sp.mammals$Code, sp.mammals$Name.Scientific)

  selectizeInput("das.sighting.code.1", h5("Select species"),
                 choices = sp.mammals.lab, multiple = TRUE,
                 selected = NULL)
})
outputOptions(output, "das.sighting.code.1_uiOut_select",
              suspendWhenHidden = FALSE)

output$das.sighting.code.2_uiOut_select <- renderUI({
  sp.turtles <- cruzSpeciesTurtles()
  sp.turtles.lab <- paste(sp.turtles$Code, sp.turtles$Name.Scientific)

  selectizeInput("das.sighting.code.2", h5("Select species"),
                 choices = sp.turtles.lab, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das.sighting.code.2_uiOut_select", suspendWhenHidden = FALSE)


###############################################################################
# Date widgets
### Get min and max dats in DAS file
dateRange_min_max <- reactive({
  req(cruz.list$das.data)
  input$das.file

  x <- cruz.list$das.data
  min.date <- as.character(as.Date(min(x$Date, na.rm = T)) - 1)
  max.date <- as.character(as.Date(max(x$Date, na.rm = T)) + 1)

  return(c(min.date, max.date))
})

### renderUI for date range filter for plotting sightings and effort
output$das_sight_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das_sight_dateRange",
                 label = h5("Range of dates for which sightings are plotted"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_sight_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)

output$das_effort_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das_effort_dateRange",
                 label = h5("Range of dates for which effort is plotted"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_effort_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)


### renderUIs for date range filters for tabular outputs
output$das.out.sight.dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das.out.sight.dateRange",
                 label = h5("Range of dates for sightings output"),
                 start = dates[1], end = dates[2])
})

output$das.out.effort.dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das.out.effort.dateRange",
                 label = h5("Range of dates for effort output"),
                 start = dates[1], end = dates[2])
})


##############################################################################
# renderUI for truncation input

output$das_sight_trunc_uiOut_numeric <- renderUI({
  isolate(curr.value <- input$das_sight_trunc)

  trunc.units <- input$das_sight_trunc_units
  if(trunc.units == 1) widget.name <- "Truncation (km)"
  if(trunc.units == 2) widget.name <- "Truncation (nmi)"

  numericInput("das_sight_trunc", label = h5(widget.name), value = curr.value)
})
outputOptions(output, "das_sight_trunc_uiOut_numeric", suspendWhenHidden = FALSE, priority = 3)

##############################################################################
