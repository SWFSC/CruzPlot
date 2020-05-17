## renderUI()'s for Plot DAS Data tab


###############################################################################
### Codes for mammals and turtles
# renderUIs for mammal and turtle species
output$das_sighting_code_1_uiOut_select <- renderUI({
  sp.mammals <- cruzSpeciesMammals()
  sp.mammals.lab <- paste(sp.mammals$Code, sp.mammals$Name.Scientific)

  selectizeInput("das_sighting_code_1", h5("Select species"),
                 choices = sp.mammals.lab, multiple = TRUE,
                 selected = NULL)
})
outputOptions(output, "das_sighting_code_1_uiOut_select",
              suspendWhenHidden = FALSE)

output$das_sighting_code_2_uiOut_select <- renderUI({
  sp.turtles <- cruzSpeciesTurtles()
  sp.turtles.lab <- paste(sp.turtles$Code, sp.turtles$Name.Scientific)

  selectizeInput("das_sighting_code_2", h5("Select species"),
                 choices = sp.turtles.lab, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_sighting_code_2_uiOut_select", suspendWhenHidden = FALSE)


###############################################################################
# Filters

# Date widgets
### Get min and max datws in DAS file
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


### Truncation input
output$das_sight_trunc_uiOut_numeric <- renderUI({
  isolate(curr.value <- input$das_sight_trunc)

  trunc.units <- input$das_sight_trunc_units
  if(trunc.units == 1) widget.name <- "Truncation (km)"
  if(trunc.units == 2) widget.name <- "Truncation (nmi)"

  numericInput("das_sight_trunc", label = h5(widget.name), value = curr.value)
})
outputOptions(output, "das_sight_trunc_uiOut_numeric", suspendWhenHidden = FALSE, priority = 3)


##############################################################################
# Name of tabular outputs

### Sightings
output$das_out_sight_save_name_uiOut_text <- renderUI({
  csv.name <- paste0("Sight_", Sys.time(), ".csv")
  csv.name <- gsub(" ", "_", csv.name)
  csv.name <- gsub(":", "-", csv.name)
  csv.name <- gsub("-", "", csv.name)

  textInput("das_out_sight_save_name", h5("Sightings file name"),
            value = csv.name)
})

##############################################################################
