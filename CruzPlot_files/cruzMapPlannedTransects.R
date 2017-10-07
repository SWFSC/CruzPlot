### cruzMapPlannedTransects
## Code to load and process planned transect files


### Turn plot checkbox on if planned transects are added
# observe({
#   if(!is.null(cruz.list$planned.transects))
#     updateCheckboxInput(session, "planned_transects_plot", value = TRUE)
# })

### Turn plot checkbox off if all planned transects are removed
observe({
  if(is.null(cruz.list$planned.transects))
    updateCheckboxInput(session, "planned_transects_plot", value = FALSE)
})

###############################################################################
# Process transect file and data and output widget to select ones to plot

### Read csv file
planned_transects_read_csv <- reactive({
  req(input$planned_transects_file)
  
  file.all <- input$planned_transects_file
  file.name <- file.all$name
  file.data <- read.csv(file.all$datapath, 
                        # header = input$planned_transects.header, 
                        # sep = input$planned_transects.sep, 
                        # quote = input$planned_transects.header.quote, 
                        stringsAsFactors = FALSE)
  
  # Validate for bad files
  
  return(list(file.name, file.data))
})


### Add transect data to reactiveValue
planned_transects <- eventReactive(input$planned_transects_execute, {
  data.all <- planned_transects_read_csv()[[2]]
  
  validate(
    need(input$planned_transects_lon != input$planned_transects_lat, 
         "The longitude column cannot be the same as the latitude column")
  )
  
  data.lon <- data.all[,as.numeric(input$planned_transects_lon)]
  data.lat <- data.all[,as.numeric(input$planned_transects_lat)]
  
  validate(
    need(all(data.lon <= 180 & data.lon >= -180), 
         "Longitude data must be in -180 to 180 range"), 
    need(all(data.lat <= 90 & data.lat >= -90), 
         "Latitude data must be in -90 to 90 range")
  )
  
  x <- seq(1, nrow(data.all), by = 3)
  data.lon1 <- sapply(x, function(i) data.lon[i])
  data.lon2 <- sapply(x + 1, function(i) data.lon[i])
  data.lat1 <- sapply(x, function(i) data.lat[i])
  data.lat2 <- sapply(x + 1, function(i) data.lat[i])
  
  
  data.name <- NA
  
  if(input$planned_transects_name_type == "1") {
    data.name <- data.all[x, as.numeric(input$planned_transects_names)]
  }
  if(input$planned_transects_name_type == "2") {
    data.name <- input$planned_transects_name_text
    if(data.name == "") data.name <- NA
  }
  
  validate(
    need(!is.na(data.name), 
         "Please choose or enter valid transect name(s)")
  )
  
  transect.data <- data.frame(lon1 = data.lon1, lon2 = data.lon2, 
                              lat1 = data.lat1, lat2 = data.lat2, 
                              name = data.name, stringsAsFactors = FALSE)
  
  cruz.list$planned.transects <- rbind(cruz.list$planned.transects, 
                                       transect.data)
  
  return("Transect data added to CruzPlot")
})

transects.names <- reactive({
  choices.list.names <- unique(cruz.list$planned.transects$name)
})


### Select widget for plotting transect
output$planned_transects_toplot_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)
  
  choices.list.names <- transects.names()
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names
  
  validate(
    need(!is.null(cruz.list$planned.transects), "Please load planned transects")
  )
  
  selectizeInput("planned_transects_toplot", 
                 h5("Select planned transect(s) to plot"),
                 choices = choices.list, selected = choices.list, 
                 multiple = TRUE)
})
outputOptions(output, "planned_transects_toplot_uiOut_select", suspendWhenHidden = FALSE)

###############################################################################
# Removing loaded transects
### Widgets
output$planned_transects_toremove_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)
  
  choices.list.names <- transects.names()
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names
  
  validate(
    need(!is.null(cruz.list$planned.transects), "")
  )
  
  selectizeInput("planned_transects_toremove", 
                 h5("Select planned transect(s) to remove"),
                 choices = choices.list, multiple = TRUE)
})

output$planned_transects_toremove_execute_uiOut_button <- renderUI({
  req(cruz.list$planned.transects)
  
  actionButton("planned_transects_toremove_execute", "Remove")
})


### Remove selected transects
planned_transects_remove <- eventReactive(input$planned_transects_toremove_execute, {
  transects.all <- cruz.list$planned.transects
  
  transects.toremove.names.which <- as.numeric(input$planned_transects_toremove)
  validate(
    need(length(transects.toremove.names.which) != 0, 
         "Please select at least one set of transects to remove")
  )
  
  transects.toremove.names  <- transects.names()[transects.toremove.names.which]
  
  transects.tokeep <- transects.all[!(transects.all$name %in% 
                                        transects.toremove.names),]
  if(nrow(transects.tokeep) == 0) transects.tokeep <- NULL
  
  cruz.list$planned.transects <- transects.tokeep
  
  return("")
})


###############################################################################
# renderUI()s for planned transects

### Create ui inputs for selecting lon/lat columns
output$planned_transects_lon_uiOut_select <- renderUI({
  transects.names <- names(planned_transects_read_csv()[[2]])
  choices.list <- seq_along(transects.names)
  names(choices.list) <- transects.names
  
  selectInput("planned_transects_lon",  h5("Longitude column"), 
              choices = choices.list, selected = 1)
})

output$planned_transects_lat_uiOut_select <- renderUI({
  transects.names <- names(planned_transects_read_csv()[[2]])
  choices.list <- seq_along(transects.names)
  names(choices.list) <- transects.names
  
  selectInput("planned_transects_lat",  h5("Latitude column"), 
              choices = choices.list, selected = 3)  
})


### Widgets for naming transects
output$planned_transects_name_type_uiOut_radio <- renderUI({
  req(planned_transects_read_csv())
  
  radioButtons("planned_transects_name_type", label = NULL, 
               choices = list("Get transect name(s) from file" = 1,
                              "Enter transect name manually" = 2))
})

output$planned_transects_names_uiOut_select<- renderUI({
  transects.names <- names(planned_transects_read_csv()[[2]])
  choices.list <- seq_along(transects.names)
  names(choices.list) <- transects.names
  
  choices.selected <- ifelse(length(choices.list) > 2, 3, NA)
  validate(need(!is.na(choices.selected), "Loaded csv file only has 2 columns-enter name manually"))
  
  selectInput("planned_transects_names",  h5("Transects name(s) column"), 
              choices = choices.list, selected = choices.selected)
})


### Button to add selected transect data to CruzPlot
output$planned_transects_execute_uiOut_button <- renderUI({
  req(planned_transects_read_csv())
  
  actionButton("planned_transects_execute", "Add selected data to CruzPlot")
})


### Line widths
# output$planned_transects_lw_uiOut_text <- renderUI({
#   textInput("planned_transects_lw", h5("Line width(s)"), 
#             value = "1")
# })


###############################################################################
# Indicator for if any planned transects are loaded

### Conditional flag for UI code
output$cruzMapPlannedTransects_Conditional <- reactive({
  !is.null(cruz.list$planned.transects)
})
outputOptions(output, "cruzMapPlannedTransects_Conditional", suspendWhenHidden = FALSE)