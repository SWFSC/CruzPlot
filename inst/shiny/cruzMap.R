# Processing for Coastline section of Range tab of Create and Save Map tab
#   Load coastline data and save it to reactiveValue
#   Updates map limits to coastline file extent


observeEvent(input$coast_file, {
  # TODO: validate checks fo file type
  coastline <- read.csv(input$coast_file$datapath)
  coastline <- rbind(c(NA, NA, NA), coastline, c(NA, NA, NA))

  cruz.list$coastline <- coastline
}, ignoreInit = TRUE)

observeEvent(cruz.list$coastline, {
  if (isTruthy(cruz.list$coastline)) {
    map.coastline <- cruz.list$coastline
    x <- map.coastline$lon[!is.na(map.coastline$lon)]
    y <- map.coastline$lat[!is.na(map.coastline$lat)]
    validate(
      need(length(x) == length(y),
           "Coastline lon and lat columns have difference number of non-NA values"),
      need(all(x >= -180) & all(x < 0),
           "CruzPlot can currently only handle coastline data with a longitude range of -180 to 0 degrees")
    )

    # Update inputs
    updateNumericInput(session, "lon.left", value = min(x))
    updateNumericInput(session, "lon.right", value = max(x))
    updateNumericInput(session, "lat.bot", value = min(y))
    updateNumericInput(session, "lat.top", value = max(y))

    # Update reactiveValues - this doesn't handle world2 coastline now
    cruz.map.range$lon.range <- c(min(x), max(x))
    cruz.map.range$lat.range <- c(min(y), max(y))
    cruz.map.range$world2 <- FALSE
  }
})


#-------------------------------------------------------------------------------
# Processing for Color and Grid tabs of Create and Save Map tab
#   cruzMapRiver() returns river data, adjusted for world2 map if necessary
#   cruzMapColorLand() returns land color
#   cruzMapGrid() returns grid line parameters
#   cruzMapColorWater() returns water color and depth data
#   Color style updating is done in server_color


###############################################################################
# River values
cruzMapRiver <- reactive({
  world2 <- cruz.map.range$world2
  rivs <- map("rivers", plot = FALSE)
  if (world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)

  rivs
})

# Land
cruzMapColorLand <- reactive({
  ifelse(input$color_land_all == TRUE, input$color_land, "white")
})


###############################################################################
# Grid values
cruzMapGrid <- reactive({
  list(
    col = input$grid_line_color, lwd = input$grid_line_width,
    lty = input$grid_line_type
  )
})

###############################################################################
# Water color

# Load bathymetry data
cruzMapBathyLoad <- eventReactive(input$depth_file, {
  req(input$depth_file)
  file.in <- input$depth_file

  cruz.list$bathy.xyz <- NULL
  bathy.xyz <- read.csv(file.in$datapath)

  validate(
    need(ncol(bathy.xyz) >= 3,
         "The bathymetric CSV file must contain at least 3 columns")
  )

  cruz.list$bathy.xyz <- bathy.xyz

  NULL
})

# Get color value and bathymetry data for water color
cruzMapColorWater <- reactive({
  if (input$color_water_style == 1) {
    bathy <- NULL

  } else { #if (input$color_water_style == 2) {
    bathy.xyz <- cruz.list$bathy.xyz
    validate(need(bathy.xyz, "Please load a CSV file with bathymetric data"))

    # Make sure lat/lon range matches world2 flag
    bathy.xyz[[1]] <- if (cruz.map.range$world2) {
      ifelse(bathy.xyz[[1]] < 0, bathy.xyz[[1]] + 360, bathy.xyz[[1]])
    } else {
      ifelse(bathy.xyz[[1]] > 180, bathy.xyz[[1]] - 360, bathy.xyz[[1]])
    }

    # Trim, and check that depth file lat/lon spans any map range
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    bathy.xyz.keep <- between(bathy.xyz[[1]], lon.range[1], lon.range[2]) &
      between(bathy.xyz[[2]], lat.range[1], lat.range[2])

    bathy <- try(
      marmap::as.bathy(bathy.xyz[bathy.xyz.keep, ]),
      silent = TRUE
    )

    validate(need(inherits(bathy, "bathy"),
                  paste("Unable to convert the loaded CSV file into a bathy object;",
                        "see `maramp::as.bathy` for data format requirements")))
    validate(need(length(bathy) > 0,
                  "The loaded bathymetric data does not cover any of the current map area")
    )
  }

  list(input$color_water, bathy)
})


###############################################################################
# Download bathymetric data

# Download button for downloading bathymetric file
output$depth_download_button <- renderUI({
  v.val <- input$depth_res
  v.mess <- "Bathymetric data resolution must be a whole number between 0 and 60"

  validate(need(!is.na(v.val), v.mess))
  validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.mess))
  validate(need(between(v.val, 0, 60), v.mess))

  downloadButton("depth_download", "Download bathymetric file")
})

# Message indicating if download using marmap::getNOAA.bathy failed
output$depth_download_message <- renderUI({
  if (cruz.list$bathy.download) {
    validate(
      paste("CruzPlot was not able to resolve host: gis.ngdc.noaa.gov.",
            "Please check your internet connection and try again")
    )
  } else {
    NULL
  }
})

# 'Reset' message if user leaves the page
observe({
  input$tabs
  input$tabset1

  isolate(cruz.list$bathy.download <- FALSE)
})

# Download bathymetric file
output$depth_download <- downloadHandler(
  filename = function() {
    # Defaults maramp file name: "marmap_coord_-135;29;-117;52_res_10.csv"
    paste0(
      paste(
        "marmap_coord",
        paste(cruz.map.range$lon.range[1], cruz.map.range$lon.range[2],
              cruz.map.range$lat.range[1], cruz.map.range$lat.range[2], sep = ";"),
        "res", input$depth_res,
        sep = "_"),
      ".csv"
    )
  },

  content = function(file) {
    cruz.list$bathy.download <- FALSE
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    world2 <- cruz.map.range$world2

    # getNOAA.bathy() operates on -180 to 180 scale; use user inputs not lon.range
    bathy <- try(marmap::getNOAA.bathy(
      lon1 = input$lon_left, lon2 = input$lon_right,
      lat1 = lat.range[1], lat2 = lat.range[2],
      resolution = input$depth_res, antimeridian = world2,
      keep = FALSE
    ), silent = TRUE)

    if (!isTruthy(bathy)) cruz.list$bathy.download <- TRUE
    validate(need(bathy, "Download did not work"))

    write.csv(marmap::as.xyz(bathy), file = file, row.names = FALSE)

  }
)

###############################################################################


#-------------------------------------------------------------------------------


# Processing for Label tab of Create and Save Map tab

# Return title label, font, and size
cruzMapLabelTitle <- reactive({
  lab <- input$label_title
  fam <- font.family.vals[as.numeric(input$label_title_font)]
  cex <- input$label_title_size

  list(lab = lab, fam = fam, cex = cex)
})

#	Return axes labels (lon and lat), font, and size
cruzMapLabelAxes <- reactive({
  lab.lon <- input$label_axis_lon
  lab.lat <- input$label_axis_lat
  fam <- font.family.vals[as.numeric(input$label_axis_font)]
  cex <- input$label_axis_size

  list(lab.lon = lab.lon, lab.lat = lab.lat, fam = fam, cex = cex)
})


#-------------------------------------------------------------------------------


# Processing for Planned Transects tab of Create and Save Map tab


###############################################################################
###############################################################################
# Indicator for if any planned transects are loaded

### Conditional flag for UI code
output$cruzMapPlannedTransects_Conditional <- reactive({
  isTruthy(cruz.list$planned.transects)
})
outputOptions(output, "cruzMapPlannedTransects_Conditional", suspendWhenHidden = FALSE)


### Turn plot checkbox on if planned transects are added
observe({
  if (isTruthy(cruz.list$planned.transects))
    updateCheckboxInput(session, "planned_transects_plot", value = TRUE)
})

### Turn plot checkbox off if all planned transects are removed
observe({
  if (is.null(cruz.list$planned.transects))
    updateCheckboxInput(session, "planned_transects_plot", value = FALSE)
})

###############################################################################
###############################################################################
# renderUI()s for loading planned transects

#----------------------------------------------------------
### Get names from loaded csv file
planned_transects_file_names <- reactive({
  req(planned_transects_read_csv())

  csv.names <- names(planned_transects_read_csv()[[2]])
  choices.list <- seq_along(csv.names)
  names(choices.list) <- csv.names

  choices.list
})

### Create ui inputs for selecting lon/lat columns
output$planned_transects_lon_uiOut_select <- renderUI({
  selectInput("planned_transects_lon",  h5("Longitude column"),
              choices = planned_transects_file_names(), selected = 1)
})

output$planned_transects_lat_uiOut_select <- renderUI({
  selectInput("planned_transects_lat",  h5("Latitude column"),
              choices = planned_transects_file_names(), selected = 2)
})

### Widgets for transect numbers
output$planned_transects_num_uiOut_select<- renderUI({
  selectInput("planned_transects_num",  h5("Transect number column"),
              choices = planned_transects_file_names(), selected = 3)
})

### Widget for transect classes 1
output$planned_transects_class1_uiOut_select<- renderUI({
  selectInput("planned_transects_class1",  h5("Transect class column"),
              choices = planned_transects_file_names(), selected = 4)
})

### Widget for transect classes 2
output$planned_transects_class2_uiOut_select<- renderUI({
  choices.list <- planned_transects_file_names()
  choices.list <- c("N/A - No class 2 info" = 0, choices.list)

  selectInput("planned_transects_class2",  h5("Transect class 2 column"),
              choices = choices.list, selected = 0)
})


#----------------------------------------------------------
### Button to add selected transect data to CruzPlot
output$planned_transects_execute_uiOut_button <- renderUI({
  req(planned_transects_read_csv())

  actionButton("planned_transects_execute", "Add data to CruzPlot")
})


###############################################################################
###############################################################################
# Process transect file and data and output widget to select ones to plot

### Read csv file
planned_transects_read_csv <- reactive({
  req(input$planned_transects_file)

  file.all <- input$planned_transects_file
  file.name <- file.all$name
  file.data <- try(read.csv(file.all$datapath, stringsAsFactors = FALSE),
                   silent = TRUE)

  validate(
    need(file.data, "Error loading planned transects CSV")
  )

  list(file.name, file.data)
})


### Add transect data to reactiveValue
planned_transects <- eventReactive(input$planned_transects_execute, {
  validate(
    need(input$planned_transects_lon != input$planned_transects_lat,
         "Error: The longitude column cannot be the same as the latitude column")
  )

  x <- planned_transects_read_csv()[[2]] %>%
    dplyr::select(lon = as.numeric(input$planned_transects_lon),
                  lat = as.numeric(input$planned_transects_lat),
                  num = as.numeric(input$planned_transects_num),
                  class1 = as.numeric(input$planned_transects_class1))

  validate(
    need(all(dplyr::between(x$lon, -180, 180)),
         "Error: Planned transect longitude data must be in range [-180, 180]"),
    need(all(dplyr::between(x$lat, -90, 90)),
         "Error: Planned transect latitude data must be in range [-90, 90]"),
    need(!anyNA(x$num),
         "Error: Planned transect 'number' column cannot have any NA values"),
    need(!anyNA(x$class1),
         "Error: Planned transect 'class' column cannot have any NA values")
  )

  if (as.numeric(input$planned_transects_class2) != 0) {
    x <- cbind(
      x, dplyr::select(planned_transects_read_csv()[[2]],
                       class2 = as.numeric(input$planned_transects_class2))
    )
    validate(
      need(!anyNA(x$class2),
           "Error: Planned transect 'class 2' column cannot have any NA values")
    )
  } else {
    x <- cbind(x, class2 = NA)
  }

  cruz.list$planned.transects <- x

  ""
})


###############################################################################
###############################################################################
# Processing loaded planned transects

planned_transects_class1 <- reactive({
  unique(cruz.list$planned.transects$class1)
})

planned_transects_class2 <- reactive({
  unique(cruz.list$planned.transects$class2)
})


###############################################################################
### Widgets for selecting planned transect class(es) to plot and their color
output$planned_transects_toplot_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  choices.list.names <- planned_transects_class1()
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.toplot())) {
      cruz.pt.load.toplot()
    } else {
      choices.list
    }
    cruz.pt.load.toplot(NULL)
  })

  selectInput("planned_transects_toplot",
              tags$h5("Class(es) to plot"),
              choices = choices.list, selected = choices.sel,
              multiple = TRUE)
})


output$planned_transects_color_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.color())) {
      cruz.pt.load.color()
    } else {
      "gray"
    }
    cruz.pt.load.color(NULL)
  })

  selectInput("planned_transects_color", label = tags$h5("Color(s)"),
              choices = cruz.palette.color, selected = choices.sel,
              multiple = TRUE)
})


#----------------------------------------------------------
### Widgets for selecting planned transect class 2(s) to plot and their lty
output$planned_transects_toplot2_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  y <- planned_transects_class2()

  if (anyNA(y)) {
    helpText("No class 2 column was selected, and thus you can only specify",
             "a single line type for all planned transects")

  } else {
    choices.list.names <- y
    choices.list <- seq_along(choices.list.names)
    names(choices.list) <- choices.list.names

    isolate({
      choices.sel <- if (isTruthy(cruz.pt.load.toplot2())) {
        cruz.pt.load.toplot2()
      } else {
        choices.list
      }
      cruz.pt.load.toplot2(NULL)
    })

    selectInput("planned_transects_toplot2", tags$h5("Class 2(s) to plot"),
                choices = choices.list, selected = choices.sel,
                multiple = TRUE)
  }
})

# output$planned_transects_lty_uiOut_message <- renderUI({
#   req(cruz.list$planned.transects, input$planned_transects_plot)
#
#   if (!anyNA(planned_transects_class2())) {
#     helpText("Select either one line type or the same number of line types as transect class 2s.",
#              "When multiple line types are selected, the order in which transect class 2(s) are",
#              "selected to be plotted corresponds to order of specified line type(s).")
#   } else {
#     NULL
#   }
# })

output$planned_transects_lty_uiOut_select <- renderUI({
  req(cruz.list$planned.transects)

  input.lab <- ifelse(
    anyNA(planned_transects_class2()), "Line type", "Line type(s)"
  )

  isolate({
    choices.sel <- if (isTruthy(cruz.pt.load.lty())) {
      cruz.pt.load.lty()
    } else {
      1
    }
    cruz.pt.load.lty(NULL)
  })

  selectInput("planned_transects_lty", label = tags$h5(input.lab),
              choices = cruz.line.type, selected = choices.sel,
              multiple = !anyNA(planned_transects_class2()))

})

###############################################################################
# Removing loaded transects
#   Currently only allows one transect file to be loaded

# ### Widget for selecting planned transect(s) to remove
# output$planned_transects_toremove_uiOut_select <- renderUI({
#   req(cruz.list$planned.transects)
#
#   choices.list.names <- planned_transects_class1()
#   choices.list <- seq_along(choices.list.names)
#   names(choices.list) <- choices.list.names
#
#   selectInput("planned_transects_toremove",
#                  tags$h5("Select planned transect class(es) to remove"),
#                  choices = choices.list, multiple = TRUE)
# })
#
# output$planned_transects_toremove_execute_uiOut_button <- renderUI({
#   req(cruz.list$planned.transects)
#
#   actionButton("planned_transects_toremove_execute", "Remove")
# })
#
#
# ### Remove selected transects
# planned_transects_remove <- eventReactive(input$planned_transects_toremove_execute, {
#   req(cruz.list$planned.transects)
#   y <- as.numeric(input$planned_transects_toremove)
#
#   validate(
#     need(length(y) != 0,
#          "Please select at least one set of transects to remove")
#   )
#
#   x <- cruz.list$planned.transects %>%
#     filter(!(class1 %in% planned_transects_class1()[y]))
#
#   if (nrow(x) == 0) {
#     cruz.list$planned.transects <- NULL
#   } else {
#     cruz.list$planned.transects <- x
#   }
#
#   "Planned transects removed"
# })

###############################################################################
###############################################################################


#-------------------------------------------------------------------------------


# Processing for Map range section of Range tab of Create and Save Map tab

###############################################################################
# Update map range when default study area buttons are clicked
world2_calc <- function(lon.min, lon.max) {
  (lon.max < lon.min) & (lon.min > 0) & (lon.max < 0)
}

default_range_set <- function(ll.vals) {
  world2 <- world2_calc(ll.vals[1], ll.vals[2])

  updateNumericInput(session, "lon_left", value = ll.vals[1])
  updateNumericInput(session, "lon_right", value = ll.vals[2])
  updateNumericInput(session, "lat_bot", value = ll.vals[3])
  updateNumericInput(session, "lat_top", value = ll.vals[4])

  cruz.map.range$lon.range <- c(ll.vals[1], if_else(world2, ll.vals[2] + 360, ll.vals[2]))
  cruz.map.range$lat.range <- c(ll.vals[3], ll.vals[4])
  cruz.map.range$world2 <- world2
  cruz.map.range$map.name <- list(
    if (world2) {
      ifelse(input$resolution == 2, "world2Hires", "world2")
    } else {
      ifelse(input$resolution == 2, "worldHires", "world")
    },
    if (world2) {if (input$resolution == 2) regions.rm.hires else regions.rm} else NULL
  )
}

### CCE
observeEvent(input$map_replot_cce, {
  ll.vals <- c(-135, -117, 29, 52)
  default_range_set(ll.vals)
}, priority = 11)

### Expanded CCE
observeEvent(input$map_replot_cce2, {
  ll.vals <- c(-135, -110, 27, 52)
  default_range_set(ll.vals)
}, priority = 11)

### ETP
observeEvent(input$map_replot_etp, {
  ll.vals <- c(-155, -75, -10, 50)
  default_range_set(ll.vals)
}, priority = 11)

### Hawaii
observeEvent(input$map_replot_hawaii, {
  ll.vals <- c(175, -150, 12, 35)
  default_range_set(ll.vals)
}, priority = 11)

### Main Hawaiian Islands
observeEvent(input$map_replot_hawaiimain, {
  ll.vals <- c(-162, -153, 17.5, 23.5)
  default_range_set(ll.vals)
}, priority = 11)

### Marianas
observeEvent(input$map_replot_marianas, {
  ll.vals <- c(140, 150, 10, 24)
  default_range_set(ll.vals)
}, priority = 11)


###############################################################################
# Update params as necessary
cruzMapParam <- reactive({
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  param.unit <- par("usr")
  param.inch <- par("pin")

  list(param.unit = param.unit, param.inch = param.inch)
})


###############################################################################
# Use brush to fill inputs with new map range
observeEvent(input$map_brush, {
  req(cruz.map.range$lon.range, cruz.map.range$lat.range)

  if (isTruthy(input$map_brush)) {
    z <- input$map_brush
    z.coords <- round(c(z$xmin, z$xmax, z$ymin, z$ymax), 1)
    lon.left <- ifelse(z.coords[1] > 180, z.coords[1] - 360, z.coords[1])
    lon.right <- ifelse(z.coords[2] > 180, z.coords[2] - 360, z.coords[2])

    updateNumericInput(session, "lon_left", value = lon.left)
    updateNumericInput(session, "lon_right", value = lon.right)
    updateNumericInput(session, "lat_bot", value = z.coords[3])
    updateNumericInput(session, "lat_top", value = z.coords[4])

  } else {
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range

    lon.range <- ifelse(lon.range > 180, lon.range - 360, lon.range)

    updateNumericInput(session, "lon_left", value = lon.range[1])
    updateNumericInput(session, "lon_right", value = lon.range[2])
    updateNumericInput(session, "lat_bot", value = lat.range[1])
    updateNumericInput(session, "lat_top", value = lat.range[2])
  }
}, ignoreNULL = FALSE)


###############################################################################
# Series of steps/actions triggered by input$map_replot
map.range.message <- reactiveVal(NULL)

observeEvent(input$map_replot, {
  lon.min <- input$lon_left
  lon.max <- input$lon_right
  lat.min <- input$lat_bot
  lat.max <- input$lat_top

  # Checks that inputs are numbers
  vals.bad <- c("", "-", "+", NA)
  m1 <- if ((lon.min %in% vals.bad) | !between(lon.min, -180, 180))
    "The left longtiude must be a number between -180 and 180" else NULL
  m2 <- if ((lon.max %in% vals.bad) | !between(lon.max, -180, 180))
    "The right longtiude must be a number between -180 and 180" else NULL
  m3 <- if ((lat.min %in% vals.bad) | !between(lat.min, -90, 90))
    "The bottom latitude must be a number between -90 and 90" else NULL
  m4 <- if ((lat.max %in% vals.bad) | !between(lat.max, -90, 90))
    "The top latitude must be a number between -90 and 90" else NULL

  m.all <- c(m1, m2, m3, m4)

  map.range.message(if (is.null(m.all)) m.all else paste(m.all, collapse = "<br/>"))
  req(is.null(m.all))

  # Determine if world2 map should be used and thus if lons need to be rescaled
  world2 <- world2_calc(lon.min, lon.max)

  if (world2) {
    lon.min <- ifelse(lon.min < 0, 360 + lon.min, lon.min)
    lon.max <- ifelse(lon.max < 0, 360 + lon.max, lon.max)
  }

  # Get map name
  hires <- input$resolution == 2

  m <- ifelse(hires, "Hires", "")
  m <- ifelse(world2, paste0("world2", m), paste0("world", m))

  #regions.rm and regions.rm.hires are created in server file
  reg.toplot <- if (world2 & hires) {
    regions.rm.hires
  } else if (world2 & !hires) {
    regions.rm
  } else {
    NULL
  }

  # Save as reactive values
  cruz.map.range$lon.range <- c(lon.min, lon.max)
  cruz.map.range$lat.range <- c(lat.min, lat.max)
  cruz.map.range$world2 <- world2
  cruz.map.range$map.name <- list(m, reg.toplot)

  # Reset map brush, in case
  session$resetBrush("map_brush")
}, ignoreNULL = FALSE, priority = 9)


output$map_range_message <- renderUI({
  HTML(req(map.range.message()))
})

###############################################################################


#-------------------------------------------------------------------------------


# saveMap for CruzPlot by Sam Woodman

plotDownload <- function() {
  plotMap()()
}

### Render download button, with checks
output$downloadMap_button <- renderUI({
  # Resolution
  v.val <- input$download_res
  v.message <- "Resolution must be a whole number greater than zero"
  validate(need(!is.na(v.val), v.message))
  validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
  validate(need(v.val > 0, v.message))

  # Plot dimensions
  if (input$download_dim == 2) {
    # Plot width
    v.val <- input$download_width
    v.message <- "Plot width must be greater than zero"
    validate(need(!is.na(v.val), v.message))
    # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
    validate(need(v.val > 0, v.message))

    # Plot height
    v.val <- input$download_height
    v.message <- "Plot height must be greater than zero"
    validate(need(!is.na(v.val), v.message))
    # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message)
    validate(need(v.val > 0, v.message))
  }

  # Button
  downloadButton("downloadMap", label = "Download map")
})


### Download map
output$downloadMap <- downloadHandler(
  filename = function() {
    file.ext <- switch(
      input$download_format, "1" = ".jpeg", "2" = ".pdf", "3" = ".png"
    )

    paste0(
      "cruzPlot_",
      cruz.map.range$lon.range[1], "_", cruz.map.range$lon.range[2], "_",
      cruz.map.range$lat.range[1], "_", cruz.map.range$lat.range[2],
      file.ext
    )
  },

  content = function(file) {
    # Get file dimension values
    file.res <- input$download_res

    if (input$download_dim == 1) {
      file.width <- session$clientData$output_plot1b_width / plot.res
      file.height <- session$clientData$output_plot1b_height / plot.res

    } else if (input$download_dim == 2) {
      file.width <- input$download_width
      file.height <- input$download_height
    }

    plot.bg <- if_else(input$background_transparent, "transparent", "white")

    # Save map
    if (input$download_format == 1) {
      jpeg(file, width = file.width, height = file.height, units = "in",
           res = file.res)
      plotDownload()
      dev.off()
    } else if (input$download_format == 2) {
      pdf(file, width = file.width, height = file.height, onefile = FALSE,
          bg = plot.bg)
      plotDownload()
      dev.off()
    } else if (input$download_format == 3) {
      png(file, width = file.width, height = file.height, units = "in",
          res = file.res, bg = plot.bg)
      plotDownload()
      dev.off()
    }
  }
)


#-------------------------------------------------------------------------------


# Processing for Scale bar section of Map range tab of Create and Save Map tab
#   update: scale bar longitude, length, and latitude
#   cruzMapScaleBar() returns a list of the scale bar coordinates and parameters


###############################################################################
### Update reactiveValues cruz.scale if inputs change and are different
observe({
  req(input$scale_lon)
  isolate({
    if (!isTRUE(all.equal(cruz.scale$scale.lon, input$scale_lon)))
      cruz.scale$scale.lon <- input$scale_lon
  })
})

observe({
  req(input$scale_lat)
  isolate({
    if (!isTRUE(all.equal(cruz.scale$scale.lat, input$scale_lat)))
      cruz.scale$scale.lat <- input$scale_lat
  })
})

observe({
  req(input$scale_len)
  isolate({
    if (cruz.scale$scale.len != input$scale_len)
      cruz.scale$scale.len <- input$scale_len
  })
})


###############################################################################
# Calculate new default scale bar lon/lat/len if map dimensions change
#    Update both reactiveVals and widgets

output$scale_lon_uiOut_numeric <- renderUI({
  numericInput("scale_lon", tags$h5("Longitude"), value = cruz.scale$scale.lon)
})

output$scale_lat_uiOut_numeric <- renderUI({
  numericInput("scale_lat", tags$h5("Latitude"), value = cruz.scale$scale.lat)
})

observeEvent(input$scale_units, {
  cruz.scale$scale.len <- if (input$scale_units == 1) {
    base::signif(cruz.scale$scale.len * 1.852, 2)
  } else {
    base::signif(cruz.scale$scale.len / 1.852, 2)
  }
})

output$out_scale_len <- renderUI({
  title.new <- ifelse(input$scale_units == 1, "Length in km", "Length in nmi")
  numericInput("scale_len", tags$h5(title.new), value = cruz.scale$scale.len)
})


### Calculate scale bar default start position if map range changes
observe({
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  isolate({
    x <- cruz.scale$scale.lon
    y <- cruz.scale$scale.lat

    if (!isTruthy(x) | !isTruthy(y)) {
      bar.in.range <- TRUE
    } else {
      x <- ifelse(cruz.map.range$world2, x + 360, x)
      bar.in.range <- between(x, lon.range[1], lon.range[2]) &
        between(y, lat.range[1], lat.range[2])
    }

    # If scale bar is off or bar is out of current map range
    if (!input$bar | !bar.in.range) {
      lon.diff <- abs(lon.range[2] - lon.range[1])
      lat.diff <- abs(lat.range[2] - lat.range[1])

      # Scale bar longitude start
      lon.new <- 0.1 * lon.diff + lon.range[1]
      lon.new <- ifelse(lon.new > 180, lon.new - 360, lon.new)

      # Scale bar latitude start
      lat.new <- 0.1 * lat.diff + lat.range[1]

      # Set reactiveValues
      cruz.scale$scale.lon <- lon.new
      cruz.scale$scale.lat <- lat.new
    }
  })
}, priority = 1) #Must run before observe() for bar length

### After getting start position, get the default scale bar length
###   Separate observe() so that lat/lon update isn't run if scale units change
###   Only update length if scale bar is not alrady on
observe({
  if (!input$bar) {
    lon.range <- cruz.map.range$lon.range
    cruz.map.range$lat.range
    isolate({
      lon.pos <- cruz.scale$scale.lon
      lat.pos <- cruz.scale$scale.lat
      scale.units <- input$scale_units
    })

    # Scale bar length; suppressWarnings() for if world2
    lon.range.m <- suppressWarnings(geosphere::distVincentyEllipsoid(
      c(lon.range[1], lat.pos), c(lon.range[2], lat.pos)
    ))
    len.new.km <- lon.range.m * 0.2 / 1000

    # Scale units
    if (scale.units == 1) {
      len.new <- base::signif(len.new.km, 2)
      title.new <- "Length in km"
    } else if (scale.units == 2) {
      # nmi, 1nmi = 1.852km
      len.new <- base::signif((len.new.km / 1.852), 2)
      title.new <- "Length in nmi"
    }

    cruz.scale$scale.len <- len.new
  }
})


###############################################################################
### Put all scale bar values in list for plotting
cruzMapScaleBar <- reactive({
  isolate(world2 <- cruz.map.range$world2)
  scale.lon <- cruz.scale$scale.lon
  scale.lat <- cruz.scale$scale.lat
  scale.len <- cruz.scale$scale.len
  scale.lwd <- input$scale_width
  scale.units <- input$scale_units

  scale.units.str <- ifelse(scale.units == 1, "km", "nmi")

  # Determine length of scale bar in meters
  scale.len.m <- if (scale.units == 1) {
    scale.len * 1000
  } else if (scale.units == 2) {
    scale.len * 1.852 * 1000
  }

  scale.x1 <- ifelse((world2 && scale.lon < 0), scale.lon + 360, scale.lon)
  scale.x2 <- geosphere::destPoint(c(scale.lon, scale.lat), 90, scale.len.m)[1]
  scale.x2 <- ifelse((world2 && scale.x2 < 0), scale.x2 + 360, scale.x2)

  scale.y <- scale.lat

  list(
    x1 = scale.x1, x2 = scale.x2, y = scale.y,
    lwd = scale.lwd, len = scale.len, units.str = scale.units.str
  )
})


#-------------------------------------------------------------------------------


# Processing for Tick tab of Create and Save Map tab
#   update: major tick interval, start of longitude tick labels, start of latitude tick labels
#   cruzMapTickLonBool() returns boolean list of whether bottom and top tick marks and tick labels are drawn, respectively
#   cruzMapTickLatBool() returns boolean list of whether left and right tick marks and tick labels are drawn, respectively
#   cruzMapTickLon() returns labels for longitude tick marks
#   cruzMapTickLat() returns labels for latitude tick marks
#   cruzMapTickParam() returns list of tick length, font, and scale


###############################################################################
#  Return list of longitude values of major tick marks/grid lines and minor tick marks
cruzMapIntervalLon <- reactive({
  lon.range <- cruz.map.range$lon.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick_interval_minor
  lon.start <- cruz.tick$label.lon.start

  if (cruz.map.range$world2) lon.start <- ifelse(lon.start < 0, lon.start + 360, lon.start)

  tick.lon <- list(label.loc = seq(lon.start, lon.range[2], by = tick.maj))
  temp.tick <- rev(seq(lon.start, lon.range[1], by = -tick.maj))
  tick.lon$maj <- sort(unique(c(tick.lon$label.loc, temp.tick)))
  tick.lon$min <- cruzTickMinor(deg.range = lon.range, maj.ticks = tick.lon$maj,
                                tick.maj.interval = tick.maj, n=tick.min)

  tick.lon
})

# Return list of latitude values of major tick marks/grid lines and minor tick marks
cruzMapIntervalLat <- reactive({
  lat.range <- cruz.map.range$lat.range
  tick.maj <- cruz.tick$tick.interval.major
  tick.min <- input$tick_interval_minor
  lat.start <- cruz.tick$label.lat.start

  tick.lat <- list(label.loc = seq(lat.start, lat.range[2], by = tick.maj))
  temp.tick <- rev(seq(lat.start, lat.range[1], by = -tick.maj))
  tick.lat$maj <- sort(unique(c(tick.lat$label.loc, temp.tick)))
  tick.lat$min <- cruzTickMinor(deg.range = lat.range, maj.ticks = tick.lat$maj,
                                tick.maj.interval = tick.maj, n=tick.min)

  tick.lat
})


###############################################################################
# Update reactiveValues cruz.tick at start (cruz.tick's = NULL) and
#    if inputs change and are different from cruz.tick

observe({
  req(input$tick_interval_major)

  in.tick.interval.major <- input$tick_interval_major
  isolate({
    if (cruz.tick$tick.interval.major != in.tick.interval.major)
      cruz.tick$tick.interval.major <- in.tick.interval.major
  })
})

observe({
  req(input$label_lon_start)

  in.label.lon.start <- as.numeric(input$label_lon_start)
  isolate({
    if (cruz.tick$label.lon.start != in.label.lon.start)
      cruz.tick$label.lon.start <- in.label.lon.start
  })
})

observe({
  req(input$label_lat_start)

  in.label.lat.start <- as.numeric(input$label_lat_start)
  isolate({
    if (cruz.tick$label.lat.start != in.label.lat.start)
      cruz.tick$label.lat.start <- in.label.lat.start
  })
})

###############################################################################
# Update inputs

# Tick major interval
observe({
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range
  tick.val <- cruzTickUpdate(lon.range, lat.range)

  updateNumericInput(session, "tick.interval.major", value = tick.val)
  cruz.tick$tick.interval.major <- tick.val
}, priority = 2)

# Tick label longitude start
observe({
  b <- cruz.tick$tick.interval.major
  if (b != 0 && !is.na(b)) {
    lon.range <- cruz.map.range$lon.range
    lon.start <- cruzTickStart(lon.range, b)

    updateTextInput(session, "label.lon.start", value = paste(lon.start))
    cruz.tick$label.lon.start <- lon.start
  }
}, priority = 1)

# Tick label latitude start
observe({
  b <- cruz.tick$tick.interval.major
  if (b != 0 && !is.na(b)) {
    lat.range <- cruz.map.range$lat.range
    lat.start <- cruzTickStart(lat.range, b)

    updateTextInput(session, "label.lat.start", value = paste(lat.start))
    cruz.tick$label.lat.start <- lat.start
  }
}, priority = 1)

###############################################################################
# Reactive functions

# Plot longtiude tick marks
cruzMapTickLonBool <- reactive({
  bot <- c(input$tick_bot, input$tick_bot_lab)
  top <- c(input$tick_top, input$tick_top_lab)
  list(bot = bot, top = top)
})

# Plot latitude tick marks
cruzMapTickLatBool <- reactive({
  left <- c(input$tick_left, input$tick_left_lab)
  right <- c(input$tick_right, input$tick_right_lab)
  list(left = left, right = right)
})

# Plot longtiude tick labels
cruzMapTickLonLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLon()$label.loc
  format <- input$tick_style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    i <- ifelse(i > 180, i - 360, i)
    i <- ifelse(i < -180, 360 - i, i)
    a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    b <- ifelse(i < 0, "~W", "~E")
    b <- ifelse(a %in% c(0, 180), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  tick.lab
})

# Plot latitude tick labels
cruzMapTickLatLab <- reactive({
  tick.lab.loc <- cruzMapIntervalLat()$label.loc
  format <- input$tick_style

  tick.lab <- parse(text = sapply(tick.lab.loc, function(i) {
    a <- ifelse(i < 0 & !(format %in% c(1, 3)), -1 * i, i)
    b <- ifelse(i < 0, "~S", "~N")
    b <- ifelse(a %in% c(0, 90), "", b)
    b <- ifelse((format == 2 || format == 4), b, "")
    l <- ifelse((format == 3 || format == 4), "*degree", "")
    paste(a, l, b, sep = "")
  }))

  tick.lab
})

cruzMapTickParam <- reactive({
  tick.len <- input$tick_length
  lab.font <- font.family.vals[as.numeric(input$label_tick_font)]
  lab.scale <- input$label_tick_size
  list(len = tick.len, font = lab.font, scale = lab.scale)
})
