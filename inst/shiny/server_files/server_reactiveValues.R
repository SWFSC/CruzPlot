### server_reactiveValues
## Code for 'initializing' and for saving/loading reactiveValues

###############################################################################
# 'Initialize' reactiveValues

cruz.load.color <- reactiveValues(load.flag = FALSE)

### For data storage and loading again in new session
cruz.list <- reactiveValues(
  planned.transects = NULL, # Dataframe of planned transect lines
  coastline = NULL,         # Coastline file
  bathy = NULL,             # Bathymetric data
  das.data = NULL,          # DAS dataframe
  das.data.name = NULL,     # Names of loaded DAS files
  ndas.data = list(),       # List of non-DAS line and point data
  ndas.df = NULL,           # Dataframe of ndas information
  ndas.toplot = NULL        # Non-DAS objects currently being plotted
)

### Map range so that range can be triggered by button
cruz.map.range <- reactiveValues(
  lon.range = NULL,
  lat.range = NULL,
  world2 = NULL,
  map.name = list()
)

### The following reactiveValues are used for inputs whose defaults are
#     updated depending on map range Using reactiveValues makes it so that
#     drawMap() isn't run multiple times 'new values calculated' ->
#     'inputs updated' -> 'new values finally used in plotting'

# Scale bar
cruz.scale <- reactiveValues(
  scale.lon = NULL,
  scale.lat = NULL,
  scale.len = NULL
)

# Tick marks
cruz.tick <- reactiveValues(
  tick.interval.major = NULL,
  label.lon.start = NULL,
  label.lat.start = NULL
)


###############################################################################
### Load 'current app environemnt' data
load_envir <- eventReactive(input$load_app_envir_file, {
  req(input$load_app_envir_file)

  file.load <- input$load_app_envir_file
  validate(
    need(identical(toupper(substr_right(file.load$name, 6)), ".RDATA") &
           file.load$type == "",
         "Error: Please load a file with the extension '.RDATA'")
  )

  cruz.load.color$load.flag <- TRUE

  withProgress(message = "Loading saved data", value = 0.5, {
    load(file.load$datapath)
    files.list <- list("cruz.list.save", "map.info", "das.info", "ndas.info")
    validate(
      need(all(sapply(files.list, function(i) exists(i))),
           "Error: Loaded .RDATA file does not contain an environment saved using CruzPlot")
    )
    rm(files.list)
    incProgress(0.4)

    #--------------------------------------------------------------------------
    ### Update reactiveValues
    cruz.list$planned.transects <- cruz.list.save[["planned.transects"]]
    cruz.list$coastline     <- cruz.list.save[["coastline"]]
    cruz.list$bathy         <- cruz.list.save[["bathy"]]
    cruz.list$das.data      <- cruz.list.save[["das.data"]]
    cruz.list$das.data.name <- cruz.list.save[["das.data.name"]]
    cruz.list$ndas.data     <- cruz.list.save[["ndas.data"]]
    cruz.list$ndas.df       <- cruz.list.save[["ndas.df"]]
    cruz.list$ndas.toplot   <- cruz.list.save[["ndas.toplot"]]

    cruz.map.range$lon.range <- map.info$lon.range
    cruz.map.range$lat.range <- map.info$lat.range
    cruz.map.range$world2    <- map.info$world2
    cruz.map.range$map.name  <- map.info$map.name
    incProgress(0.05)


    #--------------------------------------------------------------------------
    ### Update variable defaults as necessary
    if (isTruthy(cruz.list$ndas.toplot)) updateCheckboxInput(session, "ndas_plot", value = TRUE)

    #------------------------------------------------------
    ## Map info
    updateNumericInput(session, "lon.left", value = map.info$lon.left)
    updateNumericInput(session, "lon.right", value = map.info$lon.right)
    updateNumericInput(session, "lat.bot", value = map.info$lat.range[1])
    updateNumericInput(session, "lat.top", value = map.info$lat.range[2])
    updateSelectInput(session, "resolution", selected = map.info$resolution)

    updateCheckboxInput(session, "coast", value = map.info$coast)

    updateCheckboxInput(session, "bar", value = map.info$bar)
    updateNumericInput(session, "scale.lon", value = map.info$scale.lon)
    updateNumericInput(session, "scale.lat", value = map.info$scale.lat)
    updateRadioButtons(session, "scale.units", selected = map.info$scale.units)
    updateNumericInput(session, "scale.len", value = map.info$scale.len)
    updateNumericInput(session, "scale.width", value = map.info$scale.width)

    updateCheckboxInput(session, "planned_transects_plot", value = map.info$planned_transects_plot)
    updateSelectizeInput(session, "planned_transects_toplot", selected = map.info$planned_transects_toplot)
    updateSelectizeInput(session, "planned_transects_color", selected = map.info$planned_transects_color)
    updateTextInput(session, "planned_transects_lw", value = map.info$planned_transects_lw)

    updateCheckboxInput(session, "tick", value = map.info$tick)
    updateCheckboxInput(session, "tick.left", value = map.info$tick.left)
    updateCheckboxInput(session, "tick.right ", value = map.info$tick.right )
    updateCheckboxInput(session, "tick.bot", value = map.info$tick.bot)
    updateCheckboxInput(session, "tick.top", value = map.info$tick.top)
    updateNumericInput(session, "tick.interval.major", value = map.info$tick.interval.major)
    updateNumericInput(session, "tick.interval.minor", value = map.info$tick.interval.minor)
    updateSelectInput(session, "tick.style", selected = map.info$tick.style)
    updateNumericInput(session, "tick.length", value = map.info$tick.length)
    updateCheckboxInput(session, "tick.left.lab", value = map.info$tick.left.lab)
    updateCheckboxInput(session, "tick.right.lab", value = map.info$tick.right.lab)
    updateCheckboxInput(session, "tick.bot.lab", value = map.info$tick.bot.lab)
    updateCheckboxInput(session, "tick.top.lab", value = map.info$tick.top.lab)
    updateNumericInput(session, "label.lon.start", value = map.info$label.lon.start)
    updateNumericInput(session, "label.lat.start", value = map.info$label.lat.start)
    updateSelectInput(session, "label.tick.font", selected = map.info$label.tick.font)
    updateNumericInput(session, "label.tick.size", value = map.info$label.tick.size)

    updateTextInput(session, "label.title", value = map.info$label.title)
    updateSelectInput(session, "label.title.font", selected = map.info$label.title.font)
    updateNumericInput(session, "label.title.size", value = map.info$label.title.size)
    updateTextInput(session, "label.axis.lon", value = map.info$label.axis.lon)
    updateTextInput(session, "label.axis.lat", value = map.info$label.axis.lat)
    updateSelectInput(session, "label.axis.font", selected = map.info$label.axis.font)
    updateNumericInput(session, "label.axis.size", value = map.info$label.axis.size)

    updateRadioButtons(session, "color_style", selected = map.info$color_style)
    # Upadte color palettes here
    if (map.info$color_style == 1) {
      palette("default")
      updateSelectInput(session, "color.land", choices = cruz.palette.color, selected = "bisque1")
      updateSelectInput(session, "color.water", choices = cruz.palette.color, selected = "white")
      updateSelectInput(session, "grid.line.color", choices = cruz.palette.color, selected = "black")
      updateSelectizeInput(session, "das.symbol.color", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "das.effort.lineCol", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas.line.col", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas.pt.col", choices = cruz.palette.color, selected = "black")
    } else {
      palette(gray(0:5/5))
      updateSelectInput(session, "color.land", choices = cruz.palette.gray, selected = 4)
      updateSelectInput(session, "color.water", choices = cruz.palette.gray, selected = 0)
      updateSelectInput(session, "grid.line.color", choices = cruz.palette.gray, selected = 1)
      updateSelectizeInput(session, "das.symbol.color", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas.line.col", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas.pt.col", choices = cruz.palette.gray, selected = 1)
    }
    # Then update values
    updateCheckboxInput(session, "color_land_all", value = map.info$color_land_all)
    updateSelectInput(session, "color.land", selected = map.info$color.land)
    updateCheckboxInput(session, "color_lakes_rivers", value = map.info$color_lakes_rivers)
    updateRadioButtons(session, "color_water_style", selected = map.info$color_water_style)
    updateSelectInput(session, "color.water", selected = map.info$color.water)
    updateRadioButtons(session, "depth_style", selected = map.info$depth_style)

    updateCheckboxInput(session, "grid", value = map.info$grid)
    updateSelectInput(session, "grid.line.color", selected = map.info$grid.line.color)
    updateSelectInput(session, "grid.line.type", selected = map.info$grid.line.type)
    updateNumericInput(session, "grid.line.width", value = map.info$grid.line.width)


    #------------------------------------------------------
    ## Sighting info
    updateCheckboxInput(session, "das_sightings", value = das.info$das_sightings)
    updateRadioButtons(session, "das_sightings_position", selected = das.info$das_sightings_position)
    updateSelectInput(session, "das_sighting_type", selected = das.info$das_sighting_type)
    updateRadioButtons(session, "das_sighting_code_1_all", selected = das.info$das_sighting_code_1_all)
    updateRadioButtons(session, "das_sighting_code_1_all", selected = das.info$das_sighting_code_1_al2)
    updateSelectizeInput(session, "das.sighting.code.1", selected = das.info$das.sighting.code.1)
    updateSelectizeInput(session, "das.sighting.code.2", selected = das.info$das.sighting.code.2)
    updateCheckboxInput(session, "das.sighting.probable", value = das.info$das.sighting.probable)

    updateSelectizeInput(session, "das.symbol.type", selected = das.info$das.symbol.type)
    updateSelectizeInput(session, "das.symbol.color", selected = das.info$das.symbol.color)
    updateTextInput(session, "das.symbol.size", value = das.info$das.symbol.size)
    updateTextInput(session, "das.symbol.linewidth", value = das.info$das.symbol.linewidth)
    updateCheckboxInput(session, "das_symbol_mult", value = das.info$das_symbol_mult)
    updateTextInput(session, "das.symbol.type.mult", value = das.info$das.symbol.type.mult)
    updateTextInput(session, "das.symbol.color.mult", value = das.info$das.symbol.color.mult)
    updateSelectInput(session, "das.symbol.type.boat", selected = das.info$das.symbol.type.boat)
    updateSelectInput(session, "das.symbol.color.boat", selected = das.info$das.symbol.color.boat)
    updateNumericInput(session, "das.symbol.size.boat", value = map.info$das.symbol.size.boat)
    updateNumericInput(session, "das.symbol.linewidth.boat", value = map.info$das.symbol.linewidth.boat)

    updateRadioButtons(session, "das_sightings_effort", selected = das.info$das_sightings_effort)
    updateSelectInput(session, "das.sight.minBeau", selected = das.info$das.sight.minBeau)
    updateSelectInput(session, "das.sight.maxBeau", selected = das.info$das.sight.maxBeau)
    updateDateRangeInput(session, "das.sight.dateRange", start = das.info$das.sight.dateRange[1], end = das.info$das.sight.dateRange[2])
    updateTextInput(session, "das.sight.cruiseNum", value = das.info$das.sight.cruiseNum)
    updateRadioButtons(session, "das.sight.trunc.units", selected = das.info$das.sight.trunc.units)
    updateNumericInput(session, "das.sight.trunc", value = das.info$das.sight.trunc)

    updateCheckboxInput(session, "das_legend", value = das.info$das_legend)
    updateSelectInput(session, "das_legend_pos", selected = das.info$das_legend_pos)
    updateTextInput(session, "das.legend.lon", value = das.info$das.legend.lon)
    updateTextInput(session, "das.legend.lat", value = das.info$das.legend.lat)
    updateSelectInput(session, "das.legend.boxCol", selected = das.info$das.legend.boxCol)
    updateSelectInput(session, "das.legend.font", selected = das.info$das.legend.font)
    updateNumericInput(session, "das.legend.textSize", value = das.info$das.legend.textSize)
    updateTextInput(session, "das.legend.title", value = das.info$das.legend.title)
    updateCheckboxGroupInput(session, "das.legend.names", selected = das.info$das.legend.names)
    updateCheckboxInput(session, "das.legend.num", value = das.info$das.legend.num)

    updateCheckboxInput(session, "eff_legend", value = das.info$eff_legend)
    updateSelectInput(session, "eff_legend_pos", selected = das.info$eff_legend_pos)
    updateTextInput(session, "eff.legend.lon", value = das.info$eff.legend.lon)
    updateTextInput(session, "eff.legend.lat", value = das.info$eff.legend.lat)
    updateSelectInput(session, "eff.legend.boxCol", selected = das.info$eff.legend.boxCol)
    updateSelectInput(session, "eff.legend.font", selected = das.info$eff.legend.font)
    updateNumericInput(session, "eff.legend.textSize", value = das.info$eff.legend.textSize)


    #------------------------------------------------------
    ## Effort info
    updateRadioButtons(session, "das_effort", selected = das.info$das_effort)
    updateCheckboxGroupInput(session, "das.effort.closePass", selected = das.info$das.effort.closePass)
    updateCheckboxGroupInput(session, "das_effort_snf", selected = das.info$das_effort_snf)

    updateSelectInput(session, "das.effort.simp.col", selected = das.info$das.effort.simp.col)
    updateNumericInput(session, "das.effort.simp.lwd", value = das.info$das.effort.simp.lwd)

    updateCheckboxInput(session, "das_effort_det_byBft", value = das.info$das_effort_det_byBft)
    updateSelectInput(session, "das.effort.det.col.s", selected = das.info$das.effort.det.col.s)
    updateNumericInput(session, "das.effort.det.lwd.s", value = das.info$das.effort.det.lwd.s)
    updateSelectInput(session, "das.effort.det.col.n", selected = das.info$das.effort.det.col.n)
    updateNumericInput(session, "das.effort.det.lwd.n", value = das.info$das.effort.det.lwd.n)
    updateSelectInput(session, "das.effort.det.col.f", selected = das.info$das.effort.det.col.f)
    updateNumericInput(session, "das.effort.det.lwd.f", value = das.info$das.effort.det.lwd.f)

    updateCheckboxInput(session, "das_effort_filter_same", value = das.info$das_effort_filter_same)
    updateSelectInput(session, "das.effort.minBeau", selected = das.info$das.effort.minBeau)
    updateSelectInput(session, "das.effort.maxBeau", selected = das.info$das.effort.maxBeau)
    updateDateRangeInput(session, "das.effort.dateRange", start = das.info$das.effort.dateRange[1], end = das.info$das.effort.dateRange[2])
    updateTextInput(session, "das.effort.cruiseNum", value = das.info$das.effort.cruiseNum)
    updateRadioButtons(session, "das.effort.trunc.units", selected = das.info$das.effort.trunc.units)
    updateNumericInput(session, "das.effort.trunc", value = das.info$das.effort.trunc)

    updateRadioButtons(session, "das_out_effort_units", selected = das.info$das_out_effort_units)
    updateCheckboxGroupInput(session, "das.out.sight.closePass", selected = das.info$das.out.sight.closePass)
    updateCheckboxGroupInput(session, "das.out.sight.snf", selected = das.info$das.out.sight.snf)

    updateCheckboxInput(session, "ndas_plot", value = ndas.info$ndas_plot)

    incProgress(0.05)
  })

  "Workspace loaded"
})

output$load_app_text <- renderText({
  load_envir()
})


###############################################################################
### Save 'current app environemnt' data
output$save_app_envir <- downloadHandler(
  filename = function() {
    paste0("CruzPlot_", Sys.Date(), ".RDATA")
  },

  content = function(file) {
    withProgress(message = "Saving app data", value = 0.3, {
      cruz.list.save <- reactiveValuesToList(cruz.list)
      map.info <- list()
      das.info <- list()
      ndas.info <- list()


      #----------------------------------------------------
      # Map info
      map.info$lon.range <- cruz.map.range$lon.range
      map.info$lat.range <- cruz.map.range$lat.range
      map.info$world2 <- cruz.map.range$world2
      map.info$map.name <- cruz.map.range$map.name
      map.info$lon.left <- input$lon.left
      map.info$lon.right <- input$lon.right
      map.info$resolution <- input$resolution
      map.info$coast <- input$coast
      map.info$bar <- input$bar
      map.info$scale.lon <- input$scale.lon
      map.info$scale.lat <- input$scale.lat
      map.info$scale.units <- input$scale.units
      map.info$scale.len <- input$scale.len
      map.info$scale.width <- input$scale.width

      map.info$planned_transects_plot <- input$planned_transects_plot
      map.info$planned_transects_toplot <- input$planned_transects_toplot
      map.info$planned_transects_color <- input$planned_transects_color
      map.info$planned_transects_lw <- input$planned_transects_lw

      map.info$tick <- input$tick
      map.info$tick.left <- input$tick.left
      map.info$tick.right <- input$tick.right
      map.info$tick.bot <- input$tick.bot
      map.info$tick.top <- input$tick.top
      map.info$tick.interval.major <- input$tick.interval.major
      map.info$tick.interval.minor <- input$tick.interval.minor
      map.info$tick.style <- input$tick.style
      map.info$tick.length <- input$tick.length
      map.info$tick.left.lab <- input$tick.left.lab
      map.info$tick.right.lab <- input$tick.right.lab
      map.info$tick.bot.lab <- input$tick.bot.lab
      map.info$tick.top.lab <- input$tick.top.lab
      map.info$label.lon.start <- input$label.lon.start
      map.info$label.lat.start <- input$label.lat.start
      map.info$label.tick.font <- input$label.tick.font
      map.info$label.tick.size <- input$label.tick.size

      map.info$label.title <- input$label.title
      map.info$label.title.font <- input$label.title.font
      map.info$label.title.size <- input$label.title.size
      map.info$label.axis.lon <- input$label.axis.lon
      map.info$label.axis.lat <- input$label.axis.lat
      map.info$label.axis.font <- input$label.axis.font
      map.info$label.axis.size <- input$label.axis.size

      map.info$color_style <- input$color_style
      map.info$color_land_all <- input$color_land_all
      map.info$color.land <- input$color.land
      map.info$color_lakes_rivers <- input$color_lakes_rivers
      map.info$color_water_style <- input$color_water_style
      map.info$color.water <- input$color.water
      map.info$depth_style <- input$depth_style

      map.info$grid <- input$grid
      map.info$grid.line.color <- input$grid.line.color
      map.info$grid.line.type <- input$grid.line.type
      map.info$grid.line.width <- input$grid.line.width

      incProgress(0.2)


      #----------------------------------------------------
      # DAS info
      das.info$das_sightings <- input$das_sightings
      das.info$das_sightings_position <- input$das_sightings_position
      das.info$das_sighting_type <- input$das_sighting_type
      das.info$das_sighting_code_1_all <- input$das_sighting_code_1_all
      das.info$das_sighting_code_2_all <- input$das_sighting_code_2_all
      das.info$das.sighting.code.1 <- input$das.sighting.code.1
      das.info$das.sighting.code.2 <- input$das.sighting.code.2
      das.info$das.sighting.probable <- input$das.sighting.probable

      das.info$das.symbol.type <- input$das.symbol.type
      das.info$das.symbol.color <- input$das.symbol.color
      das.info$das.symbol.size <- input$das.symbol.size
      das.info$das.symbol.linewidth <- input$das.symbol.linewidth
      das.info$das_symbol_mult <- input$das_symbol_mult
      das.info$das.symbol.type.mult <- input$das.symbol.type.mult
      das.info$das.symbol.color.mult <- input$das.symbol.color.mult
      das.info$das.symbol.type.boat <- input$das.symbol.type.boat
      das.info$das.symbol.color.boat <- input$das.symbol.color.boat
      das.info$das.symbol.size.boat <- input$das.symbol.size.boat
      das.info$das.symbol.linewidth.boat <- input$das.symbol.linewidth.boat

      das.info$das_sightings_effort <- input$das_sightings_effort
      das.info$das.sight.minBeau <- input$das.sight.minBeau
      das.info$das.sight.maxBeau <- input$das.sight.maxBeau
      das.info$das.sight.dateRange <- as.character(input$das.sight.dateRange)
      das.info$das.sight.cruiseNum <- input$das.sight.cruiseNum
      das.info$das.sight.trunc.units <- input$das.sight.trunc.units
      das.info$das.sight.trunc <- input$das.sight.trunc

      das.info$das_legend <- input$das_legend
      das.info$das_legend_pos <- input$das_legend_pos
      das.info$das.legend.lon <- input$das.legend.lon
      das.info$das.legend.lat <- input$das.legend.lat
      das.info$das.legend.boxCol <- input$das.legend.boxCol
      das.info$das.legend.font <- input$das.legend.font
      das.info$das.legend.textSize <- input$das.legend.textSize
      das.info$das.legend.title <- input$das.legend.title
      das.info$das.legend.names <- input$das.legend.names
      das.info$das.legend.num <- input$das.legend.num

      das.info$eff_legend <- input$eff_legend
      das.info$eff_legend_pos <- input$eff_legend_pos
      das.info$eff.legend.lon <- input$eff.legend.lon
      das.info$eff.legend.lat <- input$eff.legend.lat
      das.info$eff.legend.boxCol <- input$eff.legend.boxCol
      das.info$eff.legend.font <- input$eff.legend.font
      das.info$eff.legend.textSize <- input$eff.legend.textSize

      das.info$das_effort <- input$das_effort
      das.info$das.effort.closePass <- input$das.effort.closePass
      das.info$das_effort_snf <- input$das_effort_snf

      das.info$das.effort.simp.col <- input$das.effort.simp.col
      das.info$das.effort.simp.lwd <- input$das.effort.simp.lwd

      das.info$das_effort_det_byBft <- input$das_effort_det_byBft
      das.info$das.effort.det.col.s <- input$das.effort.det.col.s
      das.info$das.effort.det.lwd.s <- input$das.effort.det.lwd.s
      das.info$das.effort.det.col.n <- input$das.effort.det.col.n
      das.info$das.effort.det.lwd.n <- input$das.effort.det.lwd.n
      das.info$das.effort.det.col.f <- input$das.effort.det.col.f
      das.info$das.effort.det.lwd.f <- input$das.effort.det.lwd.f

      das.info$das_effort_filter_same <- input$das_effort_filter_same
      das.info$das.effort.minBeau <- input$das.effort.minBeau
      das.info$das.effort.maxBeau <- input$das.effort.maxBeau
      das.info$das.effort.dateRange <- as.character(input$das.effort.dateRange)
      das.info$das.effort.cruiseNum <- input$das.effort.cruiseNum
      das.info$das.effort.trunc.units <- input$das.effort.trunc.units
      das.info$das.effort.trunc <- input$das.effort.trunc

      das.info$das_out_effort_units <- input$das_out_effort_units
      das.info$das.out.sight.closePass <- input$das.out.sight.closePass
      das.info$das.out.sight.snf <- input$das.out.sight.snf

      incProgress(0.3)


      #----------------------------------------------------
      # Non-DAS info
      ndas.info$ndas_plot <- input$ndas_plot

      incProgress(0.2)


      #----------------------------------------------------
      save(cruz.list.save, map.info, das.info, ndas.info, file = file)
    })
  }
)
