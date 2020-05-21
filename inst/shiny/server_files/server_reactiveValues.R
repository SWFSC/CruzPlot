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
  das.sight.filt = NULL,    # Filtered sighting data - used to print NA notice messages
  das.eff.filt = NULL,      # Filtered effort data - used to print NA notice messages
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
    # Don't need to save sighting/effort data - will get updated when reloaded
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
    updateNumericInput(session, "map_size", value = map.info$map_size)

    updateNumericInput(session, "lon_left", value = map.info$lon_left)
    updateNumericInput(session, "lon_right", value = map.info$lon_right)
    updateNumericInput(session, "lat_bot", value = map.info$lat.range[1])
    updateNumericInput(session, "lat_top", value = map.info$lat.range[2])
    updateSelectInput(session, "resolution", selected = map.info$resolution)

    updateCheckboxInput(session, "coast", value = map.info$coast)

    updateCheckboxInput(session, "bar", value = map.info$bar)
    updateNumericInput(session, "scale_lon", value = map.info$scale_lon)
    updateNumericInput(session, "scale_lat", value = map.info$scale_lat)
    updateRadioButtons(session, "scale_units", selected = map.info$scale_units)
    updateNumericInput(session, "scale_len", value = map.info$scale_len)
    updateNumericInput(session, "scale_width", value = map.info$scale_width)

    updateCheckboxInput(session, "planned_transects_plot", value = map.info$planned_transects_plot)
    updateSelectizeInput(session, "planned_transects_toplot", selected = map.info$planned_transects_toplot)
    updateSelectizeInput(session, "planned_transects_color", selected = map.info$planned_transects_color)
    updateTextInput(session, "planned_transects_lw", value = map.info$planned_transects_lw)

    updateCheckboxInput(session, "tick", value = map.info$tick)
    updateCheckboxInput(session, "tick_left", value = map.info$tick_left)
    updateCheckboxInput(session, "tick_right ", value = map.info$tick_right )
    updateCheckboxInput(session, "tick_bot", value = map.info$tick_bot)
    updateCheckboxInput(session, "tick_top", value = map.info$tick_top)
    updateNumericInput(session, "tick_interval_major", value = map.info$tick_interval_major)
    updateNumericInput(session, "tick_interval_minor", value = map.info$tick_interval_minor)
    updateSelectInput(session, "tick_style", selected = map.info$tick_style)
    updateNumericInput(session, "tick_length", value = map.info$tick_length)
    updateCheckboxInput(session, "tick_left_lab", value = map.info$tick_left_lab)
    updateCheckboxInput(session, "tick_right_lab", value = map.info$tick_right_lab)
    updateCheckboxInput(session, "tick_bot_lab", value = map.info$tick_bot_lab)
    updateCheckboxInput(session, "tick_top_lab", value = map.info$tick_top_lab)
    updateNumericInput(session, "label_lon_start", value = map.info$label_lon_start)
    updateNumericInput(session, "label_lat_start", value = map.info$label_lat_start)
    updateSelectInput(session, "label_tick_font", selected = map.info$label_tick_font)
    updateNumericInput(session, "label_tick_size", value = map.info$label_tick_size)

    updateTextInput(session, "label_title", value = map.info$label_title)
    updateSelectInput(session, "label_title_font", selected = map.info$label_title_font)
    updateNumericInput(session, "label_title_size", value = map.info$label_title_size)
    updateTextInput(session, "label_axis_lon", value = map.info$label_axis_lon)
    updateTextInput(session, "label_axis_lat", value = map.info$label_axis_lat)
    updateSelectInput(session, "label_axis_font", selected = map.info$label_axis_font)
    updateNumericInput(session, "label_axis_size", value = map.info$label_axis_size)

    updateRadioButtons(session, "color_style", selected = map.info$color_style)
    # Upadte color palettes here
    # TODO should these come after 'Then update values'?
    if (map.info$color_style == 1) {
      palette("default")
      updateSelectInput(session, "color_land", choices = cruz.palette.color, selected = "bisque1")
      updateSelectInput(session, "color_water", choices = cruz.palette.color, selected = "white")
      updateSelectInput(session, "grid_line_color", choices = cruz.palette.color, selected = "black")
      updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "das_effort_lineCol", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas_line_col", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.color, selected = "black")
    } else {
      palette(gray(0:5/5))
      updateSelectInput(session, "color_land", choices = cruz.palette.gray, selected = 4)
      updateSelectInput(session, "color_water", choices = cruz.palette.gray, selected = 0)
      updateSelectInput(session, "grid_line_color", choices = cruz.palette.gray, selected = 1)
      updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas_line_col", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.gray, selected = 1)
    }
    # Then update values
    updateCheckboxInput(session, "color_land_all", value = map.info$color_land_all)
    updateSelectInput(session, "color_land", selected = map.info$color_land)
    updateCheckboxInput(session, "color_lakes_rivers", value = map.info$color_lakes_rivers)
    updateRadioButtons(session, "color_water_style", selected = map.info$color_water_style)
    updateSelectInput(session, "color_water", selected = map.info$color_water)
    updateRadioButtons(session, "depth_style", selected = map.info$depth_style)

    updateCheckboxInput(session, "grid", value = map.info$grid)
    updateSelectInput(session, "grid_line_color", selected = map.info$grid_line_color)
    updateSelectInput(session, "grid_line_type", selected = map.info$grid_line_type)
    updateNumericInput(session, "grid_line_width", value = map.info$grid_line_width)


    #------------------------------------------------------
    ## DAS data
    if (isTruthy(cruz.list$das.data)) {
      #------------------------------------------------------
      ## Sighting info
      updateCheckboxInput(session, "das_sightings", value = das.info$das_sightings)
      updateRadioButtons(session, "das_sightings_position", selected = das.info$das_sightings_position)
      updateSelectInput(session, "das_sighting_type", selected = das.info$das_sighting_type)
      updateRadioButtons(session, "das_sighting_code_1_all", selected = das.info$das_sighting_code_1_all)
      updateRadioButtons(session, "das_sighting_code_2_all", selected = das.info$das_sighting_code_2_all)
      updateSelectizeInput(session, "das_sighting_code_1", selected = das.info$das_sighting_code_1)
      updateSelectizeInput(session, "das_sighting_code_2", selected = das.info$das_sighting_code_2)
      updateCheckboxInput(session, "das_sighting_probable", value = das.info$das_sighting_probable)
      updateCheckboxGroupInput(session, "das_sighting_events", selected = das.info$das_sighting_events)
      updateNumericInput(session, "das_file_skip", value = das.info$das_file_skip)
      updateSelectInput(session, "das_file_reset_effort", selected = das.info$das_file_reset_effort)
      updateSelectInput(session, "das_file_reset_event", selected = das.info$das_file_reset_event)
      updateSelectInput(session, "das_file_reset_day", selected = das.info$das_file_reset_day)

      updateSelectizeInput(session, "das_symbol_type", selected = das.info$das_symbol_type)
      updateSelectizeInput(session, "das_symbol_color", selected = das.info$das_symbol_color)
      updateTextInput(session, "das_symbol_size", value = das.info$das_symbol_size)
      updateTextInput(session, "das_symbol_linewidth", value = das.info$das_symbol_linewidth)
      updateCheckboxInput(session, "das_symbol_mult", value = das.info$das_symbol_mult)
      updateTextInput(session, "das_symbol_type_mult", value = das.info$das_symbol_type_mult)
      updateTextInput(session, "das_symbol_color_mult", value = das.info$das_symbol_color_mult)
      updateSelectInput(session, "das_symbol_type_boat", selected = das.info$das_symbol_type_boat)
      updateSelectInput(session, "das_symbol_color_boat", selected = das.info$das_symbol_color_boat)
      updateNumericInput(session, "das_symbol_size_boat", value = map.info$das.symbol_size_boat)
      updateNumericInput(session, "das_symbol_linewidth_boat", value = map.info$das_symbol_linewidth_boat)

      updateRadioButtons(session, "das_sight_effort", selected = das.info$das_sight_effort)
      updateCheckboxGroupInput(session, "das_sight_cp", selected = das.info$das_sight_cp)
      updateCheckboxGroupInput(session, "das_sight_snf", selected = das.info$das_sight_snf)
      updateSelectInput(session, "das_sight_minBft", selected = das.info$das_sight_minBft)
      updateSelectInput(session, "das_sight_maxBft", selected = das.info$das_sight_maxBft)
      updateDateRangeInput(session, "das_sight_dateRange", start = das.info$das_sight_dateRange[1], end = das.info$das_sight_dateRange[2])
      updateSelectizeInput(session, "das_sight_cruise", selected = das.info$das_sight_cruise)
      updateRadioButtons(session, "das_sight_trunc_units", selected = das.info$das_sight_trunc_units)
      updateNumericInput(session, "das_sight_trunc", value = das.info$das_sight_trunc)

      updateCheckboxInput(session, "das_legend", value = das.info$das_legend)
      updateSelectInput(session, "das_legend_pos", selected = das.info$das_legend_pos)
      updateTextInput(session, "das_legend_lon", value = das.info$das_legend_lon)
      updateTextInput(session, "das_legend_lat", value = das.info$das_legend_lat)
      updateSelectInput(session, "das_legend_boxCol", selected = das.info$das_legend_boxCol)
      updateSelectInput(session, "das_legend_font", selected = das.info$das_legend_font)
      updateNumericInput(session, "das_legend_textSize", value = das.info$das_legend_textSize)
      updateTextInput(session, "das_legend_title", value = das.info$das_legend_title)
      updateCheckboxGroupInput(session, "das_legend_names", selected = das.info$das_legend_names)
      updateCheckboxInput(session, "das_legend_num", value = das.info$das_legend_num)

      updateCheckboxInput(session, "eff_legend", value = das.info$eff_legend)
      updateSelectInput(session, "eff_legend_pos", selected = das.info$eff_legend_pos)
      updateTextInput(session, "eff_legend_lon", value = das.info$eff_legend_lon)
      updateTextInput(session, "eff_legend_lat", value = das.info$eff_legend_lat)
      updateSelectInput(session, "eff_legend_boxCol", selected = das.info$eff_legend_boxCol)
      updateSelectInput(session, "eff_legend_font", selected = das.info$eff_legend_font)
      updateNumericInput(session, "eff_legend_textSize", value = das.info$eff_legend_textSize)


      #------------------------------------------------------
      ## Effort info
      updateRadioButtons(session, "das_effort", selected = das.info$das_effort)
      updateCheckboxGroupInput(session, "das_effort_cp", selected = das.info$das_effort_cp)
      updateCheckboxGroupInput(session, "das_effort_snf", selected = das.info$das_effort_snf)

      updateSelectInput(session, "das_effort_simp_col", selected = das.info$das_effort_simp_col)
      updateNumericInput(session, "das_effort_simp_lwd", value = das.info$das_effort_simp_lwd)

      updateCheckboxInput(session, "das_effort_det_byBft", value = das.info$das_effort_det_byBft)
      updateSelectInput(session, "das_effort_det_col_s", selected = das.info$das_effort_det_col_s)
      updateNumericInput(session, "das_effort_det_lwd_s", value = das.info$das_effort_det_lwd_s)
      updateSelectInput(session, "das_effort_det_col_n", selected = das.info$das_effort_det_col_n)
      updateNumericInput(session, "das_effort_det_lwd_n", value = das.info$das_effort_det_lwd_n)
      updateSelectInput(session, "das_effort_det_col_f", selected = das.info$das_effort_det_col_f)
      updateNumericInput(session, "das_effort_det_lwd_f", value = das.info$das_effort_det_lwd_f)

      updateCheckboxInput(session, "das_effort_filter_same", value = das.info$das_effort_filter_same)
      updateSelectInput(session, "das_effort_minBft", selected = das.info$das_effort_minBft)
      updateSelectInput(session, "das_effort_maxBft", selected = das.info$das_effort_maxBft)
      updateDateRangeInput(session, "das_effort_dateRange",
                           start = das.info$das_effort_dateRange[1], end = das.info$das_effort_dateRange[2])
      updateSelectizeInput(session, "das_effort_cruise", selected = das.info$das_effort_cruise)
      updateRadioButtons(session, "das_effort_trunc_units", selected = das.info$das_effort_trunc_units)
      updateNumericInput(session, "das_effort_trunc", value = das.info$das_effort_trunc)

      updateRadioButtons(session, "das_out_effort_units", selected = das.info$das_out_effort_units)
      updateCheckboxGroupInput(session, "das_out_sight_cp", selected = das.info$das_out_sight_cp)
      updateCheckboxGroupInput(session, "das_out_sight_snf", selected = das.info$das_out_sight_snf)
    }

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
      map.info$map_size <- input$map_size
      map.info$lon.range <- cruz.map.range$lon.range
      map.info$lat.range <- cruz.map.range$lat.range
      map.info$world2 <- cruz.map.range$world2
      map.info$map.name <- cruz.map.range$map.name
      map.info$lon_left <- input$lon_left
      map.info$lon_right <- input$lon_right
      # latitude gotten from map.info$lat.range
      map.info$resolution <- input$resolution
      map.info$coast <- input$coast
      map.info$bar <- input$bar
      map.info$scale_lon <- input$scale_lon
      map.info$scale_lat <- input$scale_lat
      map.info$scale_units <- input$scale_units
      map.info$scale_len <- input$scale_len
      map.info$scale_width <- input$scale_width

      map.info$planned_transects_plot <- input$planned_transects_plot
      map.info$planned_transects_toplot <- input$planned_transects_toplot
      map.info$planned_transects_color <- input$planned_transects_color
      map.info$planned_transects_lw <- input$planned_transects_lw

      map.info$tick <- input$tick
      map.info$tick_left <- input$tick_left
      map.info$tick_right <- input$tick_right
      map.info$tick_bot <- input$tick_bot
      map.info$tick_top <- input$tick_top
      map.info$tick_interval_major <- input$tick_interval_major
      map.info$tick_interval_minor <- input$tick_interval_minor
      map.info$tick_style <- input$tick_style
      map.info$tick_length <- input$tick_length
      map.info$tick_left_lab <- input$tick_left_lab
      map.info$tick_right_lab <- input$tick_right_lab
      map.info$tick_bot_lab <- input$tick_bot_lab
      map.info$tick_top_lab <- input$tick_top_lab
      map.info$label_lon_start <- input$label_lon_start
      map.info$label_lat_start <- input$label_lat_start
      map.info$label_tick_font <- input$label_tick_font
      map.info$label_tick_size <- input$label_tick_size

      map.info$label_title <- input$label_title
      map.info$label_title_font <- input$label_title_font
      map.info$label_title_size <- input$label_title_size
      map.info$label_axis_lon <- input$label_axis_lon
      map.info$label_axis_lat <- input$label_axis_lat
      map.info$label_axis_font <- input$label_axis_font
      map.info$label_axis_size <- input$label_axis_size

      map.info$color_style <- input$color_style
      map.info$color_land_all <- input$color_land_all
      map.info$color_land <- input$color_land
      map.info$color_lakes_rivers <- input$color_lakes_rivers
      map.info$color_water_style <- input$color_water_style
      map.info$color_water <- input$color_water
      map.info$depth_style <- input$depth_style

      map.info$grid <- input$grid
      map.info$grid_line_color <- input$grid_line_color
      map.info$grid_line_type <- input$grid_line_type
      map.info$grid_line_width <- input$grid_line_width

      incProgress(0.2)


      #----------------------------------------------------
      # DAS info
      if (isTruthy(cruz.list$das.data)) {
        das.info$das_sightings <- input$das_sightings
        das.info$das_sightings_position <- input$das_sightings_position
        das.info$das_sighting_type <- input$das_sighting_type
        das.info$das_sighting_code_1_all <- input$das_sighting_code_1_all
        das.info$das_sighting_code_2_all <- input$das_sighting_code_2_all
        das.info$das_sighting_code_1 <- input$das_sighting_code_1
        das.info$das_sighting_code_2 <- input$das_sighting_code_2
        das.info$das_sighting_probable <- input$das_sighting_probable
        das.info$das_sighting_events <- input$das_sighting_events
        das.info$das_file_skip <- input$das_file_skip
        das.info$das_file_reset_effort <- input$das_file_reset_effort
        das.info$das_file_reset_event <- input$das_file_reset_event
        das.info$das_file_reset_day <- input$das_file_reset_day

        das.info$das_symbol_type <- input$das_symbol_type
        das.info$das_symbol_color <- input$das_symbol_color
        das.info$das_symbol_size <- input$das_symbol_size
        das.info$das_symbol_linewidth <- input$das_symbol_linewidth
        das.info$das_symbol_mult <- input$das_symbol_mult
        das.info$das_symbol_type_mult <- input$das_symbol_type_mult
        das.info$das_symbol_color_mult <- input$das_symbol_color_mult
        das.info$das_symbol_type_boat <- input$das_symbol_type_boat
        das.info$das_symbol_color_boat <- input$das_symbol_color_boat
        das.info$das_symbol_size_boat <- input$das_symbol_size_boat
        das.info$das_symbol_linewidth_boat <- input$das_symbol_linewidth_boat

        das.info$das_sight_effort <- input$das_sight_effort
        das.info$das_sight_cp <- input$das_sight_cp
        das.info$das_sight_snf <- input$das_sight_snf
        das.info$das_sight_minBft <- input$das_sight_minBft
        das.info$das_sight_maxBft <- input$das_sight_maxBft
        das.info$das_sight_dateRange <- as.character(input$das_sight_dateRange)
        das.info$das_sight_cruise <- input$das_sight_cruise
        das.info$das_sight_trunc_units <- input$das_sight_trunc_units
        das.info$das_sight_trunc <- input$das_sight_trunc

        das.info$das_legend <- input$das_legend
        das.info$das_legend_pos <- input$das_legend_pos
        das.info$das_legend_lon <- input$das_legend_lon
        das.info$das_legend_lat <- input$das_legend_lat
        das.info$das_legend_boxCol <- input$das_legend_boxCol
        das.info$das_legend_font <- input$das_legend_font
        das.info$das_legend_textSize <- input$das_legend_textSize
        das.info$das_legend_title <- input$das_legend_title
        das.info$das_legend_names <- input$das_legend_names
        das.info$das_legend_num <- input$das_legend_num

        das.info$eff_legend <- input$eff_legend
        das.info$eff_legend_pos <- input$eff_legend_pos
        das.info$eff_legend_lon <- input$eff_legend_lon
        das.info$eff_legend_lat <- input$eff_legend_lat
        das.info$eff_legend_boxCol <- input$eff_legend_boxCol
        das.info$eff_legend_font <- input$eff_legend_font
        das.info$eff_legend_textSize <- input$eff_legend_textSize

        das.info$das_effort <- input$das_effort
        das.info$das_effort_cp <- input$das_effort_cp
        das.info$das_effort_snf <- input$das_effort_snf

        das.info$das_effort_simp_col <- input$das_effort_simp_col
        das.info$das_effort_simp_lwd <- input$das_effort_simp_lwd

        das.info$das_effort_det_byBft <- input$das_effort_det_byBft
        das.info$das_effort_det_col_s <- input$das_effort_det_col_s
        das.info$das_effort_det_lwd_s <- input$das_effort_det_lwd_s
        das.info$das_effort_det_col_n <- input$das_effort_det_col_n
        das.info$das_effort_det_lwd_n <- input$das_effort_det_lwd_n
        das.info$das_effort_det_col_f <- input$das_effort_det_col_f
        das.info$das_effort_det_lwd_f <- input$das_effort_det_lwd_f

        das.info$das_effort_filter_same <- input$das_effort_filter_same
        das.info$das_effort_minBft <- input$das_effort_minBft
        das.info$das_effort_maxBft <- input$das_effort_maxBft
        das.info$das_effort_dateRange <- as.character(input$das_effort_dateRange)
        das.info$das_effort_cruise <- input$das_effort_cruise
        das.info$das_effort_trunc_units <- input$das_effort_trunc_units
        das.info$das_effort_trunc <- input$das_effort_trunc

        das.info$das_out_effort_units <- input$das_out_effort_units
        das.info$das_out_sight_cp <- input$das_out_sight_cp
        das.info$das_out_sight_snf <- input$das_out_sight_snf
      }

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
