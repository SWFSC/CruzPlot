#' Open CruzPlot
#'
#' Open the CruzPlot utility program, an R Shiny application
#'
#' @param ... passed directly to [shiny::runApp()]
#'
#' @examples
#' if (interactive()) cruzplot_gui(launch.browser = TRUE)
#'
#' @export
cruzplot_gui <- function(...) {
  ###############################################################################
  ##### Assorted other stuff...
  old <- options()
  on.exit(options(old))

  options(shiny.maxRequestSize = 50 * 1024^2) #Max file size is now 50MB
  options("digits" = 5) #for proper display of sighting and effort coordinates

  plot.res <- 72 #Resolution of displayed plots; passed torenderPlot() calls

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ###############################################################################
  ##### UI
  ui.new.line <- function() helpText(HTML("<br/>"))
  ui.select.instructions <- function() {
    helpText(
      "To remove selected input(s): click the input(s) to remove, ",
      "and then click backspace or delete"
    )
  }


  ui <- dashboardPage(
    dashboardHeader(title = "CruzPlot", titleWidth = "200"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Create and Save Map", tabName = "createmap", icon = icon("th", lib = "font-awesome")),
        # menuItem("Plot DAS Data", tabName = "DASplot", icon = icon("th")),
        # menuItem("Plot Non-DAS Data", tabName = "nonDASplot", icon = icon("th")),
        # menuItem(HTML(paste0("Color and Formatting", "<br/>", "Options")), tabName = "dispColor", icon = icon("th")),
        # menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
        # menuItem("CruzPlot Manual", tabName = "dispManual", icon = icon("th")),
        tags$br(),
        fileInput("load_app_envir_file", "Load workspace"),
        column(
          width = 12,
          textOutput("load_app_text"),
          downloadButton("save_app_envir", "Save workspace", style = "color: black")
        ),
        tags$br(), tags$br(), tags$br(),
        numericInput("map_size", "Map height (pixels)", value = 600, min = 0, step = 100),
        tags$br(),
        actionButton("stop", "Close CruzPlot"),
        column(12, tags$h5(paste0("CruzPlot v", packageVersion("CruzPlot"))))
      ), width = "200"
    ),
    dashboardBody(
      useShinyjs(),
      # See https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
      extendShinyjs(text = jscode, functions = c("closeWindow")),

      # See https://stackoverflow.com/questions/59760316/change-the-color-of-text-in-validate-in-a-shiny-app
      tags$head( #validation text
        tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red; font-weight: bold;
                      }
                      "))
      ),
      # See https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      tabItems(
        tabItem(
          tabName = "createmap",
          fluidRow(
            box(
              status = "primary", width = 6,
              # mod_plot_ui("plot"),
              conditionalPanel("input.tabset1 == 'Range'", mod_plot_ui("plot1", TRUE)),
              conditionalPanel("input.tabset1 != 'Range'", mod_plot_ui("plot2"))
            ),
            tabBox(
              title = "Map", width = 6, id = "tabset1",
              mod_map_range_ui("map_range")
              # tabPanel("bib", h5("test"))
            )
          )
        )
      )
    )
  )

  ##############################################################################
  ##### server
  server <- function(input, output, session) {
    #----------------------------------------------------------------------------
    ### Quit GUI
    session$onSessionEnded(function() {
      stopApp(returnValue = "CruzPlot was closed")
    })

    observeEvent(input$stop, {
      js$closeWindow()
      stopApp(returnValue = "CruzPlot was closed")
    })
    

    #----------------------------------------------------------------------------
    ### Create empty reactives
    # cruz.map.range <- reactiveValues()
    map.range.update <- reactiveValues()


    #----------------------------------------------------------------------------
    ### Plot
    plot1.list <- mod_plot_server("plot1", reactive(input$map_size), cruz.map.range)
    plot1.brush <- plot1.list$brush
    mod_plot_server("plot2", reactive(input$map_size), cruz.map.range)

    #----------------------------------------------------------------------------
    ### Map tab
    map.range.list <- mod_map_range_server("map_range", map.range.update, plot1.brush)
    cruz.map.range <- map.range.list[["map_range"]]


    #----------------------------------------------------------------------------
    ### App 'environment' save/load
    output$save_app_envir <- downloadHandler(
      filename = function() {
        paste0("CruzPlot_", gsub("-", "_", Sys.Date()), ".RDATA")
      },

      content = function(file) {
        withProgress(message = "Saving app data", value = 0.3, {
          cruz.list.save <- list() #reactiveValuesToList(cruz.list)
          cruz.map.range.save <- reactiveValuesToList(cruz.map.range)
          incProgress(0.5)

          input.save <- reactiveValuesToList(input)
          incProgress(0.2)

          save(cruz.list.save, cruz.map.range.save, input.save, file = file)
        })
      }
    )

    output$load_app_text <- renderText({
      load_envir()
    })


    load_envir <- eventReactive(input$load_app_envir_file, {
      file.load <- req(input$load_app_envir_file)
      validate(
        need(
          (identical(str_to_upper(str_sub(file.load$name, -6)), ".RDATA") 
            & file.load$type == ""),
          "Error: Please load a file with the extension '.RDATA'"
        )
      )

      withProgress(message = "Loading saved data", value = 0.5, {
        load(file.load$datapath)
        files.list <- list("cruz.list.save", "cruz.map.range.save", "input.save")
        validate(
          need(
            all(vapply(files.list, function(i) exists(i), as.logical(1))),
            "Error: Loaded RDATA file does not contain a saved CruzPlot environment"
          )
        )
        rm(files.list)
        incProgress(0.4)

        # #--------------------------------------------------------------------------
        # ### Update reactiveValues
        # cruz.list$planned.transects <- cruz.list.save[["planned.transects"]]
        # cruz.list$coastline     <- cruz.list.save[["coastline"]]
        # cruz.list$bathy.xyz     <- cruz.list.save[["bathy.xyz"]]
        # cruz.list$sp.codes      <- cruz.list.save[["sp.codes"]]
        # cruz.list$sp.codes.name <- cruz.list.save[["sp.codes.name"]]
        # cruz.list$das.data      <- cruz.list.save[["das.data"]]
        # # Don't need to save sighting/effort data - will get updated when reloaded
        # cruz.list$ndas.data     <- cruz.list.save[["ndas.data"]]
        # cruz.list$ndas.df       <- cruz.list.save[["ndas.df"]]
        # cruz.list$ndas.toplot   <- cruz.list.save[["ndas.toplot"]]
        #
        # cruz.map.range$lon.range <- cruz.map.range.save$lon.range
        # cruz.map.range$lat.range <- cruz.map.range.save$lat.range
        # cruz.map.range$world2    <- cruz.map.range.save$world2
        # cruz.map.range$map.name  <- cruz.map.range.save$map.name
        # cruz.map.range$res       <- cruz.map.range.save$res


        # TODO: input values (eg map resolution)

        for (i in names(cruz.map.range.save)) {
          cruz.map.range[[i]] <- cruz.map.range.save[[i]]
          map.range.update[[i]] <- cruz.map.range.save[[i]]
        }

        # map.range.update$lon.range <- cruz.map.range.save$lon.range
        # map.range.update <- cruz.map.range
        incProgress(0.05)
        #
        #
        # # If color style will be updated, set flag
        # cruz.load.color.flag(input$color_style != input.save$color_style)
        #
        #
        # #--------------------------------------------------------------------------
        # ### Update variable defaults as necessary
        #
        # #------------------------------------------------------
        # # nDAS inputs
        # if (isTruthy(cruz.list$ndas.toplot)) updateCheckboxInput(session, "ndas_plot", value = TRUE)
        #
        # #------------------------------------------------------
        # # Update options based on color palette. Update actual values below
        # updateRadioButtons(session, "color_style", selected = input.save$color_style)

        # # Update color palettes here; this code is the same as in server_color
        # if (input.save$color_style == 1) {
        #   palette("default")
        #   c.pal <- cruz.palette.color
        #   updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
        #   updateSelectInput(session, "color_land", choices = c.pal, selected = "bisque1")
        #   updateSelectInput(session, "color_water", choices = c.pal, selected = "white")
        #   updateSelectInput(session, "grid_line_color", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = eff.bft.default)
        #   updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "ndas_line_col", choices = c.pal, selected = "black")
        #   updateSelectInput(session, "ndas_pt_col", choices = c.pal, selected = "black")
        #   rm(c.pal)
        # } else {
        #   palette(gray(0:5/5))
        #   c.pal <- cruz.palette.gray
        #   updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
        #   updateSelectInput(session, "color_land", choices = c.pal, selected = 4)
        #   updateSelectInput(session, "color_water", choices = c.pal, selected = 0)
        #   updateSelectInput(session, "grid_line_color", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "ndas_line_col", choices = c.pal, selected = 1)
        #   updateSelectInput(session, "ndas_pt_col", choices = c.pal, selected = 1)
        #   rm(c.pal)
        # }




        #------------------------------------------------------
        ## Main page
        updateNumericInput(session, "map_size", value = input.save$map_size)

        #------------------------------------------------------
        ## Map range module
        # ns_map_range <- NS("map_range")
        # updateNumericInput(session, ns_map_range("lon_left"), value = "-99")
        # updateNumericInput(session, "lon_right", value = input.save$lon_right)
        # updateNumericInput(session, "lat_bot", value = input.save$lat.range[1])
        # updateNumericInput(session, "lat_top", value = input.save$lat.range[2])
        # updateSelectInput(session, "resolution", selected = input.save$resolution)
        #
        # updateCheckboxInput(session, "coast", value = input.save$coast)
        #
        # updateCheckboxInput(session, "bar", value = input.save$bar)
        # updateNumericInput(session, "scale_lon", value = input.save$scale_lon)
        # updateNumericInput(session, "scale_lat", value = input.save$scale_lat)
        # updateRadioButtons(session, "scale_units", selected = input.save$scale_units)
        # updateNumericInput(session, "scale_len", value = input.save$scale_len)
        # updateNumericInput(session, "scale_width", value = input.save$scale_width)
        #
        # if (input.save$planned_transects_plot) {
        #   cruz.pt.load.toplot(input.save$planned_transects_toplot)
        #   cruz.pt.load.toplot2(input.save$planned_transects_toplot2)
        #   cruz.pt.load.color(input.save$planned_transects_color)
        #   cruz.pt.load.lty(input.save$planned_transects_lty)
        #   if (!(input$tabs == "createmap" & input$tabset1 == "planned_transects")) {
        #     cruz.pt.load.tabs(input$tabs)
        #     cruz.pt.load.tabset1(input$tabset1)
        #     updateTabItems(session, "tabs", selected = "createmap")
        #     updateTabsetPanel(session, "tabset1", selected = "planned_transects")
        #   }
        # }
        # updateCheckboxInput(session, "planned_transects_plot", value = input.save$planned_transects_plot)
        # updateSelectInput(session, "planned_transects_toplot", selected = input.save$planned_transects_toplot)
        # updateSelectInput(session, "planned_transects_toplot2", selected = input.save$planned_transects_toplot2)
        # updateSelectInput(session, "planned_transects_color", selected = input.save$planned_transects_color)
        # updateSelectInput(session, "planned_transects_lty", selected = input.save$planned_transects_lty)
        # updateNumericInput(session, "planned_transects_lw", value = input.save$planned_transects_lw)
        #
        # updateCheckboxInput(session, "tick", value = input.save$tick)
        # updateCheckboxInput(session, "tick_left", value = input.save$tick_left)
        # updateCheckboxInput(session, "tick_right ", value = input.save$tick_right )
        # updateCheckboxInput(session, "tick_bot", value = input.save$tick_bot)
        # updateCheckboxInput(session, "tick_top", value = input.save$tick_top)
        # updateNumericInput(session, "tick_interval_major", value = input.save$tick_interval_major)
        # updateNumericInput(session, "tick_interval_minor", value = input.save$tick_interval_minor)
        # updateSelectInput(session, "tick_style", selected = input.save$tick_style)
        # updateNumericInput(session, "tick_length", value = input.save$tick_length)
        # updateCheckboxInput(session, "tick_left_lab", value = input.save$tick_left_lab)
        # updateCheckboxInput(session, "tick_right_lab", value = input.save$tick_right_lab)
        # updateCheckboxInput(session, "tick_bot_lab", value = input.save$tick_bot_lab)
        # updateCheckboxInput(session, "tick_top_lab", value = input.save$tick_top_lab)
        # updateNumericInput(session, "label_lon_start", value = input.save$label_lon_start)
        # updateNumericInput(session, "label_lat_start", value = input.save$label_lat_start)
        # updateSelectInput(session, "label_tick_font", selected = input.save$label_tick_font)
        # updateNumericInput(session, "label_tick_size", value = input.save$label_tick_size)
        #
        # updateTextInput(session, "label_title", value = input.save$label_title)
        # updateSelectInput(session, "label_title_font", selected = input.save$label_title_font)
        # updateNumericInput(session, "label_title_size", value = input.save$label_title_size)
        # updateTextInput(session, "label_axis_lon", value = input.save$label_axis_lon)
        # updateTextInput(session, "label_axis_lat", value = input.save$label_axis_lat)
        # updateSelectInput(session, "label_axis_font", selected = input.save$label_axis_font)
        # updateNumericInput(session, "label_axis_size", value = input.save$label_axis_size)
        #
        # updateCheckboxInput(session, "color_land_all", value = input.save$color_land_all)
        # updateSelectInput(session, "color_land", selected = input.save$color_land)
        # updateCheckboxInput(session, "color_lakes_rivers", value = input.save$color_lakes_rivers)
        # updateRadioButtons(session, "color_water_style", selected = input.save$color_water_style)
        # updateSelectInput(session, "color_water", selected = input.save$color_water)
        # updateRadioButtons(session, "depth_style", selected = input.save$depth_style)
        #
        # updateCheckboxInput(session, "grid", value = input.save$grid)
        # updateSelectInput(session, "grid_line_color", selected = input.save$grid_line_color)
        # updateSelectInput(session, "grid_line_type", selected = input.save$grid_line_type)
        # updateNumericInput(session, "grid_line_width", value = input.save$grid_line_width)


        # #------------------------------------------------------
        # ## DAS data
        # if (isTruthy(cruz.list$das.data)) {
        #   updateNumericInput(session, "das_file_skip", value = input.save$das_file_skip)
        #   updateNumericInput(session, "das_file_days_gap", value = input.save$das_file_days_gap)
        #   updateSelectInput(session, "das_file_reset_effort", selected = input.save$das_file_reset_effort)
        #   updateSelectInput(session, "das_file_reset_event", selected = input.save$das_file_reset_event)
        #
        #   #------------------------------------------------------
        #   ## Sighting info
        #   updateCheckboxInput(session, "das_sightings", value = input.save$das_sightings)
        #   if (input.save$das_sightings) {
        #     updateRadioButtons(session, "das_sightings_position", selected = input.save$das_sightings_position)
        #     updateSelectInput(session, "das_sighting_type", selected = input.save$das_sighting_type)
        #     updateRadioButtons(session, "das_sighting_code_1_all", selected = input.save$das_sighting_code_1_all)
        #     updateRadioButtons(session, "das_sighting_code_2_all", selected = input.save$das_sighting_code_2_all)
        #     updateSelectInput(session, "das_sighting_code_1", selected = input.save$das_sighting_code_1)
        #     updateSelectInput(session, "das_sighting_code_2", selected = input.save$das_sighting_code_2)
        #     updateCheckboxInput(session, "das_sighting_probable", value = input.save$das_sighting_probable)
        #     updateCheckboxGroupInput(session, "das_sighting_events", selected = input.save$das_sighting_events)
        #
        #     updateSelectInput(session, "das_symbol_type", selected = input.save$das_symbol_type)
        #     updateSelectInput(session, "das_symbol_color", selected = input.save$das_symbol_color)
        #     updateTextInput(session, "das_symbol_size", value = input.save$das_symbol_size)
        #     updateTextInput(session, "das_symbol_linewidth", value = input.save$das_symbol_linewidth)
        #     updateCheckboxInput(session, "das_symbol_mult", value = input.save$das_symbol_mult)
        #     updateTextInput(session, "das_symbol_type_mult", value = input.save$das_symbol_type_mult)
        #     updateTextInput(session, "das_symbol_color_mult", value = input.save$das_symbol_color_mult)
        #     updateSelectInput(session, "das_symbol_type_boat", selected = input.save$das_symbol_type_boat)
        #     updateSelectInput(session, "das_symbol_color_boat", selected = input.save$das_symbol_color_boat)
        #     updateNumericInput(session, "das_symbol_size_boat", value = input.save$das.symbol_size_boat)
        #     updateNumericInput(session, "das_symbol_linewidth_boat", value = input.save$das_symbol_linewidth_boat)
        #
        #     updateRadioButtons(session, "das_sight_effort", selected = input.save$das_sight_effort)
        #     updateCheckboxGroupInput(session, "das_sight_cp", selected = input.save$das_sight_cp)
        #     updateCheckboxGroupInput(session, "das_sight_snf", selected = input.save$das_sight_snf)
        #     updateSelectInput(session, "das_sight_minBft", selected = input.save$das_sight_minBft)
        #     updateSelectInput(session, "das_sight_maxBft", selected = input.save$das_sight_maxBft)
        #     updateDateRangeInput(session, "das_sight_dateRange", start = input.save$das_sight_dateRange[1], end = input.save$das_sight_dateRange[2])
        #     updateSelectInput(session, "das_sight_cruise", selected = input.save$das_sight_cruise)
        #     updateRadioButtons(session, "das_sight_trunc_units", selected = input.save$das_sight_trunc_units)
        #     updateNumericInput(session, "das_sight_trunc", value = input.save$das_sight_trunc)
        #
        #     updateCheckboxInput(session, "das_legend", value = input.save$das_legend)
        #     updateSelectInput(session, "das_legend_pos", selected = input.save$das_legend_pos)
        #     updateNumericInput(session, "das_legend_lon", value = input.save$das_legend_lon)
        #     updateNumericInput(session, "das_legend_lat", value = input.save$das_legend_lat)
        #     updateSelectInput(session, "das_legend_boxCol", selected = input.save$das_legend_boxCol)
        #     updateSelectInput(session, "das_legend_font", selected = input.save$das_legend_font)
        #     updateNumericInput(session, "das_legend_textSize", value = input.save$das_legend_textSize)
        #     updateTextInput(session, "das_legend_title", value = input.save$das_legend_title)
        #     updateCheckboxGroupInput(session, "das_legend_names", selected = input.save$das_legend_names)
        #
        #     updateCheckboxGroupInput(session, "das_out_sciname", selected = input.save$das_out_sciname)
        #     updateCheckboxInput(session, "das_out_allcheck", value = input.save$das_out_allcheck)
        #   }
        #
        #
        #   #------------------------------------------------------
        #   ## Effort info
        #   updateRadioButtons(session, "das_effort", selected = input.save$das_effort)
        #   if (input.save$das_effort != 1) {
        #     updateCheckboxGroupInput(session, "das_effort_cp", selected = input.save$das_effort_cp)
        #     updateCheckboxGroupInput(session, "das_effort_snf", selected = input.save$das_effort_snf)
        #
        #     updateSelectInput(session, "das_effort_simp_col", selected = input.save$das_effort_simp_col)
        #     updateNumericInput(session, "das_effort_simp_lwd", value = input.save$das_effort_simp_lwd)
        #
        #     updateCheckboxInput(session, "das_effort_det_byBft", value = input.save$das_effort_det_byBft)
        #     updateSelectInput(session, "das_effort_det_bft_col", selected = input.save$das_effort_det_bft_col)
        #     updateNumericInput(session, "das_effort_det_bft_lwd", value = input.save$das_effort_det_bft_lwd)
        #     updateSelectInput(session, "das_effort_det_col_s", selected = input.save$das_effort_det_col_s)
        #     updateNumericInput(session, "das_effort_det_lwd_s", value = input.save$das_effort_det_lwd_s)
        #     updateSelectInput(session, "das_effort_det_col_n", selected = input.save$das_effort_det_col_n)
        #     updateNumericInput(session, "das_effort_det_lwd_n", value = input.save$das_effort_det_lwd_n)
        #     updateSelectInput(session, "das_effort_det_col_f", selected = input.save$das_effort_det_col_f)
        #     updateNumericInput(session, "das_effort_det_lwd_f", value = input.save$das_effort_det_lwd_f)
        #
        #     updateCheckboxInput(session, "das_effort_filter_same", value = input.save$das_effort_filter_same)
        #     updateSelectInput(session, "das_effort_minBft", selected = input.save$das_effort_minBft)
        #     updateSelectInput(session, "das_effort_maxBft", selected = input.save$das_effort_maxBft)
        #     updateDateRangeInput(session, "das_effort_dateRange", start = input.save$das_effort_dateRange[1], end = input.save$das_effort_dateRange[2])
        #     updateSelectInput(session, "das_effort_cruise", selected = input.save$das_effort_cruise)
        #     updateRadioButtons(session, "das_effort_trunc_units", selected = input.save$das_effort_trunc_units)
        #     updateNumericInput(session, "das_effort_trunc", value = input.save$das_effort_trunc)
        #
        #     updateCheckboxInput(session, "eff_legend", value = input.save$eff_legend)
        #     updateSelectInput(session, "eff_legend_pos", selected = input.save$eff_legend_pos)
        #     updateNumericInput(session, "eff_legend_lon", value = input.save$eff_legend_lon)
        #     updateNumericInput(session, "eff_legend_lat", value = input.save$eff_legend_lat)
        #     updateSelectInput(session, "eff_legend_boxCol", selected = input.save$eff_legend_boxCol)
        #     updateTextInput(session, "eff_legend_title", value = input.save$eff_legend_title)
        #     updateSelectInput(session, "eff_legend_font", selected = input.save$eff_legend_font)
        #     updateNumericInput(session, "eff_legend_textSize", value = input.save$eff_legend_textSize)
        #
        #     updateRadioButtons(session, "das_out_effort_units", selected = input.save$das_out_effort_units)
        #   }
        # }

        incProgress(0.05)
      })

      "Workspace loaded"
    })
  }

  ##############################################################################
  ##### Start it up
  shiny::shinyApp(ui = ui, server = server, ...)
}
