#' Open CruzPlot
#'
#' Open the CruzPlot utility program, an R Shiny application
#'
#' @param ... passed directly to [shiny::shinyApp()]
#'
#' @examplesIf interactive()
#' cruzplot_gui()
#'
#' # To have Shiny listen on a specifc port
#' cruzplot_gui(options = list(browser = TRUE, port = 6305))
#'
#' @export
cruzplot_gui <- function(...) {
  ###############################################################################
  ##### Assorted other stuff...
  old <- options()
  on.exit(options(old))
  options(shiny.maxRequestSize = 50 * 1024^2) #Max file size is now 50MB
  options("digits" = 5) #for proper display of sighting and effort coordinates
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  # Tell the Shiny app where to find the www path, for eg the manual
  shiny::addResourcePath(
    "www", system.file("app/www", package = "CruzPlot")
  )
  # # If the below line is included, the pdf won't display in the app
  # on.exit(shiny::removeResourcePath("www"), add = TRUE)


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
        menuItem("Create Map", tabName = "createmap", icon = icon("th", lib = "font-awesome")),
        # menuItem("App State", tabName = "appstate", icon = icon("th", lib = "font-awesome")),
        # menuItem("Plot DAS Data", tabName = "DASplot", icon = icon("th")),
        menuItem("Plot Non-DAS Data", tabName = "nondas", icon = icon("th")),
        menuItem(
          HTML(paste0("Color and Formatting", "<br/>", "Options")), 
          tabName = "display_format", icon = icon("th")
        ),
        menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
        menuItem("CruzPlot Manual", tabName = "display_manual", icon = icon("th")),
        tags$br(),
        fileInput("load_app_envir_file", "Load workspace"),
        column(
          width = 12,
          textOutput("load_app_text"),
          downloadButton("save_app_envir", "Save workspace", style = "color: black")
        ),
        tags$br(), tags$br(), #tags$br(),
        numericInput("plot_height", "Map height (pixels)", value = 600, min = 0, step = 100),
        selectInput(
          "color_style", 
          "App-wide color style", 
          choices = list("Color" = 1, "Gray scale" = 2),
          selected = 1
        ), 
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
              conditionalPanel("input.tabset1 == 'Range'", mod_plot_ui("plot1", TRUE)),
              conditionalPanel("input.tabset1 != 'Range'", mod_plot_ui("plot2"))
            ),
            tabBox(
              title = "Map", width = 6, id = "tabset1",
              mod_map_range_ui("map_range"),
              mod_map_elements_ui("map_elements")[[1]], 
              mod_map_elements_ui("map_elements")[[2]], 
              mod_map_color_ui("map_color")
            )
          )
        ), 
        tabItem(
          tabName = "nondas",
          fluidRow(
            box(status = "primary", width = 6, mod_plot_ui("plot_ndas")), 
            mod_nondas_ui("nondas")            
          )
        ), 
        mod_display_format_ui("display_format", tab_name = "display_format"), 
        tabItem(
          tabName = "display_manual",
          tags$h5(
            "The manual can be downloaded through the below window, or from the GitHub repo", 
            tags$a(
              "at this link",
              # TODO: change link to production, and update manual to 2.0
              href = "https://github.com/SWFSC/CruzPlot/blob/modules/inst/app/www/CruzPlot_Manual_app.pdf",
              target = "_blank"
            )
          ), 
          tags$iframe(
            style = "height:600px; width:100%",
            src = "www/CruzPlot_Manual_app.pdf"
          )
        )
      )
    )
  )

  ##############################################################################
  ##### server
  server <- function(input, output, session) {
    #----------------------------------------------------------------------------
    ### Sidebar options (except load/save)

    # Quit app
    session$onSessionEnded(function() {
      stopApp(returnValue = "CruzPlot was closed")
    })
    observeEvent(input$stop, {
      js$closeWindow()
      stopApp(returnValue = "CruzPlot was closed")
    })

    # Plot height
    h <- reactive(input$plot_height)

    # Color style
    observeEvent(input$color_style, {
      if (input$color_style == 1) {
        palette("default")
        c.pal <- cruz.palette.color
        # updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
        updateSelectInput(session, NS("map_color")("color_land"), choices = c.pal, selected = "bisque1")
        updateSelectInput(session, NS("map_color")("color_water"), choices = c.pal, selected = "white")
        updateSelectInput(session, NS("map_elements")("grid_col"), choices = c.pal, selected = "black")
        # updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = "black")
        # updateTextInput(session, "das_symbol_color_mult", value = "Black")
        # updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = "black")
        # updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = eff.bft.default)
        # updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = "black")
        # updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = "black")
        # updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = "black")
        updateSelectInput(session, NS("nondas")("ndas_line_col"), choices = c.pal, selected = "black")
        updateSelectInput(session, NS("nondas")("ndas_pt_col"), choices = c.pal, selected = "black")

      } else if (input$color_style == 2) {
        palette(gray(0:5/5))
        c.pal <- cruz.palette.gray
        # updateSelectInput(session, "planned_transects_color", choices = c.pal, selected = "grey")
        updateSelectInput(session, NS("map_color")("color_land"), choices = c.pal, selected = 4)
        updateSelectInput(session, NS("map_color")("color_water"), choices = c.pal, selected = 0)
        updateSelectInput(session, NS("map_elements")("grid_col"), choices = c.pal, selected = 1)
        # updateSelectInput(session, "das_symbol_color", choices = c.pal, selected = 1)
        # updateTextInput(session, "das_symbol_color_mult", value = "Black")
        # updateSelectInput(session, "das_effort_simp_col", choices = c.pal, selected = 1)
        # updateSelectInput(session, "das_effort_det_bft_col", choices = c.pal, selected = 1)
        # updateSelectInput(session, "das_effort_det_col_s", choices = c.pal, selected = 1)
        # updateSelectInput(session, "das_effort_det_col_n", choices = c.pal, selected = 1)
        # updateSelectInput(session, "das_effort_det_col_f", choices = c.pal, selected = 1)
        updateSelectInput(session, NS("nondas")("ndas_line_col"), choices = c.pal, selected = 1)
        updateSelectInput(session, NS("nondas")("ndas_pt_col"), choices = c.pal, selected = 1)
      }
    })


    #----------------------------------------------------------------------------
    ### Map tab
    # 'Initialize' reactives
    load_state_map_range <- reactiveVal()
    load_state_map_elements <- reactiveVal()
    load_state_map_color <- reactiveVal()
    load_state_nondas <- reactiveVal()

    # Run the modules
    map.range.list <- mod_map_range_server("map_range", load_state_map_range, plot1.list$brush)
    map_range <- map.range.list[["map_range"]]

    map_elements <- mod_map_elements_server("map_elements", load_state_map_elements, map_range)
    map_color <- mod_map_color_server("map_color", load_state_map_color, map_range)
    
    nondas <- mod_nondas_server("nondas", load_state_nondas)

    #----------------------------------------------------------------------------
    ### Dashboard-level display tabs
    mod_display_format_server("display_format")
    
    #----------------------------------------------------------------------------
    ### Plots
    # plot_height <- reactive(input$plot_height)
    plot1.list <- mod_plot_server("plot1", h, map_range, map_elements, map_color, nondas)
    mod_plot_server("plot2", h, map_range, map_elements, map_color, nondas)
    mod_plot_server("plot_ndas", h, map_range, map_elements, map_color, nondas)


    #----------------------------------------------------------------------------
    ### App 'environment' save/load
    output$save_app_envir <- downloadHandler(
      filename = function() {
        paste0("CruzPlot_", gsub("-", "_", Sys.Date()), ".RDATA")
      },

      content = function(file) {
        withProgress(message = "Saving app data", value = 0.3, {
          app_state_save <- list(
            map_range = map.range.list$to_save(),
            map_elements = map_elements$to_save(), 
            map_color = map_color$to_save(), 
            nondas = nondas$to_save(),             
            color_style = input$color_style, 
            plot_height = input$plot_height
          )
          incProgress(0.6)
          save(app_state_save, file = file)
          incProgress(0.1)
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
          (identical(str_to_upper(str_sub(file.load$name, -6)), ".RDATA") & file.load$type == ""),
          "Error: Please load a file with the extension '.RDATA'"
        )
      )

      withProgress(message = "Loading saved data", value = 0.5, {
        incProgress(0.2)
        load(file.load$datapath)
        files.list <- list("app_state_save")
        validate(
          need(
            all(vapply(files.list, function(i) exists(i), as.logical(1))),
            "Error: Loaded RDATA file does not contain a saved CruzPlot environment"
          )
        )
        rm(files.list)
        incProgress(0.4)

        # 'reset' the reactiveVals, in case loading a file of the same name
        load_state_map_range(NULL)
        load_state_map_elements(NULL)
        load_state_map_color(NULL)
        load_state_nondas(NULL)

        load_state_map_range(app_state_save[["map_range"]])
        load_state_map_elements(app_state_save[["map_elements"]])
        load_state_map_color(app_state_save[["map_color"]])
        load_state_nondas(app_state_save[["nondas"]])
        incProgress(0.35)

        # Update widgets on the main page, not in a module
        updateNumericInput(session, "plot_height", value = app_state_save[["plot_height"]])
        updateSelectInput(session, "color_style", selected = app_state_save[["color_style"]])
        incProgress(0.05)
      })

      "Workspace loaded"
    })
  }

  ##############################################################################
  ##### Start it up
  shiny::shinyApp(ui = ui, server = server, ...)
}
