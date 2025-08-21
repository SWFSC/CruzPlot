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
        menuItem("App State", tabName = "appstate", icon = icon("th", lib = "font-awesome")),
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
        numericInput("plot_height", "Map height (pixels)", value = 600, min = 0, step = 100),
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
          tabName = "appstate",
          h3("Current App State"),
          p("This panel shows the values stored in the central 'app_state' object."),
          verbatimTextOutput("current_state_display")
        ),
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
              mod_map_elements_ui("map_elements")
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
    ### Create app state reactive
    app_state <- reactiveValues(
      # plot_height = NULL,
      # lon.range = NULL,
      # lat.range = NULL,
      # world2 = NULL,
      # map.name = list(),
      # grid = NULL,
      # grid_col = NULL,
      # grid_lwd = NULL,
      # grid_lty = NULL
    )
    map_range <- map_elements <- reactiveValues()

    observeEvent(input$plot_height, {
      app_state$plot_height <- input$plot_height
    })

    # observe({ print(reactiveValuesToList(app_state)) })
    output$current_state_display <- renderPrint({
      reactiveValuesToList(app_state)
    })


    #----------------------------------------------------------------------------
    ### Plot
    h <- reactive(input$plot_height)
    # plot_height <- reactive(input$plot_height)
    plot1.list <- mod_plot_server("plot1", h, map_range, map_elements)
    mod_plot_server("plot2", h, map_range, map_elements)

    #----------------------------------------------------------------------------
    ### Map tab
    map.range.list <- mod_map_range_server("map_range", app_state, plot1.list$brush)
    map_range <- map.range.list[["map_range"]]

    map_elements <- mod_map_elements_server("map_elements", app_state, map_range)

    #----------------------------------------------------------------------------
    ### App 'environment' save/load
    output$save_app_envir <- downloadHandler(
      filename = function() {
        paste0("CruzPlot_", gsub("-", "_", Sys.Date()), ".RDATA")
      },

      content = function(file) {
        withProgress(message = "Saving app data", value = 0.3, {
          # cruz.list.save <- list() #reactiveValuesToList(cruz.list)
          app_state_save <- reactiveValuesToList(app_state)
          incProgress(0.7)
          save(app_state_save, file = file)

          # input.save <- reactiveValuesToList(input)
          # incProgress(0.2)
          # save(app_state_save, input.save, file = file)
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

        # Update all of app_state
        for (i in names(app_state_save)) {
          app_state[[i]] <- app_state_save[[i]]
          # map.range.update[[i]] <- cruz.map.range.save[[i]]
        }
        incProgress(0.35)
        browser()

        # Update widgets on the main page, not in a module
        updateNumericInput(session, "plot_height", value = app_state$plot_height)
        incProgress(0.05)
      })

      "Workspace loaded"
    })
  }

  ##############################################################################
  ##### Start it up
  shiny::shinyApp(ui = ui, server = server, ...)
}
