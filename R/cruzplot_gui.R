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
        # tags$br(),
        # fileInput("load_app_envir_file", "Load workspace"),
        # column(
        #   width = 12,
        #   textOutput("load_app_text"),
        #   downloadButton("save_app_envir", "Save workspace", style = "color: black")
        # ),
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
              mod_plot_ui("plot"),
              # helpText("todo plots"),
              # plotOutput("plot1b", height = "auto")
              # conditionalPanel("input.tabset1 == 'Range'", plotOutput("plot1", height = "auto", brush = "map_brush")),
              # conditionalPanel("input.tabset1 != 'Range'", plotOutput("plot1b", height = "auto"))
            ),
            tabBox(
              title = "Map", width = 6, id = "tabset1",
              tabItem("map_range", mod_map_range_ui("map_range")),
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
    ### Map tab
    map.height <- reactive(input$map_size)
    map.range.list <- mod_map_range_server("map_range")
    cruz.map.range <- map.range.list[["map_range"]]

    #----------------------------------------------------------------------------
    ### Plot
    mod_plot_server(
      "plot",
      map.height,
      cruz.map.range
    )

    # plotMap <- reactive({
    #   function() {
    #     # The on.exit call causes the coordinates, eg from a click or brush event,
    #     #   to not be scaled to the data space, aka their range is 0-1.
    #     #   This is ok because the only par calls in CruzPlot are around legend
    #     #   calls for the sake of the font family.
    #     # oldpar <- par(no.readonly = TRUE)
    #     # on.exit(par(oldpar))
    #
    #     lon.range <- cruz.map.range$lon.range
    #     lat.range <- cruz.map.range$lat.range
    #     world2 <- cruz.map.range$world2
    #     stopifnot("world2 param is not a logical" = inherits(world2, "logical"))
    #
    #     vals.bad <- c("", "-", "+", NA)
    #     validate( #lats
    #       need(all(!(lat.range %in% vals.bad) & between(lat.range, -90, 90)),
    #           "The latitudes must be a number between -90 and 90")
    #     )
    #
    #     if ((0 <= lon.range[1] & 0 <= lon.range[2]) || (lon.range[1] < 0 & lon.range[2] < 0))
    #     validate( #lons
    #       need(lon.range[1] < lon.range[2],
    #             paste(
    #               "Left longitude must be less than right longitude,",
    #               "unless left longitude is positive and right longitude",
    #               "is negative (Pacific-centered map)"
    #             )
    #           )
    #     )
    #     if (world2) {
    #       validate(
    #         need(all(!(lon.range %in% vals.bad) & between(lon.range, 0, 360)),
    #              "The longtiudes must be a number between -180 and 180")
    #       )
    #     } else {
    #       validate(
    #         need(all(!(lon.range %in% vals.bad) & between(lon.range, -180, 180)),
    #              "The longtiudes must be a number between -180 and 180")
    #
    #       )
    #     }
    #
    #   map.name <- cruz.map.range$map.name
    #   map(map.name[[1]], regions = map.name[[2]],
    #       xlim = lon.range[1:2], ylim = lat.range[1:2],
    #       fill = TRUE, col = "yellow",
    #       # add = TRUE
    #     )
    #   }
    # })

    # output$plot1b <- renderPlot({
    #   plotMap()()
    #   # map(map.name[[1]], regions = map.name[[2]],
    #   #     xlim = lon.range[1:2], ylim = lat.range[1:2],
    #   #     fill = TRUE, col = "yellow",
    #   #     # add = TRUE
    #   #   )
    # }, height = map.height, units = "px", res = plot.res)


  }

  ##############################################################################
  ##### Start it up
  shiny::shinyApp(ui = ui, server = server, ...)
}
