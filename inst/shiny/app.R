### app.R for eSDM GUI by Sam Woodman, modified 2015 by Tim Gerrodette

###############################################################################
# Check for and attach packages
list.packages <- list(
  "dplyr", "DT", "geosphere", "mapdata", "marmap", "maps", "shiny",
  "shinydashboard", "shinyjs", "stringr"
)

p.check <- vapply(list.packages, requireNamespace, as.logical(1), quietly = TRUE)
if (!all(p.check))
  stop("To use CruzPlot, the following packages must be installed: ",
       paste(list.packages, collapse = ", "), "\n",
       "To install the missing packages, run the following:\n",
       "install.packages(c(\"", paste(list.packages[!p.check],
                                      collapse = "\", \""), "\"))")

stopifnot(
  "Error attaching CruzPlot package - please reintall CruzPlot" = require(CruzPlot),
  "Error attaching packages - please reinstall CruzPlot" = all(sapply(list.packages, require, character.only = TRUE))
)


###############################################################################
### Read default values for map - better way to do this..?
source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)

if(file.exists("StarterVals.csv")) {
  start.ll <- suppressWarnings(read.csv("StarterVals.csv", header = TRUE))
} else {
  start.ll <- data.frame(X = c(-180, -110, 0, 33, 1))
}

start.tick <- NULL
start.tick$interval <- cruzTickUpdate(c(start.ll$X[2], start.ll$X[1]), c(start.ll$X[4], start.ll$X[3]))
start.tick$lon <- cruzTickStart(c(start.ll$X[1], start.ll$X[2]), start.tick$interval)
start.tick$lat <- cruzTickStart(c(start.ll$X[3], start.ll$X[4]), start.tick$interval)


###############################################################################
##### Assorted other stuf...
options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
options("digits" = 5)   # for proper display of sighting and effort coordinates
# map.height <- 950     # set to 630 for laptops, 950 for standard monitor, 5% larger than in server.R

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


source(file.path("app_vals.R"), local = TRUE, chdir = TRUE)


###############################################################################
##### UI
ui.new.line <- function() {helpText(HTML("<br/>"))}
ui.selectize.instructions <- function() {
  helpText("To remove selected input(s): click the input(s) to remove and then click backspace or delete")
}


### Load files with UI code
source(file.path("ui_files", "ui_createMap.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_dasPlot.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_nonDasPlot.R"), local = TRUE, chdir = TRUE)
source(file.path("ui_files", "ui_other.R"), local = TRUE, chdir = TRUE)


### UI function
ui <- dashboardPage(
  dashboardHeader(title = "CruzPlot", titleWidth = "200"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Create and Save Map", tabName = "createmap", icon = icon("th", lib = "font-awesome")),
      menuItem("Plot DAS Data", tabName = "DASplot", icon = icon("th")),
      menuItem("Plot Non-DAS Data", tabName = "nonDASplot", icon = icon("th")),
      menuItem(HTML(paste0("Color and Formatting", "<br/>", "Options")), tabName = "dispColor", icon = icon("th")),
      menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
      menuItem("CruzPlot Manual", tabName = "dispManual", icon = icon("th")),
      ui.new.line(),
      fileInput("load_app_envir_file", "Load workspace"),
      column(
        width = 12,
        textOutput("load_app_text"),
        downloadButton("save_app_envir", "Save workspace", style = "color: black")
      ),
      ui.new.line(),
      ui.new.line(),
      radioButtons("map.size", NULL,
                   choices = list("Large map window" = 1,
                                  "Small map window" = 2),
                   selected = 1),
      ui.new.line(),
      actionButton("stop", "Close CruzPlot")
    ), width = "200"
  ),

  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    tags$head( #validation text
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red; font-weight: bold;
                      }
                      "))
    ),
    tabItems(
      ui.createMap(),
      ui.dasPlot(),
      ui.nonDasPlot(),
      ui.dispColor(),
      ui.dispSp(),
      ui.dispManual()
    )
  )
)


###############################################################################
##### server
`%then%` <- shiny:::`%OR%` # For sequential need() evals within one validate()

##### CruzPlot server function
server <- function(input, output, session) {
  ### Quit GUI
  session$onSessionEnded(function() {
    stopApp(returnValue = "CruzPlot was closed")
  })

  observeEvent(input$stop, {
    # browser()
    js$closeWindow()
    stopApp(returnValue = "CruzPlot was closed")
  })


  ### Code for reactive values and handling load/save environment options
  source(file.path("server_files", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Map tab

  # map height on screen in pixels????
  #   set for specific computer display here and 5% larger in ui.R
  #   set to 600 for laptops, 900 for standard monitor
  map.height.server <- reactive({
    if(input$map.size == 1) map.h <- 900
    if(input$map.size == 2) map.h <- 600

    map.h
  })

  map.height.ui <- reactive({
    if(input$map.size == 1) map.h <- 950
    if(input$map.size == 2) map.h <- 630

    map.h
  })

  # map.height.server.val <- reactiveVal(value = map.height.server())

  # map.height.server = 600
  # observe({
  #   map.height.server <- map.height.func()
  #   print(map.height.server)
  # })

  source(file.path("server_1_map", "cruzMapCoastline.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapColorGrid.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapLabel.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapPlannedTransects.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapScaleBar.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapTick.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_map", "cruzMapSave.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### DAS data tab
  # Read Species codes and renderUI for mammal and turtle codes
  source(file.path("server_files", "cruzSpecies.R"), local = TRUE, chdir = TRUE)

  # Load DAS file and update symbol properties
  # using fileInput, the output dataframe das.file has name,size,type and datapath
  # the actual data are stored at the temporary file and location given by datapath
  source(file.path("server_2_das", "cruzDasGeneral.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasRenderUI.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasSight.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightFilter.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightSymbol.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasSightLegend.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasEffort.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasEffortFilter.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasEffortLegend.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasInteractive.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasInteractiveSight.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_das", "cruzDasInteractiveEffort.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_2_das", "cruzDasTabular.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Non-DAS data tab
  # Read csv file and plot lines or points
  source(file.path("server_files", "cruzNonDas.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Other
  source(file.path("server_files", "cruzServerRender.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzWorld2DataRange.R"), local = TRUE, chdir = TRUE)


  #----------------------------------------------------------------------------
  ### Plot Land/Water
  plotMap <- reactive({ function() {
    # Set values and call reactive functions; done first to trigger validate statements
    source(file.path("server_draw_local", "draw_setVals.R"), local = TRUE, chdir = TRUE)

    # Plot map: window, water, land, and map extras
    source(file.path("server_draw_local", "drawMap.R"), local = TRUE, chdir = TRUE)

    # Plot data: sightings, legend, and effort
    source(file.path("server_draw_local", "drawData.R"), local = TRUE, chdir = TRUE)
  }})


  #----------------------------------------------------------------------------
  ### Plot Interactive Labels
  plotInteractive <- reactive({ function() {
    source(file.path("server_draw_local", "drawInteractive.R"), local = TRUE, chdir = TRUE)
  }})


  #----------------------------------------------------------------------------
  ### Outputs

  # Plot Map
  output$plot1 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot2 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot3 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot4 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot5 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot6 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })
  output$plot7 <- renderPlot({
    plotMap()()
    plotInteractive()()
  })

  # Plot Map pt 2
  output$plot1.ui <- renderUI({
    plotOutput("plot1", height = map.height.ui())
  })
  output$plot2.ui <- renderUI({
    plotOutput("plot2", height = map.height.ui())
  })
  output$plot3.ui <- renderUI({
    plotOutput("plot3", height = map.height.ui(), click = "sight_click")
  })
  output$plot4.ui <- renderUI({
    plotOutput("plot4", height = map.height.ui(), hover  = "sight_hover")
  })
  output$plot5.ui <- renderUI({
    plotOutput("plot5", height = map.height.ui(), click = "effort_click")
  })
  output$plot6.ui <- renderUI({
    plotOutput("plot6", height = map.height.ui(), hover = "effort_hover")
  })
  output$plot7.ui <- renderUI({
    plotOutput("plot7", height = map.height.ui())
  })


  # Display color/formatting options
  # slight bug: display does not occur when window is resized
  output$plotDisplay <- renderPlot({
    cruzDisplaySymbolProp()
  })


  # Display mammals, turtles, and all species codes
  output$sp1 <- renderDataTable({
    sp.mammals <- cruzSpeciesMammals()
    names(sp.mammals) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
    sp.mammals
  })
  output$sp2 <- renderDataTable({ # Turtles
    sp.turtles <- cruzSpeciesTurtles()
    names(sp.turtles) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
    sp.turtles
  })
  output$sp3 <- renderDataTable({ # All
    sp.all <- cruzSpecies()
    names(sp.all) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
    sp.all
  })


  ### Display pdf of manual
  # Manual opens in new window from RStudio Shiny viewer, but displays in-app on Chrome
  output$manual_pdf <- renderUI({
    tags$iframe(style = "height:850px; width:100%", src = "CruzPlot_Manual_app.pdf")
  })
}

shiny::shinyApp(ui = ui, server = server)

