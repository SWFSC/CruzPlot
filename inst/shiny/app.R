### app.R for eSDM GUI by Sam Woodman, modified 2015 by Tim Gerrodette

###############################################################################
# Check for and attach packages
stopifnot(require(CruzPlot))
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

sapply(list.packages, require, character.only = TRUE)


###############################################################################
##### Assorted...
options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
options("digits" = 5)   # for proper display of sighting and effort coordinates
# map.height <- 950     # set to 630 for laptops, 950 for standard monitor, 5% larger than in server.R

source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)


### Read default values for map
if(file.exists("StarterVals.csv")) {
  start.ll <- suppressWarnings(read.csv("StarterVals.csv", header = TRUE))
} else {
  start.ll <- data.frame(X = c(-180, -110, 0, 33, 1))
}

start.tick <- NULL
start.tick$interval <- cruzTickUpdate(c(start.ll$X[2], start.ll$X[1]), c(start.ll$X[4], start.ll$X[3]))
start.tick$lon <- cruzTickStart(c(start.ll$X[1], start.ll$X[2]), start.tick$interval)
start.tick$lat <- cruzTickStart(c(start.ll$X[3], start.ll$X[4]), start.tick$interval)


cruz.palette.color <- list(
  "Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
  "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange",
  "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2",
  "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green",
  "Light Brown" = "wheat3", "Light Blue" = "lightblue",
  "Light Red" = "indianred2", "Gray" = "gray", "White" = "white"
)
cruz.palette.gray <- list(
  "Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
  "Gray" = 4, "Light Gray" = 5, "White" = 0
)
font.family <- list(
  "Sans" = 1, "Serif" = 2, "Mono" = 3
)
cruz.symbol.type   <- list(
  "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
  "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
  "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
  "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
  "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
  "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
  "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
)
cruz.line.type <- list(
  "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4,
  "Long dash" = 5, "Dot-long dash" = 6
)
cruz.beaufort <- list(
  "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4,
  "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9
)


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
      menuItem("CruzPlot Manual", tabName = "manual", icon = icon("th")),
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

    ### Control validate text output
    tags$head(
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

      # Display color/Format options
      tabItem(
        tabName = "dispColor",
        fluidRow(
          box(
            title = "Color/Format Options", status = "primary", solidHeader = TRUE,  width = 12,
            plotOutput("plotDisplay")
          )
        )
      ),

      # Display species codes and names
      tabItem(
        tabName = "dispSp",
        fluidRow(
          box(
            title = "Species Information", status = "primary", solidHeader = TRUE,  width = 12,
            radioButtons("sp_type", "Select species codes to display",
                         choices = list("Mammals" = 1, "Turtles" = 2, "All" = 3)),
            conditionalPanel(condition = "input.sp_type == 1", dataTableOutput("sp1")),
            conditionalPanel(condition = "input.sp_type == 2", dataTableOutput("sp2")),
            conditionalPanel(condition = "input.sp_type == 3", dataTableOutput("sp3"))
          )
        )
      ),

      # Display CruzPlot manual
      tabItem(
        tabName = "manual",
        helpText("Click 'Open in Browser' at top of the app in order to display manual in-app"),
        # tags$iframe(style="height:850px; width:100%; scrolling=yes", src="CruzPlot_Manual_app.pdf")
        uiOutput("manual_pdf") #output$manual_pdf is in 'server.R'
      )
    )
  )
)


###############################################################################
##### server
`%then%` <- shiny:::`%OR%` # For sequential need() evals within one validate()

##### CruzPlot server function
server <- function(input, output, session) {
  ### Quit CruzPlot
  observeEvent(input$stop, {
    stopApp(returnValue = "CruzPlot closed")
  })

  ### Code for reactive values and handling load/save environment options
  source(file.path("server_files", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)


  ########################  Non-reactive expressions  #########################

  source(file.path("server_files", "server_funcs.R"), local = TRUE, chdir = TRUE)
  # Reading DAS file is done using CruzPlot::das_read()

  # Countries to be removed for world2 map
  # Reference: http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
  remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali",
              "Burkina Faso", "Ghana", "Togo")
  mapnames <- map("world2", fill=TRUE, plot=FALSE)$names
  mapnames.hires <- map("world2Hires", fill=TRUE, plot=FALSE)$names
  regions.rm <- mapnames[!(mapnames %in% remove)]
  regions.rm.hires <- mapnames.hires[!(mapnames.hires %in% remove)]

  bathy.col <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")

  # font.family = c("sans", "serif", "mono")

  ######################## MUST BE UPDATED IF TURTLE CODES IN SpCodes.dat ARE CHANGED ########################
  # NOTE: Assumed less likely to have new turtle codes added than mammal codes, so turtle codes are hardcoded
  #    so that app can split up codes into mammal and turtle categories
  turtle.codes <- c("CC", "CM", "DC", "EI", "HT", "LK", "LV", "ND", "UH", "UT")
  ############################################################################################################

  #----------
  # DAS data-symbol property text inputs
  symbol.col <- c(
    "Black", "Dark Blue", "Dark Red", "Green", "Orange",
    "Blue", "Brown", "Red", "Yellow", "Aqua", "Tan", "Pink",
    "Light Green", "Light Brown", "Light Blue", "Light Red", "Gray", "White"
  )
  symbol.col.code <- c(
    "black", "darkblue", "red4", "forestgreen", "orange",
    "blue", "tan4", "red", "yellow", "aquamarine2", "bisque1", "hotpink",
    "green", "wheat3", "lightblue", "indianred2", "gray", "white"
  )
  symbol.col.gray <- list(
    "Black", "Dark Gray", "Charcoal", "Gray", "Light Gray", "White"
  )
  symbol.col.code.gray <- c(1, 2, 3, 4, 5, 0)

  # cruz.palette.color <- list(
  #   "Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
  #   "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange",
  #   "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2",
  #   "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green",
  #   "Light Brown" = "wheat3", "Light Blue" = "lightblue",
  #   "Light Red" = "indianred2", "Gray" = "gray", "White" = "white"
  # )
  # cruz.palette.gray <- list(
  #   "Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
  #   "Gray" = 4, "Light Gray" = 5, "White" = 0
  # )
  # cruz.symbol.type <- list(
  #   "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
  #   "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
  #   "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
  #   "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
  #   "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
  #   "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
  #   "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
  # )
  # cruz.line.type <- list(
  #   "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4,
  #   "Long dash" = 5, "Dot-long dash" = 6
  # )
  #----------

  # colors for displaying effort by Beaufort
  # effort lines will be shown from 0 to => max.bft
  # # number of colors = max.bft + 1
  max.bft <- 3
  bft.color <- c("darkblue", "blue", "green3", "red")

  ################################## Map Data #################################
  ### map height on screen in pixels
  #     set for specific computer display here and 5% larger in ui.R
  #     set to 600 for laptops, 900 for standard monitor
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

  # Lon dimensions and world2, lat dimensions
  source(file.path("server_files", "cruzMapRange.R"), local = TRUE, chdir = TRUE)

  # Map name and countries to remove if world2 map
  source(file.path("server_files", "cruzMapName.R"), local = TRUE, chdir = TRUE)

  # Water color and depth, land color
  source(file.path("server_files", "cruzMapColor.R"), local = TRUE, chdir = TRUE)

  # Rivers
  source(file.path("server_files", "cruzMapRiver.R"), local = TRUE, chdir = TRUE)

  # Scale bar
  source(file.path("server_files", "cruzMapScaleBar.R"), local = TRUE, chdir = TRUE)

  # Coastline
  source(file.path("server_files", "cruzMapCoastline.R"), local = TRUE, chdir = TRUE)

  # Major intervals
  source(file.path("server_files", "cruzMapInterval.R"), local = TRUE, chdir = TRUE)

  # Tick labels
  # source(file.path("server_files", "funcTickUpdate.R"), local = TRUE, chdir = TRUE)
  # source(file.path("server_files", "funcTickMinor.R"), local = TRUE, chdir = TRUE)
  # source(file.path("server_files", "funcTickStart.R"), local = TRUE, chdir = TRUE)

  source(file.path("server_files", "cruzMapTick.R"), local = TRUE, chdir = TRUE)

  # Grid lines
  source(file.path("server_files", "cruzMapGrid.R"), local = TRUE, chdir = TRUE)

  # Figure labels
  source(file.path("server_files", "cruzMapLabel.R"), local = TRUE, chdir = TRUE)

  # Planned transect lines
  source(file.path("server_files", "cruzMapPlannedTransects.R"), local = TRUE, chdir = TRUE)


  ################################## DAS Data #################################
  # Read Species codes and renderUI for mammal and turtle codes
  source(file.path("server_files", "cruzSpecies.R"), local = TRUE, chdir = TRUE)

  # Load DAS file and update symbol properties
  # using fileInput, the output dataframe das.file has name,size,type and datapath
  # the actual data are stored at the temporary file and location given by datapath
  source(file.path("server_files", "cruzDasGeneral.R"), local = TRUE, chdir = TRUE)

  # Sightings
  source(file.path("server_files", "cruzDasSight.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasSightFilter.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasSightRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasSightSymbol.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasSightLegend.R"), local = TRUE, chdir = TRUE)

  # Effort
  source(file.path("server_files", "cruzDasEffort.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasEffortFilter.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasEffortLegend.R"), local = TRUE, chdir = TRUE)

  # Interactive labels
  source(file.path("server_files", "cruzDasInteractive.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasInteractiveSight.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasInteractiveEffort.R"), local = TRUE, chdir = TRUE)
  # source(file.path("server_files", "funcCruzClosestPt.R"), local = TRUE, chdir = TRUE)


  ############################### Non-DAS Data ################################
  # Read csv file and plot lines or points
  source(file.path("server_files", "cruzNonDas.R"), local = TRUE, chdir = TRUE)


  ################################## Other ####################################
  source(file.path("server_files", "cruzServerRender.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzWorld2DataRange.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasRenderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_files", "cruzDasOutTabular.R"), local = TRUE, chdir = TRUE)


  ############################## Plot Land/Water ##############################
  plotMap <- reactive({ function() {
    # Set values and call reactive functions
    #   Both done first so validate statements are triggered before drawing

    source(file.path("server_files", "drawMap_setVals.R"), local = TRUE, chdir = TRUE)
    source(file.path("server_files", "drawData_setVals.R"), local = TRUE, chdir = TRUE)

    # Plot map: window, water, land, and map extras
    source(file.path("server_files", "drawMap.R"), local = TRUE, chdir = TRUE)

    # Plot data: sightings, legend, and effort
    source(file.path("server_files", "drawData.R"), local = TRUE, chdir = TRUE)
  }})


  ########################## Plot Interactive Labels ##########################
  plotInteractive <- reactive({ function() {
    source(file.path("server_files", "drawInteractive.R"), local = TRUE, chdir = TRUE)
  }})


  ################################## Outputs ##################################
  # Download Map
  source(file.path("server_files", "saveMap.R"), local = TRUE, chdir = TRUE)


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
  output$plotDisplay <- renderPlot({cruzDisplaySymbolProp()})


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

