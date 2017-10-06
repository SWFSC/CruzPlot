### CruzPlot server file
## designed by Sam Woodman August 2015
## modifications by Tim Gerrodette September 2015  
## modifications by Sam Woodman July 2017


`%then%` <- shiny:::`%OR%` # For sequential need() evals within one validate()

##### CruzPlot server function
server <- function(input, output, session) {
  ### Quit CruzPlot
  observeEvent(input$stop, {
    stopApp(returnValue = "CruzPlot closed")
  })
  
  ### Code for reactive values and handling load/save environment options
  source('CruzPlot Files/server_reactiveValues.R', local=TRUE, echo=FALSE)
  
  
  ######################## Non-reactive expressions   #########################
  
  source("CruzPlot Files/cruzDisplaySymbolProp.R", local=TRUE, echo=FALSE) # Display symbols
  source('CruzPlot Files/funcTickMinor.R', local = TRUE, echo=FALSE) # Determine minor tick locations
  source('CruzPlot Files/funcCruzDasRead.R', local=TRUE, echo=FALSE) # Read .das data file
  source('CruzPlot Files/funcCruzSpeciesRead.R', local=TRUE, echo=FALSE) # Read .dat species file
  
  # Countries to be removed for world2 map
  # Reference: http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
  remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali",
              "Burkina Faso", "Ghana", "Togo")
  mapnames <- map("world2", fill=TRUE, plot=FALSE)$names
  mapnames.hires <- map("world2Hires", fill=TRUE, plot=FALSE)$names
  regions.rm <- mapnames[!(mapnames %in% remove)]
  regions.rm.hires <- mapnames.hires[!(mapnames.hires %in% remove)]
  
  bathy.col <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
  
  font.family = c("sans", "serif", "mono")
  
  ######################## MUST BE UPDATED IF TURTLE CODES IN SpCodes.dat ARE CHANGED ########################
  turtle.codes <- c("CC", "CM", "DC", "EI", "HT", "LK", "LV", "ND", "UH", "UT") 
  # NOTE: Assumed less likely to have new turtle codes added than mammal codes, so turtle codes are hardcoded
  #    so that app can split up codes into mammal and turtle categories
  ############################################################################################################
  
  # DAS data-symbol property text inputs
  symbol.col <- c("Black", "Dark Blue", "Dark Red", "Green", "Orange", 
                  "Blue", "Brown", "Red", "Yellow", "Aqua", "Tan", "Pink", 
                  "Light Green", "Light Brown", "Light Blue", "Light Red", "Gray", "White")
  symbol.col.code <- c("black", "darkblue", "red4", "forestgreen", "orange", 
                       "blue", "tan4", "red", "yellow", "aquamarine2", "bisque1", "hotpink", 
                       "green", "wheat3", "lightblue", "indianred2", "gray", "white")
  symbol.col.gray <- list("Black", "Dark Gray", "Charcoal", "Gray", "Light Gray", "White")
  symbol.col.code.gray <- c(1, 2, 3, 4, 5, 0)
  
  cruz.palette.color <- list("Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
                             "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange", 
                             "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2", 
                             "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green", 
                             "Light Brown" = "wheat3", "Light Blue" = "lightblue", 
                             "Light Red" = "indianred2", "Gray" = "gray", "White" = "white")
  cruz.palette.gray <- list("Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
                            "Gray" = 4, "Light Gray" = 5, "White" = 0)
  cruz.symbol.type <- list("0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3, 
                           "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7, 
                           "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10, 
                           "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13, 
                           "14: Square with Up Triangle" = 14, "15: Filled Square" = 15, 
                           "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18, 
                           "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20)
  cruz.line.type <- list("Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, 
                         "Long dash" = 5, "Dot-long dash" = 6)
  
  # colors for displaying effort by Beaufort
  # effort lines will be shown from 0 to => max.bft
  # # number of colors = max.bft + 1
  max.bft <- 3
  bft.color <- c("darkblue","blue","green3","red")  
  
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
  source('CruzPlot Files/cruzMapRange.R', local = TRUE, echo=FALSE)
  
  # Map name and countries to remove if world2 map
  source('CruzPlot Files/cruzMapName.R', local = TRUE, echo=FALSE)
  
  # Water color and depth, land color
  source('CruzPlot Files/cruzMapColor.R', local=TRUE, echo=FALSE)
  
  # Rivers
  source('CruzPlot Files/cruzMapRiver.R', local=TRUE, echo=FALSE)
  
  # Scale bar
  source('CruzPlot Files/cruzMapScaleBar.R', local=TRUE, echo=FALSE)
  
  # Coastline
  source('CruzPlot Files/cruzMapCoastline.R', local=TRUE, echo=FALSE)
  
  # Major intervals
  source('CruzPlot Files/cruzMapInterval.R', local=TRUE, echo=FALSE)
  
  # Tick labels
  source('CruzPlot Files/funcTickUpdate.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/funcTickMinor.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/funcTickStart.R', local=TRUE, echo=FALSE)
  
  source('CruzPlot Files/cruzMapTick.R', local=TRUE, echo=FALSE)
  
  # Grid lines
  source('CruzPlot Files/cruzMapGrid.R', local=TRUE, echo=FALSE)
  
  # Figure labels
  source('CruzPlot Files/cruzMapLabel.R', local=TRUE, echo=FALSE)
  
  # Planned transect lines
  source('CruzPlot Files/cruzMapPlannedTransects.R' ,local=TRUE, echo=FALSE)
  
  
  ################################## DAS Data #################################
  # Read Species codes and renderUI for mammal and turtle codes
  source('CruzPlot Files/cruzSpecies.R', local = TRUE, echo=FALSE)
  
  # Load DAS file and update symbol properties
  # using fileInput, the output dataframe das.file has name,size,type and datapath
  # the actual data are stored at the temporary file and location given by datapath
  source('CruzPlot Files/cruzDasGeneral.R', local=TRUE, echo=FALSE)
  
  # Sightings
  source('CruzPlot Files/cruzDasSight.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasSightFilter.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasSightRange.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasSightSymbol.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasSightLegend.R', local=TRUE, echo=FALSE)
  
  # Effort
  source('CruzPlot Files/cruzDasEffort.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasEffortFilter.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasEffortLegend.R', local=TRUE, echo=FALSE)
  
  # Interactive labels
  source('CruzPlot Files/cruzDasInteractive.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/funcCruzClosestPt.R', local=TRUE, echo=FALSE)
  
  
  ############################### Non-DAS Data ################################
  # Read csv file and plot lines or points
  source('CruzPlot Files/cruzNonDas.R', local=TRUE, echo=FALSE)
  
  
  ################################## Other ####################################
  source('CruzPlot Files/cruzServerRender.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzWorld2DataRange.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasRenderUI.R', local=TRUE, echo=FALSE)
  source('CruzPlot Files/cruzDasOutTabular.R', local=TRUE, echo=FALSE)
  
  
  ############################## Plot Land/Water ##############################
  plotMap <- reactive({ function() {
    # Set values and call reactive functions
    #   Both done first so validate statements are triggered before drawing
    
    source('CruzPlot Files/drawMap_setVals.R', local=TRUE, echo=FALSE)
    source('CruzPlot Files/drawData_setVals.R', local=TRUE, echo=FALSE)
    
    # Plot map: window, water, land, and map extras
    source('CruzPlot Files/drawMap.R', local=TRUE, echo=FALSE)
    
    # Plot data: sightings, legend, and effort
    source('CruzPlot Files/drawData.R', local=TRUE, echo=FALSE)
  }})
  
  
  ########################## Plot Interactive Labels ##########################
  plotInteractive <- reactive({ function() {
    source('CruzPlot Files/drawInteractive.R', local=TRUE, echo=FALSE)
  }})
  
  
  ################################## Outputs ##################################
  # Download Map
  source('CruzPlot Files/saveMap.R', local=TRUE, echo=FALSE)
  
  
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
    names(sp.mammals) <- c("Species Code", "Abbreviation", 
                           "Scientific Name", "Common Name")
    sp.mammals
  })
  output$sp2 <- renderDataTable({ # Turtles
    sp.turtles <- cruzSpeciesTurtles()
    names(sp.turtles) <- c("Species Code", "Abbreviation", 
                           "Scientific Name", "Common Name")
    sp.turtles
  })
  output$sp3 <- renderDataTable({ # All
    sp.all <- cruzSpecies()
    names(sp.all) <- c("Species Code", "Abbreviation", 
                       "Scientific Name", "Common Name")
    sp.all
  })
  
  
  ### Display pdf of manual
  #     Manual opens in new window from RStudio Shiny viewer, 
  #     but displays in-app on Chrome
  output$manual_pdf <- renderUI({
    tags$iframe(style = "height:600px; width:100%", 
                src = "CruzPlot_Manual_app.pdf")
  })
}