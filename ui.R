### CruzPlot ui file 
### designed by Sam Woodman August 2015
### modifications by Tim Gerrodette October 2015
### modifications by Sam Woodman July 2017

# note: when running browser from RStudio, the line width and text size numeric input boxes have the up-down arrows, 
#       but when running default browser launched from desktop shortcut, the arrows do not show
#       I think this might be an issue for Windows Explorer as the browser; RStudio uses another (Chrome?)

### Sep/Oct 2015: customized for 2015 vaquita cruise with the following changes:
# default map of northern Gulf of California (lines 25-26); also in StarterVals.csv file
# default high map resolution (line 92)
# default minor tick number = 4 (line 150)
# default effort mode = passing (line 613)
# drawMap has additional code to read a MapLines file and plot numbered transects lines automatically
# lat/lon in effort hover message to 4 decimal places (lines 19-26 in funcCruzClosestPt)
# widths of map and tabbox windows changed from 12 to 6 so that map and input appear side-by-side
# to make map larger, width and height are specified in renderPlot in server.R output
# height of map window set to 900 to display large map (lines 79,373,718)
# under Map Limits, helptext about lat/lon limits moved below map limits input boxes, now commented out (lines 85,99)
# Map Limits box height set to automatic (line 98)
# width of dashboard sidebar set just beyond longest string, "Formatting" changed to "Format" (line 61)
# text of coast line input changed to be specific to vaquita cruise (line 123)
# add display of planned transects (lines 127-132); also new code in drawMap
# actionButton to close CruzPlot deactivated (line 61)
# min date for vaquita cruise is 2015-09-20 (line 530,691)
# label for sighting and effort date filter changed slightly (line 530,691)
# helptext for cruise number and truncation moved below input boxes (lines 535-540,692)
# input box labels for species selection changed slightly for in cruzSpecies
# default code for plotting sightings set to 041 = Phocoena sinus (cruzSpecies line 33)
# changed helptext note for effort line plotting slightly (line 516)
# icon for map changed in sidebar menu (line 61)
# Effort type choices are Primary, Core 1, Core 2 and Core 3 (line 640)
# eliminate title and bar at top of map
# set map.height in pixels in server.R, also in ui.R but 5% greater
# map margins dynamic with title (line 48-49 in drawMap)
# change default legend position to topright for sightings legend and bottomright for effort legend (line 601)
# options("digits"=5) in server.R to allow decimal values for longitude tick labels
# sighting positions computed using angle and reticle: library geosphere in server.R, function destPoint in cruzDasSightRange(),  
#   add sight.lon and sight.lat to data.sight; add Course variable in funcCruzDasRead; change drawData to use sight locations
# fixed date range bug in cruzDasSightFilter, line 48-49, and cruzDasEffortFilter, lines 36-37
# DAS file does not include Events # (funcCruzDasRead, lines 9-10)
# matched sightings with same sighting number plotted only once (cruzDasSight line 77)
# fixed bug in sighting indexing (cruzDasSightFilter line 93 and cruzDasSightRange line 21)
# changed message if cursor not near enought to point or line (drawInteractive lines 17,27,34)
# shortened common name in cruzDasSightLegend to name before first comma (line 22)
# changed conditionality of Symbol properties box (line 473)
# more flexible reading of non-DAS file in (cruzNonDas, lines 9-12)
# variable effort line width by Beaufort (line 748-751 and line 46 in drawData) 
# legend for effort lines (drawData line 66-73 and file cruzDasEffortLegend)
# rearranged boxes in Plot Sighting/Legend page
# added control of effort line colors by Beaufort
# added additional core transects for vaquita survey (core 1-3)
# turned off choice of non-standard effort display (line 736)
# special possible vaquita code 977 (cruzDasSight line 53-55)
# added ship bearing into sighting location calculation (funcCruzDasRead, line 78-86; cruzDasSight, lines 161-162)
# to do: allow non-DAS points/lines to be added multiple times
# to do: allow interactive setting of map height
# to do: modify tabs in create.map page

### July 2017: edits for HICEAS
# New libraries: 'DT', 'shinyjs', 'dplyr'
# Moved ui code for 'Create Map', 'Plot DAS Data', and 'Plot non-DAS data' into separate ui_... files in 'CruzPlot Files' folder
# Changed default map to: 14N to 33N, -149W to -176W; also changed values in StarterVals.csv file
# Changed default effort type to 'Closing'
# Added 'Simplified effort' back in as an option
# Added leg.box.col to list returned by cruzDasSightLegend() in order to control legend box color
# Added extra graphics::box() after plotting legend so legend with no box doesn't erase border box
# Commented out projection options since nothing is implemented (yet)
# Commented out output$contents code at top of server.R
# Added reactiveValues 'cruz.list' for storing multiple dynamic inputs
# reactiveValues: coastline data, planned transects, DAS data, non-DAS data
# Added load/save app environment buttons that load/save reactiveValues (server.R)
# Made transect input general using reactiveValue (cruzMapPlannedTransects.R)
# Commented out 'Choose NGulf.coast.csv' button in coastline options
# Made reactiveValues for Scale bar and Tick variables whose defaults are updated based on map limits
#    This makes it so that the map isn't regenerated multiple times when using the world2 map
# Commented out observe() code in cruzDasGeneral that did not appear to be doing anything (line 69-78)
# Commented out write.csv(file = "sighting_position.csv") in cruzDasSight (line 172)
# Added column in cruzDasRead() 'EffType1' for closing/passing indicator
# Cleaned up effort processing code so that effort segments are split up by V events...
#    and Beufort filtering happens in 'Filter' file/code
# Simplified effort now defined as all R to E events
# All color and line width processing for effort data is done in cruzDasEffort.R's cruzDasEffortLines()
# Added Tabular Output section to summarize sightings and effort by species code and Beaufort level, respectively
# Added 'Planed Transect' and the majority of 'Non-DAS Data', both of which use reactiveValues and can have multiple objects loaded




library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(maps)
library(mapdata)
library(marmap)
library(stringr)
library(geosphere)
library(dplyr)

options(shiny.maxRequestSize = 50*1024^2) # Max file size is 50MB
options("digits" = 5)   # for proper display of sighting and effort coordinates
# map.height <- 950     # set to 630 for laptops, 950 for standard monitor, 5% larger than in server.R


### Read default values for map
if(file.exists("StarterVals.csv")) start.ll <- suppressWarnings(read.csv("StarterVals.csv", header = TRUE))
if(!file.exists("StarterVals.csv")) start.ll <- data.frame(X = c(-180, -110, 0, 33, 1))


source('CruzPlot Files/funcTickUpdate.R', local=TRUE, echo=FALSE)
source('CruzPlot Files/funcTickStart.R', local=TRUE, echo=FALSE)

start.tick <- NULL
start.tick$interval <- cruzTickUpdate(c(start.ll$X[2], start.ll$X[1]), c(start.ll$X[4], start.ll$X[3]))
start.tick$lon <- cruzTickStart(c(start.ll$X[1], start.ll$X[2]), start.tick$interval)
start.tick$lat <- cruzTickStart(c(start.ll$X[3], start.ll$X[4]), start.tick$interval)


cruz.palette.color <- list("Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
                           "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange", 
                           "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2", 
                           "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green", 
                           "Light Brown" = "wheat3", "Light Blue" = "lightblue", 
                           "Light Red" = "indianred2", "Gray" = "gray", "White" = "white")
cruz.palette.gray <- list("Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
                          "Gray" = 4, "Light Gray" = 5, "White" = 0)
font.family <- list("Sans" = 1, "Serif" = 2, "Mono" = 3)
cruz.symbol.type <- list("0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3, 
                         "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7, 
                         "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10, 
                         "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13, 
                         "14: Square with Up Triangle" = 14, "15: Filled Square" = 15, 
                         "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18, 
                         "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20)
cruz.line.type <- list("Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, 
                       "Long dash" = 5, "Dot-long dash" = 6)
cruz.beaufort <- list("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, 
                      "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

ui.new.line <- function() {helpText(HTML("<br/>"))}
ui.selectize.instructions <- function() {
  helpText("To remove selected input(s), click the input(s) to remove and then backspace or delete")
}


### Load files with UI code
source("CruzPlot Files/ui_createMap.R", local = TRUE, echo = FALSE)
source("CruzPlot Files/ui_dasPlot.R", local = TRUE, echo = FALSE)
source("CruzPlot Files/ui_nonDasPlot.R", local = TRUE, echo = FALSE)


### UI function
ui <- dashboardPage(
  dashboardHeader(title = "CruzPlot"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Create and Save Map", tabName = "createmap", 
               icon = icon("th", lib = "font-awesome")),
      menuItem("Plot DAS Data", tabName = "DASplot", 
               icon = icon("th")),
      menuItem("Plot Non-DAS Data", tabName = "nonDASplot", 
               icon = icon("th")),
      menuItem(HTML(paste0("Color and Formatting", "<br/>", "Options")), 
               tabName = "dispColor", icon = icon("th")),
      menuItem("Species Information", tabName = "dispSp", icon = icon("th")),
      menuItem("CruzPlot Manual", tabName = "manual", icon = icon("th")), 
      ui.new.line(),
      ui.new.line(),
      helpText(HTML(paste0("Click button in order to load", "<br/>", 
                           "or save current CruzPlot", "<br/>", 
                           "workspace:")), style = "color: white"), 
      actionButton("load_app_envir", "Load saved workspace", 
                   style = "color: NA; background-color: NA; 
                                  border-color: black"), 
      textOutput("load_app_text"), 
      actionButton("save_app_envir", "Save current workspace"), 
      textOutput("save_app_text"),
      ui.new.line(),
      radioButtons("map.size", NULL, 
                   choices = list("Large map window" = 1, 
                                  "Small map window" = 2),
                   selected = 2),
      ui.new.line(),
      actionButton("stop", label = "Close CruzPlot")
    ), width = "200"
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      # Create a map of any region of the world and 
      #    specify colors/axes/scale bar and other aesthetics about the map. 
      ui.createMap(),
      
      # Load, filter, and display data from .DAS file
      ui.dasPlot(),
      
      # Load and display non-DAS point/line data from .csv files
      ui.nonDasPlot(), 
      
      # Display color/Format options
      tabItem(tabName = "dispColor",
              fluidRow(
                box( 
                  title = "Color/Format Options", status = "primary", 
                  solidHeader = TRUE,  width = 12,
                  plotOutput("plotDisplay")
                )
              )       
      ),
      
      # Display species codes and names
      tabItem(tabName = "dispSp",
              fluidRow(
                box( 
                  title = "Species Information", status = "primary", 
                  solidHeader = TRUE,  width = 12,
                  radioButtons("sp_type", "Select species codes to display", 
                               choices = list("Mammals" = 1, "Turtles" = 2, 
                                              "All" = 3)),
                  conditionalPanel(condition = "input.sp_type == 1",
                                   dataTableOutput("sp1")
                  ),
                  conditionalPanel(condition = "input.sp_type == 2",
                                   dataTableOutput("sp2")
                  ),
                  conditionalPanel(condition = "input.sp_type == 3",
                                   dataTableOutput("sp3")
                  )
                )
              )       
      ),
      
      # Display CruzPlot manual
      tabItem(tabName = "manual",
              helpText("Click 'Open in Browser' at top of the app in order to display manual in-app"),
              # tags$iframe(style="height:400px; width:100%; scrolling=yes",
              #             src="CruzPlot_Manual_app.pdf")
              uiOutput("manual_pdf")
      )
    )
  )
)
