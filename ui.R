### CruzPlot ui file 
### Created by Sam Woodman August 2015
### Modifications by Tim Gerrodette October 2015
### Modifications by Sam Woodman July 2017


###############################################################################
### Install packages that are required for CruzPlot but aren't downloaded
list.of.packages <- c("maps", "mapdata", "marmap", "geosphere", 
                      "dplyr", "stringr", 
                      "shiny", "shinyjs", "shinydashboard", "DT")

list.of.packages.tf <- list.of.packages %in% installed.packages()[, "Package"]
new.packages <- list.of.packages[!list.of.packages.tf]
if (length(new.packages)) install.packages(new.packages)


###############################################################################
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


###############################################################################
options(shiny.maxRequestSize = 50 * 1024^2) # Max file size is 50MB
options("digits" = 5)   # for proper display of sighting and effort coordinates
# map.height <- 950     # set to 630 for laptops, 950 for standard monitor, 5% larger than in server.R


source(file.path("CruzPlot_files", "funcTickUpdate.R"), local = TRUE, chdir = TRUE)
source(file.path("CruzPlot_files", "funcTickStart.R"), local = TRUE, chdir = TRUE)


### Read default values for map
if(file.exists("StarterVals.csv")) start.ll <- suppressWarnings(read.csv("StarterVals.csv", header = TRUE))
if(!file.exists("StarterVals.csv")) start.ll <- data.frame(X = c(-180, -110, 0, 33, 1))

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
cruz.palette.gray  <- list("Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
                           "Gray" = 4, "Light Gray" = 5, "White" = 0)
font.family        <- list("Sans" = 1, "Serif" = 2, "Mono" = 3)
cruz.symbol.type   <- list("0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3, 
                           "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7, 
                           "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10, 
                           "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13, 
                           "14: Square with Up Triangle" = 14, "15: Filled Square" = 15, 
                           "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18, 
                           "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20)
cruz.line.type     <- list("Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, 
                           "Long dash" = 5, "Dot-long dash" = 6)
cruz.beaufort      <- list("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, 
                           "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

ui.new.line <- function() {helpText(HTML("<br/>"))}
ui.selectize.instructions <- function() {
  helpText("To remove selected input(s), click the input(s) to remove and then click backspace or delete")
}


### Load files with UI code
source(file.path("CruzPlot_files", "ui_createMap.R"), local = TRUE, chdir = TRUE)
source(file.path("CruzPlot_files", "ui_dasPlot.R"), local = TRUE, chdir = TRUE)
source(file.path("CruzPlot_files", "ui_nonDasPlot.R"), local = TRUE, chdir = TRUE)


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
