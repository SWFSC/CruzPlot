### ui_createMap
## UI code for Create Map tab in CruzPlot

ui.createMap <- function() {
  tabItem(
    tabName = "createmap",
    fluidRow(
      box(
        status = "primary",  width = 6,
        uiOutput("plot1.ui")
        # plotOutput("plot1")
      ),

      tabBox(
        title = "Map", width = 6, id = "tabset1",

        ################################## Panel 1
        tabPanel(
          title = "Range",
          fluidRow(
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Map range", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  helpText("For longitude values, please use the range -180 to 180. ",
                           "Thus, for a map of the Pacific, you could enter 130 and -110 for ",
                           "the left and right longitude, respectively.", tags$br(),
                           "Click the 'Replot map' button to replot map after changing map range values,",
                           "or if the map isn't properly sized within the map box.", tags$br(),
                           "Change 'Starter_Vals.csv' to update default lat/long range values."),
                  fluidRow(
                    column(
                      width = 6,
                      numericInput("lon.left", h5("Left longitude"), value = start.ll$X[1]),
                      numericInput("lat.bot", h5("Bottom latitude"), value = start.ll$X[3]),
                      selectInput("resolution", label = h5("Resolution"), choices = list("Low" = 1, "High" = 2),
                                  selected = start.ll$X[5])
                    ),
                    column(
                      width = 6,
                      numericInput("lon.right", h5("Right longitude"), value = start.ll$X[2]),
                      numericInput("lat.top", h5("Top latitude"), value = start.ll$X[4]),
                      ui.new.line(),
                      actionButton("map.replot", "Replot map")
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Coastline", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  checkboxInput("coast", label = "Use coastline file", value = FALSE),
                  conditionalPanel(
                    condition = "input.coast==true",
                    helpText("Map limits will automatically be updated to the extent of the",
                             "coastline file. Note: CruzPlot can only process coastline files",
                             "with points are between -180 and 0"),
                    fileInput("coast.file", label = h5("Coastline file"), accept = '.csv')
                  )
                ),
                box(
                  title = "Scale bar", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  checkboxInput("bar", "Plot scale bar", value = FALSE),
                  conditionalPanel(
                    condition = "input.bar==true",
                    helpText("Provide the coordinates for the left edge of the scale bar"),
                    fluidRow(
                      column(
                        width = 6,
                        uiOutput("out.scale.lon"),
                        radioButtons("scale.units", h5("Scale bar units"),
                                     choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                     selected = 2),
                        numericInput("scale.width", h5("Width of bar"), value = 2, min = 1, max = 6, step = 1)
                      ),
                      column(
                        width = 6,
                        uiOutput("out.scale.lat"),
                        uiOutput("out.scale.len")
                      )
                    )
                  )
                )
              )
            )
          )
        ),

        ################################### Panel 2
        tabPanel(
          title = "Planned Transects",
          fluidRow(
            box(
              title = "Planned transects", status = "warning", solidHeader = FALSE, width = 12, collapsible = FALSE,
              fluidRow(
                box(
                  width = 6,
                  tags$strong("Load planned transects"),
                  helpText(paste("Longitudes must be in -180 to 180 range. See the manual for the required .csv file format")),
                  fileInput("planned_transects_file", h5("Load planned transects .csv file")),
                  fluidRow(
                    column(
                      width = 6,
                      uiOutput("planned_transects_lon_uiOut_select"),
                      uiOutput("planned_transects_num_uiOut_select"),
                      uiOutput("planned_transects_class2_uiOut_select")
                    ),
                    column(
                      width = 6,
                      uiOutput("planned_transects_lat_uiOut_select"),
                      uiOutput("planned_transects_class1_uiOut_select")
                    )
                  ),
                  fluidRow(
                    ui.new.line(),
                    column(6, uiOutput("planned_transects_execute_uiOut_button")),
                    column(6, textOutput("planned_transects_text"))
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "output.cruzMapPlannedTransects_Conditional",
                    fluidRow(
                      box(
                        width = 12,
                        tags$strong("Plot loaded planned transects"),
                        checkboxInput("planned_transects_plot", "Plot planned transect lines", value = TRUE),
                        conditionalPanel(
                          condition = "input.planned_transects_plot",
                          column(12, helpText("For the color(s) and (if a class 2 column is specified) the line type(s),",
                                              "select either one or the same number as transect classes or class 2s, respectively.",
                                              "When multiple colors or line types are selected,",
                                              "the order in which transect classes and class 2s are selected to be plotted",
                                              "corresponds to order of specified colors and line types, respectively.")),
                          box(
                            width = 12,
                            ui.selectize.instructions(),
                            uiOutput("planned_transects_toplot_uiOut_selectize"),
                            # helpText("Select either one color the same number of colors as transect classes.",
                            #          "When multiple colors are selected, the order in which transect class(es)",
                            #          "are selected to be plotted corresponds to order of specified color(s)."),
                            uiOutput("planned_transects_color_uiOut_selectize")
                          ),
                          box(
                            width = 12,
                            uiOutput("planned_transects_toplot2_uiOut_selectize"),
                            # uiOutput("planned_transects_lty_uiOut_message"),
                            uiOutput("planned_transects_lty_uiOut_selectize")
                          ),
                          box(width = 12, numericInput("planned_transects_lwd", h5("Line width"),
                                                       value = 1, min = 0, step = 1))
                        )
                      )#,
                      # box(
                      #   width = 12,
                      #   tags$strong("Remove loaded planned transects"),
                      #   uiOutput("planned_transects_toremove_uiOut_select"),
                      #   uiOutput("planned_transects_toremove_execute_uiOut_button"),
                      #   textOutput("planned_transects_remove_text")
                      # )
                    )
                  )
                )
              )
            )
          )
        ),

        ################################### Panel 3
        tabPanel(
          title = "Ticks & Labels",
          fluidRow(
            box(
              title = NULL, status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("tick", label = "Plot tick marks and/or their labels", value = TRUE)
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.tick==true",
              box(
                title = "Tick marks", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput("tick.left", label = "Left", value = TRUE),
                    checkboxInput("tick.bot", label = "Bottom", value = TRUE),
                    numericInput("tick.interval.major", label = h5("Degrees between each major tick"),
                                 value = start.tick$interval, min = 0, max = 45, step = 5),
                    selectInput("tick.style", label = h5("Tick label style"),
                                choices = list("120" = 1, "120W" = 2, "120°" = 3, "120°W" = 4),
                                selected = 4)
                  ),
                  column(
                    width = 6,
                    checkboxInput("tick.right", label = "Right", value = TRUE),
                    checkboxInput("tick.top", label = "Top", value = TRUE),
                    numericInput("tick.interval.minor", label = h5("Minor ticks between each major tick"),
                                 value = 4, min = 0, max = 45, step = 1),
                    numericInput("tick.length", label = h5("Tick length"), value = 1.0, min = 0, max = 2.5, step = 0.1)
                  )
                )
              ),
              box(
                title = "Tick labels", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 437,
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput("tick.left.lab", label = "Left", value = TRUE),
                    checkboxInput("tick.bot.lab", label = "Bottom", value = TRUE),
                    numericInput("label.lon.start", h5("Start longitude tick labels at"), value = as.character(start.tick$lon)),
                    selectInput("label.tick.font", label = h5("Tick label font"), choices = font.family, selected = 1)
                  ),
                  column(
                    width = 6,
                    checkboxInput("tick.right.lab", label = "Right", value = TRUE),
                    checkboxInput("tick.top.lab", label = "Top", value = TRUE),
                    numericInput("label.lat.start", h5("Start latitude tick labels at"), value = as.character(start.tick$lat)),
                    numericInput("label.tick.size", label = h5("Tick label size"), value = 1.0, min = 0.1, max = 3, step = 0.1)
                  )
                )
              )
            )
          )
        ),

        #################################### Panel 4
        tabPanel(
          title = "Map Labels",
          fluidRow(
            box(
              title = "Title", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 315,
              textInput("label.title", h5("Map title"), value = ""),
              fluidRow(
                column(6, selectInput("label.title.font", label = h5("Title font"), choices = font.family, selected = 1)),
                column(6, numericInput("label.title.size", label = h5("Title size"), value = 1.5, min = 0.1, max = 3, step = 0.1))
              )
            ),
            box(
              title = "Axis labels", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 402,
              textInput("label.axis.lon", h5("Longitude axis label"), value = ""),
              textInput("label.axis.lat", h5("Latitude axis label"), value = ""),
              fluidRow(
                column(6, selectInput("label.axis.font", label = h5("Axis label font"), choices = font.family, selected = 1)),
                column(6, numericInput("label.axis.size", label = h5("Axis label size"), value = 1.2, min = 0.1, max = 3, step = 0.1))
              )
            )
          )
        ),

        #################################### Panel 5
        tabPanel(
          title = "Color",
          fluidRow(
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Color style", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 12,
                  radioButtons("color_style", label = NULL, choices = list("Color" = 1, "Gray scale" = 2),
                               selected = 1)
                ),
                box(
                  title = "Land", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 12, height = 340,
                  fluidRow(
                    column(6, checkboxInput("color_land_all", label = "Color all land", value = TRUE)),
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.color_land_all==true",
                        selectInput("color.land", label = h5("Land color"), choices = cruz.palette.color, selected = "bisque1")
                      )
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Water", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  checkboxInput("color_lakes_rivers", label = "Color lakes and rivers", value = FALSE),
                  radioButtons("color_water_style", label = h5("Ocean color style"),
                               choices = list("Single color" = 1, "Depth shading" = 2),
                               selected = 1),
                  conditionalPanel(
                    condition = "input.color_water_style==1",
                    selectInput("color.water", label = h5("Water color"), choices = cruz.palette.color, selected = "white")
                  ),
                  conditionalPanel(
                    condition = "input.color_water_style==2",
                    helpText("Options:"),
                    helpText("1) Load csv file with 3 columns: latitude, longitude, and depth"),
                    helpText("2) Download bathymetric data from NOAA website based on lat/lon of map.", tags$br(),
                             "The data will be saved and will be automatically loaded next",
                             "time for a map with these lat/lon coordinates."),
                    radioButtons("depth_style", label = h5("Bathymetric data source"),
                                 choices = list("csv file" = 2, "NOAA server" = 1), selected = 2),
                    conditionalPanel(
                      condition = "input.depth_style==2",
                      fileInput("depth.file", h5("Bathymetric csv file"), accept = '.csv'),
                      checkboxInput("depth.header", label = h5("Header"), value = TRUE),
                      fluidRow(
                        column(6, radioButtons("depth.sep", label = h5("Separator"),
                                               choices = list(Comma = ", ", Semicolon = ";", Tab = "\t"),
                                               selected = ", ")
                        ),
                        column(6, radioButtons("depth.quote", label = h5("Quote"),
                                               choices = list("None" = "", "Double quote" = '"', "Single quote" = "'"),
                                               selected = '"')
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.depth_style==1",
                      textInput("depth.res", h5("Bathymetric data resolution, in minutes (range: 0-60)"), value = "10")
                    )
                  )
                )
              )
            )
          )
        ),

        #################################### Panel 6
        tabPanel(
          title = "Grid",
          fluidRow(
            # box(
            #   # Only cylindrical projection is implemented
            #   title = "Projection", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
            #   height = 306,
            #   fluidRow(
            #     column(width = 6,
            #            radioButtons("projection", label = h5("Map projection"),
            #                         choices = list("Cylindrical" = 1), selected = 1)
            #     ),
            #     column(width = 6,
            #            # Does not have any functionality
            #            selectInput("projection.factor", label = h5("Projection factor"),
            #                        choices = list("0" = 1, "0.2" = 2, "0.3" = 3,
            #                                       "0.6" = 4, "0.8" = 5, "1" = 6),
            #                        selected = 1)
            #     )
            #   )
            # ),

            box(
              title = "Grid", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, height = 385,
              checkboxInput("grid", label = "Include grid lines at major tick marks", value = FALSE),
              conditionalPanel(
                condition = "input.grid==true",
                fluidRow(
                  column(
                    width = 6,
                    selectInput("grid.line.color", label = h5("Line color"), choices = cruz.palette.color, selected = "black"),
                    numericInput("grid.line.width", label = h5("Line width"), value = 1, min = 1, max = 6, step = 1)
                  ),
                  column(6, selectInput("grid.line.type", label = h5("Line type"), choices = cruz.line.type, selected = 1)))
              )
            )
          )
        ),

        #################################### Panel 6
        tabPanel(
          title = "Save",
          fluidRow(
            box(
              title = "Save map", status = "warning", solidHeader = FALSE, width = 12,
              helpText("When saving file in RStudio window, be sure to specify '.png', '.pdf', or '.jpeg extention"),
              # radioButtons("download.format", label = h5("Download map as"), choices = list("jpeg" = 1, "pdf" = 2, "png" = 3),
              #              selected = 3),
              fluidRow(
                column(6, radioButtons("download.format", label = h5("Download map as"),
                                       choices = list("jpeg" = 1, "pdf" = 2, "png" = 3),
                                       selected = 3)),
                column(6, radioButtons("download.res", tags$h5("Resolution"),
                                       choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                       selected = 1))
              ),
              downloadButton("downloadMap", label = "Download map")
            )
          )
        )
      )
    )
  )
}
