### ui_dasPlot
## UI code for Plot DAS Data tab in CruzPlot


ui.dasPlot <- function() {
  tabItem(
    tabName = "DASplot",
    fluidRow(
      box(
        status = "primary", width = 6, #height = map.height,
        conditionalPanel(
          condition = "input.das_sight_interactive==1",
          conditionalPanel(
            condition = "input.das_effort_interactive==1",
            plotOutput("plot2")
          )
        ),
        conditionalPanel(
          condition = "input.das_sight_interactive==2",
          conditionalPanel(
            condition = "input.das_effort_interactive==1",
            plotOutput("plot3", click = "sight_click")
          )
        ),
        conditionalPanel(
          condition = "input.das_sight_interactive==3",
          conditionalPanel(
            condition = "input.das_effort_interactive==1",
            plotOutput("plot4", hover  = "sight_hover")
          )
        ),
        conditionalPanel(
          condition = "input.das_effort_interactive==2",
          plotOutput("plot5", click = "effort_click")
        ),
        conditionalPanel(
          condition = "input.das_effort_interactive==3",
          plotOutput("plot6", hover = "effort_hover")
        )
      ),
      tabBox(
        title = "Plot Sightings and/or Effort", id = "tabset2", width = 6,
        ##############################################################################################################
        tabPanel(
          title = "Data & Sightings",
          fluidRow(
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Data", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  fileInput("das.file", label = h5("DAS file input"), multiple = TRUE),
                  textOutput("das_loaded_text"),
                  helpText("To load DAS data file(s), first click the \"Browse...\" button. In the pop-up window",
                           "select the file(s) you want to load. Hold down the Shift key to select multiple files if desired.",
                           "Currently, you can 'remove' a file by browsing again and selecting only the DAS file(s)",
                           "that you want.")
                ),
                conditionalPanel(
                  condition = "input.das_sightings==true",
                  box(
                    title = "Sighting type & species", status = "warning", solidHeader = FALSE,
                    collapsible = TRUE, width = 12,
                    selectInput("das_sighting_type", label = h5("Sighting type"),
                                choices = list("Mammals" = 1, "Turtles" = 2, "Boats" = 3, "CPODs" = 4),
                                selected = 1),
                    conditionalPanel(
                      condition = "input.das_sighting_type==1",
                      radioButtons("das_sighting_code_1_all", label = NULL,
                                   choices = list("Plot all mammal sightings" = 1, "Plot selected mammal sightings" = 2),
                                   selected = 2),
                      conditionalPanel(
                        condition = "input.das_sighting_code_1_all==2",
                        checkboxInput("das.sighting.probable", label = "Include probable species sightings", value = FALSE),
                        ui.selectize.instructions(),
                        uiOutput("das.sighting.code.1_uiOut_select")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.das_sighting_type==2",
                      radioButtons("das_sighting_code_2_all", label = NULL,
                                   choices = list("Plot all turtle sightings" = 1, "Plot selected turtle sightings" = 2),
                                   selected = 2),
                      conditionalPanel(
                        condition = "input.das_sighting_code_2_all==2",
                        ui.selectize.instructions(),
                        uiOutput("das.sighting.code.2_uiOut_select")
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
                  title = "Sightings", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, height = 124,
                  checkboxInput("das_sightings", label = h5("Plot sightings"), value = FALSE)
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                conditionalPanel(
                  condition = "input.das_sightings==true & input.das_sighting_code_1_all==2",
                  box(
                    title = "Symbol properties", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                    helpText("To remove selected species, click the input(s) to remove and then click backspace or delete"),
                    helpText("The order in which species are selected to be plotted",
                             "corresponds to the order of specified symbol properties"),

                    # Mammal or turtle symbol properties, allows species properties and multiple input
                    conditionalPanel(
                      condition = "input.das_sighting_type==1 | input.das_sighting_type==2",
                      conditionalPanel(
                        condition = "input.das_symbol_mult==false",
                        selectizeInput("das.symbol.type", label = h5("Symbol type(s)"),
                                       choices = cruz.symbol.type, selected = 1, multiple = TRUE),
                        selectizeInput("das.symbol.color", label = h5("Symbol color(s)"),
                                       choices = cruz.palette.color, selected = "black", multiple = TRUE)
                      ),
                      conditionalPanel(
                        condition = "input.das_symbol_mult",
                        textInput("das.symbol.type.mult", h5("Symbol type(s)-text input"), value = "1"),
                        textInput("das.symbol.color.mult", h5("Symbol color(s)-text input"), value = "Black")
                      ),
                      fluidRow(
                        column(6, textInput("das.symbol.size", h5("Symbol size(s)"), "1")),
                        column(6, textInput("das.symbol.linewidth", h5("Symbol line width(s)"), "1"))
                      ),
                      checkboxInput("das_symbol_mult", label = "Input symbol properties as text", value = FALSE)
                    ),
                    # Boat symbol properties (or CPOD for vaquita cruise)
                    conditionalPanel(
                      condition = "input.das_sighting_type==3 | input.das_sighting_type==4",
                      fluidRow(
                        column(
                          width = 7,
                          selectInput("das.symbol.type.boat", label = h5("Symbol type"),
                                      choices = cruz.symbol.type, selected = 1),
                          numericInput("das.symbol.size.boat", label = h5("Symbol size"),
                                       value = 1, min = 0.1, max = 6, step = 0.1)
                        ),
                        column(
                          width = 5,
                          selectInput("das.symbol.color.boat", label = h5("Symbol color"),
                                      choices = cruz.palette.color, selected = "black"),
                          numericInput("das.symbol.linewidth.boat", label = h5("Symbol line width"),
                                       value = 1, min = 1, max = 6, step = 1)
                        )
                      )
                    )
                    # # CPOD symbol properties
                    #             conditionalPanel(condition = "input.das_sighting_type==4",
                    #                  selectInput("das.symbol.type.cpod", label = h5("Symbol type"),
                    #                              choices = cruz.symbol.type,
                    #                              selected = 1),
                    #                  selectInput("das.symbol.color.cpod", label = h5("Symbol color"),
                    #                              choices = cruz.palette.color,
                    #                              selected = "black"),
                    #                  textInput("das.symbol.size.cpod", label = h5("Symbol size"),
                    #                            value = "1"),
                    #                  textInput("das.symbol.linewidth.cpod", label = h5("Symbol line width"),
                    #                            value = "1")
                    #             )
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.das_sightings==true",
              box(title = "Interactive sighting labels", status = "warning", solidheader = FALSE, width = 12, collapsible = TRUE,
                  fluidRow(
                    column(6, radioButtons("das_sight_interactive", label = NULL,
                                           choices = list("Non-interactive plot" = 1, "Label sightings interactively" = 2),
                                           #"View sightings interactively" = 3
                                           selected = 1)),
                    column(
                      width = 6,
                      actionButton("das.sight.interactive.reset.last", "Remove last sighting label"),
                      actionButton("das.sight.interactive.reset.all", "Remove all sighting labels")
                    )
                  )
              )
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Filters",
          fluidRow(
            conditionalPanel(
              condition = "input.das_sightings!=true",
              column(12, helpText("Because 'Plot sightings' is not checked, there are no sightings to filter"))
            ),
            conditionalPanel(
              condition = "input.das_sightings==true",
              box(
                title = "Sightings to plot", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                radioButtons("das_sightings_effort", label = NULL,
                             choices = list("On and off effort" = 1, "On effort only" = 2, "Off effort only" = 3),
                             selected = 1),
                helpText("To plot effort lines, use Effort tab")
              ),
              box(
                title = "Sightings filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                fluidRow(
                  column(6, selectInput("das.sight.minBeau", label = h5("Min Beaufort"),
                                        choices = cruz.beaufort, selected = 0)),
                  column(6, selectInput("das.sight.maxBeau", label = h5("Max Beaufort"),
                                        choices = cruz.beaufort, selected = 9))
                ),
                uiOutput("das.sight.dateRange_uiOut_date"),
                br(),
                helpText("To stop applying the cruise number(s) and truncation filters, delete all text from their boxes"),
                fluidRow(
                  column(
                    width = 6,
                    textInput("das.sight.cruiseNum", label = h5("Cruise number(s)"), value = ""),
                    helpText("Only sightings from entered cruise(s) will be plotted. Enter cruise numbers as 'number, number'")
                  ),
                  column(
                    width = 6,
                    uiOutput("das.sight.trunc.uiOut.numeric"),
                    radioButtons("das.sight.trunc.units", h5("Truncation distance units"),
                                 choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                 selected = 2),
                    helpText("Only sightings less than or equal to this perpendicular distance from the trackline will be plotted")
                  )
                )
              )
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Legends",
          fluidRow(
            column(
              width = 6,
              fluidRow(
                box(
                  title = "Legends", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                  conditionalPanel(
                    condition = "input.das_sightings!=true",
                    helpText("*** No legend for sightings unless 'Plot sightings' is selected (Data & Sightings tab)")
                  ),
                  conditionalPanel(
                    condition = "input.das_sightings==true",
                    checkboxInput("das_legend", label = "Include legend for sightings", value = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.das_effort!=3 | input.das_effort_det_byBft!=true",
                    helpText("*** No legend for effort unless both 'Detailed effort' and",
                             "'Show effort by Beaufort' are selected (Effort tab)")
                  ),
                  conditionalPanel(
                    condition = "input.das_effort==3 & input.das_effort_det_byBft==true",
                    checkboxInput("eff_legend", label = "Include legend for effort", value = TRUE)
                  )
                ),
                conditionalPanel(
                  condition = "input.das_legend == true & input.das_sightings==true",
                  box(
                    title = "Sighting legend options", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                    fluidRow(
                      column(
                        width = 7,
                        selectInput("das_legend_pos", label = h5("Position"),
                                    choices = list("Specify" = 1, "Top Left" = "topleft", "Top Right"= "topright",
                                                   "Bottom Left" = "bottomleft", "Bottom Right" = "bottomright"),
                                    selected = "topright"),
                        conditionalPanel(
                          condition = "input.das_legend_pos == 1",
                          textInput("das.legend.lon", label = h5("Longitude"), value = ""),
                          textInput("das.legend.lat", label = h5("Latitude"), value = "")
                        ),
                        selectInput("das.legend.boxCol", label = h5("Box style"),
                                    choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                                    selected = 3
                        )
                      ),
                      column(
                        width = 5,
                        selectInput("das.legend.font", label = h5("Font"), choices = font.family, selected = 1),
                        numericInput("das.legend.textSize", label = h5("Legend size"), value = 1.0, min = 0.1, max = 3, step = 0.1)
                      )
                    )
                  )
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                conditionalPanel(
                  condition="input.das_legend==true & input.das_sightings==true",
                  box(
                    title = "Sighting legend contents", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                    textInput("das.legend.title", label = h5("Title (optional)"), value = ""),
                    conditionalPanel(
                      condition = "input.das_sighting_type!=3",
                      checkboxGroupInput("das.legend.names", h5("Legend sighting information"),
                                         choices = list("Species code" = 1, "Species abbreviation" = 2,
                                                        "Scientific name" = 3, "Common name" = 4),
                                         selected = 3)
                    ),
                    checkboxInput("das.legend.num", label = "Include number of sightings", value = FALSE)
                  )
                )
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition="input.eff_legend==true & input.das_effort==3 & input.das_effort_det_byBft",
              box(
                title = "Effort legend options", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(
                    width = 3,
                    selectInput("eff_legend_pos", label = h5("Position"),
                                choices = list("Specify" = 1, "Top Left" = "topleft", "Top Right"= "topright",
                                               "Bottom Left" = "bottomleft", "Bottom Right" = "bottomright"),
                                selected = "bottomright"),
                    conditionalPanel(
                      condition = "input.eff_legend_pos == 1",
                      textInput("eff.legend.lon", label = h5("Longitude"), value = "")
                    )
                  ),
                  column(
                    width = 4,
                    selectInput("eff.legend.boxCol", label = h5("Box color"),
                                choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                                selected = 3),
                    conditionalPanel(
                      condition = "input.eff_legend_pos == 1",
                      textInput("eff.legend.lat", label = h5("Latitude"), value = "")
                    )
                  ),
                  column(3, selectInput("eff.legend.font", label = h5("Font"), choices = font.family, selected = 1)),
                  column(2, numericInput("eff.legend.textSize", label = h5("Legend size"),
                                         value = 1.0, min = 0.1, max = 3, step = 0.1))
                )
              )
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Effort",
          fluidRow(
            box(
              title = "Effort to plot", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
              radioButtons("das_effort", label = NULL,
                           choices = list("No effort lines" = 1, "Simplified effort" = 2, "Detailed effort" = 3),
                           selected = 1)

            ),
            conditionalPanel(
              condition = "input.das_effort != 1",
              box(
                title = "Effort types to plot", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                fluidRow(
                  column(5, checkboxGroupInput("das.effort.closePass", label = NULL,
                                               choices = list("Closing" = "C", "Passing" = "P"),
                                               selected = "C")
                  ),
                  column(
                    width = 7,
                    conditionalPanel(
                      condition = "input.das_effort == 2",
                      helpText("Standard/Non-standard/Fine filters are not applicable for Simplified effort")
                    ),
                    conditionalPanel(
                      condition = "input.das_effort == 3",
                      checkboxGroupInput("das_effort_snf", label = NULL,
                                         choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                                         selected = "S")
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.das_effort != 1",
            fluidRow(
              box(
                title = "Line properties", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                fluidRow(
                  conditionalPanel(
                    condition = "input.das_effort == 2",
                    column(6, selectInput("das.effort.simp.col", h5("Simplified effort line color"),
                                          choices = cruz.palette.color, selected = "black")),
                    column(6, numericInput("das.effort.simp.lwd", h5("Simplified effort line width"),
                                           value = 2, min = 1, max = 6, step = 1))
                  ),
                  conditionalPanel(
                    condition = "input.das_effort == 3",
                    column(12, checkboxInput("das_effort_det_byBft", "Show effort by Beaufort", value = TRUE)),
                    conditionalPanel(
                      condition = "input.das_effort_det_byBft",
                      column(12, helpText("Plotted effort segments will be color-coded by Beaufort")),
                      helpText("See 'Legends' tab to control effort legend")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.das_effort_det_byBft != true",
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "output.das_effort_det_s_flag",
                        selectInput("das.effort.det.col.s", h5("Standard effort line color"),
                                    choices = cruz.palette.color, selected = "black")
                      ),
                      conditionalPanel(
                        condition = "output.das_effort_det_n_flag",
                        selectInput("das.effort.det.col.n", h5("Non-standard effort line color"),
                                    choices = cruz.palette.color, selected = "black")
                      ),
                      conditionalPanel(
                        condition = "output.das_effort_det_f_flag",
                        selectInput("das.effort.det.col.f", h5("Fine effort line color"),
                                    choices = cruz.palette.color, selected = "black")
                      )
                    ),
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "output.das_effort_det_s_flag",
                        numericInput("das.effort.det.lwd.s", h5("Standard effort line width"),
                                     value = 2, min = 1, max = 6, step = 1)
                      ),
                      conditionalPanel(
                        condition = "output.das_effort_det_n_flag",
                        numericInput("das.effort.det.lwd.n", h5("Non-standard effort line width"),
                                     value = 2, min = 1, max = 6, step = 1)
                      ),
                      conditionalPanel(
                        condition = "output.das_effort_det_f_flag",
                        numericInput("das.effort.det.lwd.f", h5("Fine effort line width"),
                                     value = 2, min = 1, max = 6, step = 1)
                      )
                    )
                  )
                )
              ),
              box(
                title = "Effort filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
                checkboxInput("das_effort_filter_same", label = "Same as 'Sightings filters'", value = TRUE),
                conditionalPanel(
                  condition = "input.das_effort_filter_same == false",
                  conditionalPanel(
                    condition = "input.das_effort == 3",
                    fluidRow(
                      column(6, selectInput("das.effort.minBeau", h5("Minimum Beaufort"), choices = cruz.beaufort, selected = 0)
                      ),
                      column(6, selectInput("das.effort.maxBeau", h5("Maximum Beaufort"), choices = cruz.beaufort, selected = 9)
                      )
                    )
                  ),
                  conditionalPanel("input.das_effort == 2", helpText("Only detailed effort lines can be  by Beaufort")),
                  uiOutput("das.effort.dateRange_uiOut_date"),
                  textInput("das.effort.cruiseNum", h5("Cruise number(s)"), value = ""),
                  helpText("Only effort lines from this cruise number will be plotted")
                )
              )
            ),
            fluidRow(
              box(
                title = "Interactive effort labels", status = "warning", solidheader = FALSE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(7, radioButtons("das_effort_interactive", label = NULL,
                                         choices = list("Non-interactive plot" = 1, "Label effort lines interactively" = 2,
                                                        "View effort line data interactively" = 3),
                                         selected = 1)),
                  column(
                    width = 5,
                    actionButton("das.effort.interactive.reset.last", "Remove last effort label"),
                    actionButton("das.effort.interactive.reset.all", "Remove all effort labels")
                  )
                )
              )
            )
          )
        ),
        ##############################################################################################################
        tabPanel(
          title = "Tabular Output",
          fluidRow(
            box(
              title = "Effort", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel("input.das_effort == 1", helpText("Effort must be plotted to generate tabular output for effort")),
              conditionalPanel(
                condition = "input.das_effort != 1",
                fluidRow(
                  column(
                    width = 4,
                    helpText("Uses the same filters as those applied to plotted effort"),
                    radioButtons("das_out_effort_units", h5("Effort distance units"),
                                 choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                 selected = 2),
                    ui.new.line(),
                    uiOutput("das_out_effort_save_name_uiOut_text"),
                    downloadButton("das_out_effort_save", "Save table of tabular output for effort")
                    # actionButton("das_out_effort_save_execute", "Save specified effort data"),
                    # textOutput("cruzDasOutEffort_Save_text")
                  ),
                  column(8, tableOutput("das_out_effort_table"))
                )
              )
            ),
            box(
              title = "Sightings", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              conditionalPanel(
                condition = "input.das_sightings != true",
                helpText("'Plot sightings' must be selected to generate tabular output for sightings")
              ),
              conditionalPanel(
                condition = "input.das_sightings",
                helpText("Uses the same species, on or off effort, date, Beaufort, cruise number, and truncation",
                         "filters as those applied to plotted sightings in the 'Filters' tab"),
                fluidRow(
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = "input.das_sightings_effort == 3",
                      helpText(paste("Effort type filters for sightings are not applicable when",
                                     "sightings are filtered for off effort sightings"))
                    ),
                    conditionalPanel(
                      condition = "input.das_sightings_effort != 3",
                      tags$strong("Filter sightings by effort type"),
                      fluidRow(
                        column(5, checkboxGroupInput("das.out.sight.closePass", label = NULL,
                                                     choices = list("Closing" = "C", "Passing" = "P"),
                                                     selected = c("C", "P"))),
                        column(7, checkboxGroupInput("das.out.sight.snf", label = NULL,
                                                     choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                                                     selected = c("S", "N", "F")))
                      ),
                      ui.new.line(),
                      uiOutput("das_out_sight_save_name_uiOut_text"),
                      downloadButton("das_out_sight_save", "Save table of tabular output for sightings")
                      # actionButton("das_out_sight_save_execute", "Save specified sighting data"),
                      # textOutput("cruzDasOutSight_Save_text")
                    )
                  ),
                  column(8, tableOutput("das_out_sight_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}
