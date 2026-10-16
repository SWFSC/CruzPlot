#' DAS data module
#'
#' Shiny module for DAS data
#'
#' @name mod_das
#'
#' @inheritParams mod_map_range
#' @param color_style reactive of the app color style. Specifically
#'   "1" for Color or "2" for Gray scale
#' @param map_range_config the `config` output of [mod_map_range_server()]
#'
#' @details
#' This module handles the loading, processing, and plotting of DAS data.
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#'
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file.
#'   See [cruzplot_gui()] for more info.
#' - `sp_codes`: reactive of a data frame of loaded species codes
#' - `todo`: ...
#'
#' @export
mod_das_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabPanel(
      title = "Data",
      fluidRow(
        cruz_box(
          title = "DAS Data", width = 12,
          helpText("For details about these parameters, see the ",
                   # actionLink(ns("das_file_help"), "click here"), "or see the official",
                   tags$a(
                     href = "https://swfsc.github.io/swfscDAS/reference/das_process.html",
                     "swfscDAS documentation",
                     target = "_blank"
                   ),
                   " (external link)"),
          fluidRow(
            column(3, tags$h5("Number of lines to skip before reading each file")),
            column(3, tags$h5("days.gap argument of das_process()")),
            column(3, tags$h5("reset.event argument of das_process()")),
            column(3, tags$h5("reset.effort argument of das_process()"))
          ),
          fluidRow(
            column(3, numericInput(ns("das_file_skip"), NULL,
                                   value = 0, min = 0)),
            column(3, numericInput(ns("das_file_days_gap"), NULL,
                                   value = 20, min = 0, step = 1)),
            column(3, selectInput(ns("das_file_reset_event"), NULL,
                                  choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE)),
            column(3, selectInput(ns("das_file_reset_effort"), NULL,
                                  choices = list("TRUE" = 1, "FALSE" = 2), selected = TRUE))
          ),
          helpText("To load DAS data file(s), first set the desired parameters, and then click the \"Browse...\" button and",
                   "select the file(s) you want to load. Hold the Shift key to select multiple files.",
                   "If you change the parameters, you will have to Browse and select the file(s) again.",
                   "To 'remove' a file, browse again and select only the desired DAS file(s). ",
                   "Or simply uncheck the 'plot' checkbox for sightings or effort. "),
          fileInput(ns("das_file"), tags$h5("DAS file input"), multiple = TRUE),
          textOutput(ns("das_file_load_text")),
          tags$span(textOutput(ns("das_loaded_text")), style = "color: blue;")
        ),
        cruz_box(
          title = "Species codes", width = 12,
          helpText("Processing DAS sightings requires a species codes file, typically named SpCodes.dat,",
                   "to translate the species codes to scientific or common species names.",
                   "CruzPlot can load the default ",
                   tags$a(
                     href = "https://github.com/SWFSC/swfscDAS/blob/main/inst/extdata/SpCodes.dat",
                     "SpCodes.dat file",
                     target = "_blank"
                   ),
                   "  from swfscDAS (external link), ",
                   "or you can also load your own species codes file.",
                   "If you load your own file, it must follow the same format ",
                   "as the default SpCodes.dat; see the manual for details. "),
          fluidRow(
            column(5, fileInput(ns("das_spcodes_file"), tags$h5("Load species codes file"), accept = ".dat")),
            column(
              width = 6, offset = 1,
              tags$br(), tags$br(),
              actionButton(ns("das_spcodes_default"), "Load default species codes")
            )
          ),
          textOutput(ns("spcodes_user_read_text")),
          textOutput(ns("spcodes_default_read_text")),
          tags$span(textOutput(ns("spcodes_message")), style = "color: blue;")
        )
      )
    ),

    ##############################################################################################################
    tabPanel(
      title = "Sightings",
      fluidRow(
        cruz_box(
          title = "Sightings", width = 12,
          conditionalPanel(
            condition = "!output.das_loaded_flag || !output.das_spcodes_loaded_flag",
            ns = ns,
            tags$span(tags$h5("Please load at least one DAS file and a species ",
                              "codes file to use this section"),
                      style = "color: red;")
          ),
          conditionalPanel(
            condition = "output.das_loaded_flag && output.das_spcodes_loaded_flag",
            ns = ns,
            fluidRow(
              column(6, checkboxInput(ns("das_sightings"), tags$h5("Plot sightings"), value = FALSE)),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.das_sightings", ns = ns,
                  radioButtons(ns("das_sightings_position"), tags$h5("Position to plot"),
                               choices = list("Plot ship position" = 1, "Plot sighting position" = 2),
                               selected = 1)
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.das_sightings", ns = ns,
            textOutput("das_sight_spcodes_message"),
            helpText("Sighting position is calculated using the ship position, ",
                     "ship course, sighting bearing (angle),",
                     "and radial distance to the sighting.",
                     "If any of these values are NA, then the sighting position will be NA")
          )
        ),
        conditionalPanel(
          condition = "input.das_sightings", ns = ns,
          # column(
          #   width = 12,
          # fluidRow(
          cruz_box(
            title = "Sighting type & species", width = 12,
            fluidRow(
              column(
                width = 8,
                selectInput(
                  ns("das_sighting_type"), tags$h5("Sighting type"),
                  choices = list("Mammals" = 1, "Turtles" = 2, "Boats" = 3), #, "CPODs" = 4),
                  selected = 1
                ),
                conditionalPanel(
                  condition = "input.das_sighting_type==1", ns = ns,
                  radioButtons(ns("das_sighting_code_1_all"), NULL,
                               choices = list("Plot all mammal sightings" = 1,
                                              "Plot selected mammal sightings" = 2),
                               selected = 2),
                  conditionalPanel(
                    condition = "input.das_sighting_code_1_all==2", ns = ns,
                    ui_select_instructions(),
                    uiOutput(ns("das_sighting_code_1_uiOut_select"))
                  ),
                  checkboxInput(ns("das_sighting_probable"), "Use probable species code", value = FALSE),
                ),
                conditionalPanel(
                  condition = "input.das_sighting_type==2", ns = ns,
                  radioButtons(ns("das_sighting_code_2_all"), NULL,
                               choices = list("Plot all turtle sightings" = 1, "Plot selected turtle sightings" = 2),
                               selected = 2),
                  conditionalPanel(
                    condition = "input.das_sighting_code_2_all==2", ns = ns,
                    ui_select_instructions(),
                    uiOutput(ns("das_sighting_code_2_uiOut_select"))
                  )
                )
              ),
              column(
                width = 4,
                conditionalPanel(
                  condition = "input.das_sighting_type==1", ns = ns,
                  checkboxGroupInput(
                    ns("das_sighting_events"),
                    tags$h5(
                      "Plot sightings from selected events:",
                      actionLink(ns("das_sighting_events_help"), "(details)")
                    ),
                    choices = list(
                      "S events" = "S", "G events" = "G",
                      "K events" = "K", "M events" = "M", "p events" = "p",
                      "s events" = "s", "g events" = "g", "k events" = "k"
                    ),
                    selected = c("S")
                    # inline = TRUE
                  )
                )
              )
            ),
            tags$span(uiOutput(ns("cruzDasSightEventResight_uiOut_message")), style = "color: blue;")
          ),
          cruz_box(
            title = "Symbol properties", width = 12,
            conditionalPanel(
              condition = "!((input.das_sighting_type == 1 && input.das_sighting_code_1_all == 2) || (input.das_sighting_type==2 && input.das_sighting_code_2_all == 2))",
              ns = ns,
              helpText("Symbol properties are not available when 'Plot all...sightings' is selected")
            ),

            # Multiple selections: Mammal or turtle symbol properties for selected species
            conditionalPanel(
              condition = "(input.das_sighting_type == 1 && input.das_sighting_code_1_all == 2) || (input.das_sighting_type==2 && input.das_sighting_code_2_all == 2)",
              ns = ns,
              helpText(
                "The order in which species are selected to be plotted",
                "corresponds to the order of specified symbol properties. ",
                "For text inputs, separate values with a single comma (,)"
              ),
              box(
                width = 12,
                conditionalPanel(
                  condition = "input.das_symbol_mult == false", ns = ns,
                  helpText(
                    "To remove selected symbol type or color, click the value(s) ",
                    "to remove, and then click backspace or delete"
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      selectInput(
                        ns("das_symbol_type"), tags$h5("Type(s)"),
                        choices = cruz.symbol.type, selected = 1,
                        multiple = TRUE
                      )
                    ),
                    column(
                      width = 6,
                      selectInput(
                        ns("das_symbol_color"), tags$h5("Color(s)"),
                        choices = cruz.palette.color, selected = "black",
                        multiple = TRUE
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.das_symbol_mult", ns = ns,
                  fluidRow(
                    column(
                      width = 6,
                      textInput(
                        ns("das_symbol_type_mult"),
                        tags$h5("Type(s) - text input"),
                        value = "1"
                      )
                    ),
                    column(
                      width = 6,
                      textInput(
                        ns("das_symbol_color_mult"),
                        tags$h5("Color(s) - text input (case sensitive)"),
                        value = "Black"
                      )
                    )
                  )
                ),
                checkboxInput(
                  ns("das_symbol_mult"),
                  "Input symbol type and color as text",
                  value = FALSE
                )
              ),
              fluidRow(
                column(6, textInput(ns("das_symbol_size"), tags$h5("Size(s)"), "1")),
                column(6, textInput(ns("das_symbol_linewidth"), tags$h5("Line width(s)"), "1"))
              )
            ),

            # Single selections: Boat (or CPOD) symbol properties
            conditionalPanel(
              condition = "input.das_sighting_type==3 | input.das_sighting_type==4", ns = ns,
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    ns("das_symbol_type_boat"), tags$h5("Symbol type"),
                    choices = cruz.symbol.type, selected = 1
                  ),
                  numericInput(
                    ns("das_symbol_size_boat"), tags$h5("Symbol size"),
                    value = 1, min = 0.1, max = 6, step = 0.1
                  )
                ),
                column(
                  width = 6,
                  selectInput(
                    ns("das_symbol_color_boat"), tags$h5("Symbol color"),
                    choices = cruz.palette.color, selected = "black"
                  ),
                  numericInput(
                    ns("das_symbol_linewidth_boat"), tags$h5("Symbol line width"),
                    value = 1, min = 1, max = 6, step = 1
                  )
                )
              )
            )
          ),
          cruz_box(
            title = "Interactive sighting labels", width = 12,
            fluidRow(
              column(
                width = 4,
                radioButtons(
                  ns("das_sight_interactive"), NULL,
                  choices = list("Non-interactive plot" = 1,
                                 "View and label sightings interactively" = 2),
                  selected = 1
                )
              ),
              column(
                width = 8,
                actionButton(ns("das_sight_interactive_reset_last"), "Remove last sighting label"),
                actionButton(ns("das_sight_interactive_reset_all"), "Remove all sighting labels")
              )
            )
          )
        )
      )
    ),

    ##############################################################################################################
    tabPanel(
      title = "Sighting Filters",
      fluidRow(
        conditionalPanel(
          condition = "input.das_sightings==false", ns = ns,
          column(12, helpText("Because 'Plot sightings' is not checked, there are no sightings to filter"))
        ),
        conditionalPanel(
          condition = "input.das_sightings", ns = ns,
          cruz_box(
            title = "Sightings to plot", width = 12,
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  ns("das_sight_effort"), NULL,
                  choices = list("On and off effort" = 1,
                                 "On effort only" = 2,
                                 "Off effort only" = 3),
                  selected = 2
                )
              ),
              conditionalPanel(
                condition = "input.das_sight_effort != 2", ns = ns,
                column(5, helpText("Mode and effort type filters are only available when plotting strictly on effort sightings"))
              ),
              conditionalPanel(
                condition = "input.das_sight_effort == 2", ns = ns,
                column(
                  width = 3,
                  checkboxGroupInput(
                    ns("das_sight_cp"), tags$h5("Mode"), #inline = TRUE,
                    choices = list("Closing" = "C", "Passing" = "P"),
                    selected = c("C", "P")
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    ns("das_sight_snf"), tags$h5("Effort type"), #inline = TRUE,
                    choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                    selected = c("S", "N", "F")
                  )
                )
              )
            )
          ),
          cruz_box(
            title = "Sighting filters", width = 12,
            fluidRow(
              column(3, selectInput(ns("das_sight_minBft"), tags$h5("Minimum Beaufort"),
                                    choices = cruz.beaufort, selected = 0)),
              column(3, selectInput(ns("das_sight_maxBft"), tags$h5("Maximum Beaufort"),
                                    choices = cruz.beaufort, selected = 9)),
              column(6, uiOutput(ns("das_sight_dateRange_uiOut_date"))),
              column(12, helpText("Note that if the min and max Beaufort values are 0 and 9, respectively,",
                                  "then sightings with NA Beaufort values will be plotted"))
            ),
            tags$br(),
            helpText("To stop applying the cruise number(s) and truncation (perpendicular distance) filters,",
                     "delete all text from their boxes"),
            fluidRow(
              column(4, uiOutput(ns("das_sight_cruise_uiOut_select"))),
              column(4, uiOutput(ns("das_sight_trunc_uiOut_numeric"))),
              column(4, radioButtons(ns("das_sight_trunc_units"), tags$h5("Truncation distance units"),
                                     choices = list("Kilometers" = 1, "Nautical miles" = 2),
                                     selected = 2)),
              column(4, helpText("Note that if any cruise numbers are selected,",
                                 "sightings with an NA cruise number will not be plotted")),
              column(8, helpText("Only sightings less than or equal to this perpendicular distance from the trackline will be plotted.",
                                 "Sightings with NA perpendicular distance values will not be plotted"))
            )
          )
        )
      )
    ),

    ##############################################################################################################
    tabPanel(
      title = "Effort",
      fluidRow(
        cruz_box(
          title = "Effort to plot", width = 12,
          conditionalPanel(
            condition = "output.das_loaded_flag == false", ns=ns,
            tags$span(
              tags$h5("Please load at least one DAS file to use this section"),
              style = "color: red;"
            )
          ),
          conditionalPanel(
            condition = "output.das_loaded_flag", ns = ns,
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  ns("das_effort"), NULL,
                  choices = list("No effort lines" = 1, "Simplified effort" = 2, "Detailed effort" = 3),
                  selected = 1
                )
              ),
              conditionalPanel(
                condition = "input.das_effort != 1", ns = ns,
                column(
                  width = 3,
                  checkboxGroupInput(
                    ns("das_effort_cp"), tags$h5("Mode"), #inline = TRUE,
                    choices = list("Closing" = "C", "Passing" = "P"),
                    selected = c("C", "P")
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    ns("das_effort_snf"), tags$h5("Effort type"), #inline = TRUE,
                    choices = list("Standard" = "S", "Non-standard" = "N", "Fine" = "F"),
                    selected = c("S", "N", "F")
                  )
                )
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.das_effort != 1", ns = ns,
        fluidRow(
          cruz_box(
            title = "Line properties", width = 6,
            conditionalPanel(
              condition = "input.das_effort == 2", ns = ns,
              tags$h5("Simplified effort line color and width"),
              fluidRow(
                column(6, selectInput(ns("das_effort_simp_col"), NULL, choices = cruz.palette.color, selected = "black")),
                column(6, numericInput(ns("das_effort_simp_lwd"), NULL, value = 2, min = 1, max = 6, step = 1))
              )
            ),
            conditionalPanel(
              condition = "input.das_effort == 3", ns = ns,
              checkboxInput(ns("das_effort_det_byBft"), "Show effort by Beaufort", value = TRUE),
              conditionalPanel(
                condition = "input.das_effort_det_byBft", ns = ns,
                helpText(
                  "Each selected color corresponds to a Beaufort value, from low to high.",
                  "You must choose at least as many colors as the maximum Beaufort value, plus one.",
                  "Additional selected colors will be ignored.",
                  "The first color corresponds to Beaufort 0, the second color to Beaufort 1, and so on,",
                  "even if the Beaufort filter values change."
                ),
                conditionalPanel(
                  condition = "input.color_style == 2", ns = ns,
                  tags$span(
                    tags$h5("You cannot plot effort color-coded by Beaufort when using grey scale"),
                    style = "color: red;"
                  )
                ),
                selectInput(ns("das_effort_det_bft_col"), tags$h5("Beaufort colors"),
                            choices = cruz.palette.color, selected = eff.bft.default,
                            multiple = TRUE),
                numericInput(ns("das_effort_det_bft_lwd"), tags$h5("Line width"),
                             value = 2, min = 1, max = 6, step = 1)
              )
            ),
            conditionalPanel(
              condition = "input.das_effort_det_byBft == false", ns = ns,
              conditionalPanel(
                condition = "output.das_effort_det_s_flag",  ns = ns,
                tags$h5("Standard effort line color and width"),
                fluidRow(
                  column(6, selectInput(ns("das_effort_det_col_s"), NULL, choices = cruz.palette.color, selected = "black")),
                  column(6, numericInput(ns("das_effort_det_lwd_s"), NULL, value = 2, min = 1, max = 6, step = 1))
                )
              ),
              conditionalPanel(
                condition = "output.das_effort_det_n_flag", ns = ns,
                tags$h5("Non-standard effort line color and width"),
                fluidRow(
                  column(6, selectInput(ns("das_effort_det_col_n"), NULL, choices = cruz.palette.color, selected = "dodgerblue2")),
                  column(6, numericInput(ns("das_effort_det_lwd_n"), NULL, value = 2, min = 1, max = 6, step = 1))
                )
              ),
              conditionalPanel(
                condition = "output.das_effort_det_f_flag", ns = ns,
                tags$h5("Fine scale effort line color and width"),
                fluidRow(
                  column(6, selectInput(ns("das_effort_det_col_f"), NULL, choices = cruz.palette.color, selected = "green")),
                  column(6, numericInput(ns("das_effort_det_lwd_f"), NULL, value = 2, min = 1, max = 6, step = 1))
                )
              )
            )
          ),
          cruz_box(
            title = "Effort filters", width = 6,
            checkboxInput(
              ns("das_effort_filter_same"),
              "Same as 'Sighting filters' for Beaufort, date range, and cruise numbers",
              value = TRUE
            ),
            conditionalPanel(
              condition = "input.das_effort_filter_same == false", ns = ns,
              conditionalPanel(
                condition = "input.das_effort == 3", ns = ns,
                fluidRow(
                  column(6, selectInput(ns("das_effort_minBft"), tags$h5("Minimum Beaufort"), choices = cruz.beaufort, selected = 0)),
                  column(6, selectInput(ns("das_effort_maxBft"), tags$h5("Maximum Beaufort"), choices = cruz.beaufort, selected = 9)),
                  column(12, helpText("Note that if the min and max Beaufort values are 0 and 9, respectively,",
                                      "then effort lines with NA Beaufort values will be plotted"))
                )
              ),
              conditionalPanel(
                "input.das_effort == 2", ns = ns,
                helpText("Only detailed effort lines can be plotted by Beaufort")
              ),
              uiOutput(ns("das_effort_dateRange_uiOut_date")),
              uiOutput(ns("das_effort_cruise_uiOut_select")),
              helpText("Note that if no cruise numbers are selected,",
                       "effort with NA cruise number values will be plotted")
            )
          )
        ),
        fluidRow(
          cruz_box(
            title = "Interactive effort labels", width = 12,
            conditionalPanel(
              condition = "input.das_effort != 2", ns = ns,
              tags$h5("Interactive effort labels can only be used with simplified effort")
            ),
            conditionalPanel(
              condition = "input.das_effort == 2", ns = ns,
              helpText("Note that to display interactive effort your cursor",
                       "must be near the start or end points of the effort,",
                       "rather than just the effort line displayed in the map"),
              fluidRow(
                column(
                  width = 4,
                  radioButtons(
                    ns("das_effort_interactive"), NULL,
                    choices = list("Non-interactive plot" = 1,
                                   "View and label effort interactively" = 2),
                    selected = 1
                  )
                ),
                column(
                  width = 8,
                  actionButton(ns("das_effort_interactive_reset_last"), "Remove last effort label"),
                  actionButton(ns("das_effort_interactive_reset_all"), "Remove all effort labels")
                )
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
        cruz_box(
          title = "Sightings legend", width = 12,
          conditionalPanel(
            condition = "input.das_sightings==false", ns = ns,
            helpText("*** No legend for sightings unless 'Plot sightings' is selected in the 'Sightings' tab")
          ),
          conditionalPanel(
            condition = "input.das_sightings", ns = ns,
            checkboxInput(ns("das_legend"), "Include legend for sightings", value = TRUE),
            conditionalPanel(
              condition = "input.das_legend", ns = ns,
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    ns("sight_legend_pos"), tags$h5("Position"),
                    choices = list("Specify" = 1,
                                   "Top Left" = "topleft",
                                   "Top Right"= "topright",
                                   "Bottom Left" = "bottomleft",
                                   "Bottom Right" = "bottomright"),
                    selected = "topright"
                  ),
                  selectInput(
                    ns("sight_legend_boxCol"), tags$h5("Box style"),
                    choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                    selected = 3
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    ns("sight_legend_font"), tags$h5("Font"),
                    choices = font.family, selected = 1
                  ),
                  numericInput(
                    ns("sight_legend_textSize"), tags$h5("Legend size"),
                    value = 1.0, min = 0.1, max = 3, step = 0.1
                  )
                ),
                column(
                  width = 5,
                  textInput(ns("sight_legend_title"), tags$h5("Title (optional)"), value = ""),
                  conditionalPanel(
                    condition = "input.das_sighting_type == 1 | input.das_sighting_type == 2",
                    ns = ns,
                    checkboxGroupInput(
                      ns("sight_legend_names"), tags$h5("Legend information"),
                      choices = list("Species code" = 1, "Species abbreviation" = 2,
                                     "Scientific name" = 3, "Common name" = 4,
                                     "Include number of sightings" = 5),
                      selected = c(1, 2, 5)
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.sight_legend_pos == 1", ns = ns,
                  box(
                    width = 12,
                    helpText("Coordinates for the top left corner of the legend"),
                    fluidRow(
                      column(
                        width = 4,
                        numericInput(ns("sight_legend_lon"), tags$h5("Longitude"), value = 0),
                      ),
                      column(
                        width = 4,
                        numericInput(ns("sight_legend_lat"), tags$h5("Latitude"), value = 0)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      fluidRow(
        cruz_box(
          title = "Effort legend", width = 12,
          conditionalPanel(
            condition = "input.das_effort == 1", ns = ns,
            helpText("*** No legend for effort unless effort is plotted in the 'Effort' tab")
          ),
          conditionalPanel(
            condition = "input.das_effort != 1", ns = ns,
            checkboxInput(ns("eff_legend"), "Include legend for effort", value = TRUE),
            conditionalPanel(
              condition = "input.eff_legend", ns = ns,
              fluidRow(
                column(
                  width = 4,
                  selectInput(
                    ns("eff_legend_pos"), tags$h5("Position"),
                    choices = list("Specify" = 1, "Top Left" = "topleft", "Top Right"= "topright",
                                   "Bottom Left" = "bottomleft", "Bottom Right" = "bottomright"),
                    selected = "bottomleft"
                  ),
                  selectInput(
                    ns("eff_legend_boxCol"), tags$h5("Box color"),
                    choices = list("Transparent" = 1, "White" = 2, "White with border" = 3),
                    selected = 3
                  )
                ),
                column(
                  width = 3,
                  selectInput(ns("eff_legend_font"), tags$h5("Font"), choices = font.family, selected = 1),
                  numericInput(ns("eff_legend_textSize"), tags$h5("Legend size"),
                               value = 1.0, min = 0.1, max = 3, step = 0.1)
                ),
                column(
                  width = 4,
                  textInput(ns("eff_legend_title"), tags$h5("Title (optional)"), value = "")
                ),
                conditionalPanel(
                  condition = "input.eff_legend_pos == 1", ns = ns,
                  box(
                    width = 12,
                    helpText("Coordinates for the top left corner of the legend"),
                    fluidRow(
                      column(
                        width = 4,
                        numericInput(ns("eff_legend_lon"), tags$h5("Longitude"), value = 0),
                      ),
                      column(
                        width = 4,
                        numericInput(ns("eff_legend_lat"), tags$h5("Latitude"), value = 0)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    ##############################################################################################################
    tabPanel(
      title = "Tabular Output",
      helpText("todo")
      #   fluidRow(
      #     box(
      #       title = "Effort", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
      #       conditionalPanel("input.das_effort == 1", helpText("Effort must be plotted to generate tabular output for effort")),
      #       conditionalPanel(
      #         condition = "input.das_effort != 1",
      #         helpText("Reports the distance traveled while on effort, summarized by effort type.",
      #                   "The effort that is summarized is the same as the effort that is plotted,",
      #                   "i.e. it has been filtered using the same filters",
      #                   "(mode, effort type, date, Beaufort, and cruise number)",
      #                   "specified in the 'Effort' tab"),
      #         radioButtons("das_out_effort_units", "", #NULL puts the widget too close to the helpText
      #                       choices = list("Distance unit: kilometers" = 1, "Distance unit: nautical miles" = 2),
      #                       selected = 2),
      #         tableOutput("das_out_effort_table"),
      #         downloadButton("das_out_effort_save", "Save effort table")
      #       )
      #     ),
      #     box(
      #       title = "Sightings", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
      #       conditionalPanel(
      #         condition = "input.das_sightings != true",
      #         helpText("Sightings must be selected to generate tabular output for sightings")
      #       ),
      #       conditionalPanel(
      #         condition = "input.das_sightings",
      #         helpText("Reports the number of sightings with the selected species, summarized by effort type.",
      #                   "The sightings that are summarized are those that are plotted,",
      #                   "i.e. they have been filtered using the same filters",
      #                   "(species, on or off effort, mode, effort type, date, Beaufort, cruise number, and truncation distance)",
      #                   "specified in the 'Filters' tab"),
      #         tags$br(),
      #         tags$h5("Total number of sightings in the DAS file for selected events and species,",
      #                 "with only a filter for species code applied (if applicable).",
      #                 "Note that for instance when plotting all marine mammal species,",
      #                 "an S sighting with two species counts as two in this sum.",
      #                 "These values are intended to be a reference for the total number of sightings in the DAS file(s)"),
      #         tableOutput("das_out_sight_tot_table"),
      #         checkboxGroupInput("das_out_sciname", tags$h5("Additional species information to include"),
      #                             choices = list("Species abbreviation" = 2,
      #                                           "Scientific name" = 3, "Common name" = 4),
      #                             selected = NULL, inline = TRUE),
      #         checkboxInput("das_out_allcheck", "Include an 'All' summary row", value = TRUE),
      #         tableOutput("das_out_sight_table"),
      #         downloadButton("das_out_sight_save", "Save sightings table")
      #       )
      #     )
      #   )
    )
  )
}


#' @name mod_das
#' @export
mod_das_server  <- function(id, load_state, color_style, map_range_config) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(color_style)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
      sp.codes = NULL,      # Species code file
      sp.codes.name = NULL, # Either "default" or "personal"
      das.data = NULL       # DAS dataframe
    )

    cruz.eff.leg       <- reactiveVal(FALSE)
    cruz.eff.leg.title <- reactiveVal("")

    # Load state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          cruz.list[[item$id]] <- item$value
        } else {
          update_widget(item, session)
        }
      }
    }, priority = 10)


    ###############################################################################
    # Help pages

    # ### Help page for das file load
    # observeEvent(input$das_file_help, {
    #   showModal(modalDialog(
    #     tags$h5(tags$strong("Lines to skip:"),
    #             "The number of lines that are ignored before starting to read data in the provided file."),
    #     tags$h5(tags$strong("days.gap argument:"),
    #             "This argument helps the user keep information from spilling from",
    #             "one cruise into the next in concatenated DAS files.",
    #             "For instance, if days.gap is 20, then all of the DAS state/condition information",
    #             "(Cruise, Mode, Beaufort, Visibility, etc.)",
    #             "are reset when there are 20 or more days between records in the file."),
    #     tags$h5(tags$strong("reset.event argument:"),
    #             "This argument specifies what happens if a state/condition is not entered in the DAS data.",
    #             "For example, say the Beaufort value is left blank in a V event.",
    #             "If reset.event is TRUE, then the Beaufort value is set to NA",
    #             "in the processed data until the next V event.",
    #             "If reset.event is FALSE, then the previous Beaufort value will be carried through,",
    #             "meaning the NA value will be ignored.",
    #             "This argument should be FALSE only if state/condition information (e.g. Beaufort value)",
    #             "was purposefully not entered if it did not change."),
    #     tags$h5(tags$strong("reset.effort argument:"),
    #             "When using WinCruz, an R event or BR event series signifying the start of a new continuous effort section",
    #             "is generally immediately followed by a PVNW event sequence in which all state/condition values are updated.",
    #             "If reset.effort is TRUE, then the state/condition arguments are all reset to NA at the R event,",
    #             "or at the B event is it immediately precedes said R event.",
    #             "This argument should be FALSE only if 1) R events are not always followed by a PVNW event sequence or",
    #             "2) state/condition information was purposefully not entered if it did not change (similar to reset.event)."),
    #     tags$h5(tags$strong("reset.day argument:"),
    #             "This argument is always set to TRUE - the user cannot specify it when using CruzPlot.",
    #             "When TRUE, all of the state and condition information is reset at the beginning of each new day,",
    #             "rather than being carried over from the previous day."),
    #     easyClose = FALSE
    #   ))
    # })


    ### Help page for sighting events
    observeEvent(input$das_sighting_events_help, {
      showModal(modalDialog(
        tags$h5(tags$strong("S event:"), "Cetacean sighting - standard"),
        tags$h5(tags$strong("G event:"), "Cetacean subgroup sighting"),
        tags$h5(tags$strong("K event:"), "Cetacean sighting - tracker. Historically only used during cruise number 1611 in 1998"),
        tags$h5(tags$strong("M event:"), "Cetacean sighting - matched. Historically only used during cruise number 1608 in 1997"),
        tags$h5(tags$strong("p event:"), "Pinniped sighting. Used beginning in 2018"),
        tags$h5(tags$strong("s event:"), "Standard cetacean resight. Corresponds to a previous S event"),
        tags$h5(tags$strong("g event:"), "Cetacean subgroup resight. Corresponds to a previous G event"),
        tags$h5(tags$strong("k event:"), "Tracker cetacean resight. Corresponds to a previous K event"),
        tags$br(), tags$br(),
        tags$h5(
          "When plotting resights (s, k, or g events): 1) you can only plot a single sighting type (e.g. S and s event)",
          "and a single species at one time,",
          "and 2) The 'Symbol color...' entry corresponds the the order of the selected events.",
          "In addition, for resight plotting please ensure that the loaded DAS file is not a concatenated file, ",
          "i.e. each sighting number in the file corresponds to a single sighting/resighting group."
        ),
        easyClose = FALSE
      ))
    })


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # General

    # validate(
    #   need((input$das_effort_interactive == 1) || (input$das_sight_interactive == 1),
    #       message = "Cannot have both sighting and effort interactive plots selected")
    # )
    # if ((input$das_effort_interactive != 1) || (input$das_sight_interactive != 1)) {
    #   validate(
    #     need(cruz.list$das.data,
    #         "Please load a DAS data file before selecting an interactive plot")
    #   )
    # }

    ### renderUIs for filters, used by sightings and effort

    #--------------------------------------------
    # Date widgets
    dateRange_min_max <- reactive({
      x <- req(cruz.list$das.data)
      input$das.file

      min.date <- as.character(as.Date(min(x$Date, na.rm = T)) - 1)
      max.date <- as.character(as.Date(max(x$Date, na.rm = T)) + 1)

      c(min.date, max.date)
    })

    output$das_sight_dateRange_uiOut_date <- renderUI({
      dates <- dateRange_min_max()
      dateRangeInput(
        session$ns("das_sight_dateRange"), tags$h5("Date range"),
        start = dates[1], end = dates[2]
      )
    })
    outputOptions(
      output, "das_sight_dateRange_uiOut_date",
      suspendWhenHidden = FALSE, priority = 3
    )

    output$das_effort_dateRange_uiOut_date <- renderUI({
      dates <- dateRange_min_max()
      dateRangeInput(
        session$ns("das_effort_dateRange"), tags$h5("Date range"),
        start = dates[1], end = dates[2]
      )
    })
    outputOptions(
      output, "das_effort_dateRange_uiOut_date",
      suspendWhenHidden = FALSE, priority = 3
    )


    #--------------------------------------------
    # Cruise number

    ### Reactive returning cruise numbers in the file
    das_cruise_nums <- reactive({
      x <- req(cruz.list$das.data)
      input$das.file

      unique(na.omit(x$Cruise))
    })

    ### Cruise number - sight
    output$das_sight_cruise_uiOut_select <- renderUI({
      selectInput(
        session$ns("das_sight_cruise"), tags$h5("Cruise number(s)"),
        choices = das_cruise_nums(), multiple = TRUE, selected = NULL
      )
    })
    outputOptions(
      output, "das_sight_cruise_uiOut_select",
      suspendWhenHidden = FALSE, priority = 3
    )

    ### Cruise number - effort
    output$das_effort_cruise_uiOut_select <- renderUI({
      selectInput(
        session$ns("das_effort_cruise"), tags$h5("Cruise number(s)"),
        choices = das_cruise_nums(), multiple = TRUE, selected = NULL)
    })
    outputOptions(
      output, "das_effort_cruise_uiOut_select",
      suspendWhenHidden = FALSE, priority = 3
    )

    #--------------------------------------------
    ### Truncation input
    output$das_sight_trunc_uiOut_numeric <- renderUI({
      isolate(curr.value <- input$das_sight_trunc)

      trunc.units <- input$das_sight_trunc_units
      if(trunc.units == 1) widget.name <- "Truncation (km)"
      if(trunc.units == 2) widget.name <- "Truncation (nmi)"

      numericInput(
        session$ns("das_sight_trunc"),
        tags$h5(widget.name),
        value = curr.value
      )
    })
    outputOptions(
      output, "das_sight_trunc_uiOut_numeric",
      suspendWhenHidden = FALSE, priority = 3
    )


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Sp Codes

    spcodes_default_read <- eventReactive(input$das_spcodes_default, {
      cruz.list$sp.codes <- NULL
      cruz.list$sp.codes.name <- NULL
      shinyjs::reset("das_spcodes_file")

      spcodes_path <- system.file("extdata", "SpCodes.dat", package = "swfscDAS")
      x <- try(das_spcodes_read(spcodes_path), silent = TRUE)
      validate(
        need(x, paste("Error reading the default SpCodes.dat file.",
                      "Please try reinstalling CruzPlot and swfscDAS"))
      )

      cruz.list$sp.codes <- x
      cruz.list$sp.codes.name <- "default"

      ""
    })

    spcodes_user_read <- eventReactive(input$das_spcodes_file, {
      cruz.list$sp.codes <- NULL
      cruz.list$sp.codes.name <- NULL

      x <- try(das_spcodes_read(input$das_spcodes_file$datapath), silent = TRUE)
      validate(
        need(x, "Error reading the loaded SpCodes.dat file")
      )

      cruz.list$sp.codes <- x
      cruz.list$sp.codes.name <- input$das_spcodes_file$name

      ""
    })

    output$spcodes_user_read_text <- renderText(spcodes_user_read())
    output$spcodes_default_read_text <- renderText(spcodes_default_read())
    output$spcodes_message <- renderText({
      if (req(cruz.list$sp.codes.name) == "default") {
        "The default species code file, 'SpCodes.dat' from swfscDAS, is loaded"
      } else {
        paste(
          "The following user-provided species code file is loaded:",
          cruz.list$sp.codes.name
        )
      }
    })


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Loading DAS file(s)

    ### Render DAS load messages
    output$das_file_load_text <- renderText(das_file_load())

    output$das_loaded_text <- renderText({
      req(cruz.list$das.data)
      data.name <- unique(cruz.list$das.data$file_das)
      if (length(data.name) == 1) {
        paste("The following DAS file is loaded:", data.name)
      } else {
        paste("The following DAS files are loaded:",
              paste(data.name, collapse = ", "))
      }
    })

    ### Output flag indicating if DAS data has been loaded
    output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
    outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)

    ### Output flag indicating if SpCodes has been loaded
    output$das_spcodes_loaded_flag <- reactive(isTruthy(cruz.list$sp.codes))
    outputOptions(output, "das_spcodes_loaded_flag", suspendWhenHidden = FALSE)

    ### Read and process DAS file(s)
    das_file_load <- eventReactive(input$das_file, {
      # Clear reactive vals, and reset plot sightings and effort selections
      cruz.list$das.data <- NULL
      cruz.list$das.data.name <- NULL
      updateCheckboxInput(session, "das_sightings", value = FALSE)
      updateRadioButtons(session, "das_effort", selected = 1)

      # Get and check additional parameters
      skip <- input$das_file_skip
      days.gap <- input$das_file_days_gap
      reset.event  <- input$das_file_reset_event == 1
      reset.effort <- input$das_file_reset_effort == 1
      # reset.day    <- input$das_file_reset_day == 1

      v.tmp <- "skip must be a valid, whole number greater than or equal to 0"
      validate(need(!is.na(skip), v.tmp))
      validate(need(isTRUE(all.equal(skip %% 1, 0)), v.tmp))
      validate(need(skip >= 0, v.tmp))
      rm(v.tmp)

      v.tmp <- "days.gap must be a valid, whole number greater than or equal to 0"
      validate(need(!is.na(days.gap), v.tmp))
      validate(need(isTRUE(all.equal(days.gap %% 1, 0)), v.tmp))
      validate(need(days.gap >= 0, v.tmp))
      rm(v.tmp)

      # Process DAS file
      withProgress(message = "Processing DAS file", value = 0.6, {
        das.proc <- try(suppressWarnings(
          swfscDAS::das_process(
            input$das_file$datapath, skip = skip, days.gap = days.gap,
            reset.event = reset.event, reset.effort = reset.effort,
            reset.days = TRUE
          )
        ), silent = TRUE)
      })

      validate(
        need(isTruthy(das.proc),
             paste0("Error: unable to read and process the provided DAS file(s).",
                    "\n\nThe call to das_read and das_process from the ",
                    "swfscDAS package returned the following error. ",
                    "Please use `swfscDAS::das_check` ",
                    "for a more informative error message.\n\n",
                    attr(das.proc, "condition")))
      )

      # Correct filename
      filename.key <- data.frame(
        tmp = basename(input$das_file$datapath),
        actual = input$das_file$name,
        stringsAsFactors = FALSE
      )

      das.proc <- das.proc  |>
        left_join(filename.key, by = c("file_das" = "tmp")) |>
        mutate(file_das = .data$actual) |>
        select(-.data$actual)

      # Save reactive values
      cruz.list$das.data <- das.proc
      # cruz.list$das.data.name <- input$das_file$name

      ""
    }, ignoreInit = TRUE)


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Sightings

    #---------------------------------------------------------------------------
    ### Species codes
    # Text in sightings tab about no SpCodes.data file
    output$das_sight_spcodes_message <- renderText({
      validate(
        need(cruz.list$sp.codes,
             "You must load a species code file in the 'Data' window to plot sightings")
      )

      ""
    })

    # renderUIs for mammal and turtle species
    output$das_sighting_code_1_uiOut_select <- renderUI({
      sp.mammals <- spcodes_mammals(req(cruz.list$sp.codes))
      sp.codes.list <- as.list(sp.mammals$SpCode)
      names(sp.codes.list) <- paste(sp.mammals$SpCode, sp.mammals$Abbr, sep = " - ")

      selectInput(
        session$ns("das_sighting_code_1"), tags$h5("Select mammal species"),
        choices = sp.codes.list, multiple = TRUE,
        selected = NULL
      )
    })
    outputOptions(output, "das_sighting_code_1_uiOut_select", suspendWhenHidden = FALSE)

    output$das_sighting_code_2_uiOut_select <- renderUI({
      sp.turtles <- spcodes_turtles(req(cruz.list$sp.codes))
      sp.codes.list <- as.list(sp.turtles$SpCode)
      names(sp.codes.list) <- paste(sp.turtles$SpCode, sp.turtles$SciName, sep = " - ")

      selectInput(
        session$ns("das_sighting_code_2"), tags$h5("Select turtle species"),
        choices = sp.codes.list, multiple = TRUE, selected = NULL
      )
    })
    outputOptions(output, "das_sighting_code_2_uiOut_select", suspendWhenHidden = FALSE)


    # Extract and process species codes
    cruzDasSightSpeciesMammals <- reactive({
      sp.codes <- if (input$das_sighting_code_1_all == 1) {
        spcodes_mammals(req(cruz.list$sp.codes))$SpCode
      } else if (input$das_sighting_code_1_all == 2) {
        gsub(" ", "", substring(input$das_sighting_code_1, 1, 3))
      } else {
        stop("Invalid CruzPlot input$das_sighting_code_1_all value. ",
             "Please report this as an issue")
      }

      validate(
        need(length(sp.codes) > 0, "Please choose at least one valid mammal species code")
      )

      sp.codes
    })

    cruzDasSightSpeciesTurtles <- reactive({
      sp.codes <- if (input$das_sighting_code_2_all == 1) {
        spcodes_turtles(req(cruz.list$sp.codes))$SpCode
      } else if (input$das_sighting_code_2_all == 2) {
        substring(input$das_sighting_code_2, 1, 2)
      } else {
        stop("Invalid CruzPlot input$das_sighting_code_2_all value. ",
             "Please report this as an issue")
      }

      validate(
        need(length(sp.codes) > 0, "Please choose at least one valid turtle species code")
      )

      sp.codes
    })


    ### Code for keeping current inputs the same when switching
    #  from or to text symbol properties input
    observeEvent(input$das_symbol_mult, {
      if (input$das_symbol_mult) {
        ### Convert codes to text
        # Covert numerics to symbols
        curr.pch <- as.numeric(input$das_symbol_type)
        if (length(curr.pch) == 0) curr.pch <- "1"

        # Covert color codes to color names
        curr.col <- input$das_symbol_color
        if (is.null(curr.col)) {
          curr.col <- "Black"

        } else {
          # This keeps numbers in order
          if (color_style() == 1) {
            curr.col.idx <- vapply(
              curr.col, function(i) which(symbol.col.code %in% i), 1
            )
            curr.col <- symbol.col[curr.col.idx]

          } else if (color_style() == 2) {
            curr.col.idx <- vapply(
              curr.col, function(i) which(symbol.col.code.gray %in% i), 1
            )
            curr.col <- symbol.col.gray[curr.col.idx]
          }
        }

        updateTextInput(
          session, "das_symbol_type_mult",
          value = paste(curr.pch, collapse = ", ")
        )
        updateTextInput(
          session, "das_symbol_color_mult",
          value = paste(curr.col, collapse = ", ")
        )


      } else {
        ### Convert from text to codes
        # Convert symbol codes to symbols
        curr.pch <- suppressWarnings(
          as.numeric(unlist(strsplit(input$das_symbol_type_mult, ", ")))
        )
        if (length(curr.pch) == 0) {
          curr.pch <- 1
        } else {
          if (!all(curr.pch %in% unname(cruz.symbol.type))) curr.pch <- 1
        }
        updateSelectInput(session, "das_symbol_type", selected = curr.pch)


        # Covert color names to color codes
        curr.col <- unlist(strsplit(input$das_symbol_color_mult, ", "))

        if (is.null(curr.col)) {
          curr.col <- "black"

        } else {
          if (!all(curr.col %in% symbol.col)) {
            curr.col <- "black"
          } else {
            # This keeps numbers in order
            if (color_style() == 1) {
              curr.col.idx <- vapply(
                curr.col, function(i) which(symbol.col %in% i), 1
              )
              curr.col <- symbol.col.code[curr.col.idx]

            } else if (color_style() == 2) {
              curr.col.idx <- vapply(
                curr.col, function(i) which(symbol.col.gray %in% i), 1
              )
              curr.col <- symbol.col.code.gray[curr.col.idx]
            }
          }
        }
        updateSelectInput(session, "das_symbol_color", selected = curr.col)
      }
    }, ignoreInit = TRUE)


    #---------------------------------------------------------------------------
    ### 1. Run das_sight
    cruzDasSightSpeciesProcess <- reactive({
      swfscDAS::das_sight(req(cruz.list$das.data), return.format = "default")
    })


    #---------------------------------------------------------------------------
    ### 2. Perform NA position filter, species/event filter

    ### Checks on NA positions to print modal, and 'select' selected position
    cruzDasSightPosition <- reactive({
      das.sight <- cruzDasSightSpeciesProcess()

      #------------------------------------------
      # Verbosely remove sightings with NA positions
      ll.na <- if (input$das_sightings_position == 1) {
        which(is.na(das.sight$Lat) | is.na(das.sight$Lon))
      } else (
        which(
          is.na(das.sight$Lat) | is.na(das.sight$Lon) | is.na(das.sight$Course) |
            is.na(das.sight$Bearing) | is.na(das.sight$DistNm)
        )
      )

      if (length(ll.na) > 0) {
        table.out <- das.sight %>%
          slice(ll.na) %>%
          mutate(DateTime = as.character(DateTime),
                 Cruise = as.character(Cruise),
                 Resight = Event %in% c("s", "k", "g")) %>%
          select(Event, DateTime, Lat, Lon, OnEffort, Cruise,
                 SightNo, SpCode, Resight,
                 # File = file_das,
                 `Line number` = line_num) %>%
          distinct()
        txt.out1 <- ifelse(nrow(table.out) == 1, "sighting has", "sightings have")
        txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

        showModal(modalDialog(
          title = "CruzPlot notice",
          tags$h5("The following", txt.out1, "an NA value that causes the",
                  "specified plotted position to be NA.",
                  txt.out2, "will be automatically removed (filtered) and thus",
                  "not plotted or included in tabular output:"),
          tags$br(), tags$br(),
          renderTable(table.out),
          tags$br(),
          tags$h5("This notice will not be shown again unless a new DAS file is loaded or",
                  "'Position to plot' is changed.",
                  "See the manual for more details"),
          easyClose = FALSE,
          size = "l"
        ))
      }

      das.sight <- if (input$das_sightings_position == 1) {
        das.sight %>% filter(!is.na(.data$Lat), !is.na(.data$Lon))
      } else {
        das.sight %>%
          filter(!is.na(.data$Lat), !is.na(.data$Lon),
                 !is.na(.data$Course), !is.na(.data$Bearing), !is.na(.data$DistNm))
      }

      #------------------------------------------
      # Calculate sighting location,, select selected position, and return
      bearing2 <- (das.sight$Course + das.sight$Bearing) %% 360
      ll.sight <- geosphere::destPoint(
        matrix(c(das.sight$Lon, das.sight$Lat), ncol = 2),
        bearing2, das.sight$DistNm * 1852
      )

      # # Calculate sighting location using swfscMisc
      # ll.sight.dest <- apply(das.sight, 1, function(i) {
      # i <- as.numeric(i[c("Lat", "Lon", "bearing2", "DistNm")])
      #   swfscMisc::destination(i["Lat"], i["Lon"], i["bearing2"], i["DistNm"],
      #                          units = "nm", type = "ellipsoid")
      # })

      das.sight <- das.sight %>%
        mutate(Lat_ship = .data$Lat, Lon_ship = .data$Lon,
               Lat_sight = ll.sight[, "lat"],
               Lon_sight = ll.sight[, "lon"])


      # 'Select' ship or sighting position.
      #   Might as well do this here since position input is already being used
      if (input$das_sightings_position == 1) {
        das.sight$Lat <- das.sight$Lat_ship
        das.sight$Lon <- das.sight$Lon_ship

      } else if (input$das_sightings_position == 2) {
        das.sight$Lat <- das.sight$Lat_sight
        das.sight$Lon <- das.sight$Lon_sight
      }

      # Adjust longitudes if world2 map is being used
      # #this is done in cruzDasSightRange()
      # if (map_range_config()$world2)
      #   das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

      das.sight
    })


    #---------------------------------------------------------------------------
    ### 3. Filter for events specified either directly or by sighting type
    #   And associated things that depend on the selected event
    cruzDasSightEventResight <- reactive({
      (input$das_sighting_type == 1) &
        any(c("s", "k", "g") %in% input$das_sighting_events)
    })

    output$cruzDasSightEventResight_uiOut_message <- renderUI({
      req(cruzDasSightEventResight())
      tags$h5("When plotting resights, symbol color entries correspond to selected events")
    })

    cruzDasSightEvent <- reactive({
      das.sight <- cruzDasSightPosition()
      sight.type <- input$das_sighting_type

      if (sight.type == 2) {
        ### Turtle sightings
        das.sight <- das.sight %>% filter(.data$Event == "t")
        validate(
          need(nrow(das.sight) > 0,
               "There are no turtle sightings (t events) in the loaded DAS file(s)")
        )

      } else if (sight.type == 3) {
        ### Boat sightings
        das.sight <- das.sight %>% filter(.data$Event == "F")
        validate(
          need(nrow(das.sight) > 0,
               "There are no boat sightings (F events) in the loaded DAS file(s)")
        )

      } else if (sight.type == 1) {
        ### Marine mammal sightings
        sp.events <- input$das_sighting_events
        validate(need(sp.events, "Please select at least one event code to plot"))

        das.sight <- das.sight %>%
          filter(.data$Event %in% sp.events) %>%
          mutate(idx = seq_along(.data$Event))

        # Resights
        if (cruzDasSightEventResight()) {
          # Checks - other
          validate(
            need(length(unique(na.omit(cruz.list$das.data$file_das))) == 1,
                 "You can only process resights when plotting data from a single DAS file")
            # ^b/c different files could have same SightNo, etc.
            # Doesn't solve concatenated files..
          )
          validate(
            need(sum(c("s", "k", "g") %in% sp.events) == 1,
                 "You can only plot one type of resight at a time")
          )
          validate(
            if ("s" %in% sp.events) need("S" %in% sp.events, "To plot s events, S events must also be plotted"),
            if ("k" %in% sp.events) need("K" %in% sp.events, "To plot k events, K events must also be plotted"),
            if ("g" %in% sp.events) need("G" %in% sp.events, "To plot g events, G events must also be plotted"),
            need(length(sp.events) == 2,
                 "When plotting resights, you can only plot the resight and the corresponding primary sighting event")
          )

          # Get species, etc., for s and k events
          if (any(c("s", "k") %in% sp.events)) {
            das.sight.main <- das.sight %>% filter(!(.data$Event %in% c("s", "k")))
            das.sight.res <- das.sight %>% filter(.data$Event %in% c("s", "k"))
            validate(
              need(all(das.sight.res$SightNo %in% das.sight.main$SightNo),
                   paste("Not all of the selected s/k resight event(s) have primary sightings with",
                         "the same sighting numbers -",
                         "this is a DAS error that needs to be fixed to plot s/k events"))
            )

            col.names <- c("Prob", "SpCode", "SpCodeProb")
            d.toadd <- das.sight.main %>%
              select(SightNo, !!col.names) %>%
              filter(SightNo %in% das.sight.res$SightNo) %>%
              full_join(select(das.sight.res, -!!col.names), by = "SightNo") %>%
              select(!!names(das.sight.main))

            das.sight <- bind_rows(das.sight.main, d.toadd) %>% arrange(idx)
            rm(das.sight.main, das.sight.res, col.names, d.toadd)
          }

          # Get species, etc., for g events
          if (any("g" %in% sp.events)) {
            das.sight.main <- das.sight %>%
              filter(!(.data$Event %in% c("g"))) %>%
              mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
            das.sight.res <- das.sight %>%
              filter(.data$Event %in% c("g")) %>%
              mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
            validate(
              need(all(das.sight.res$ss_id %in% das.sight.main$ss_id),
                   paste("Not all of the selected g resight event(s) have primary sightings with",
                         "the same sighting numbers/subgroup identified",
                         "- this is a DAS error that needs to be fixed to plot g events"))
            )

            col.names <- c("Prob", "SpCode", "SpCodeProb")
            d.toadd <- das.sight.main %>%
              select(.data$ss_id, !!col.names) %>%
              filter(.data$ss_id %in% das.sight.res$ss_id) %>%
              full_join(select(das.sight.res, -!!col.names), by = c("ss_id")) %>%
              select(!!names(das.sight.main))

            das.sight <- bind_rows(das.sight.main, d.toadd) %>%
              select(-ss_id) %>%
              arrange(idx)
            rm(das.sight.main, das.sight.res, col.names, d.toadd)
          }
        }

        validate(
          need(nrow(das.sight) > 0,
               paste("There are no mammal sightings for the selected event(s)",
                     "in the loaded DAS file(s)"))
        )

        das.sight <- das.sight %>% select(-idx)

      } else  {
        ### Error
        validate("Invalid input$das_sighting_type value")
      }

      das.sight
    })


    #---------------------------------------------------------------------------
    ### 4. Filter for specfied species and (if applicable) events
    cruzDasSightSpecies <- reactive({
      das.proc <- req(cruz.list$das.data)

      ### Sightings to plot
      sight.type <- input$das_sighting_type
      stopifnot(sight.type %in% 1:4)
      sp.selection <- isTRUE(
        (sight.type == 1 && input$das_sighting_code_1_all == 2) ||
          (sight.type == 2 && input$das_sighting_code_2_all == 2)
      )

      das.sight <- cruzDasSightEvent()

      #------------------------------------------
      if (sight.type == 1) {
        # 1: Mammals
        # Get species codes - also does validate() check for valid species
        sp.codes <- cruzDasSightSpeciesMammals()
        if (cruzDasSightEventResight()) {
          validate(
            need(length(sp.codes) == 1,
                 "You currently can only plot resights for one species at a time")
          )
        }

        # Update probable sightings species if necessary
        if (input$das_sighting_probable) {
          das.sight$Prob[das.sight$Event %in% c("p", "s", "k", "g")] <- FALSE
          if (any(is.na(das.sight$Prob)))
            warning("A marine mammal sighting has an unexpected NA 'Prob' value")

          validate(
            need(sum(das.sight$Prob) > 0,
                 "There are no probable sightings in the loaded DAS file(s)")
          )

          # 977 used as probable vaquita sighting on some cruises
          das.sight <- das.sight %>%
            mutate(SpCode = ifelse(.data$SpCode == "977", "041", .data$SpCode),
                   SpCode = ifelse(.data$Prob, .data$SpCodeProb, .data$SpCode))
        }

        # Filter for selected species, and check that all selected species are in data
        das.sight <- das.sight %>% filter(.data$SpCode %in% sp.codes)

        if (input$das_sighting_code_1_all == 2) {
          sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
          validate(
            need(length(sp.codes.none) == 0,
                 paste("The following species code(s) does (do) not",
                       "have any sightings in the loaded DAS file(s):",
                       paste(sp.codes.none, collapse = ", ")))
          )
        }


        #----------------------------------------
      } else if (sight.type == 2) {
        # 2: Turtles
        sp.codes <- cruzDasSightSpeciesTurtles()

        das.sight <- das.sight %>%
          filter(.data$SpCode %in% sp.codes)

        if (input$das_sighting_code_2_all == 2) {
          sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
          validate(
            need(length(sp.codes.none) == 0,
                 paste("The following species code(s) does (do) not",
                       "have any sightings in the loaded DAS file(s):",
                       paste(sp.codes.none, collapse = ", ")))
          )
        }


        #----------------------------------------
      } else if (sight.type == 3) {
        # 3: Boats
        das.sight <- das.sight %>% mutate(SpCode = "Boat")
        sp.codes <- NULL


        #----------------------------------------
      } else {
        validate("Invalid sighting type (input$das_sighting_type) selection")
      }


      # #----------------------------------------
      # # SMW: Added for vaquita-specifc cruise. Does not seem applicable anymore
      # } else if (sight.type == 4) {
      #   # 4: C-PODs
      #   # C-POD sightings are entered as objects with sighting angle and distance
      #   # the string "cpod" in the comment on the next line indicates object is a CPOD
      #   sp.codes <- NULL
      #
      #   validate(
      #     need(sum(das.proc$Event == "X") > 0,
      #          "There are no C-POD sightings in the loaded DAS file(s)")
      #   )
      #
      #
      #   ndx.x <- which(das.proc$Event == "X")
      #   comm.x1 <- apply(das.proc[ndx.X + 1, paste0("Data", 1:7)], 1, function(i) {
      #     paste(na.omit(i), collapse = "")
      #   })
      #   comm.x1.cpod <- grepl("cpod", comm.x1, ignore.case = TRUE)
      #   stopifnot(length(ndx.x) == length(comm.x1))
      #
      #   ndx.x <- ndx.x[comm.x1.cpod]
      #
      #   das.sight <- das.proc %>%
      #     slice(ndx.x) %>%
      #     mutate(Sp = "CPOD",
      #            Bearing = as.numeric(.data$Data2),
      #            DistNm = as.numeric(.data$Data4),
      #            PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852)
      #
      #   validate(
      #     need(nrow(das.sight) > 0,
      #          "There are no C-POD sightings in the loaded DAS file(s)")
      #   )
      #   # comment.str.df <- data.all[,6:13]
      #   # comment.str <- apply(comment.str.df,1,paste,collapse="")
      #   # ndx.cpod <- grep("cpod",comment.str)
      #   # ndx <- ndx.X[(ndx.X+1) %in% ndx.cpod]
      #   # data.sight <- data.all[ndx,]
      #   # angle <- as.numeric(data.sight$Data2)
      #   # dist.nmi <- as.numeric(data.sight$Data4)
      # }


      #------------------------------------------
      # Final check to ensure some sightings match provided selection
      validate(
        need(nrow(das.sight) > 0,
             "No sightings exist for the selected type and code(s)")
      )



      # If "Plot all..." was selected, only return codes in sighting data
      if ((sight.type == 1 && input$das_sighting_code_1_all == 1) |
          (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
        sp.codes <- base::intersect(sp.codes, das.sight$SpCode)
      }

      # Return list
      list(
        das.sight = das.sight,
        sight.type = sight.type,
        sp.codes = sp.codes,
        sp.selection = sp.selection
      )
    })


    #---------------------------------------------------------------------------
    ### 5. returns list, which includes sightings within map range,
    # selected sighting type, species codes, and counts for each species
    cruzDasSightRange <- reactive({
      #------------------------------------------
      req(cruz.list$das.data)

      data.list <- cruzDasSightSpecies()

      das.sight    <- data.list$das.sight
      sight.type   <- data.list$sight.type
      sp.codes     <- data.list$sp.codes
      sp.selection <- data.list$sp.selection

      lon.range <- map_range_config()$lon.range
      lat.range <- map_range_config()$lat.range

      # Adjust longitudes if world2 map is being used
      if (map_range_config()$world2)
        das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

      # NA values removed back in cruzDasSightSpeciesProcess()
      ll.na <- sum(is.na(das.sight$Lat) | is.na(das.sight$Lon))
      validate(
        need(ll.na == 0,
             "Error processing sighting positions - please report this as an issue")
      )


      #------------------------------------------
      # Filter to map range
      das.sight.filt <- das.sight %>%
        filter(between(.data$Lat, lat.range[1], lat.range[2]),
               between(.data$Lon, lon.range[1], lon.range[2]))
      validate(
        need(
          nrow(das.sight.filt) > 0,
          paste("No sightings are within the map boundaries for the",
                "selected sighting type/species")
        )
      )


      #------------------------------------------
      list(
        das.sight = das.sight.filt,
        sight.type = sight.type,
        sp.codes = sp.codes,
        sp.selection = sp.selection
      )
    })


    #---------------------------------------------------------------------------
    ### 6. Filter sightings

    # Helper functions that filter sightings data by single property

    .func_sight_filt_validate <- function(x, x.txt) {
      validate(
        need(any(x), paste("None of the specified sightings within the map range",
                           "match the given", x.txt, "filter"))
      )
      x
    }

    #--------------------------------------------
    # On/off effort
    cruzDasSightFilterEffort <- reactive({
      das.sight <- cruzDasSightRange()$das.sight
      effort.val <- switch(as.numeric(input$das_sight_effort), c(0, 1), 1, 0)
      keep <- as.numeric(das.sight$OnEffort) %in% effort.val
      .func_sight_filt_validate(keep, "on/off effort")
    })

    # Mode: C/P
    cruzDasSightFilterMode <- reactive({
      das.sight <- cruzDasSightRange()$das.sight
      keep <- das.sight$Mode %in% input$das_sight_cp
      .func_sight_filt_validate(keep, "mode (closing/passing)")
    })

    # Effort type: S/N/F
    cruzDasSightFilterEfftype <- reactive({
      das.sight <- cruzDasSightRange()$das.sight
      keep <- das.sight$EffType %in% input$das_sight_snf
      .func_sight_filt_validate(keep, "effort type (standard/non-standard/fine)")
    })

    #--------------------------------------------
    # Beaufort
    cruzDasSightFilterBeaufort <- reactive({
      das.sight <- cruzDasSightRange()$das.sight
      bft.min <- as.numeric(input$das_sight_minBft)
      bft.max <- as.numeric(input$das_sight_maxBft)

      validate(
        need(input$das_sight_minBft <= input$das_sight_maxBft,
             "Sightings filter: minimum Beaufort must be less than or equal to maximum Beaufort")
      )

      keep <- if (identical(c(bft.min, bft.max), c(0, 9))) {
        TRUE
      } else {
        between(das.sight$Bft, bft.min, bft.max)
      }
      .func_sight_filt_validate(keep, "Beaufort")
    })

    #--------------------------------------------
    # Dates
    cruzDasSightFilterDate <- reactive({
      das.sight <- cruzDasSightRange()$das.sight
      date.vals <- input$das_sight_dateRange

      validate(
        need(input$das_sight_dateRange[1] <= input$das_sight_dateRange[2],
             "Sightings filter: minimum date must be less than or equal to maximum date")
      )

      keep <- between(
        as.Date(das.sight$DateTime), date.vals[1], date.vals[2]
      )
      .func_sight_filt_validate(keep, "date")
    })

    #--------------------------------------------
    # Cruise numbers
    cruzDasSightFilterCruise <- reactive({
      das.sight <- cruzDasSightRange()$das.sight

      if (is.null(input$das_sight_cruise)) {
        # Return here to keep records that have 'NA' value
        TRUE

      } else {
        cruise.vals <- as.numeric(input$das_sight_cruise)
        keep <- das.sight$Cruise %in% cruise.vals
        .func_sight_filt_validate(keep, "cruise number")
      }
    })

    #--------------------------------------------
    # Perpendicular distance truncation
    cruzDasSightFilterTrunc <- reactive({
      das.sight <- cruzDasSightRange()$das.sight

      pdist.val <- ifelse(
        input$das_sight_trunc_units == 1,
        input$das_sight_trunc, input$das_sight_trunc * 1.852
      )

      if (is.na(pdist.val)) {
        TRUE

      } else {
        keep <- das.sight$PerpDistKm <= pdist.val
        validate(
          need(any(keep),
               "There are no selected sightings within the given truncation distance")
        )
        .func_sight_filt_validate(keep, "truncation (perpendicular distance)")
      }
    })


    #--------------------------------------------
    # Main filter function
    cruzDasSightFilter <- reactive({
      data.list <- cruzDasSightRange()

      das.sight    <- data.list$das.sight
      sight.type   <- data.list$sight.type
      sp.codes     <- data.list$sp.codes
      sp.selection <- data.list$sp.selection

      ### Collect logical vectors
      keep1 <- cruzDasSightFilterEffort()
      keep2 <- if (input$das_sight_effort == 2) cruzDasSightFilterMode() else TRUE
      keep3 <- if (input$das_sight_effort == 2) cruzDasSightFilterEfftype() else TRUE
      keep4 <- cruzDasSightFilterBeaufort()
      keep5 <- cruzDasSightFilterDate()
      keep6 <- cruzDasSightFilterCruise()
      keep7 <- cruzDasSightFilterTrunc()

      keep.all <- keep1 & keep2 & keep3 & keep4 & keep5 & keep6 & keep7
      keep.all.na <- which(is.na(keep.all)) #works b/c (NA & F) output is (F)

      ### Verbosely change NA filter values to FALSE
      if (length(keep.all.na) > 0) {
        table.out <- das.sight %>%
          slice(keep.all.na) %>%
          mutate(DateTime = as.character(DateTime),
                 Cruise = as.character(Cruise)) %>%
          select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
                 SightNo, SpCode, PerpDistKm,
                 # File = file_das, #Can take up too much space
                 `Line number` = line_num) %>%
          distinct()
        if (input$das_sight_effort == 2)
          table.out <- table.out %>% select(-Mode, -EffType)

        txt.out <- ifelse(nrow(table.out) == 1, "sighting", "sightings")
        txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

        showModal(modalDialog(
          title = "CruzPlot notice",
          tags$h5("The following", txt.out, "had at least one NA filter value.",
                  txt.out2, "will be removed (filtered) and thus not plotted",
                  "or included in tabular output:"),
          tags$br(), tags$br(),
          renderTable(table.out),
          easyClose = TRUE,
          footer = "Click anywhere or press any button to close this notice",
          size = "l"
        ))
      }

      keep.all[is.na(keep.all)] <- FALSE
      das.sight.filt <- das.sight[keep.all, ]

      ### Final checks, calculate sp.count, and return
      validate(
        need(sum(is.na(das.sight.filt$Event)) == 0,
             "Error in CruzPlot sighting filtering - please report this as an issue")
      )
      validate(need(nrow(das.sight) > 0, "No sightings match the given filters"))

      # Check that at least one sighting is still within map range, and get sp.count
      if (sight.type %in% c(1, 2)) {
        if (!sp.selection) {
          sp.codes <- base::intersect(sp.codes, das.sight.filt$SpCode)
        }

        # Calculate count for each species
        sp.count <- vapply(sp.codes, function(i, j) {
          sum(j$SpCode == i)
        }, 1, j = das.sight.filt, USE.NAMES = FALSE)

      } else {
        sp.count <- nrow(das.sight.filt)
      }

      list(
        das.sight = das.sight.filt,
        sight.type = sight.type,
        sp.codes = sp.codes,
        sp.selection = sp.selection,
        sp.count = sp.count
      )
    })


    #--------------------------------------------------------------------------
    ### 7. cruzDasSightSymbol for CruzPlot - step 4 of processing species data
    #   cruzDasSightSymbol() returns list of sighting type, species count, and
    #     point type, color, size, and linewidth for legend and points, respectively
    #   cruzDasSightSymbolAnimalSelected() returns list of point type, color, size, and linewidth for selected mammal or turtle sightings
    #   cruzDasSightSymbolAnimalAll() returns list of point type, color, size, and linewidth for all mammal or turtle sightings
    #   cruzDasSightSymbolBoat() returns list of point type, color, size, and linewidth for boat sightings
    #   cruzDasSightSymbolCPOD() returns list of point type, color, size, and linewidth for CPOD sightings

    # Two things need to be done - get/format pch, col, cex, and lwd for 1) legend and 2) sighting points.
    #   Boats and C-PODs al have singl value for each paramter, so (1) and (2) will be the same (and of length one).
    #   For mammals and turtles, 1) legend stuff requires one value per species code,
    #   while 2) sighting points require one value per sighting
    #   For (2) for mammals/turtles, we can use the legend values to build a species - plot params key,
    #   which is then joined with das.sight
    cruzDasSightSymbol <- reactive({
      data.list <- cruzDasSightFilter()

      sight.type   <- data.list$sight.type
      das.sight    <- data.list$das.sight
      sp.codes     <- data.list$sp.codes
      sp.count     <- data.list$sp.count
      sp.selection <- data.list$sp.selection

      # Species code sanity check
      if (sight.type %in% c(1, 2)) {
        stopifnot(isTRUE(all.equal(length(sp.codes), length(sp.count))))
      } else {
        stopifnot(is.null(sp.codes))
      }

      if (sight.type %in% c(3, 4)) {
        # Boats and C-Pods
        symbol.prop <- cruzDasSightSymbolBoat()
        leg.df <- data.frame(
          SpCode = ifelse(sight.type == 3, "Boat", "CPOD"),
          pch = symbol.prop$pt.pch,
          col = symbol.prop$pt.col,
          cex = symbol.prop$pt.cex,
          lwd = symbol.prop$pt.lwd,
          stringsAsFactors = FALSE
        )

      } else {
        # Mammals and turtles
        symbol.prop <- if (sp.selection) {
          cruzDasSightSymbolAnimalSelected()
        } else {
          cruzDasSightSymbolAnimalAll()
        }

        # For validate statements
        leg.pch <- symbol.prop$pt.pch
        leg.col <- symbol.prop$pt.col
        leg.cex <- symbol.prop$pt.cex
        leg.lwd <- symbol.prop$pt.lwd

        # Empty input check
        validate(
          need(leg.pch != "",
               "Please enter at least one number for symbol type"),
          need(leg.col != "",
               "Please enter at least one number for symbol color"),
          need(leg.cex != "",
               "Please enter at least one number for symbol size"),
          need(all(0 < leg.cex & leg.cex < 20),
               paste("Please ensure all symbol size entries are numbers",
                     "greater than zero and less than twenty")),
          need(leg.lwd != "",
               "Please enter at least one number for symbol line width"),
          need(all(0 < leg.lwd & leg.lwd < 20),
               paste("Please ensure all symbol line width entries are numbers",
                     "greater than zero and less than twenty"))
        )

        # For each parameter, rep() until it's length == sp.codes.len,
        #   and check that there aren't more selected than species
        if (cruzDasSightEventResight()) {
          # Requires that only one species is selected
          validate(
            need(length(leg.cex) == 1,
                 "Please provide only one symbol size when plotting resights")
          )
          leg.df <- data.frame(
            SpCode = sp.codes,
            Event = sort(input$das_sighting_events, decreasing = TRUE),#Ensures S, s, or equivalent
            pch = .func_sight_symbol_pt(leg.pch, "type", 1),
            col = .func_sight_symbol_pt(leg.col, "color", 2),
            cex = .func_sight_symbol_pt(c(leg.cex, 0.75 * leg.cex), "size", 2),
            lwd = .func_sight_symbol_pt(leg.lwd, "line width", 1),
            stringsAsFactors = FALSE
          )

        } else {
          sp.codes.len <- length(sp.codes)
          leg.df <- data.frame(
            SpCode = sp.codes,
            pch = .func_sight_symbol_pt(leg.pch, "type", sp.codes.len),
            col = .func_sight_symbol_pt(leg.col, "color", sp.codes.len),
            cex = .func_sight_symbol_pt(leg.cex, "size", sp.codes.len),
            lwd = .func_sight_symbol_pt(leg.lwd, "line width", sp.codes.len),
            stringsAsFactors = FALSE
          )
        }
      }

      if (cruzDasSightEventResight()) {
        pt.df <- left_join(das.sight, leg.df, by = c("SpCode", "Event")) %>%
          select(.data$SpCode, .data$Lon, .data$Lat,
                 .data$pch, .data$col, .data$cex, .data$lwd)
      } else {
        pt.df <- left_join(das.sight, leg.df, by = "SpCode") %>%
          select(.data$SpCode, .data$Lon, .data$Lat,
                 .data$pch, .data$col, .data$cex, .data$lwd)
      }

      # # If specified, make the symbol color correspond to event code
      # if (sight.type %in% c(1, 2) & input$das_symbol_event) {
      #   pt.df
      # }

      list(
        sight.type = sight.type, sp.count = sp.count, sp.codes = sp.codes,
        leg.df = leg.df, pt.df = pt.df, das.sight = das.sight
      )
    })

    .func_sight_symbol_pt <- function(pt.x, pt.txt, sp.codes.len, valid.check = TRUE) {
      if (valid.check)
        validate(
          need(sp.codes.len >= length(pt.x),
               paste("There are more symbol", pt.txt, "entries than species"))
        )

      if (sp.codes.len > length(pt.x)) {
        pt.x <- rep(pt.x, ceiling(sp.codes.len / length(pt.x)))
        pt.x <- pt.x[1:sp.codes.len]
      }
      # else {
      #   pt.x <- pt.x[1:sp.codes.len]
      # }

      pt.x
    }


    # Mammal or turtle symbol properties
    # Plot parameters for plotting selected mammal/turtle species
    cruzDasSightSymbolAnimalSelected <- reactive({
      if (!input$das_symbol_mult) {
        pt.pch <- as.numeric(input$das_symbol_type)
        pt.col <- input$das_symbol_color

      } else {
        pt.pch <- as.numeric(unlist(strsplit(input$das_symbol_type_mult, ",")))
        pt.col <- str_trim(unlist(strsplit(input$das_symbol_color_mult, ",")))
        # pt.pch <- as.numeric(unlist(strsplit(cruz.das.symbol.type(), ",")))
        # pt.col <- str_trim(unlist(strsplit(cruz.das.symbol.color(), ",")))
        validate(
          need(length(pt.pch) > 0, # duplicate of check in main function
               "Please enter at least one value for symbol type"),
          need(all(pt.pch %in% 0:20),
               paste("Not all symbol type entries are valid." ,
                     "Please be sure all entries are a whole number from 0 to 20")),
          need(length(pt.col) > 0,
               "Please enter at least one value for symbol color")
        )

        # Covert color names to color codes-codes established in server file,
        #   keeping them in order
        valid.message <- paste(
          "Not all symbol color entries are valid. Please be sure each entry",
          "matches a color in the Color and Formatting Options page"
        )
        if (input$color_style == 1) {
          validate(need(all(pt.col %in% symbol.col), valid.message))
          pt.col <- vapply(pt.col, function(i) which(symbol.col %in% i), 1)
          pt.col <- symbol.col.code[pt.col]

        } else if (input$color_style == 2) {
          validate(need(all(pt.col %in% symbol.col.gray), valid.message))
          pt.col <- vapply(pt.col, function(i) which(symbol.col.gray %in% i), 1)
          pt.col <- symbol.col.code.gray[pt.col]
        }
      }

      # Same whether input$das_symbol_mult is checked or not
      pt.cex <- as.numeric(unlist(strsplit(input$das_symbol_size, ",")))
      pt.lwd <- as.numeric(unlist(strsplit(input$das_symbol_linewidth, ",")))

      list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
    })

    # Plot parameters for plotting all mammal/turtle species
    cruzDasSightSymbolAnimalAll <- reactive({
      sp.codes.len <- length(cruzDasSightFilter()$sp.codes)
      pch.all <- unname(unlist(cruz.symbol.type))
      col.all <- c(
        "black", "red", "forestgreen", "orange",
        "blue", "tan4", "yellow", "aquamarine2", "bisque1", "hotpink",
        "green", "wheat3", "lightblue", "indianred2", "gray"
      )

      pt.pch <- rep(pch.all, ceiling(sp.codes.len / length(pch.all)))
      pt.pch <- pt.pch[1:sp.codes.len]

      pt.col <- rep(col.all, each = length(pch.all))
      if (length(pt.col) < sp.codes.len) stop("Sight symbol error - report as issue")
      pt.col <- pt.col[1:sp.codes.len]

      pt.cex <- 1
      pt.lwd <- 1

      # browser()
      # TODO: validate/error if
      stopifnot(
        length(pt.pch) == length(unique(cruzDasSightFilter()$das.sight$SpCode)),
        length(pt.col) == length(unique(cruzDasSightFilter()$das.sight$SpCode))
      )

      list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
    })

    # Plot parameters for plotting boats or CPODs
    cruzDasSightSymbolBoat <- reactive({
      pt.pch <- as.numeric(input$das_symbol_type_boat)
      pt.col <- input$das_symbol_color_boat
      pt.cex <- as.numeric(input$das_symbol_size_boat)
      pt.lwd <- as.numeric(input$das_symbol_linewidth_boat)

      list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
    })

    # CPOD symbol properties
    # cruzDasSightSymbolCPOD <- reactive({
    #   pt.pch <- as.numeric(input$das.symbol.type.cpod)
    #   pt.col <- input$das.symbol.color.cpod
    #   pt.cex <- as.numeric(input$das.symbol.size.cpod)
    #   pt.lwd <- as.numeric(input$das.symbol.linewidth.cpod)
    #
    #   list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
    # })


    #---------------------------------------------------------------------------
    ### 8. Sightings legend
    cruzDasSightLegend <- reactive({
      symbol.list <- cruzDasSightSymbol()

      leg.df       <- symbol.list$leg.df
      sight.type   <- symbol.list$sight.type
      sp.codes     <- symbol.list$sp.codes
      sp.codes.len <- length(sp.codes)
      sp.count     <- symbol.list$sp.count
      das.sight    <- symbol.list$das.sight

      font.fam <- font.family.vals[as.numeric(input$sight_legend_font)]

      names.lab <- input$sight_legend_names

      if (sight.type %in% c(3, 4)) {
        leg.lab <- leg.df$SpCode
        if ("5" %in% names.lab) leg.lab <- paste0(leg.lab, ", n = ", sp.count)

      } else {
        sp.codes.all <- cruz.list$sp.codes
        temp.use <- vapply(
          sp.codes, function(i) which(sp.codes.all$SpCode == i), 1,
          USE.NAMES = FALSE
        )

        sp.codes.all.use <- sp.codes.all[temp.use, ]

        # # This piece cuts the common name at the first comma, which results in incorrect names for some codes
        # sp.codes.all.use$Name_Common <- vapply(sp.codes.all.use$Name_Common, function(i) {
        #   unlist(strsplit(i, ","))[1]
        # }, as.character(1))

        leg.lab <- NULL
        if ("1" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$SpCode)
        if ("2" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Abbr)
        if ("3" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$SciName)
        if ("4" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$CommonName)
        validate(
          need(leg.lab, "Please select species information to display in the sighting legend")
        )

        if (cruzDasSightEventResight()) leg.lab <- paste0(leg.lab, ", Event: ", input$das_sighting_events)

        if ("5" %in% names.lab) {
          if (cruzDasSightEventResight()) {
            validate(need(length(unique(das.sight$Event)) <= 2, "Error processing sighting legend"))
            # Primary sighting will always come before resight
            sp.count.res <- c(table(das.sight$Event)[input$das_sighting_events[1]],
                              table(das.sight$Event)[input$das_sighting_events[2]])
            sp.count.res[is.na(sp.count.res)] <- 0
            leg.lab <- paste0(leg.lab, ", n = ", sp.count.res)
            rm(sp.count.res)

          } else {
            leg.lab <- paste0(leg.lab, ", n = ", sp.count)
            # leg.lab <- if (any(names.lab %in% 1:4)) {
            #   paste0(leg.lab, ", n = ", sp.count)
            # } else {
            #   paste0("n = ", sp.count)
            # }
          }
        }
      }

      leg.title <- if (input$sight_legend_title == "") NULL else input$sight_legend_title
      leg.bty <-     ifelse(input$sight_legend_boxCol == 1, "n", "o")
      leg.box.col <- ifelse(input$sight_legend_boxCol == 2, NA, "black")
      leg.box.lwd <- ifelse(input$sight_legend_boxCol == 2, 0, 1)
      leg.box.cex <- input$sight_legend_textSize

      leg.pos <- input$sight_legend_pos
      if (leg.pos == 1) {
        validate(
          need(
            !is.na(input$sight_legend_lat),
            "Please enter a valid legend latitude value"
          ),
          need(
            !is.na(input$sight_legend_lon),
            "Please enter a valid legend longitude value"
          )
        )
        leg.x <- input$sight_legend_lon
        leg.y <- input$sight_legend_lat

      } else {
        leg.x <- leg.pos
        leg.y <- NULL
      }

      list(
        leg.x = leg.x, leg.y = leg.y, leg.lab = leg.lab, leg.title = leg.title,
        leg.pch = leg.df$pch, leg.col = leg.df$col,
        leg.cex = leg.df$cex, leg.lwd = leg.df$lwd,
        leg.bty = leg.bty, leg.box.col = leg.box.col,
        leg.box.lwd = leg.box.lwd, leg.box.cex = leg.box.cex,
        font.fam = font.fam
      )
    })


    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Effort

    observeEvent(input$das_sightings, {
      if (!input$das_sightings) {
        updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
      }
    })

    # Flags for inputs for plotting detailed effort not by Beaufort
    output$das_effort_det_s_flag <- reactive({
      flag <- FALSE
      if (input$das_effort == 3) {
        if (!input$das_effort_det_byBft) {
          if ("S" %in% input$das_effort_snf) flag <- TRUE
        }
      }

      flag
    })
    outputOptions(output, "das_effort_det_s_flag", suspendWhenHidden = FALSE)

    output$das_effort_det_n_flag <- reactive({
      flag <- FALSE
      if (input$das_effort == 3) {
        if (!input$das_effort_det_byBft) {
          if ("N" %in% input$das_effort_snf) flag <- TRUE
        }
      }

      flag
    })
    outputOptions(output, "das_effort_det_n_flag", suspendWhenHidden = FALSE)

    output$das_effort_det_f_flag <- reactive({
      flag <- FALSE
      if (input$das_effort == 3) {
        if (!input$das_effort_det_byBft) {
          if ("F" %in% input$das_effort_snf) flag <- TRUE
        }
      }

      flag
    })
    outputOptions(output, "das_effort_det_f_flag", suspendWhenHidden = FALSE)



    #---------------------------------------------------------------------------
    ### 1. Extract and format simple or detailed effort data from DAS data
    cruzDasEffortEvent <- reactive({
      #----------------------------------------------------------------------------
      req(input$das_effort != 0)
      das.proc <- req(cruz.list$das.data)

      eff.events <- if (input$das_effort == 2) c("R", "E") else c("R", "V", "E")

      das.eff <- das.proc |>
        filter(.data$OnEffort | .data$Event == "E",
               .data$Event %in% eff.events)

      validate(
        need(sum(das.eff$Event == "R") == sum(das.eff$Event == "E"),
             "There are not an equal number of R and E events in the data")
      )
      validate(
        need(all((which(das.eff$Event == "E") - which(das.eff$Event == "R")) > 0),
             "R and E events do not properly alternate"),
        need(identical(tail(das.eff$Event, 1), "E"),
             "The DAS data effort must end with an E event")
      )

      # For simplified effort, we don't need Beaufort values
      # For detailed effort, we remove R to V events; distance should be 0
      # Each continuous effort section will end with an E event
      das.eff.lines <- das.eff |>
        mutate(st_lat = .data$Lat,
               st_lon = .data$Lon,
               end_lat = c(.data$Lat[-1], NA),
               end_lon = c(.data$Lon[-1], NA)) |>
        filter(.data$Event != "E")

      if (input$das_effort == 3)
        das.eff.lines <- das.eff.lines |> filter(Event != "R")


      #------------------------------------------
      # Verbosely remove any effort lines with NA lat/lon and return
      ll.na <- which(
        is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
          is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon)
      )
      if (length(ll.na) > 0) {
        table.out <- das.eff.lines |>
          slice(ll.na) |>
          select("Event", "DateTime", "Lat", "Lon", "OnEffort",
                 "Cruise", "file_das", "line_num") |>
          mutate(DateTime = as.character(.data$DateTime),
                 Cruise = as.character(.data$Cruise))
        txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
        txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

        showModal(modalDialog(
          title = "CruzPlot notice",
          tags$h5("The following effort", txt.out, "had an NA latitude or longitude value.",
                  txt.out2, "will be removed (filtered) and thus not plotted",
                  "or included in tabular output:"),
          tags$br(), tags$br(),
          renderTable(table.out),
          tags$br(),
          tags$h5("This notice will not be shown again unless a new DAS file is loaded"),
          easyClose = FALSE,
          size = "l"
        ))
      }


      # Filter for non-NA lines
      das.eff.lines |>
        filter(!is.na(.data$st_lat), !is.na(.data$end_lat),
               !is.na(.data$st_lon), !is.na(.data$end_lon))
    })


    #---------------------------------------------------------------------------
    ### 2. Take cruzDasEffortEvent, and filter effort data to map range
    cruzDasEffortRange <- reactive({
      das.eff.lines <- cruzDasEffortEvent()

      # Check for any effort lines with NA coordinates
      ll.na <- sum(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$st_lon) |
                     is.na(das.eff.lines$end_lat) | is.na(das.eff.lines$end_lon))
      validate(
        need(ll.na == 0,
             "Error processing effort line positions - please report this as an issue")
      )

      # Adjust longitudes if world2 map is being used
      if (map_range_config()$world2) {
        das.eff.lines <- das.eff.lines |>
          mutate(st_lon = ifelse(.data$st_lon < 0, .data$st_lon + 360, .data$st_lon),
                 end_lon = ifelse(.data$end_lon < 0, .data$end_lon + 360, .data$end_lon))
      }

      # Remove any effort lines with both st and end points outside map range
      lon.range <- map_range_config()$lon.range
      lat.range <- map_range_config()$lat.range

      das.eff.lines.range <- das.eff.lines |>
        filter(between(.data$st_lat, lat.range[1], lat.range[2]),
               between(.data$st_lon, lon.range[1], lon.range[2]),
               between(.data$end_lat, lat.range[1], lat.range[2]),
               between(.data$end_lon, lon.range[1], lon.range[2]))

      validate(
        need(nrow(das.eff.lines.range) > 0,
             "No effort lines are completely within the map boundaries")
      )

      # Return
      das.eff.lines.range
    })


    #---------------------------------------------------------------------------
    ### 3. Individual filters filter for indices of data.effort to keep

    #--------------------------------------------
    .func_eff_filt_validate <- function(x, x.txt) {
      if (anyNA(x)) warning(paste("some", x.txt, "filter values were NA"))

      validate(
        need(any(x), paste("No effort lines within the map range",
                           "match the given", x.txt, "filter"))
      )
      x
    }


    #--------------------------------------------
    ### Closing/passing mode filter
    cruzDasEffortFilterMode <- reactive ({
      das.eff.lines <- cruzDasEffortRange()

      keep <- das.eff.lines$Mode %in% input$das_effort_cp
      .func_eff_filt_validate(keep, "mode (closing/passing)")
    })


    #--------------------------------------------
    ### S/N/F effort type filter
    cruzDasEffortFilterEfftype <- reactive ({
      das.eff.lines <- cruzDasEffortRange()

      keep <- das.eff.lines$EffType %in% input$das_effort_snf
      .func_eff_filt_validate(keep, "effort type (standard/non-standard/fine)")
    })


    #--------------------------------------------
    ### Beaufort filter
    cruzDasEffortFilterBeaufortVal <- reactive({
      # Separate function to be used in Legend
      if (input$das_effort_filter_same) {
        eff.bft.min <- as.numeric(input$das_sight_minBft)
        eff.bft.max <- as.numeric(input$das_sight_maxBft)
      } else {
        eff.bft.min <- as.numeric(input$das_effort_minBft)
        eff.bft.max <- as.numeric(input$das_effort_maxBft)
      }

      validate(
        need(eff.bft.min <= eff.bft.max,
             "Effort filter: minimum Beaufort must be less than or equal to maximum Beaufort")
      )

      c(eff.bft.min, eff.bft.max)
    })

    cruzDasEffortFilterBeaufort <- reactive ({
      das.eff.lines <- cruzDasEffortRange()
      bft.vals <- cruzDasEffortFilterBeaufortVal()

      keep <- if (identical(bft.vals, c(0, 9))) {
        TRUE
      } else {
        between(das.eff.lines$Bft, bft.vals[1], bft.vals[2])
      }
      .func_eff_filt_validate(keep, "Beaufort")
    })


    #--------------------------------------------
    ### Date Filter
    cruzDasEffortFilterDate <- reactive({
      das.eff.lines <- cruzDasEffortRange()

      eff.date.vals <- if (input$das_effort_filter_same) {
        input$das_sight_dateRange
      } else {
        input$das_effort_dateRange
      }

      validate(
        need(eff.date.vals[1] <= eff.date.vals[2],
             "Effort filter: minimum date must be less than or equal to maximum date")
      )

      keep <- between(
        as.Date(das.eff.lines$DateTime), eff.date.vals[1], eff.date.vals[2]
      )
      .func_eff_filt_validate(keep, "date")
    })


    #--------------------------------------------
    ### Cruise number filter
    cruzDasEffortFilterCruise <- reactive ({
      das.eff.lines <- cruzDasEffortRange()

      if (input$das_effort_filter_same) {
        if (is.null(input$das_sight_cruiseNum)) {
          TRUE
        } else {
          eff.cruise.vals <- as.numeric(input$das_sight_cruise)
          keep <- das.eff.lines$Cruise %in% eff.cruise.vals
          .func_eff_filt_validate(keep, "cruise number") }

      } else {
        if (is.null(input$das_effort_cruise)) {
          TRUE
        } else {
          eff.cruise.vals <- as.numeric(input$das_effort_cruise)
          keep <- das.eff.lines$Cruise %in% eff.cruise.vals
          .func_eff_filt_validate(keep, "cruise number")}
      }
    })


    #---------------------------------------------------------------------------
    ### 4. Pull together filters, and filter the effort data
    cruzDasEffortFilter <- reactive({
      das.eff.lines <- cruzDasEffortRange()

      if (input$das_effort_filter_same) {
        validate(
          need(input$das_sightings,
               "Sightings must be plotted to use the sighting filters for effort lines")
        )
      }

      ### Collect logical vectors
      keep1 <- cruzDasEffortFilterMode()
      keep2 <- cruzDasEffortFilterEfftype()
      keep3 <- if (input$das_effort == 3) cruzDasEffortFilterBeaufort() else TRUE
      keep4 <- cruzDasEffortFilterDate()
      keep5 <- cruzDasEffortFilterCruise()

      keep.all <- keep1 & keep2 & keep3 & keep4 & keep5
      keep.all.na <- which(is.na(keep.all))

      ### Show modal if any filter values are NA
      # Can be this simple b/c (NA & F) output is (F)
      if (length(keep.all.na) > 0) {
        table.out <- das.eff.lines |>
          slice(keep.all.na) |>
          mutate(DateTime = as.character(.data$DateTime),
                 Cruise = as.character(.data$Cruise)) |>
          select("Event", "DateTime", "OnEffort",
                 "Cruise", "Mode", "EffType", "Bft",
                 # File = file_das, #Can take up too much space
                 `Line number` = "line_num")
        if (input$das_effort != 3) table.out <- table.out |> select("Bft")

        txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
        txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

        showModal(modalDialog(
          title = "CruzPlot notice",
          tags$h5("The following effort", txt.out, "at least one NA filter value.",
                  txt.out2, "will be removed (filtered) and thus not plotted",
                  "or included in tabular output:"),
          tags$br(), tags$br(),
          renderTable(table.out),
          easyClose = TRUE,
          footer = "Click anywhere or press any button to close this notice",
          size = "l"
        ))
      }

      keep.all[is.na(keep.all)] <- FALSE
      das.eff.lines.filt <- das.eff.lines[keep.all, ]

      ### Final checks and return
      validate(
        need(sum(is.na(das.eff.lines.filt$Event)) == 0,
             "Error in CruzPlot effort filtering - please report this as an issue")
      )
      validate(need(nrow(das.eff.lines.filt) > 0, "No effort lines match the provided filters"))

      das.eff.lines.filt
    })


    #---------------------------------------------------------------------------
    ### 5. Pull together effort plotting params
    cruzDasEffortParams <- reactive({
      if (input$das_effort == 2) {
        ## If simplified effort, simple results
        eff.col <- input$das_effort_simp_col
        eff.lwd <- input$das_effort_simp_lwd

      } else if (input$das_effort == 3) {
        ## If detailed effort, not as simple
        das.eff.lines <- cruzDasEffortFilter()

        # Use Beaufort or effort type values to generate colors
        if (input$das_effort_det_byBft) {
          bft.cols <- input$das_effort_det_bft_col
          bft.col.num <- cruzDasEffortFilterBeaufortVal()[2] + 1
          validate(
            need(length(bft.cols) >= bft.col.num,
                 paste("Please choose at least", bft.col.num,
                       "colors, one for each possible Beaufort value between 0",
                       "and the specified maximum Beaufort"))
          )

          eff.col <- bft.cols[das.eff.lines$Bft + 1]
          eff.lwd <- input$das_effort_det_bft_lwd

        } else {
          eff.col <- case_when(
            das.eff.lines$EffType == "S" ~ input$das_effort_det_col_s,
            das.eff.lines$EffType == "N" ~ input$das_effort_det_col_n,
            das.eff.lines$EffType == "F" ~ input$das_effort_det_col_f
          )
          eff.lwd <- case_when(
            das.eff.lines$EffType == "S" ~ input$das_effort_det_lwd_s,
            das.eff.lines$EffType == "N" ~ input$das_effort_det_lwd_n,
            das.eff.lines$EffType == "F" ~ input$das_effort_det_lwd_f
          )
        }
      }

      list(eff.col = eff.col, eff.lwd = eff.lwd)
    })


    #--------------------------------------------------------------------------
    ### 6. Effort legend
    # Turn off effort legend when switching to Simplified Effort
    observeEvent(input$das_effort, {
      if (input$das_effort == 2) {
        updateCheckboxInput(session, "eff_legend", value = FALSE)
        cruz.eff.leg(FALSE)

        if (input$eff_legend_title == "Effort by Beaufort") {
          updateTextInput(session, "eff_legend_title", value = "")
          cruz.eff.leg.title("")
        }
      }

      if (input$das_effort == 3) {
        updateCheckboxInput(session, "eff_legend", value = TRUE)
        cruz.eff.leg(TRUE)

        if (input$eff_legend_title == "") {
          updateTextInput(session, "eff_legend_title", value = "Effort by Beaufort")
          cruz.eff.leg.title("Effort by Beaufort")
        }
      }
    })

    # Use reactiveVals so that map doesn't have to plot twice due to update
    observeEvent(input$eff_legend, {
      cruz.eff.leg(input$eff_legend)
    })

    observeEvent(input$eff_legend_title, {
      cruz.eff.leg.title(input$eff_legend_title)
    })


    # Get and return parameters for effort legend
    cruzDasEffortLegend <- reactive({
      req(input$das_effort != 1)

      ### General parameters, set in Legends section
      eff.leg.pos <- input$eff_legend_pos
      if (eff.leg.pos == 1) {
        validate(
          need(
            !is.na(input$eff_legend_lat),
            "Please enter a valid effort legend latitude value"
          ),
          need(
            !is.na(input$eff_legend_lon),
            "Please enter a valid effort legend longitude value"
          )
        )
        eff.leg.x = input$eff_legend_lon
        eff.leg.y = input$eff_legend_lat

      } else {
        eff.leg.x <- eff.leg.pos
        eff.leg.y <- NULL
      }

      font.fam <- font.family.vals[as.numeric(input$eff_legend_font)]

      # eff.leg.title <- if (input$eff_legend_title == "") NULL else input$eff_legend_title
      eff.leg.title <- if (cruz.eff.leg.title() == "") NULL else cruz.eff.leg.title()

      eff.leg.bty <- ifelse(input$eff_legend_boxCol == 1, "n", "o")
      eff.leg.box.col <- ifelse(input$eff_legend_boxCol == 2, NA, "black")
      eff.leg.box.lwd <- ifelse(input$eff_legend_boxCol == 2, 0, 1)
      eff.leg.box.cex <- input$eff_legend_textSize


      ### Parameters that are effort-type specific
      if (input$das_effort == 2) {
        # Simplified effort
        eff.leg.lab <- "Simplified effort"
        eff.leg.col <- input$das_effort_simp_col
        eff.leg.lwd <- input$das_effort_simp_lwd

      } else if (input$das_effort == 3) {
        # Detailed effort
        if (input$das_effort_det_byBft) {
          # Detailed effort - plot by Bft
          bft.range <- cruzDasEffortFilterBeaufortVal()
          bft.which <- (bft.range[1]:bft.range[2]) + 1
          validate(
            need(length(bft.which) <= 10, "Beaufort legend error 1"),
            need(length(bft.which) <= length(input$das_effort_det_bft_col),
                 "Beaufort legend error 2")
          )

          eff.leg.lab <- (0:9)[bft.which]
          eff.leg.col <- input$das_effort_det_bft_col[bft.which]
          eff.leg.lwd <- input$das_effort_det_bft_lwd

        } else {
          # Detailed effort - plot by S/N/F
          snf.idx <- which(c("S", "N", "F") %in% req(input$das_effort_snf))
          eff.leg.lab <- c("Standard", "Non-standard", "Fine")[snf.idx]
          eff.leg.col <- c(
            input$das_effort_det_col_s,
            input$das_effort_det_col_n,
            input$das_effort_det_col_f
          )[snf.idx]
          eff.leg.lwd <- c(
            input$das_effort_det_lwd_s,
            input$das_effort_det_lwd_n,
            input$das_effort_det_lwd_f
          )[snf.idx]
        }
      }


      ### Return list
      list(
        eff.leg.x = eff.leg.x, eff.leg.y = eff.leg.y,
        eff.leg.title = eff.leg.title, eff.leg.lab = eff.leg.lab,
        eff.leg.col = eff.leg.col, eff.leg.lwd = eff.leg.lwd,
        eff.leg.bty = eff.leg.bty, eff.leg.box.col = eff.leg.box.col,
        eff.leg.box.lwd = eff.leg.box.lwd,
        eff.leg.box.cex = eff.leg.box.cex, font.fam = font.fam
      )
    })


    ###############################################################################
    ### Save values
    to_save <- reactive({
      list(
        save_widget("sp.codes", "reactive", cruz.list$sp.codes),
        save_widget("sp.codes.name", "reactive", cruz.list$sp.codes.name),
        save_widget("das.data", "reactive", cruz.list$das.data),
        save_widget("das_sightings", "check"),
        save_widget("das_sightings_position", "radio"),
        save_widget("das_sighting_type", "select"),
        save_widget("das_sighting_code_1_all", "radio"),
        save_widget("das_sighting_code_1", "select"),
        save_widget("das_sighting_probable", "check"),
        save_widget("das_sighting_events", "checkGroup"),
        save_widget("das_sighting_code_2_all", "radio"),
        save_widget("das_sighting_code_2", "select"),
        save_widget("das_symbol_type", "select"),
        save_widget("das_symbol_color", "select"),
        save_widget("das_symbol_type_mult", "text"),
        save_widget("das_symbol_color_mult", "text"),
        save_widget("das_symbol_size", "text"),
        save_widget("das_symbol_linewidth", "text"),
        save_widget("das_symbol_mult", "check"),
        save_widget("das_symbol_type_boat", "select"),
        save_widget("das_symbol_size_boat", "numeric"),
        save_widget("das_symbol_color_boat", "select"),
        save_widget("das_symbol_linewidth_boat", "numeric"),
        save_widget("das_sight_interactive", "radio"),

        save_widget("das_sight_effort", "radio"),
        save_widget("das_sight_cp", "checkGroup"),
        save_widget("das_sight_snf", "checkGroup"),
        save_widget("das_sight_dateRange", "dateRange"),
        save_widget("das_sight_cruise", "select"),
        save_widget("das_sight_minBft", "select"),
        save_widget("das_sight_maxBft", "select"),
        save_widget("das_sight_trunc_units", "radio"),
        save_widget("das_sight_trunc", "numeric"),

        save_widget("das_effort", "radio"),
        save_widget("das_effort_cp", "checkGroup"),
        save_widget("das_effort_snf", "checkGroup"),
        save_widget("das_effort_simp_col", "select"),
        save_widget("das_effort_simp_lwd", "numeric"),
        save_widget("das_effort_det_byBft", "check"),
        save_widget("das_effort_det_bft_col", "select"),
        save_widget("das_effort_det_bft_lwd", "numeric"),
        save_widget("das_effort_det_col_s", "select"),
        save_widget("das_effort_det_lwd_s", "numeric"),
        save_widget("das_effort_det_col_n", "select"),
        save_widget("das_effort_det_lwd_n", "numeric"),
        save_widget("das_effort_det_col_f", "select"),
        save_widget("das_effort_det_lwd_f", "numeric"),
        save_widget("das_effort_filter_same", "check"),
        save_widget("das_effort_cruise", "select"),
        save_widget("das_effort_dateRange", "dateRange"),
        save_widget("das_effort_minBft", "select"),
        save_widget("das_effort_maxBft", "select"),
        save_widget("das_effort_interactive", "radio"),

        save_widget("das_legend", "check"),
        save_widget("sight_legend_pos", "select"),
        save_widget("sight_legend_lon", "numeric"),
        save_widget("sight_legend_lat", "numeric"),
        save_widget("sight_legend_boxCol", "select"),
        save_widget("sight_legend_font", "select"),
        save_widget("sight_legend_textSize", "numeric"),
        save_widget("sight_legend_title", "text"),
        save_widget("sight_legend_names", "checkGroup"),

        save_widget("eff_legend", "check"),
        save_widget("eff_legend_pos", "select"),
        save_widget("eff_legend_title", "text"),
        save_widget("eff_legend_font", "select"),
        save_widget("eff_legend_textSize", "numeric"),
        save_widget("eff_legend_lon", "numeric"),
        save_widget("eff_legend_lat", "numeric"),
        save_widget("eff_legend_boxCol", "select")


        # save_widget("ndas_plot", "check")
      )
    })

    ### Return values
    list(
      to_save = to_save,
      das_data = reactive(cruz.list$das.data),
      sp_codes = reactive(cruz.list$sp.codes),
      effort = list(
        plot = reactive(isTruthy(cruz.list$das.data) && input$das_effort != "1"),
        data = cruzDasEffortFilter,
        params = cruzDasEffortParams,
        legend_plot = reactive(input$eff_legend),
        legend = cruzDasEffortLegend
      ),
      sight = list(
        plot = reactive(input$das_sightings),
        # data = cruzDasSightFilter,
        data = cruzDasSightSymbol,
        symbol = cruzDasSightSymbol,
        legend_plot = reactive(input$das_legend),
        legend = cruzDasSightLegend
      )
    )
  })
}
