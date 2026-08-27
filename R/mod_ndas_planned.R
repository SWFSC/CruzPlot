#' Planned transect module
#'
#' Shiny module for planned transects
#'
#' @name mod_ndas_planned
#'
#' @inheritParams mod_map_color
#'
#' @details
#' This module allows users to load non-DAS planned transect files.
#' See the CruzPlot manual for planned transect CSV format requirements.
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#'
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file.
#'   See [cruzplot_gui()] for more info.
#' - `pltransect`: a reactive of a list with the planned transect data to plot.
#'   `NULL` if there are no planned transects to plot
#' - `pltransect_class2` a reactive of a boolean indicating if
#'   the loaded planned transects have any 'class2' info.
#'
#' @export
mod_ndas_planned_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Planned Transects",
    fluidRow(
      cruz_box(
        title = "Load planned transects", width = 12,
        fluidRow(
          column(
            width = 6,
            helpText(
              "Longitudes must be in -180 to 180 range.",
              "See the manual for the required CSV file format"
            )
          ),
          column(
            width = 6,
            fileInput(ns("file"), tags$h5("Load CSV file"), accept = ".csv")
          )
        ),
        fluidRow(
          column(3, uiOutput(ns("lon_uiOut_select"))),
          column(3, uiOutput(ns("lat_uiOut_select"))),
          column(3, uiOutput(ns("num_uiOut_select"))),
          column(3, uiOutput(ns("class1_uiOut_select")))
        ),
        fluidRow(
          column(3, uiOutput(ns("class2_uiOut_select"))),
          column(3, offset = 1, tags$br(), tags$br(), uiOutput(ns("execute_uiOut_button"))),
          column(5, tags$br(), tags$br(), textOutput(ns("text")))
        ),
        tags$span(textOutput(ns("message")), style = "color: blue;"),
      ),
      conditionalPanel(
        condition = "output.cruzMapPlannedTransects_Conditional", ns = ns,
        cruz_box(
          title = "Plot loaded planned transects", width = 12,
          checkboxInput(ns("plot"), "Plot planned transect lines", value = FALSE),
          conditionalPanel(
            condition = "input.plot", ns = ns,
            column(
              width = 12,
              helpText(
                "For the color(s) and (if a class 2 column is specified) the line type(s),",
                "select either one or the same number as transect classes or class 2s, respectively.",
                "When multiple colors or line types are selected,",
                "the order in which transect classes and class 2s are selected to be plotted",
                "corresponds to order of specified colors and line types, respectively."
              )
            ),
            box(
              width = 12,
              ui_select_instructions(),
              fluidRow(
                column(6, uiOutput(ns("toplot_uiOut_select"))),
                column(6, uiOutput(ns("color_uiOut_select")))
              ),
              fluidRow(
                column(4, uiOutput(ns("toplot2_uiOut_select"))),
                column(4, uiOutput(ns("lty_uiOut_select"))),
                column(4, numericInput(ns("lwd"), tags$h5("Line width"),
                                       value = 1, min = 0, step = 1))
              )
            )
          )
        )
        # box(
        #   width = 12,
        #   tags$strong("Remove loaded planned transects"),
        #   uiOutput("toremove_uiOut_select"),
        #   uiOutput("toremove_execute_uiOut_button"),
        #   textOutput("remove_text")
        # )
        # )
        # )
      )
    )
  )
}


#' @name mod_ndas_planned
#' @export
mod_ndas_planned_server  <- function(id, load_state, map_range_config) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state),
      is.reactive(map_range_config)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
      toplot = NULL,
      toplot2 = NULL,
      color = NULL,
      lty = NULL,
      planned.transects = NULL
    )

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
    output$text <- renderText(planned_transects())
    # output$remove_text <- renderText(remove())

    output$message <- renderText({
      req(cruz.list$planned.transects)
      "A planned transects file is loaded"
    })


    ###############################################################################
    ###############################################################################
    # Indicator for if any planned transects are loaded

    ### Conditional flag for UI code
    output$cruzMapPlannedTransects_Conditional <- reactive({
      isTruthy(cruz.list$planned.transects)
    })
    outputOptions(output, "cruzMapPlannedTransects_Conditional", suspendWhenHidden = FALSE)


    ### Turn plot checkbox on if planned transects are added
    observe({
      if (isTruthy(cruz.list$planned.transects))
        updateCheckboxInput(session, "plot", value = TRUE)
    })

    ### Turn plot checkbox off if all planned transects are removed
    observe({
      if (is.null(cruz.list$planned.transects))
        updateCheckboxInput(session, "plot", value = FALSE)
    })

    ###############################################################################
    ###############################################################################
    # renderUI()s for loading planned transects

    #----------------------------------------------------------
    ### Get names from loaded csv file
    file_names <- reactive({
      req(read_csv())

      csv.names <- names(read_csv()[[2]])
      choices.list <- seq_along(csv.names)
      names(choices.list) <- csv.names

      choices.list
    })

    ### Create ui inputs for selecting lon/lat columns
    output$lon_uiOut_select <- renderUI({
      selectInput(
        session$ns("lon"), h5("Longitude column"),
        choices = file_names(), selected = 1
      )
    })

    output$lat_uiOut_select <- renderUI({
      selectInput(
        session$ns("lat"), h5("Latitude column"),
        choices = file_names(), selected = 2
      )
    })

    ### Widgets for transect numbers
    output$num_uiOut_select<- renderUI({
      selectInput(
        session$ns("num"), h5("Transect number column"),
        choices = file_names(), selected = 3
      )
    })

    ### Widget for transect classes 1
    output$class1_uiOut_select<- renderUI({
      selectInput(
        session$ns("class1"), h5("Transect class column"),
        choices = file_names(), selected = 4
      )
    })

    ### Widget for transect classes 2
    output$class2_uiOut_select<- renderUI({
      choices.list <- file_names()
      choices.list <- c("N/A - No class 2 info" = 0, choices.list)

      selectInput(
        session$ns("class2"), h5("Transect class 2 column"),
        choices = choices.list, selected = 0
      )
    })


    #----------------------------------------------------------
    ### Button to add selected transect data to CruzPlot
    output$execute_uiOut_button <- renderUI({
      req(read_csv())

      actionButton(session$ns("execute"), "Add data to CruzPlot")
    })


    ###############################################################################
    ###############################################################################
    # Process transect file and data and output widget to select ones to plot

    ### Read csv file
    read_csv <- reactive({
      req(input$file)

      file.all <- input$file
      file.name <- file.all$name
      file.data <- try(read.csv(file.all$datapath, stringsAsFactors = FALSE),
                       silent = TRUE)

      validate(
        need(file.data, "Error loading planned transects CSV")
      )

      list(file.name, file.data)
    })


    ### Add transect data to reactiveValue
    planned_transects <- eventReactive(input$execute, {
      validate(
        need(input$lon != input$lat,
             "Error: The longitude column cannot be the same as the latitude column")
      )

      x <- read_csv()[[2]] |>
        dplyr::select(lon = as.numeric(input$lon),
                      lat = as.numeric(input$lat),
                      num = as.numeric(input$num),
                      class1 = as.numeric(input$class1))

      validate(
        need(all(dplyr::between(x$lon, -180, 180)),
             "Error: Planned transect longitude data must be in range [-180, 180]"),
        need(all(dplyr::between(x$lat, -90, 90)),
             "Error: Planned transect latitude data must be in range [-90, 90]"),
        need(!anyNA(x$num),
             "Error: Planned transect 'number' column cannot have any NA values"),
        need(!anyNA(x$class1),
             "Error: Planned transect 'class' column cannot have any NA values")
      )

      if (as.numeric(input$class2) != 0) {
        x <- cbind(
          x, dplyr::select(read_csv()[[2]],
                           class2 = as.numeric(input$class2))
        )
        validate(
          need(!anyNA(x$class2),
               "Error: Planned transect 'class 2' column cannot have any NA values")
        )
      } else {
        x <- cbind(x, class2 = NA)
      }

      cruz.list$planned.transects <- x

      ""
    })


    ###############################################################################
    ###############################################################################
    # Processing loaded planned transects

    class1 <- reactive({
      unique(cruz.list$planned.transects$class1)
    })

    class2 <- reactive({
      unique(cruz.list$planned.transects$class2)
    })


    ###############################################################################
    ### Widgets for selecting planned transect class(es) to plot and their color
    output$toplot_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      choices.list.names <- class1()
      choices.list <- seq_along(choices.list.names)
      names(choices.list) <- choices.list.names

      isolate({
        choices.sel <- if (isTruthy(cruz.list$toplot)) {
          cruz.list$toplot
        } else {
          choices.list
        }
        cruz.list$toplot <- NULL
      })

      selectInput(
        session$ns("toplot"),
        tags$h5("Class(es) to plot"),
        choices = choices.list,
        selected = choices.sel,
        multiple = TRUE
      )
    })
    outputOptions(output, "toplot_uiOut_select", suspendWhenHidden = FALSE)


    output$color_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      isolate({
        choices.sel <- if (isTruthy(cruz.list$color)) {
          cruz.list$color
        } else {
          "gray"
        }
        cruz.list$color <- NULL
      })

      selectInput(
        session$ns("color"),
        tags$h5("Color(s)"),
        choices = cruz.palette.color,
        selected = choices.sel,
        multiple = TRUE
      )
    })
    outputOptions(output, "color_uiOut_select", suspendWhenHidden = FALSE)


    #----------------------------------------------------------
    ### Widgets for selecting planned transect class 2(s) to plot and their lty
    output$toplot2_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      y <- class2()

      if (anyNA(y)) {
        helpText("No class 2 column was selected, and thus you can only specify",
                 "a single line type for all planned transects")

      } else {
        choices.list.names <- y
        choices.list <- seq_along(choices.list.names)
        names(choices.list) <- choices.list.names

        isolate({
          choices.sel <- if (isTruthy(cruz.list$toplot2)) {
            cruz.list$toplot2
          } else {
            choices.list
          }
          cruz.list$toplot2 <- NULL
        })

        selectInput(
          session$ns("toplot2"),
          tags$h5("Class 2(s) to plot"),
          choices = choices.list,
          selected = choices.sel,
          multiple = TRUE
        )
      }
    })
    outputOptions(output, "toplot2_uiOut_select", suspendWhenHidden = FALSE)

    # output$lty_uiOut_message <- renderUI({
    #   req(cruz.list$planned.transects, input$plot)
    #
    #   if (!anyNA(class2())) {
    #     helpText("Select either one line type or the same number of line types as transect class 2s.",
    #              "When multiple line types are selected, the order in which transect class 2(s) are",
    #              "selected to be plotted corresponds to order of specified line type(s).")
    #   } else {
    #     NULL
    #   }
    # })

    output$lty_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      input.lab <- ifelse(
        anyNA(class2()), "Line type", "Line type(s)"
      )

      isolate({
        choices.sel <- if (isTruthy(cruz.list$lty)) {
          cruz.list$lty
        } else {
          1
        }
        cruz.list$lty <- NULL
      })

      selectInput(
        session$ns("lty"),
        tags$h5(input.lab),
        choices = cruz.line.type,
        selected = choices.sel,
        multiple = !anyNA(class2())
      )
    })
    outputOptions(output, "lty_uiOut_select", suspendWhenHidden = FALSE)

    ###############################################################################
    # Removing loaded transects
    #   Currently only allows one transect file to be loaded

    # ### Widget for selecting planned transect(s) to remove
    # output$toremove_uiOut_select <- renderUI({
    #   req(cruz.list$planned.transects)
    #
    #   choices.list.names <- class1()
    #   choices.list <- seq_along(choices.list.names)
    #   names(choices.list) <- choices.list.names
    #
    #   selectInput("toremove",
    #                  tags$h5("Select planned transect class(es) to remove"),
    #                  choices = choices.list, multiple = TRUE)
    # })
    #
    # output$toremove_execute_uiOut_button <- renderUI({
    #   req(cruz.list$planned.transects)
    #
    #   actionButton("toremove_execute", "Remove")
    # })
    #
    #
    # ### Remove selected transects
    # remove <- eventReactive(input$toremove_execute, {
    #   req(cruz.list$planned.transects)
    #   y <- as.numeric(input$toremove)
    #
    #   validate(
    #     need(length(y) != 0,
    #          "Please select at least one set of transects to remove")
    #   )
    #
    #   x <- cruz.list$planned.transects |>
    #     filter(!(class1 %in% class1()[y]))
    #
    #   if (nrow(x) == 0) {
    #     cruz.list$planned.transects <- NULL
    #   } else {
    #     cruz.list$planned.transects <- x
    #   }
    #
    #   "Planned transects removed"
    # })


    ###############################################################################
    ###############################################################################
    pltransect <- reactive({
      if (input$plot) {
        pltransect_list()
      } else {
        NULL
      }
    })

    pltransect_list <- reactive({
      validate(
        need(input$toplot,
             "Please select at least one class of planned transects to plot")
      )

      #So that renderUI()'s can catch up
      req(input$color, input$lty)

      # browser()

      # Get user inputs
      pltrans <- cruz.list$planned.transects
      pltrans.which <- as.numeric(input$toplot)
      pltrans.which2 <- as.numeric(input$toplot2)
      pltrans.colors <- input$color
      pltrans.lty <- as.numeric(input$lty)
      pltrans.lwd <- input$lwd

      # Process user inputs
      if (length(pltrans.colors) == 1) {
        pltrans.colors <- rep(pltrans.colors, length(pltrans.which))
      }

      validate(
        need(length(pltrans.colors) == length(pltrans.which),
             paste("The number of selected planned transect colors must either be",
                   "1 or equal to than the number of selected planned transects"))
      )

      pltrans.class1 <- class1()[pltrans.which]
      names(pltrans.colors) <- pltrans.class1

      # Filter for class 1 selections, and do world2 conversion
      world2 <- map_range_config()$world2
      pltrans <- pltrans |>
        dplyr::filter(.data$class1 %in% pltrans.class1) |>
        mutate(lon = ifelse(world2 & .data$lon < 0, .data$lon + 360, .data$lon))

      if (pltransect_class2()) {
        # Class 2 was specified
        validate(
          need(pltrans.which2,
               "Please select at least one class 2 type to plot")
        )

        pltrans.class2 <- class2()[pltrans.which2]
        pltrans <- dplyr::filter(pltrans, .data$class2 %in% pltrans.class2)

        if (length(pltrans.lty) == 1) {
          pltrans.lty <- rep(pltrans.lty, length(pltrans.class2))
        }
        validate(
          need(length(pltrans.lty) == length(pltrans.class2),
               paste("The number of selecetd planned transect line types must either be",
                     "1 or equal to than the number unique class 2 values"))
        )
        names(pltrans.lty) <- pltrans.class2

        pltrans.list <- lapply(pltrans.class1, function(i) {
          x <- dplyr::filter(pltrans, .data$class1 == i)
          lapply(pltrans.class2, function(j) {
            x <- dplyr::filter(x, .data$class2 == j)
            lapply(unique(x$num), function(k) {
              x <- dplyr::filter(x, .data$num == k)
              if (nrow(x) == 0) {
                NULL
              } else if (nrow(x) == 1){
                validate(need(FALSE, "Error in planned transect processing"))
              } else {
                list(
                  x$lon,
                  x$lat,
                  unname(pltrans.colors[as.character(i)]),
                  unname(pltrans.lty[as.character(j)]),
                  pltrans.lwd
                )
              }
            })
          })
        })

      } else {
        # Class 2 was not specified
        pltrans.list <- lapply(pltrans.class1, function(i) {
          x <- dplyr::filter(pltrans, .data$class1 == i)
          lapply(unique(x$num), function(k) {
            x <- dplyr::filter(x, .data$num == k)
            if (nrow(x) == 0) {
              NULL
            } else if (nrow(x) == 1){
              validate(need(FALSE, "Error in planned transect processing"))
            } else {
              list(
                x$lon,
                x$lat,
                unname(pltrans.colors[as.character(i)]),
                pltrans.lty,
                pltrans.lwd
              )
            }
          })
        })
      }

      pltrans.list
    })

    # Do the planned transect have class2 info?
    pltransect_class2 <- reactive({
      !anyNA(class2())
    })

    ###############################################################################
    ###############################################################################
    ### Save values
    to_save <- reactive({
      list(
        save_widget("plot", "check"),
        save_widget("toplot", "select"),
        save_widget("toplot2", "select"),
        save_widget("color", "select"),
        save_widget("lty", "select"),
        save_widget("lwd", "numeric"),
        save_widget("planned.transects", "reactive", cruz.list$planned.transects)
      )
    })

    ### Return values
    list(
      to_save = to_save,
      pltransect = pltransect,
      pltransect_class2 = pltransect_class2
    )
  })
}
