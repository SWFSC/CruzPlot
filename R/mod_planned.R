#' Planned transect module
#'
#' Shiny module for planned transects
#'
#' @name mod_planned
#'
#' @inheritParams mod_map_range
#' 
#' @details
#' This module handles...
#'
#' @returns The UI function returns a [shiny::tabPanel()] object
#' 
#' The server function returns a list with the following named elements:
#' - `to_save`: a list of values to be saved in an 'app state' file. 
#'   See [cruzplot_gui()] for more info. 
#' - `todo`: ...
#' 
#' @export
mod_planned_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = "Planned Transects", 
    fluidRow(
      cruz_box(
        title = "Load planned transects", width = 12, 
        # fluidRow(
          # box(
            # width = 12,
            # tags$strong("Load planned transects"),
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
                fileInput(ns("planned_transects_file"), tags$h5("Load CSV file"), accept = ".csv")
              )
            ), 
            fluidRow(
              column(3, uiOutput(ns("planned_transects_lon_uiOut_select"))),
              column(3, uiOutput(ns("planned_transects_lat_uiOut_select"))),
              column(3, uiOutput(ns("planned_transects_num_uiOut_select"))),
              column(3, uiOutput(ns("planned_transects_class1_uiOut_select")))
            ),
            fluidRow(
              column(3, uiOutput(ns("planned_transects_class2_uiOut_select"))),
              column(3, offset = 1, tags$br(), tags$br(), uiOutput(ns("planned_transects_execute_uiOut_button"))),
              column(5, tags$br(), tags$br(), textOutput(ns("planned_transects_text")))
            ),
            tags$span(textOutput(ns("planned_transects_message")), style = "color: blue;"), 
          ),
          conditionalPanel(
            condition = "output.cruzMapPlannedTransects_Conditional", ns = ns, 
            # box(
            #   width = 12,
            #   tags$strong("Plot loaded planned transects"),
            cruz_box(
              title = "Plot loaded planned transects", width = 12, 
              checkboxInput(ns("planned_transects_plot"), "Plot planned transect lines", value = FALSE),
              conditionalPanel(
                condition = "input.planned_transects_plot", ns = ns, 
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
                    column(6, uiOutput(ns("planned_transects_toplot_uiOut_select"))),
                    column(6, uiOutput(ns("planned_transects_color_uiOut_select")))
                  ),
                  fluidRow(
                    column(4, uiOutput(ns("planned_transects_toplot2_uiOut_select"))),
                    column(4, uiOutput(ns("planned_transects_lty_uiOut_select"))),
                    column(4, numericInput(ns("planned_transects_lwd"), tags$h5("Line width"),
                                            value = 1, min = 0, step = 1))
                  )
                )
              )
            )
            # box(
            #   width = 12,
            #   tags$strong("Remove loaded planned transects"),
            #   uiOutput("planned_transects_toremove_uiOut_select"),
            #   uiOutput("planned_transects_toremove_execute_uiOut_button"),
            #   textOutput("planned_transects_remove_text")
            # )
          # )
        # )
      )
    )
  )
}


#' @name mod_planned
#' @export
mod_planned_server  <- function(id, load_state) {
  moduleServer(id, function(input, output, session) {
    stopifnot(
      is.reactive(load_state)
    )

    # Stored reactiveValues for the module
    cruz.list <- reactiveValues(
      planned.transects = NULL
    )

    cruz.pt.load.toplot <- reactiveVal(NULL)
    cruz.pt.load.toplot2 <- reactiveVal(NULL)
    cruz.pt.load.color <- reactiveVal(NULL)
    cruz.pt.load.lty <- reactiveVal(NULL)
    cruz.pt.load.tabs <- reactiveVal(FALSE)
    cruz.pt.load.tabset1 <- reactiveVal(FALSE)

    # if (input.save$planned_transects_plot) {
    #   cruz.pt.load.toplot(input.save$planned_transects_toplot)
    #   cruz.pt.load.toplot2(input.save$planned_transects_toplot2)
    #   cruz.pt.load.color(input.save$planned_transects_color)
    #   cruz.pt.load.lty(input.save$planned_transects_lty)
    #   if (!(input$tabs == "createmap" & input$tabset1 == "planned_transects")) {
    #     cruz.pt.load.tabs(input$tabs)
    #     cruz.pt.load.tabset1(input$tabset1)
    #     updateTabItems(session, "tabs", selected = "createmap")
    #     updateTabsetPanel(session, "tabset1", selected = "planned_transects")
    #   }
    # }

    # # Reset selected tab panel after switching to planned transet if needed
    # observe({
    #   input$tabs
    #   input$tabset1

    #   isolate({
    #     if (isTruthy(cruz.pt.load.tabset1()) & isTruthy(cruz.pt.load.tabs())) {
    #       updateTabsetPanel(session, "tabset1", selected = cruz.pt.load.tabset1())
    #       updateTabItems(session, "tabs", selected = cruz.pt.load.tabs())
    #     }
    #     cruz.pt.load.tabset1(NULL)
    #     cruz.pt.load.tabs(NULL)
    #   })
    # })



    # Load state
    observeEvent(load_state(), {
      for (item in load_state()) {
        if (item$type == "reactive") {
          cruz.list[[item$id]] <- item$value
        } else {
          update_widget(item, session)
          # if (item$id == "planned_transects_toplot") {
          #   browser()
          #   cruz.pt.load.toplot(input.save$planned_transects_toplot)
          # }
          # if (item$id == "planned_transects_toplot2") {
          #   cruz.pt.load.toplot2(input.save$planned_transects_toplot2)
          # }

        }
      }
    }, priority = 10)


    ###############################################################################
    output$planned_transects_text <- renderText(planned_transects())
    # output$planned_transects_remove_text <- renderText(planned_transects_remove())

    output$planned_transects_message <- renderText({
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
        updateCheckboxInput(session, "planned_transects_plot", value = TRUE)
    })

    ### Turn plot checkbox off if all planned transects are removed
    observe({
      if (is.null(cruz.list$planned.transects))
        updateCheckboxInput(session, "planned_transects_plot", value = FALSE)
    })

    ###############################################################################
    ###############################################################################
    # renderUI()s for loading planned transects

    #----------------------------------------------------------
    ### Get names from loaded csv file
    planned_transects_file_names <- reactive({
      req(planned_transects_read_csv())

      csv.names <- names(planned_transects_read_csv()[[2]])
      choices.list <- seq_along(csv.names)
      names(choices.list) <- csv.names

      choices.list
    })

    ### Create ui inputs for selecting lon/lat columns
    output$planned_transects_lon_uiOut_select <- renderUI({
      selectInput(
        session$ns("planned_transects_lon"), h5("Longitude column"),
        choices = planned_transects_file_names(), selected = 1
      )
    })

    output$planned_transects_lat_uiOut_select <- renderUI({
      selectInput(
        session$ns("planned_transects_lat"), h5("Latitude column"),
        choices = planned_transects_file_names(), selected = 2
      )
    })

    ### Widgets for transect numbers
    output$planned_transects_num_uiOut_select<- renderUI({
      selectInput(
        session$ns("planned_transects_num"), h5("Transect number column"),
        choices = planned_transects_file_names(), selected = 3
      )
    })

    ### Widget for transect classes 1
    output$planned_transects_class1_uiOut_select<- renderUI({
      selectInput(
        session$ns("planned_transects_class1"), h5("Transect class column"),
        choices = planned_transects_file_names(), selected = 4
      )
    })

    ### Widget for transect classes 2
    output$planned_transects_class2_uiOut_select<- renderUI({
      choices.list <- planned_transects_file_names()
      choices.list <- c("N/A - No class 2 info" = 0, choices.list)

      selectInput(
        session$ns("planned_transects_class2"), h5("Transect class 2 column"),
        choices = choices.list, selected = 0
      )
    })


    #----------------------------------------------------------
    ### Button to add selected transect data to CruzPlot
    output$planned_transects_execute_uiOut_button <- renderUI({
      req(planned_transects_read_csv())

      actionButton(session$ns("planned_transects_execute"), "Add data to CruzPlot")
    })


    ###############################################################################
    ###############################################################################
    # Process transect file and data and output widget to select ones to plot

    ### Read csv file
    planned_transects_read_csv <- reactive({
      req(input$planned_transects_file)

      file.all <- input$planned_transects_file
      file.name <- file.all$name
      file.data <- try(read.csv(file.all$datapath, stringsAsFactors = FALSE),
                      silent = TRUE)

      validate(
        need(file.data, "Error loading planned transects CSV")
      )

      list(file.name, file.data)
    })


    ### Add transect data to reactiveValue
    planned_transects <- eventReactive(input$planned_transects_execute, {
      validate(
        need(input$planned_transects_lon != input$planned_transects_lat,
            "Error: The longitude column cannot be the same as the latitude column")
      )

      x <- planned_transects_read_csv()[[2]] %>%
        dplyr::select(lon = as.numeric(input$planned_transects_lon),
                      lat = as.numeric(input$planned_transects_lat),
                      num = as.numeric(input$planned_transects_num),
                      class1 = as.numeric(input$planned_transects_class1))

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

      if (as.numeric(input$planned_transects_class2) != 0) {
        x <- cbind(
          x, dplyr::select(planned_transects_read_csv()[[2]],
                          class2 = as.numeric(input$planned_transects_class2))
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

    planned_transects_class1 <- reactive({
      unique(cruz.list$planned.transects$class1)
    })

    planned_transects_class2 <- reactive({
      unique(cruz.list$planned.transects$class2)
    })


    ###############################################################################
    ### Widgets for selecting planned transect class(es) to plot and their color
    output$planned_transects_toplot_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      choices.list.names <- planned_transects_class1()
      choices.list <- seq_along(choices.list.names)
      names(choices.list) <- choices.list.names

      isolate({
        choices.sel <- if (isTruthy(cruz.pt.load.toplot())) {
          cruz.pt.load.toplot()
        } else {
          choices.list
        }
        cruz.pt.load.toplot(NULL)
      })

      selectInput(session$ns("planned_transects_toplot"),
                  tags$h5("Class(es) to plot"),
                  choices = choices.list, selected = choices.sel,
                  multiple = TRUE)
    })


    output$planned_transects_color_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      isolate({
        choices.sel <- if (isTruthy(cruz.pt.load.color())) {
          cruz.pt.load.color()
        } else {
          "gray"
        }
        cruz.pt.load.color(NULL)
      })

      selectInput(session$ns("planned_transects_color"), tags$h5("Color(s)"),
                  choices = cruz.palette.color, selected = choices.sel,
                  multiple = TRUE)
    })


    #----------------------------------------------------------
    ### Widgets for selecting planned transect class 2(s) to plot and their lty
    output$planned_transects_toplot2_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      y <- planned_transects_class2()

      if (anyNA(y)) {
        helpText("No class 2 column was selected, and thus you can only specify",
                "a single line type for all planned transects")

      } else {
        choices.list.names <- y
        choices.list <- seq_along(choices.list.names)
        names(choices.list) <- choices.list.names

        isolate({
          choices.sel <- if (isTruthy(cruz.pt.load.toplot2())) {
            cruz.pt.load.toplot2()
          } else {
            choices.list
          }
          cruz.pt.load.toplot2(NULL)
        })

        selectInput(session$ns("planned_transects_toplot2"), tags$h5("Class 2(s) to plot"),
                    choices = choices.list, selected = choices.sel,
                    multiple = TRUE)
      }
    })

    # output$planned_transects_lty_uiOut_message <- renderUI({
    #   req(cruz.list$planned.transects, input$planned_transects_plot)
    #
    #   if (!anyNA(planned_transects_class2())) {
    #     helpText("Select either one line type or the same number of line types as transect class 2s.",
    #              "When multiple line types are selected, the order in which transect class 2(s) are",
    #              "selected to be plotted corresponds to order of specified line type(s).")
    #   } else {
    #     NULL
    #   }
    # })

    output$planned_transects_lty_uiOut_select <- renderUI({
      req(cruz.list$planned.transects)

      input.lab <- ifelse(
        anyNA(planned_transects_class2()), "Line type", "Line type(s)"
      )

      isolate({
        choices.sel <- if (isTruthy(cruz.pt.load.lty())) {
          cruz.pt.load.lty()
        } else {
          1
        }
        cruz.pt.load.lty(NULL)
      })

      selectInput(session$ns("planned_transects_lty"), tags$h5(input.lab),
                  choices = cruz.line.type, selected = choices.sel,
                  multiple = !anyNA(planned_transects_class2()))

    })

    ###############################################################################
    # Removing loaded transects
    #   Currently only allows one transect file to be loaded

    # ### Widget for selecting planned transect(s) to remove
    # output$planned_transects_toremove_uiOut_select <- renderUI({
    #   req(cruz.list$planned.transects)
    #
    #   choices.list.names <- planned_transects_class1()
    #   choices.list <- seq_along(choices.list.names)
    #   names(choices.list) <- choices.list.names
    #
    #   selectInput("planned_transects_toremove",
    #                  tags$h5("Select planned transect class(es) to remove"),
    #                  choices = choices.list, multiple = TRUE)
    # })
    #
    # output$planned_transects_toremove_execute_uiOut_button <- renderUI({
    #   req(cruz.list$planned.transects)
    #
    #   actionButton("planned_transects_toremove_execute", "Remove")
    # })
    #
    #
    # ### Remove selected transects
    # planned_transects_remove <- eventReactive(input$planned_transects_toremove_execute, {
    #   req(cruz.list$planned.transects)
    #   y <- as.numeric(input$planned_transects_toremove)
    #
    #   validate(
    #     need(length(y) != 0,
    #          "Please select at least one set of transects to remove")
    #   )
    #
    #   x <- cruz.list$planned.transects %>%
    #     filter(!(class1 %in% planned_transects_class1()[y]))
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
      if (input$planned_transects_plot) {
        pltransect_list()
        } else {
          NULL
        }
    }) 
    
    pltransect_list <- reactive({     
      validate(
        need(input$planned_transects_toplot,
            "Please select at least one class of planned transects to plot")
      )
      #So that renderUI()'s can catch up
      req(input$planned_transects_color, input$planned_transects_lty)

      # Get user inputs
      pltrans <- cruz.list$planned.transects
      pltrans.which <- as.numeric(input$planned_transects_toplot)
      pltrans.which2 <- as.numeric(input$planned_transects_toplot2)
      pltrans.colors <- input$planned_transects_color
      pltrans.lty <- as.numeric(input$planned_transects_lty)
      pltrans.lwd <- input$planned_transects_lwd

      # Process user inputs
      if (length(pltrans.colors) == 1) {
        pltrans.colors <- rep(pltrans.colors, length(pltrans.which))
      }

      validate(
        need(length(pltrans.colors) == length(pltrans.which),
            paste("The number of selected planned transect colors must either be",
                  "1 or equal to than the number of selected planned transects"))
      )

      pltrans.class1 <- planned_transects_class1()[pltrans.which]
      names(pltrans.colors) <- pltrans.class1

      pltrans <- dplyr::filter(pltrans, class1 %in% pltrans.class1)

      if (anyNA(planned_transects_class2())) {
        # Class 2 was not specified
        pltrans.list <- lapply(pltrans.class1, function(i) {
          x <- dplyr::filter(pltrans, class1 == i)
          lapply(unique(x$num), function(k) {
            x <- dplyr::filter(x, num == k)
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

      } else {
        # Class 2 was specified
        validate(
          need(pltrans.which2,
              "Please select at least one class 2 type to plot")
        )

        pltrans.class2 <- planned_transects_class2()[pltrans.which2]
        pltrans <- dplyr::filter(pltrans, class2 %in% pltrans.class2)

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
          x <- dplyr::filter(pltrans, class1 == i)
          lapply(pltrans.class2, function(j) {
            x <- dplyr::filter(x, class2 == j)
            lapply(unique(x$num), function(k) {
              x <- dplyr::filter(x, num == k)
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
      } 

      pltrans.list
    })

    ###############################################################################
    ###############################################################################
    ### Save values
    to_save <- reactive({
      list(
        save_widget("planned_transects_plot", "check"),
        save_widget("planned_transects_toplot", "select"), 
        save_widget("planned_transects_toplot2", "select"), 
        save_widget("planned_transects_color", "select"), 
        save_widget("planned_transects_lty", "select"), 
        save_widget("planned_transects_lw", "numeric"), 
        save_widget("planned.transects", "reactive", cruz.list$planned.transects)
      )
    })

    ### Return values
    list(
      to_save = to_save, 
      pltransect = pltransect, 
      planned_transects_class2 = planned_transects_class2
    )
  })
}