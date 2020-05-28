# cruzDasGeneral for CruzPlot
#   read and process DAS file
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


###############################################################################
# Output flag indicating if DAS data has been loaded
output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)

# Output flag indicating if SpCodes has been loaded
output$das_spcodes_loaded_flag <- reactive(isTruthy(cruz.list$sp.codes))
outputOptions(output, "das_spcodes_loaded_flag", suspendWhenHidden = FALSE)


###############################################################################
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

  validate(
    need(!is.na(skip), "skip must be a valid number") %then%
      need(isTRUE(all.equal(skip %% 1, 0)), "skip must be a whole number") %then%
      need(skip >= 0, "skip must be greater than or equal to zero")
  )

  validate(
    need(!is.na(days.gap), "days.gap must be a valid number") %then%
      need(isTRUE(all.equal(days.gap %% 1, 0)), "days.gap must be a whole number") %then%
      need(days.gap >= 0, "days.gap must be greater than or equal to zero")
  )

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
         "Error: unable to read and process the provided DAS file(s)")
  )

  # Savein reactive values
  cruz.list$das.data <- das.proc
  cruz.list$das.data.name <- input$das_file$name

  ""
}, ignoreInit = TRUE)


### Conditional flag for UI code for truthy cruz.list$das.data
output$cruzDasFile_Conditional <- reactive({
  isTruthy(cruz.list$das.data)
})
outputOptions(output, "cruzDasFile_Conditional", suspendWhenHidden = FALSE)


###############################################################################
# Code for keeping current inputs the same when switching
#    from or to text symbol properties input
observeEvent(input$das_symbol_mult, {
  if (input$das_symbol_mult) {
    # Covert numerics to symbols
    curr.pch <- as.numeric(input$das_symbol_type)
    if (length(curr.pch) == 0) curr.pch <- "1"

    # Covert color codes to color names
    curr.col <- input$das_symbol_color
    if (is.null(curr.col)) {
      curr.col <- "Black"

    } else {
      # This keeps numbers in order
      if (input$color_style == 1) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code %in% i), 1)
        curr.col <- symbol.col[curr.col.idx]

      } else if (input$color_style == 2) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code.gray %in% i), 1)
        curr.col <- symbol.col[curr.col.idx]
      }
    }

    updateTextInput(session, "das_symbol_type_mult", value = paste(curr.pch, collapse = ", "))
    updateTextInput(session, "das_symbol_color_mult", value = paste(curr.col, collapse = ", "))


  } else {
    # Convert symbol codes to symbols
    curr.pch <- suppressWarnings(
      as.numeric(unlist(strsplit(input$das_symbol_type_mult, ", ")))
    )
    if (length(curr.pch) == 0) {
      curr.pch <- 1
    } else {
      if (!all(curr.pch %in% unname(cruz.symbol.type))) curr.pch <- 1
    }
    updateSelectizeInput(session, "das_symbol_type", selected = curr.pch)


    # Covert color names to color codes
    curr.col <- unlist(strsplit(input$das_symbol_color_mult, ", "))

    if (is.null(curr.col)) {
      curr.col <- "black"

    } else {
      if (!all(curr.col %in% symbol.col)) {
        curr.col <- "black"
      } else {
        # This keeps numbers in order
        if (input$color_style == 1) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col %in% i), 1)
          curr.col <- symbol.col.code[curr.col.idx]

        } else if (input$color_style == 2) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.gray %in% i), 1)
          curr.col <- symbol.col.code.gray[curr.col.idx]
        }
      }
    }
    updateSelectizeInput(session, "das_symbol_color", selected = curr.col)
  }
})


###############################################################################
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

###############################################################################
