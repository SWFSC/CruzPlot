# cruzDasGeneral for CruzPlot
#   read and process DAS file
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


###############################################################################
# Output flag indicating if DAS data has been loaded
output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)


###############################################################################
### Read and process DAS file(s)
das_file_load <- eventReactive(input$das_file, {
  # Clear reactive vals
  cruz.list$das.data <- NULL
  cruz.list$das.data.name <- NULL

  # Check file name
  file.name <- input$das_file$name
  validate(
    need(all(tolower(substr_right(file.name, 4)) ==".das"),
         "Error: All DAS files must have the file extension '.das'")
  )

  # Get and check additional parameters
  skip <- input$das_file_skip
  reset.event  <- input$das_file_reset_event == 1
  reset.effort <- input$das_file_reset_effort == 1
  reset.day    <- input$das_file_reset_day == 1

  validate(
    need(!is.na(skip), "skip must be a valid number") %then%
      need(isTRUE(all.equal(skip %% 1, 0)), "skip must be a whole number") %then%
      need(skip >= 0, "skip must be greater than or equal to zero")
  )

  # Process DAS file
  withProgress(message = "Processing DAS file", value = 0.6, {
    das.proc <- try(suppressWarnings(
      swfscDAS::das_process(
        input$das_file$datapath, skip = skip, reset.event = reset.event,
        reset.effort = reset.effort, reset.day = reset.day
      )
    ), silent = TRUE)
  })

  validate(
    need(isTruthy(das.proc),
         "Error: unable to read and process the provided DAS files")
  )

  # Savein reactive values
  cruz.list$das.data <- das.proc
  cruz.list$das.data.name <- file.name

  ""
}, ignoreInit = TRUE)


### Conditional flag for UI code for non-null cruz.list$das.data
output$cruzDasFile_Conditional <- reactive({
  isTruthy(cruz.list$das.data)
})
outputOptions(output, "cruzDasFile_Conditional", suspendWhenHidden = FALSE)


###############################################################################
# Code for keeping current inputs the same when switching
#    from or to text symbol properties input

observeEvent(input$das_symbol_mult, {
  curr.pch <- as.numeric(input$das_symbol_type)
  curr.col <- input$das_symbol_color

  if (length(curr.pch) == 0) curr.pch <- "1"
  if (is.null(curr.col)) curr.col <- "Black"

  # Covert color codes to color names
  if (curr.col[1] != "Black") {
    if (input$color_style == 1) {
      curr.col <- sapply(1:length(curr.col), function(i) {
        which(symbol.col.code %in% curr.col[i])
      }) # keeps numbers in order
      curr.col <- symbol.col[curr.col]
    } else if (input$color_style == 2) {
      curr.col <- sapply(1:length(curr.col), function(i) {
        which(symbol.col.code.gray %in% curr.col[i])
      }) # keeps numbers in order
      curr.col <- symbol.col.gray[curr.col]
    }
  }

  updateTextInput(session, "das_symbol_type_mult", value = paste(curr.pch, collapse = ", "))
  updateTextInput(session, "das_symbol_color_mult", value = paste(curr.col, collapse = ", "))
})

observeEvent(!input$das_symbol_mult, {
  curr.pch <- as.numeric(unlist(strsplit(input$das_symbol_type_mult, ", ")))
  curr.col <- unlist(strsplit(input$das_symbol_color_mult, ", "))

  if (length(curr.pch) == 0) curr.pch <- 1
  if (is.null(curr.col)) curr.col <- "black"

  # Covert color names to color codes
  if (curr.col[1] != "black") {
    if (input$color_style == 1) {
      curr.col <- sapply(1:length(curr.col), function(i) {
        which(symbol.col %in% curr.col[i])
      }) # keeps numbers in order
      curr.col <- symbol.col.code[curr.col]
    } else if (input$color_style == 2) {
      curr.col <- sapply(1:length(curr.col), function(i) {
        which(symbol.col.gray %in% curr.col[i])
      }) # keeps numbers in order
      curr.col <- symbol.col.code.gray[curr.col]
    }
  }

  updateSelectizeInput(session, "das_symbol_type", selected = curr.pch)
  updateSelectizeInput(session, "das_symbol_color", selected = curr.col)
})

# observe({
#   mult <- input$das_symbol_mult
#   isolate({
#     sight <- input$das_sighting_type
#     if(sight == 1) sp.len <- length(input$das.sighting.code.1)
#     if(sight == 2) sp.len <- length(input$das.sighting.code.2)
#     if(sight == 3) sp.len <- NULL
#   })
#
# })


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
