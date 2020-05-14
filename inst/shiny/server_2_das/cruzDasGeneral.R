# cruzDasGeneral for CruzPlot
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


###############################################################################
### Read DAS file(s)
das_file_load <- eventReactive(input$das.file, {
  # Clear reactive vals
  cruz.list$das.data <- NULL
  cruz.list$das.data.name <- NULL

  # Check file name
  file.name <- input$das.file$name
  validate(
    need(all(tolower(substr_right(file.name, 4)) ==".das"),
         "Error: All DAS files must have the file extension '.das'")
  )

  # Process DAS file
  withProgress(message = "Processing DAS file", value = 0.6, {
    # TODO: provide some way for the user to specify das_process arguments
    das.proc <- try(suppressWarnings(
      swfscDAS::das_process(
        input$das.file$datapath, skip = 0,
        reset.event = TRUE, reset.effort = TRUE, reset.day = TRUE
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
  curr.pch <- as.numeric(input$das.symbol.type)
  curr.col <- input$das.symbol.color
  if(length(curr.pch) == 0) curr.pch <- "1"
  if(is.null(curr.col)) curr.col <- "Black"

  # Covert color codes to color names
  if(curr.col[1] != "Black") {
    if(input$color_style == 1) {
      curr.col <- sapply(1:length(curr.col), function(i) which(symbol.col.code %in% curr.col[i])) # keeps numbers in order
      curr.col <- symbol.col[curr.col]
    }
    if(input$color_style == 2) {
      curr.col <- sapply(1:length(curr.col), function(i) which(symbol.col.code.gray %in% curr.col[i])) # keeps numbers in order
      curr.col <- symbol.col.gray[curr.col]
    }
  }

  updateTextInput(session, "das.symbol.type.mult", value = paste(curr.pch, collapse = ", "))
  updateTextInput(session, "das.symbol.color.mult", value = paste(curr.col, collapse = ", "))
})

observeEvent(!input$das_symbol_mult, {
  curr.pch <- as.numeric(unlist(strsplit(input$das.symbol.type.mult, ", ")))
  curr.col <- unlist(strsplit(input$das.symbol.color.mult, ", "))
  if(length(curr.pch) == 0) curr.pch <- 1
  if(length(curr.col) == 0) curr.col <- "black"

  # Covert color names to color codes
  if(curr.col[1] != "black") {
    if(input$color_style == 1) {
      curr.col <- sapply(1:length(curr.col), function(i) which(symbol.col %in% curr.col[i])) # keeps numbers in order
      curr.col <- symbol.col.code[curr.col]
    }
    if(input$color_style == 2) {
      curr.col <- sapply(1:length(curr.col), function(i) which(symbol.col.gray %in% curr.col[i])) # keeps numbers in order
      curr.col <- symbol.col.code.gray[curr.col]
    }
  }

  updateSelectizeInput(session, "das.symbol.type", selected = curr.pch)
  updateSelectizeInput(session, "das.symbol.color", selected = curr.col)
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
  if(input$das_effort == 3) {
    if(!input$das_effort_det_byBft) {
      if("S" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_s_flag", suspendWhenHidden = FALSE)

output$das_effort_det_n_flag <- reactive({
  flag <- FALSE
  if(input$das_effort == 3) {
    if(!input$das_effort_det_byBft) {
      if("N" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_n_flag", suspendWhenHidden = FALSE)

output$das_effort_det_f_flag <- reactive({
  flag <- FALSE
  if(input$das_effort == 3) {
    if(!input$das_effort_det_byBft) {
      if("F" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_f_flag", suspendWhenHidden = FALSE)

###############################################################################
