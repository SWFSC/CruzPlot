# cruzDasGeneral for CruzPlot
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


### Read DAS file(s)
observeEvent(input$das.file, {
  withProgress(message = "Processing DAS file", value = 0.6, {
    das.tosave.list <- lapply(input$das.file$datapath, function(i) {
      suppressWarnings(CruzPlot::das_read(i))
    })
    das.tosave <- do.call(rbind, das.tosave.list)

    cruz.list$das.data <- das.tosave
  })
}, ignoreInit = TRUE)

### Conditional flag for UI code for non-null cruz.list$das.data
output$cruzDasFile_Conditional <- reactive({
  !is.null(cruz.list$das.data)
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
