# cruzDasSightSymbol for CruzPlot
#   cruzDasSightSymbol() returns list of sighting type, species count, and
#     point type, color, size, and linewidth for legend and points, respectively
#   cruzDasSightSymbolAnimalSelected() returns list of point type, color, size, and linewidth for selected mammal or turtle sightings
#   cruzDasSightSymbolAnimalAll() returns list of point type, color, size, and linewidth for all mammal or turtle sightings
#   cruzDasSightSymbolBoat() returns list of point type, color, size, and linewidth for boat sightings
#   cruzDasSightSymbolCPOD() returns list of point type, color, size, and linewidth for CPOD sightings


cruzDasSightSymbol <- reactive({
  data.list <- cruzDasSightRange()
  sight.type <- data.list$sight.type
  sp.codes <- data.list$sp.codes
  sp.codes.len <- length(sp.codes)
  sp.count <- data.list$sp.count

  if(sight.type == 3 | sight.type == 4) {
    symbol.prop <- cruzDasSightSymbolBoat()
    leg.pch <- pt.pch <- symbol.prop$pt.pch
    leg.col <- pt.col <- symbol.prop$pt.col
    leg.cex <- pt.cex <- symbol.prop$pt.cex
    leg.lwd <- pt.lwd <- symbol.prop$pt.lwd
  }
  #   if(sight.type == 4) {
  #     symbol.prop <- cruzDasSightSymbolCPOD()
  #     leg.pch <- pt.pch <- symbol.prop$pt.pch
  #     leg.col <- pt.col <- symbol.prop$pt.col
  #     leg.cex <- pt.cex <- symbol.prop$pt.cex
  #     leg.lwd <- pt.lwd <- symbol.prop$pt.lwd
  #   }
  else {
    if((sight.type == 1 && input$das_sighting_code_1_all == 1) || (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
      symbol.prop <- cruzDasSightSymbolAnimalAll()
    }
    if((sight.type == 1 && input$das_sighting_code_1_all == 2) || (sight.type == 2 && input$das_sighting_code_2_all == 2)) {
      symbol.prop <- cruzDasSightSymbolAnimalSelected()
    }

    # Set before lengthened for vector
    leg.pch <- pt.pch <- symbol.prop$pt.pch
    leg.col <- pt.col <- symbol.prop$pt.col
    leg.cex <- pt.cex <- symbol.prop$pt.cex
    leg.lwd <- pt.lwd <- symbol.prop$pt.lwd

    # Empty input check
    validate(
      need(pt.pch != "",
           message = "Please enter at least one number for symbol type"),
      need(pt.col != "",
           message = "Please enter at least one number for symbol color"),
      need(pt.cex != "",
           message = "Please enter at least one number for symbol size"),
      need(all(0 < pt.cex & pt.cex < 20),
           message = "Please ensure all symbol size entries are valid numberical entries"),
      need(pt.lwd != "",
           message = "Please enter at least one number for symbol line width"),
      need(all(0 < pt.lwd & pt.lwd < 20),
           message = "Please ensure all symbol line width entries are valid numberical entries")
    )

    #
    if(length(pt.pch)>sp.codes.len)  pt.pch <- pt.pch[1:sp.codes.len]
    #     if(length(pt.pch)<sp.codes.len)  pt.pch <- rep(pt.pch,sp.codes.len)[1:sp.codes.len]
    #     if(length(pt.col)<sp.codes.len)  pt.col <- rep(pt.col,sp.codes.len)[1:sp.codes.len]
    #     if(length(pt.cex)<sp.codes.len)  pt.cex <- rep(pt.cex,sp.codes.len)[1:sp.codes.len]
    #     if(length(pt.lwd)<sp.codes.len)  pt.lwd <- rep(pt.lwd,sp.codes.len)[1:sp.codes.len]
    # If sp.codes is longer, recycle for each pt vector
    if(length(pt.pch) > 1) {
      ratio.pch.dec <- sp.codes.len/length(pt.pch)
      ratio.pch <- ceiling(ratio.pch.dec)
      validate(
        need(ratio.pch.dec >= 1, 'There are more symbol type entries than species')
      )
      if(ratio.pch > 1) pt.pch <- rep(pt.pch, ratio.pch)
      pt.pch <- unlist(sapply(1:sp.codes.len, function(j) rep(pt.pch[j], sp.count[j])))
    }
    if(length(pt.col) > 1) {
      ratio.col.dec <- sp.codes.len/length(pt.col)
      ratio.col <- ceiling(ratio.col.dec)
      validate(
        need(ratio.col.dec >= 1, 'There are more symbol color entries than species')
      )
      if(ratio.col > 1) pt.col <- rep(pt.col, ratio.col)
      pt.col <- unlist(sapply(1:sp.codes.len, function(j) rep(pt.col[j], sp.count[j])))
    }
    if(length(pt.cex) > 1) {
      ratio.cex.dec <- sp.codes.len/length(pt.cex)
      ratio.cex <- ceiling(ratio.cex.dec)
      validate(
        need(ratio.cex.dec >= 1, 'There are more symbol size entries than species')
      )
      if(ratio.cex > 1) pt.cex <- rep(pt.cex, ratio.cex)
      pt.cex <- unlist(sapply(1:sp.codes.len, function(j) rep(pt.cex[j], sp.count[j])))
    }
    if(length(pt.lwd) > 1) {
      ratio.lwd.dec <- sp.codes.len/length(pt.lwd)
      ratio.lwd <- ceiling(ratio.lwd.dec)
      validate(
        need(ratio.lwd.dec >= 1, 'There are more symbol linewidth entries than species')
      )
      if(ratio.lwd > 1) pt.lwd <- rep(pt.lwd, ratio.lwd)
      pt.lwd <- unlist(sapply(1:sp.codes.len, function(j) rep(pt.lwd[j], sp.count[j])))
    }
  }
  return(list(sight.type = sight.type, sp.count = sp.count, sp.codes = sp.codes,
              leg.pch = leg.pch, leg.col = leg.col, leg.cex = leg.cex, leg.lwd = leg.lwd,
              pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd))
})

# Mammal or turtle symbol properties
# Plot selected species
cruzDasSightSymbolAnimalSelected <- reactive({
  if(!input$das_symbol_mult) {
    pt.pch <- as.numeric(input$das.symbol.type)
    pt.col <- input$das.symbol.color
  }
  if(input$das_symbol_mult) {
    pt.pch <- as.numeric(unlist(strsplit(input$das.symbol.type.mult, ",")))
    pt.col <- str_trim(unlist(strsplit(input$das.symbol.color.mult, ",")))
    validate(
      need(all(pt.pch %in% 0:20),
           message = "Not all symbol type entries are valid. Please be sure all entries are a number from 0 to 20"),
      need(length(pt.col) != 0,
           message = "Please enter at least one number for symbol color"),
      need(all(pt.col %in% symbol.col),
           message = paste("Not all symbol color entries are valid. Please be sure each entry matches",
                           "a color in the Display Color/Formatt Options tab"))
    )

    # Covert color names to color codes-codes established in server file
    if(input$color_style == 1) {
      pt.col <- sapply(1:length(pt.col), function(i) which(symbol.col %in% pt.col[i])) # keeps numbers in order
      pt.col <- symbol.col.code[pt.col]
    }
    if(input$color_style == 2) {
      pt.col <- sapply(1:length(pt.col), function(i) which(symbol.col.gray %in% pt.col[i])) # keeps numbers in order
      pt.col <- symbol.col.code.gray[pt.col]
    }
  }
  # Same whether input$das_symbol_mult is checked or not
  pt.cex <- as.numeric(unlist(strsplit(input$das.symbol.size, ",")))
  pt.lwd <- as.numeric(unlist(strsplit(input$das.symbol.linewidth, ",")))

  return(list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd))
})

# Plot all species
cruzDasSightSymbolAnimalAll <- reactive({
  pt.pch <- 0:20
  pt.col <- "black"
  pt.cex <- 1
  pt.lwd <- 1

  return(list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd))
})

# Boat symbol properties (or CPODs for vaquita cruise)
cruzDasSightSymbolBoat <- reactive({
  pt.pch <- as.numeric(input$das.symbol.type.boat)
  pt.col <- input$das.symbol.color.boat
  pt.cex <- as.numeric(input$das.symbol.size.boat)
  pt.lwd <- as.numeric(input$das.symbol.linewidth.boat)

  return(list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd))
})

# CPOD symbol properties
# cruzDasSightSymbolCPOD <- reactive({
#   pt.pch <- as.numeric(input$das.symbol.type.cpod)
#   pt.col <- input$das.symbol.color.cpod
#   pt.cex <- as.numeric(input$das.symbol.size.cpod)
#   pt.lwd <- as.numeric(input$das.symbol.linewidth.cpod)
#
#   return(list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd))
# })
