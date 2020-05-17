# Returns parameters for sighting legend

cruzDasSightLegend <- reactive({
  symbol.list <- cruzDasSightSymbol()

  leg.df       <- symbol.list$leg.df
  sight.type   <- symbol.list$sight.type
  sp.codes     <- symbol.list$sp.codes
  sp.codes.len <- length(sp.codes)
  sp.count     <- symbol.list$sp.count

  font.fam <- font.family.vals[as.numeric(input$das.legend.font)]

  if (sight.type %in% c(3, 4)) {
    leg.lab <- leg.df$Sp
    if (input$das.legend.num) leg.lab <- paste0(leg.lab, ", n = ", sp.count)

  } else {
    sp.codes.all <- cruzSpecies()
    temp.use <- unlist(sapply(1:sp.codes.len, function(i) {
      which(sp.codes.all$Code == sp.codes[i])
    }))
    sp.codes.all.use <- sp.codes.all[temp.use,]
    sp.codes.all.use$Name.Common <- sapply(sp.codes.all.use$Name.Common, function(i) {
      unlist(strsplit(i,","))[1]
    })

    leg.lab <- NULL
    names.lab <- input$das.legend.names
    if ("1" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Code)
    if ("2" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Abbr)
    if ("3" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name.Scientific)
    if ("4" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name.Common)

    if (input$das.legend.num) {
      if ("1" %in% names.lab || "2" %in% names.lab || "3" %in% names.lab || "4" %in% names.lab)
        leg.lab <- paste0(leg.lab, ", ")
      leg.lab <- paste0(leg.lab, "n = ", sp.count)
    }
  }
  leg.title <- NULL
  if (input$das.legend.title != "") leg.title <- input$das.legend.title
  leg.bty <-     ifelse(input$das.legend.boxCol == 1, "n", "o")
  leg.box.col <- ifelse(input$das.legend.boxCol == 2, NA, "black")
  leg.box.lwd <- ifelse(input$das.legend.boxCol == 2, 0, 1)
  leg.box.cex <- input$das.legend.textSize

  leg.pos <- input$das_legend_pos
  if (leg.pos == 1) {
    leg.x <- as.numeric(input$das.legend.lon)
    leg.y <- as.numeric(input$das.legend.lat)

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
