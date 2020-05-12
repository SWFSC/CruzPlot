# Processing for Color and Grid tabs of Create and Save Map tab
#   cruzMapRiver() returns river data, adjusted for world2 map if necessary
#   cruzMapColorWater() returns water color and depth data
#   cruzMapColorLand() returns land color
#   Update various color selections depending on if color or gray scale is selected
#   cruzMapGrid() returns grid line parameters


###############################################################################
cruzMapRiver <- reactive({
  world2 <- cruz.map.range$world2
  rivs <- map("rivers", plot = F)
  if(world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)

  return(rivs)
})


###############################################################################
cruzMapColorWater <- reactive({
  ## One color
  if(input$color_water_style == 1) {
    water.col <- ifelse(input$color_water_style == 1,
                        input$color.water, 0)
    water.bathy <- 0
  }

  ## Depth
  if(input$color_water_style == 2) {
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    world2 <- cruz.map.range$world2
    if(is.null(cruz.list$bathy)){
      if(input$depth_style == 1) {
        validate(
          need(1 <= as.numeric(input$depth.res) &&
                 as.numeric(input$depth.res) <= 60,
               message = "Please ensure depth resolution is between 0 and 60")
        )

        # getNOAA.bathy() operates on -180 to 180 scale, thus use user inputs not lonRange() output
        bathy <- getNOAA.bathy(lon1 = as.numeric(input$lon.left),
                               lon2 = as.numeric(input$lon.right),
                               lat1 = lat.range[1], lat2 = lat.range[2],
                               resolution = as.numeric(input$depth.res),
                               keep = TRUE, antimeridian = world2)
      }
      else {
        validate(
          need(!is.null(input$depth.file$datapath),
               message = "Please load csv depth file")
        )
        bathy <- read.csv(input$depth.file$datapath, header=input$depth.header,
                          sep=input$depth.sep, quote=input$depth.quote)
        bath.keep <- (lon.range[1] <= bathy$V1 & bathy$V1 <= lon.range[2]) &
          (lat.range[1] <= bathy$V2 & bathy$V2 <= lat.range[2])
        bathy <- as.bathy(bathy[bath.keep,])
      }

      cruz.list$bathy <- bathy
      water.bathy <- bathy
    } else {
      water.bathy <- cruz.list$bathy
    }
    water.col <- "blue"
  }

  return(list(water.col, water.bathy))
})

## Land
cruzMapColorLand <- reactive({

  land.col <- ifelse(input$color_land_all == TRUE,
                     input$color.land,
                     "white")

  return(land.col)
})


###############################################################################
# Update options for greyscale or normal colors
observe({
  c <- input$color_style

  isolate({
    if(!cruz.load.color$load.flag) {
      if(c == 1) {
        palette("default")
        updateSelectInput(session, "color.land", choices = cruz.palette.color, selected = "bisque1")
        updateSelectInput(session, "color.water", choices = cruz.palette.color, selected = "white")
        updateSelectInput(session, "grid.line.color", choices = cruz.palette.color, selected = "black")
        updateSelectizeInput(session, "das.symbol.color", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "das.effort.lineCol", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "ndas.line.col", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "ndas.pt.col", choices = cruz.palette.color, selected = "black")
      }

      if(c == 2) {
        palette(gray(0:5/5))
        updateSelectInput(session, "color.land", choices = cruz.palette.gray, selected = 4)
        updateSelectInput(session, "color.water", choices = cruz.palette.gray, selected = 0)
        updateSelectInput(session, "grid.line.color", choices = cruz.palette.gray, selected = 1)
        updateSelectizeInput(session, "das.symbol.color", choices = cruz.palette.gray, selected = 1)
        updateSelectInput(session, "ndas.line.col", choices = cruz.palette.gray, selected = 1)
        updateSelectInput(session, "ndas.pt.col", choices = cruz.palette.gray, selected = 1)
      }
    }

    cruz.load.color$load.flag <- FALSE
  })
})


###############################################################################
cruzMapGrid <- reactive({
  grid.col <- input$grid.line.color
  grid.lwd <- input$grid.line.width
  grid.lty <- input$grid.line.type

  return(list(col = grid.col, lwd = grid.lwd, lty = grid.lty))
})

###############################################################################
