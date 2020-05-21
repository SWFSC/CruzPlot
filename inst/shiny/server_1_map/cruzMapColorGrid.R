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
  if (world2) rivs$x <- ifelse(rivs$x < 0, rivs$x+360, rivs$x)

  return(rivs)
})


###############################################################################
cruzMapColorWater <- reactive({
  ## One color
  if (input$color_water_style == 1) {
    water.col <- ifelse(input$color_water_style == 1,
                        input$color_water, 0)
    water.bathy <- 0
  }

  ## Depth
  if (input$color_water_style == 2) {
    lon.range <- cruz.map.range$lon.range
    lat.range <- cruz.map.range$lat.range
    world2 <- cruz.map.range$world2
    if (is.null(cruz.list$bathy)){
      if (input$depth_style == 1) {
        validate(
          need(1 <= as.numeric(input$depth_res) &&
                 as.numeric(input$depth_res) <= 60,
               message = "Please ensure depth resolution is between 0 and 60")
        )

        # getNOAA.bathy() operates on -180 to 180 scale, thus use user inputs not lonRange() output
        bathy <- getNOAA.bathy(
          lon1 = as.numeric(input$lon_left), lon2 = as.numeric(input$lon_right),
          lat1 = lat.range[1], lat2 = lat.range[2],
          resolution = as.numeric(input$depth_res), keep = TRUE,
          antimeridian = world2
        )
      }
      else {
        validate(
          need(!is.null(input$depth_file$datapath),
               message = "Please load csv depth file")
        )
        bathy <- read.csv(input$depth_file$datapath, header = input$depth_header,
                          sep = input$depth_sep, quote = input$depth_quote)
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

  list(water.col, water.bathy)
})

## Land
cruzMapColorLand <- reactive({
  ifelse(input$color_land_all == TRUE, input$color_land, "white")
})


###############################################################################
# Update options for greyscale or normal colors
#   Note includes nDAS and DAS color inputs as well
observe({
  col.style <- input$color_style

  isolate({
    if (!cruz.load.color$load.flag) {
      if (col.style == 1) {
        palette("default")
        updateSelectInput(session, "color_land", choices = cruz.palette.color, selected = "bisque1")
        updateSelectInput(session, "color_water", choices = cruz.palette.color, selected = "white")
        updateSelectInput(session, "grid_line_color", choices = cruz.palette.color, selected = "black")
        updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "das_effort_lineCol", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "ndas_line_col", choices = cruz.palette.color, selected = "black")
        updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.color, selected = "black")

      } else if (col.style == 2) {
        palette(gray(0:5/5))
        updateSelectInput(session, "color_land", choices = cruz.palette.gray, selected = 4)
        updateSelectInput(session, "color_water", choices = cruz.palette.gray, selected = 0)
        updateSelectInput(session, "grid_line_color", choices = cruz.palette.gray, selected = 1)
        updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.gray, selected = 1)
        updateSelectInput(session, "ndas_line_col", choices = cruz.palette.gray, selected = 1)
        updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.gray, selected = 1)
      }
    }

    cruz.load.color$load.flag <- FALSE
  })
})


###############################################################################
cruzMapGrid <- reactive({
  list(
    col = input$grid_line_color, lwd = input$grid_line_width,
    lty = input$grid_line_type
  )
})

###############################################################################
