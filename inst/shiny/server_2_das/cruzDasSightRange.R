# cruzDasSightRange for CruzPlot - step 3 of processing species data
#   cruzDasSightRange() returns a list of sightings within map range, selected sighting type,
#     species codes, and counts for each species


cruzDasSightRange <- reactive({
  #----------------------------------------------------------------------------
  req(cruz.list$das.data)
  data.list <- cruzDasSightFilter()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range


  #----------------------------------------------------------------------------
  # 'Select' ship or sighting position
  if (input$das_sightings_position == 1) {
    das.sight$Lat <- das.sight$Lat_ship
    das.sight$Lon <- das.sight$Lon_ship

  } else { #input$das_sightings_position == 2
    das.sight$Lat <- das.sight$Lat_sight
    das.sight$Lon <- das.sight$Lon_sight
  }

  # Adjust longitudes if world2 map is being used
  if (cruz.map.range$world2)
    das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

  if (anyNA(das.sight$Lat) | anyNA(das.sight$Lon)) {
    warning("NA sighting lat/lon")
  }

  das.sight.filt <- das.sight %>%
    filter(!is.na(.data$Lat), !is.na(.data$Lon),
           between(.data$Lat, lat.range[1], lat.range[2]),
           between(.data$Lon, lon.range[1], lon.range[2]))


  #----------------------------------------------------------------------------
  # General sightings check
  validate(
    need(nrow(das.sight.filt) > 0,
         "No sightings are within the map boundaries")
  )

  # Check that all selected mammal/turtle codes are still present, and get sp.count
  if (sight.type %in% c(1, 2)) {
    if (sp.selection) {
      # If only selected species are plotted, check all passed range filter
      sp.codes.none <- base::setdiff(sp.codes, das.sight.filt$Sp)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings within the provided map range:",
                   paste(sp.codes.none, collapse = ", ")))
      )

    } else {
      # If all species are being plotted, filter for species in filtered data
      sp.codes <- base::intersect(sp.codes, das.sight.filt$Sp)
    }

    # Calculate count for each species
    sp.count <- vapply(sp.codes, function(i, j) {
      sum(j$Sp == i)
    }, 1, j = das.sight.filt, USE.NAMES = FALSE)

  } else {
    sp.count <- nrow(das.sight.filt)
  }


  #----------------------------------------------------------------------------
  list(
    das.sight = das.sight.filt, sight.type = sight.type, sp.codes = sp.codes,
    sp.selection = sp.selection, sp.count = sp.count
  )
})
