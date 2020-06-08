# cruzDasSightRange for CruzPlot - step 3 of processing species data
#   cruzDasSightPosition returns das_sight with Lat/Lon columns adjusted for
#     world2 and ship/sighting position.
#     NOTE: Now done when removing records with NA positions
#   cruzDasSightRange() returns list, which includes sightings within map range,
#     selected sighting type, species codes, and counts for each species


###############################################################################
cruzDasSightRange <- reactive({
  #----------------------------------------------------------------------------
  req(cruz.list$das.data)

  das.sight <- cruzDasSightFilter()$das.sight
  # browser()

  data.list <- cruzDasSightFilter()
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  # Adjust longitudes if world2 map is being used
  if (cruz.map.range$world2)
    das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

  # NA values removed back in cruzDasSightSpeciesProcess()
  ll.na <- sum(is.na(das.sight$Lat) | is.na(das.sight$Lon))
  validate(
    need(ll.na == 0,
         "Error processing sighting positions - please report this as an issue")
  )


  #----------------------------------------------------------------------------
  # Filter to map range
  das.sight.filt <- das.sight %>%
    filter(between(.data$Lat, lat.range[1], lat.range[2]),
           between(.data$Lon, lon.range[1], lon.range[2]))
  validate(
    need(nrow(das.sight.filt) > 0,
         "No sightings are within the map boundaries")
  )

  # Check that all selected mammal/turtle codes are still present, and get sp.count
  if (sight.type %in% c(1, 2)) {
    if (sp.selection) {
      # If only selected species are plotted, check all passed range filter
      sp.codes.none <- base::setdiff(sp.codes, das.sight.filt$SpCode)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings within the provided map range:",
                   paste(sp.codes.none, collapse = ", ")))
      )

    } else {
      # If all species are being plotted, filter for species in filtered data
      sp.codes <- base::intersect(sp.codes, das.sight.filt$SpCode)
    }

    # Calculate count for each species
    sp.count <- vapply(sp.codes, function(i, j) {
      sum(j$SpCode == i)
    }, 1, j = das.sight.filt, USE.NAMES = FALSE)

  } else {
    sp.count <- nrow(das.sight.filt)
  }


  #----------------------------------------------------------------------------
  list(
    das.sight = das.sight.filt, sight.type = sight.type,
    sp.codes = sp.codes, sp.selection = sp.selection, sp.count = sp.count
  )
})
