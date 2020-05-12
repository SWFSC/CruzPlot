# cruzDasSightRange for CruzPlot
#   cruzDasSightRange() returns a list of sightings within map range, selected sighting type,
#     species codes, and counts for each species


cruzDasSightRange <- reactive({
  req(cruz.list$das.data)

  # data.all <- cruz.list$das.data
  data.list <- cruzDasSightFilter()
  data.sight <- data.list$data.sight
  sight.type <- data.list$sight.type
  sp.codes <- data.list$sp.codes

  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  # Adjust longitudes if world2 map is being used
  if(cruz.map.range$world2) {
    lon.ship <- data.sight$Lon
    lon.sight <- data.sight$sight.lon

    data.sight$Lon <- ifelse(lon.ship < 0, lon.ship + 360, lon.ship)
    data.sight$sight.lon <- ifelse(lon.sight < 0, lon.sight + 360, lon.sight)
  }

  ship.lon <- data.sight$Lon     # ship's location at time of sighting
  ship.lat <- data.sight$Lat

  ### Commented out because this filtering is done in cruzDasSightFilterTrunc() ???
  #   # get angle and distance in nm
  #   if(sight.type == 1) {
  #     ndx <- as.numeric(rownames(data.sight))-1    # row names are the indices of the A records
  #     data.temp <- data.all[ndx,]
  #     angle <- as.numeric(data.temp$Data5)
  #     dist <- as.numeric(data.temp$Data7)
  #   }
  #   if(sight.type == 2) {
  #     ndx <- as.numeric(rownames(data.sight))
  #     angle <- as.numeric(data.sight$Data3)
  #     dist <- as.numeric(data.sight$Data4)
  #   }
  #   if(sight.type == 3) {
  #     ndx <- as.numeric(rownames(data.sight))
  #     angle <- as.numeric(data.sight$Data2)
  #     dist <- as.numeric(data.sight$Data3)
  #   }
  #
  # #  ndx.S <- as.numeric(rownames(data.sight))-1    # row names are the indices of the A records
  # #  data.S <- data.all[ndx.S,]
  # #  angle <- as.numeric(data.S$Data5)
  #   angle[is.na(angle)] <- 0
  #   dist[is.na(dist)] <- 0
  #   course <- cruz.list$das.data$Course[ndx]
  #   bearing <- (angle+course) %% 360
  #   dist.m <- dist*1852                 # dist in m
  #   sight.loc <- destPoint(matrix(c(ship.lon,ship.lat),ncol=2),bearing,dist.m) # location of sighting
  #   data.sight$sight.lon <- sight.loc[,"lon"]
  #   data.sight$sight.lat <- sight.loc[,"lat"]
  #   data.sight$angle <- angle
  #   data.sight$dist <- dist
  #
  has.loc <- !is.na(ship.lon) & !is.na(ship.lat)
  in.lon.range <- ship.lon >= lon.range[1] & ship.lon <= lon.range[2]
  in.lat.range <- ship.lat >= lat.range[1] & ship.lat <= lat.range[2]
  to.plot <- has.loc & in.lon.range & in.lat.range
  data.sight <- data.sight[to.plot,]

  # General/boat check
  validate(
    need(any(to.plot),
         message = paste("No sightings are within the map boundaries"))
  )

  # Mammals
  if(sight.type == 1) sp.count <- unname(sapply(sp.codes, function(i) length(c(which(data.sight$Data5 == i),
                                                                               which(data.sight$Data6 == i),
                                                                               which(data.sight$Data7 == i),
                                                                               which(data.sight$Data8 == i)))))
  # Turtles
  if(sight.type == 2) sp.count <- unname(sapply(sp.codes, function(j) length(which(data.sight$Data2 == j))))

  if((sight.type == 1 && input$das_sighting_code_1_all == 2) || (sight.type == 2 && input$das_sighting_code_2_all == 2)) {
    sp.code.false <- sp.codes[which(sp.count == 0)]
    validate(
      need(all(sp.count > 0),
           message = paste("Species with code", sp.code.false,
                           "does not have any sightings within the map boundaries"))
    )
  }
  # Remove sp.codes without sightings if plot all is selected and sp.count = 0
  if((sight.type == 1 && input$das_sighting_code_1_all == 1) || (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
    sp.codes <- sp.codes[-which(sp.count == 0)]
    sp.count <- sp.count[-which(sp.count == 0)]
  }

  if(sight.type == 3) sp.count <- length(data.sight[,1])
  if(sight.type == 4) sp.count <- length(data.sight[,1])

  return(list(data.sight = data.sight, sight.type = sight.type, sp.codes = sp.codes, sp.count = sp.count))
})
