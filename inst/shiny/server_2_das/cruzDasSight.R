# cruzDasSight for CruzPlot
#   cruzDasSightSpeciesMammals() returns mammal species codes selected by user
#   cruzDasSightSpeciesTurtles() returns turtle species codes selected by user
#   cruzDasSightSpecies() returns list of data frames containing data for selected species sightings,
#	    sighting type, and species codes; also computes sighting location based on angle and distance;
#     adds sight.lat, sight.lon, angle, distance (nmi) to data.sight dataframe


###############################################################################
cruzDasSightSpeciesMammals <- reactive({
  sp.code.all <- if (input$das_sighting_code_1_all == 1) {
    cruzSpeciesMammals()$Code
  } else if (input$das_sighting_code_1_all == 2) {
    gsub(" ", "", substring(input$das.sighting.code.1, 1, 3))
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_1_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.code.all) > 0, "Please choose at least one valid mammal species code")
  )

  sp.code.all
})

cruzDasSightSpeciesTurtles <- reactive({
  sp.code.all <- if (input$das_sighting_code_2_all == 1) {
    cruzSpeciesTurtles()$Code
  } else if (input$das_sighting_code_2_all == 2) {
    substring(input$das.sighting.code.2, 1, 2)
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_2_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.code.all) > 0, "Please choose at least one valid turtle species code")
  )

  sp.code.all
})


###############################################################################
cruzDasSightSpecies <- reactive({
  das.proc <- req(cruz.list$das.data)

  ### Sightings to plot
  sight.type <- input$das_sighting_type
  stopifnot(sight.type %in% 1:4)

  das.sight <- das_sight(das.proc, mixed.multi = TRUE)

  #----------------------------------------------------------------------------
  if (sight.type == 1) {
    # 1: Mammals
    # Get species codes - also does validate() check for valid species
    sp.code.all <- cruzDasSightSpeciesMammals()
    sp.code.len <- length(sp.code.all)
    das.sight <- das.sight %>% filter(.data$Event %in% c("S", "K", "M"))

    # Update probable sightings species if necessary
    if (input$das.sighting.probable) {
      validate(need(FALSE, "CruzPlot not ready for probable yet"))
      if (any(is.na(das.sight$Prob)))
        warning("A marine mammal sighting has an NA Prob value")

      validate(
        need(sum(das.sight$Prob) > 0,
             "There are no probable sightings in the loaded DAS file(s)")
      )

      # 977 used as probable vaquita sighting on some cruises
      das.sight$Sp <- ifelse(das.sight$Sp == "977", "041", das.sight$Sp)
    }

    # Filter for selected species, and check that all selected species are in data
    das.sight <- das.sight %>% filter(.data$Sp %in% sp.code.all)

    if (input$das_sighting_code_1_all == 2) {
      sp.code.false <- sp.code.all[!(sp.code.all %in% das.sight$Sp)]
      validate(
        need(length(sp.code.false) == 0,
             paste("Species with code(s)", sp.code.false,
                   "does (do) not have any sightings in the data"))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 2) {
    # 2: Turtles
    validate(
      need(sum(das.sight$Event == "t") > 0,
           "There are no turtle sightings in the loaded DAS file(s)")
    )

    sp.code.all <- cruzDasSightSpeciesTurtles()
    sp.code.len <- length(sp.code.all)

    das.sight <- das.sight %>%
      filter(.data$Event %in% c("t"),
             .data$TurtleSp %in% sp.code.all) %>%
      mutate(Sp = .data$TurtleSp)

    if (input$das_sighting_code_2_all == 2) {
      sp.code.false <- sp.code.all[!(sp.code.all %in% das.sight$Sp)]
      validate(
        need(length(sp.code.false) == 0,
             paste("Species with code(s)", sp.code.false,
                   "does (do) not have any sightings in the data"))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 3) {
    # 3: Boats
    das.sight <- das.sight %>% filter(.data$Event == "F")
    sp.code.all <- NULL

    validate(
      need(nrow(das.sight) > 0,
           "There are no boat sightings in the loaded DAS file(s)")
    )


    #--------------------------------------------------------------------------
  } else if (sight.type == 4) {
    # 4: C-PODs
    # C-POD sightings are entered as objects with sighting angle and distance
    # the string "cpod" in the comment on the next line indicates object is a CPOD
    sp.code.all <- NULL

    validate(
      need(sum(das.proc$Event == "X") > 0,
           "There are no C-POD sightings in the loaded DAS file(s)")
    )


    ndx.x <- which(das.proc$Event == "X")
    comm.x1 <- apply(das.proc[ndx.X + 1, paste0("Data", 1:7)], 1, function(i) {
      paste(na.omit(i), collapse = "")
    })
    comm.x1.cpod <- grepl("cpod", comm.x1, ignore.case = TRUE)
    stopifnot(length(ndx.x) == length(comm.x1))

    ndx.x <- ndx.x[comm.x1.cpod]

    das.sight <- das.proc %>%
      slice(ndx.x) %>%
      mutate(Bearing = as.numeric(.data$Data2),
             DistNm = as.numeric(.data$Data4),
             PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852)

    validate(
      need(nrow(das.sight) > 0,
           "There are no C-POD sightings in the loaded DAS file(s)")
    )
    # comment.str.df <- data.all[,6:13]
    # comment.str <- apply(comment.str.df,1,paste,collapse="")
    # ndx.cpod <- grep("cpod",comment.str)
    # ndx <- ndx.X[(ndx.X+1) %in% ndx.cpod]
    # data.sight <- data.all[ndx,]
    # angle <- as.numeric(data.sight$Data2)
    # dist.nmi <- as.numeric(data.sight$Data4)
  }


  #----------------------------------------------------------------------------
  # Final check to ensure some sightings match provided selection
  validate(
    need(nrow(das.sight) > 0,
         "No sightings exist for the selected type and code(s)")
  )

  # Calculate sighting location
  bearing2 <- (das.sight$Course + das.sight$Bearing) %% 360
  ll.sight <- destPoint(matrix(c(das.sight$Lon, das.sight$Lat), ncol = 2),
                        bearing2, das.sight$DistNm * 1852)

  das.sight <- das.sight %>%
    mutate(Lat_ship = .data$Lat,
           Lon_ship = .data$Lon,
           Lat_sight = ll.sight[, "lat"],
           Lon_sight = ll.sight[, "lon"])

  # # Calculate sighting location using swfscMisc
  # ll.sight.dest <- apply(das.sight, 1, function(i) {
  # i <- as.numeric(i[c("Lat", "Lon", "bearing2", "DistNm")])
  #   swfscMisc::destination(i["Lat"], i["Lon"], i["bearing2"], i["DistNm"],
  #                          units = "nm", type = "ellipsoid")
  # })

  # Return list
  list(das.sight = das.sight, sight.type = sight.type, sp.codes = sp.code.all)
})

###############################################################################
