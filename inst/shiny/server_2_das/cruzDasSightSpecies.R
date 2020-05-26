# cruzDasSight for CruzPlot - step 1 of processing species data
#   cruzDasSightSpeciesMammals() returns mammal species codes selected by user
#   cruzDasSightSpeciesTurtles() returns turtle species codes selected by user
#   cruzDasSightSpecies() returns list of data frames containing data for selected species sightings,
#	    sighting type, and species codes; also computes sighting location based on angle and distance;
#     adds sight.lat, sight.lon, angle, distance (nmi) to data.sight dataframe


###############################################################################
cruzDasSightSpeciesMammals <- reactive({
  sp.codes <- if (input$das_sighting_code_1_all == 1) {
    cruzSpeciesMammals()$Code
  } else if (input$das_sighting_code_1_all == 2) {
    gsub(" ", "", substring(input$das_sighting_code_1, 1, 3))
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_1_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.codes) > 0, "Please choose at least one valid mammal species code")
  )

  sp.codes
})

cruzDasSightSpeciesTurtles <- reactive({
  sp.codes <- if (input$das_sighting_code_2_all == 1) {
    cruzSpeciesTurtles()$Code
  } else if (input$das_sighting_code_2_all == 2) {
    substring(input$das_sighting_code_2, 1, 2)
  } else {
    stop("Invalid CruzPlot input$das_sighting_code_2_all value. ",
         "Please report this as an issue")
  }

  validate(
    need(length(sp.codes) > 0, "Please choose at least one valid turtle species code")
  )

  sp.codes
})


###############################################################################
cruzDasSightSpecies <- reactive({
  das.proc <- req(cruz.list$das.data)
  cruz.list$das.sight.filt <- NULL

  ### Sightings to plot
  sight.type <- input$das_sighting_type
  stopifnot(sight.type %in% 1:4)
  sp.selection <- isTRUE(
    (sight.type == 1 && input$das_sighting_code_1_all == 2) ||
      (sight.type == 2 && input$das_sighting_code_2_all == 2)
  )

  das.sight <- swfscDAS::das_sight(das.proc, returnformat = "default")

  #----------------------------------------------------------------------------
  if (sight.type == 1) {
    # 1: Mammals
    # Get species codes - also does validate() check for valid species
    sp.codes <- cruzDasSightSpeciesMammals()
    sp.events <- input$das_sighting_events
    validate(
      need(sp.events, "Please select at least one event code to plot")
    )

    das.sight <- das.sight %>%
      filter(.data$Event %in% sp.events) #c("S", "K", "M", "G", "p"))

    validate(
      need(nrow(das.sight) > 0,
           paste("There are no mammal sightings for the selected event(s)",
                 "in the loaded DAS file(s)"))
    )

    # Update probable sightings species if necessary
    if (input$das_sighting_probable) {
      das.sight$Prob[das.sight$Event == "p"] <- FALSE
      if (any(is.na(das.sight$Prob)))
        warning("A marine mammal sighting has an NA Prob value")

      validate(
        need(sum(das.sight$Prob) > 0,
             "There are no probable sightings in the loaded DAS file(s)")
      )

      # 977 used as probable vaquita sighting on some cruises
      das.sight <- das.sight %>%
        mutate(Sp = ifelse(.data$Sp == "977", "041", .data$Sp),
               Sp = ifelse(.data$Prob, .data$ProbSp, .data$Sp))
    }

    # Filter for selected species, and check that all selected species are in data
    das.sight <- das.sight %>% filter(.data$Sp %in% sp.codes)

    if (input$das_sighting_code_1_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$Sp)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings in the loaded DAS file(s):",
                   paste(sp.codes.none, collapse = ", ")))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 2) {
    # 2: Turtles
    validate(
      need(sum(das.sight$Event == "t") > 0,
           "There are no turtle sightings (t events) in the loaded DAS file(s)")
    )

    sp.codes <- cruzDasSightSpeciesTurtles()

    das.sight <- das.sight %>%
      filter(.data$Event %in% c("t"),
             .data$Sp %in% sp.codes)

    if (input$das_sighting_code_2_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$Sp)
      validate(
        need(length(sp.codes.none) == 0,
             paste("The following species code(s) does (do) not",
                   "have any sightings in the loaded DAS file(s):",
                   paste(sp.codes.none, collapse = ", ")))
      )
    }


    #--------------------------------------------------------------------------
  } else if (sight.type == 3) {
    # 3: Boats
    das.sight <- das.sight %>%
      filter(.data$Event == "F") %>%
      mutate(Sp = "Boat")
    sp.codes <- NULL

    validate(
      need(nrow(das.sight) > 0,
           "There are no boat sightings (F events) in the loaded DAS file(s)")
    )


    #--------------------------------------------------------------------------
  } else if (sight.type == 4) {
    # 4: C-PODs
    # C-POD sightings are entered as objects with sighting angle and distance
    # the string "cpod" in the comment on the next line indicates object is a CPOD
    sp.codes <- NULL

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
      mutate(Sp = "CPOD",
             Bearing = as.numeric(.data$Data2),
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

  # If "Plot all..." was selected, only return codes in sighting data
  if ((sight.type == 1 && input$das_sighting_code_1_all == 1) |
      (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
    sp.codes <- base::intersect(sp.codes, das.sight$Sp)
  }

  # Return list
  list(das.sight = das.sight, sight.type = sight.type, sp.codes = sp.codes,
       sp.selection = sp.selection)
})

###############################################################################
