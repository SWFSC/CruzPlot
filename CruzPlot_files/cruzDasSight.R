# cruzDasSight for CruzPlot by Sam Woodman
#   cruzDasSightSpeciesMammals() returns mammal species codes selected by user
#   cruzDasSightSpeciesTurtles() returns turtle species codes selected by user
#   cruzDasSightSpecies() returns list of data frames containing data for selected species sightings, 
#	    sighting type, and species codes; also computes sighting location based on angle and distance;
#     adds sight.lat, sight.lon, angle, distance (nmi) to data.sight dataframe


cruzDasSightSpeciesMammals <- reactive({
  if(input$das_sighting_code_1_all == 1) sp.code.all <- cruzSpeciesMammals()$Code
  if(input$das_sighting_code_1_all == 2) {
    sp.code.all <- gsub(" ", "", substring(input$das.sighting.code.1, 1, 3))
    validate(
      need(length(sp.code.all) != 0, message = "Please choose at least one valid mammal species code")
    )
  }
  
  return(sp.code.all)
})

cruzDasSightSpeciesTurtles <- reactive({
  if(input$das_sighting_code_2_all == 1) sp.code.all <- cruzSpeciesTurtles()$Code
  if(input$das_sighting_code_2_all == 2) {
    sp.code.all <- substring(input$das.sighting.code.2, 1, 2)
    validate(
      need(length(sp.code.all) != 0, message = "Please choose at least one valid turtle species code")
    )
  }
  
  return(sp.code.all)
})


cruzDasSightSpecies <- reactive({
  req(cruz.list$das.data)
  
  data.all <- cruz.list$das.data
  
  ## Sightings to plot
  # Filter by type of sighting (Mammal(A), Turtle(t), Boat(F)) and species code for M and T
  sight.type <- input$das_sighting_type
  # 1: Mammals
  if(sight.type == 1) {
    data.all$num <- seq(1:length(data.all[,1]))
    data.sight <- data.all[data.all$Event == "A", ]
    
    # Update probable sightings species if necessary
    if(input$das.sighting.probable) {
      prob <- data.all[data.all$Event == "?", ]
      
      validate(
        need(nrow(prob) != 0, 
             "There are no probable sightings in given .DAS file")
      )
      
      prob.loc <- prob$num - 1
      data.sight[which(data.sight$num %in% prob.loc),]$Data5 <- prob$Data5
      data.sight[which(data.sight$num %in% prob.loc),]$Data6 <- prob$Data6
      data.sight[which(data.sight$num %in% prob.loc),]$Data7 <- prob$Data7
      data.sight[which(data.sight$num %in% prob.loc),]$Data8 <- prob$Data8
      # special code for possible vaquita sighting
      prob.vaq <- which(data.all$Data5 == "977")
      data.sight[which(data.sight$num %in% prob.vaq),]$Data5 <- "041"
      # Data1 method
      # data.sight[which(data.sight$Data1 %in% prob$Data1),]$Data5 <- prob$Data5
      # data.sight[which(data.sight$Data1 %in% prob$Data1),]$Data6 <- prob$Data6
      # data.sight[which(data.sight$Data1 %in% prob$Data1),]$Data7 <- prob$Data7
      # data.sight[which(data.sight$Data1 %in% prob$Data1),]$Data8 <- prob$Data8
    }
    
    data.all <- data.all #[,1:15]
    data.sight <- data.sight #[,1:15]
    
    sp.code.all <- cruzDasSightSpeciesMammals()
    sp.code.len <- length(sp.code.all)
    
    ndx.sp <- NULL
    for(i in 1:sp.code.len)
    {
      ndx <- c(which(data.sight$Data5 == sp.code.all[i]), 
               which(data.sight$Data6 == sp.code.all[i]), 
               which(data.sight$Data7 == sp.code.all[i]), 
               which(data.sight$Data8 == sp.code.all[i]))
      ndx.sp <- c(ndx.sp, ndx)
    }
    data.sight <- data.sight[ndx.sp, ]
    data.sight <- data.sight[!duplicated(data.sight$Data1),]  # if a sighting is matched (2 teams), it has the same sighting number
    # this code might be a problem with sighting numbers starting
    # over each day in data for 1990 and earlier
    
    ndx <- as.numeric(rownames(data.sight))-1    # row names are the indices of the A records
    data.temp <- data.all[ndx,]
    angle <- as.numeric(data.temp$Data5)
    dist.nmi <- as.numeric(data.temp$Data7)
    
    if(input$das_sighting_code_1_all == 2) {                # selected species codes
      temp.all <- sapply(sp.code.all, function(i) i %in% data.sight$Data5 || 
                           i %in% data.sight$Data6 ||
                           i %in% data.sight$Data7 || 
                           i %in% data.sight$Data8)
      sp.code.false <- sp.code.all[which(!temp.all)]
      validate(
        need(all(temp.all), 
             message = paste("Species with code", sp.code.false, 
                             "does not have any sightings in the data"))
      )
    }
  }
  
  # 2: Turtles
  if(sight.type == 2) {
    data.sight <- data.all[data.all$Event == "t", ]
    
    validate(
      need(nrow(data.sight) != 0, 
           "There are no turtle sightings in given .DAS file")
    )
    
    sp.code.all <- cruzDasSightSpeciesTurtles()
    sp.code.len <- length(sp.code.all)
    data.temp <- NULL
    for(i in 1:sp.code.len)
    {
      sp.code <- sp.code.all[i]
      temp <- which(data.sight$Data2 == sp.code)
      data.temp <- c(data.temp, temp)
    }
    data.sight <- data.sight[data.temp, ]
    
    ndx <- as.numeric(rownames(data.sight))
    angle <- as.numeric(data.sight$Data3)
    dist.nmi <- as.numeric(data.sight$Data4)
    
    if(input$das_sighting_code_2_all == 2) {
      temp.all <- sapply(sp.code.all, function(i) i %in% data.sight$Data2)
      sp.code.false <- sp.code.all[which(!temp.all)]
      
      validate(
        need(all(temp.all), 
             message = paste("Species with code", sp.code.false, 
                             "does not have any sightings in the given data"))
      )
    }
  }
  
  # 3: Boats
  if(sight.type == 3) {
    data.sight <- data.all[data.all$Event == "F", ]
    
    validate(
      need(nrow(data.sight) != 0, 
           "There are no boat sightings in given .DAS file")
    )
    
    ndx <- as.numeric(rownames(data.sight))
    angle <- as.numeric(data.sight$Data2)
    dist.nmi <- as.numeric(data.sight$Data3)
    sp.code.all <- NULL
  }
  
  # 4: C-PODs
  # C-POD sightings are entered as objects with sighting angle and distance
  # the string "cpod" in the comment on the next line indicates object is a CPOD
  if(sight.type == 4) {
    ndx.X <- which(data.all$Event == "X")
    
    validate(
      need(length(ndx.X) != 0, 
           "There are no boat sightings in given .DAS file")
    )
    
    comment.str.df <- data.all[,6:13]
    comment.str <- apply(comment.str.df,1,paste,collapse="")
    ndx.cpod <- grep("cpod",comment.str)
    ndx <- ndx.X[(ndx.X+1) %in% ndx.cpod]   
    data.sight <- data.all[ndx,]
    angle <- as.numeric(data.sight$Data2)
    dist.nmi <- as.numeric(data.sight$Data4)
    sp.code.all <- NULL
  }
  
  
  ### Check to see if any sightings match species code
  validate( 
    need(length(data.sight[,1]) > 0, 
         message = paste("No sightings exist for the selected code(s)"))
  )
  
  
  ### Compute lat and lon of sighted object from sighting angle, ship course, ship bearing and distance
  angle[is.na(angle)] <- 0
  dist.nmi[is.na(dist.nmi)] <- 0  # Distance in DAS file is nmi
  course <- cruz.list$das.data$Course[ndx]
  ship.bearing <- cruz.list$das.data$Bearing[ndx]
  sight.bearing <- ifelse(is.na(ship.bearing), course+angle, ship.bearing+angle) %% 360
  dist.m <- dist.nmi*1852  # dist in m
  sight.loc <- destPoint(matrix(c(data.sight$Lon, data.sight$Lat), ncol=2), 
                         sight.bearing, dist.m)  # location of sighting
  
  data.sight$sight.lon <- sight.loc[,"lon"]
  data.sight$sight.lat <- sight.loc[,"lat"]
  data.sight$angle <- angle
  data.sight$dist.nmi <- dist.nmi
  data.sight$perp.dist.nmi <- abs(dist.nmi * sin(angle / 180 * pi))
  
  return(list(data.sight = data.sight, sight.type = sight.type, sp.codes = sp.code.all))
})
