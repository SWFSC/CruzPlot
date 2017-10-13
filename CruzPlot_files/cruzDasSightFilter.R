# cruzDasSightFilter for CruzPlot by Sam Woodman
#   cruzDasSightFilterEffort() returns row numbers of data.sight that satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns row numbers of data.sight that satisfy the beaufort filter
#   cruzDasSightFilterDate() returns row numbers of data.sight that satisfy the date filter
#   cruzDasSightFilterCruise() returns row numbers of data.sight entries from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns row numbers of data.sight entries within the given truncation distance
#   cruiseDasSightFilter() returns list of data frame containing data for selected species that satisfy the filters, 
#     counts for each selected species, and selected species codes

## Filter sightings data
# On/off effort
cruzDasSightFilterEffort <- reactive({
  data.sight <- cruzDasSightSpecies()$data.sight
  sight.eff <- input$das_sightings_effort
  ndx.keep <- seq(1:length(data.sight[,1]))
  
  if(sight.eff == 2) ndx.keep <- which(data.sight$OnEffort == TRUE)
  if(sight.eff == 3) ndx.keep <- which(data.sight$OnEffort == FALSE)
  
  return(ndx.keep)
})

# Beaufort
cruzDasSightFilterBeaufort <- reactive({
  data.sight <- cruzDasSightSpecies()$data.sight
  bft.min <- input$das.sight.minBeau
  bft.max <- input$das.sight.maxBeau
  
  ndx.keep <- which((data.sight$Bft >= bft.min) & (data.sight$Bft <= bft.max))
  
  validate(
    need(length(ndx.keep) != 0,
         message = "No sightings match the given beaufort filters")
  )
  
  return(ndx.keep)
})

# Dates
cruzDasSightFilterDate <- reactive({
  data.sight <- cruzDasSightSpecies()$data.sight
  date.range <- input$das.sight.dateRange
  
  date.keep <- rep(FALSE, length(data.sight[,1]))
  if(length(data.sight[,1]) != 0) {
    for(i in 1:length(data.sight[,1])) {
      current.date <- substring(data.sight$Date[i], 1, 10)
      diff.min <- difftime(current.date, date.range[1],units="days")
      diff.max <- difftime(current.date, date.range[2],units="days")-1
      if((diff.min >= 0 ) && (diff.max <= 0)) date.keep[i] <- TRUE
    }
    ndx.keep <- which(date.keep)
  }
  validate(
    need(length(ndx.keep) != 0,
         message = "No sightings match the given date filters")
  )
  
  return(ndx.keep)
})

# Cruise numbers
cruzDasSightFilterCruise <- reactive({ 
  data.sight <- cruzDasSightSpecies()$data.sight
  cruise.nums.in <- input$das.sight.cruiseNum
  ndx.keep <- seq(1:length(data.sight[,1]))
  
  if(cruise.nums.in != "") {
    cruise.nums <- as.numeric(unlist(strsplit(cruise.nums.in, split = ",")))
    cruise.present <- cruise.nums %in% data.sight$Cruise
    cruise.absent.num <- cruise.nums[!cruise.present]
    validate(
      need(all(cruise.present), 
           message = paste("Based on given filter parameters, no sightings found for cruise number", cruise.absent.num))
    )
    ndx.keep <- which(data.sight$Cruise %in% cruise.nums)
  }
  
  return(ndx.keep)
})

# Truncation
cruzDasSightFilterTrunc <- reactive({
  req(cruz.list$das.data)
  
  data.all <- cruz.list$das.data
  data.list <- cruzDasSightSpecies()
  data.sight <- data.list$data.sight
  sight.type <- data.list$sight.type
  
  ### Convert truncation distance 
  trunc.dist <- input$das.sight.trunc
  isolate(trunc.units <- input$das.sight.trunc.units) 
  # ^ changing trunc.units always changes trunc.dist
  
  ndx.keep <- seq(1:nrow(data.sight))
  
  if(!is.na(trunc.dist)) {
    validate(
      need(trunc.dist > 0, 
           "Please ensure truncation distance is greater than 0")
    )
    
    angle <- data.sight$angle
    perp.dist.nmi <- data.sight$perp.dist.nmi
    if (trunc.units == 2) { 
      perp.dist <- perp.dist.nmi
    } else {
      perp.dist <- perp.dist.nmi * 1.852
    }
    
    ndx.keep <- which(perp.dist < trunc.dist)
  }
  
  validate(
    need(length(ndx.keep) != 0,
         "No sightings are within the given truncation distance")
  )
  
  return(ndx.keep)
})

cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightSpecies()
  data.temp <- data.list$data.sight
  sight.type <- data.list$sight.type
  sp.codes <- data.list$sp.codes
  
  num.keep1 <- cruzDasSightFilterEffort()
  num.keep2 <- cruzDasSightFilterBeaufort()
  num.keep3 <- cruzDasSightFilterDate()
  num.keep4 <- cruzDasSightFilterCruise()
  num.keep5 <- cruzDasSightFilterTrunc()
  
  num.keep <- unique(c(num.keep1, num.keep2, num.keep3, num.keep4, num.keep5))
  
  num.keep <- num.keep[num.keep%in%num.keep1 & 
                         num.keep%in%num.keep2 & 
                         num.keep%in%num.keep3 & 
                         num.keep%in%num.keep4 & 
                         num.keep%in%num.keep5]
  data.sight <- data.temp[num.keep,]
  
  if(sight.type == 1 && input$das_sighting_code_1_all == 2) {
    temp.all <- sapply(sp.codes, function(i) i %in% data.sight$Data5 || i %in% data.sight$Data6 ||
                         i %in% data.sight$Data7 || i %in% data.sight$Data8)
    sp.code.false <- sp.codes[which(!temp.all)]
    validate(
      need(all(temp.all), 
           message = paste("Species with code", sp.code.false, 
                           "does not have any sightings that match the given filters"))
    )
  }
  if(sight.type == 2 && input$das_sighting_code_2_all == 2) {
    temp.all <- sapply(sp.codes, function(i) i %in% data.sight$Data2)
    sp.code.false <- sp.codes[which(!temp.all)]
    validate(
      need(all(temp.all), 
           message = paste("Species with code", sp.code.false, 
                           "does not have any sightings that match the given filters"))
    )
  }
  
  return(list(data.sight = data.sight, sight.type = sight.type, sp.codes = data.list$sp.codes))
})