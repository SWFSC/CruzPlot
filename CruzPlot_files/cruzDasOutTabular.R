### cruzDasOutTabular
## Code for tabular output of sighitngs and effort


###############################################################################
# Sightings

cruzDasOutSight_Table <- reactive({
  req(cruz.list$das.data)
  validate(
    need(input$das_sightings, 
         paste("'Plot sightings' must be selected to", 
               "generate sightings tabular output"))
  )
  
  data.all <- cruz.list$das.data
  data.list <- cruzDasSightRange() # Handles error message if nothing to plot
  data.sight <- data.list$data.sight
  data.sight <- data.sight[order(row.names(data.sight)),]
  sight.eff.filter <- input$das_sightings_effort
  
  ### Filter by selected effort type if sightings are on-effort
  if(sight.eff.filter != 3) {
    efftype.cp <- input$das.out.sight.closePass
    efftype.snf <- input$das.out.sight.snf
    
    data.sight <- data.sight[data.sight$EffType1 %in% efftype.cp,]
    data.sight <- data.sight[data.sight$EffType2 %in% efftype.snf,]
  }
  
  validate(
    need(nrow(data.sight) > 0,
         "No sighting events occurred during specified effort types")
  )
  
  ### Get proper code depending on sighting type
  ## Mammals
  if(input$das_sighting_type == 1) {
    data.sight.codes <- c(data.sight$Data5, data.sight$Data6, 
                          data.sight$Data7, data.sight$Data8)
    names(data.sight.codes) <- rep(1:nrow(data.sight), 4)
    data.sight.codes <- data.sight.codes[!is.na(data.sight.codes)]
    
    # All species: keep all sightings pulled out above
    # Selected species: filter by selected species codes
    if(input$das_sighting_code_1_all == 2) {
      sight.keep <- substr(input$das.sighting.code.1, 1, 3)
      data.sight.codes <- data.sight.codes[data.sight.codes %in% sight.keep]
    }
  }
  
  ## Turtles
  if(input$das_sighting_type == 2) {
    data.sight.codes <- data.sight$Data2
    names(data.sight.codes) <- 1:nrow(data.sight)
    
    # Turtles only have one data field for species code, thus nothing extra needed
  }
  
  ## Fishing boats
  if(input$das_sighting_type == 3) {
    data.sight.codes <- data.sight$Event
    names(data.sight.codes) <- 1:nrow(data.sight)
  }
  
  ### Process sightings so each row is one sighting of one species
  data.sight.df <- data.frame(idx = as.numeric(names(data.sight.codes)), 
                              sp.code = unname(data.sight.codes), 
                              stringsAsFactors = FALSE)
  data.sight.df$eff <- data.sight$OnEffort[data.sight.df$idx]
  data.sight.df$snf <- data.sight$EffType2[data.sight.df$idx]
  data.sight.df$idx.all <- as.numeric(row.names(data.sight)[data.sight.df$idx])
  
  
  # Attempt at getting average group size
  # if(input$das_sighting_type == 1) {
  #   x <- sapply(1:nrow(data.sight.df), function(i) {
  #     row.curr <- data.sight.df[i,]
  #     curr.all.idx <- row.curr$idx.all
  #     
  #     # Get events 1 - 3
  #     if(data.all$Event[curr.all.idx + 1] == "1") {
  #       data.curr.a123 <- data.all[curr.all.idx:(curr.all.idx + 3),]
  #       data.curr.a <- data.curr.a123[1,]
  #       data.curr.123 <- data.curr.a123[(data.curr.a123$Event %in% 1:3),]
  #       print(nrow(data.curr.123))
  #       
  #       y <- which(row.curr$sp.code == c(data.curr.a$Data5, data.curr.a$Data6, 
  #                                        data.curr.a$Data7, data.curr.a$Data8))
  #       y.idx <- y + 9 # index to data5 to data8 columns
  #       
  #       mean(as.numeric(data.curr.123$Data2) * 
  #              as.numeric(data.curr.123[,y.idx]) * 0.01)
  #     } else {
  #       NA
  #     }
  #   })
  # 
  # }
  
  
  ### Create new table summarised by species
  df.to.return <- data.sight.df %>% 
    group_by(sp.code) %>% 
    summarise(Std = sum(eff & snf == "S"), NonStd = sum(eff & snf == "N"), 
              Fine = sum(eff & snf == "F"), OffEff = sum(!eff), Total = n())
  df.to.return <- as.data.frame(df.to.return)
  df.to.return <- rbind(df.to.return, 
                        c("All", apply(df.to.return[,2:6], 2, sum)))
  if(input$das_sighting_type == 3) df.to.return <- df.to.return[1,]
  
  # sp.codes.all <- cruzSpecies()
  
  ### Set rows to NA if that type is not selected to plot
  if(sight.eff.filter == 3) {
    df.to.return[,c("Std", "NonStd", "Fine")] <- NA
  } else {
    if(sight.eff.filter == 2) df.to.return$OffEff <- NA
    if(!("S" %in% efftype.snf)) df.to.return$Std <- NA
    if(!("N" %in% efftype.snf)) df.to.return$NonStd <- NA
    if(!("F" %in% efftype.snf)) df.to.return$Fine <- NA
  }
  
  
  names(df.to.return) <- c("Species code", "Standard", "Non-standard", 
                           "Fine", "Off effort", "Total")
  df.to.return  
})

output$das_out_sight_save_name_uiOut_text <- renderUI({
  # cruzDasOutSight_Table()
  
  csv.name <- paste0("Sight_", Sys.time(), ".csv")
  csv.name <- gsub(" ", "_", csv.name)
  csv.name <- gsub(":", "-", csv.name)
  csv.name <- gsub("-", "", csv.name)
  
  textInput("das_out_sight_save_name", h5("Sightings file name"), 
            value = csv.name)
})

output$das_out_sight_save <- downloadHandler(
  filename = function() input$das_out_sight_save_name, 
  content = function(file) {
    write.csv(cruzDasOutSight_Table(), file = file, row.names = FALSE)
  }
)

# cruzDasOutSight_Save <- eventReactive(input$das_out_sight_save_execute, {
#   validate(
#     need(!is.null(cruz.list$das.data), "Please load DAS file")
#   )
# 
#   csv.towrite <- cruzDasOutSight_Table()
#   csv.name <- paste0("Outputs/", input$das_out_sight_save_name)
#   write.csv(csv.towrite, file = csv.name, quote = TRUE, row.names = FALSE)
# 
#   return("Saved to 'Outputs' folder")
# })


###############################################################################
# Effort
cruzDasOutEffort_Table <- reactive({
  data.list <- cruzDasEffort()
  data.effort <- data.list$data.effort
  data.effort.start <- data.effort[data.list$ndx.R,]
  data.effort.end <- data.effort[data.list$ndx.E,]
  efftype.snf <- input$das_effort_snf
  
  validate(
    need(nrow(data.effort.start) == nrow(data.effort.end), 
         "Odd number of points sent to cruzDasOutEffort")
  )
  
  # Calculate distance
  dist.effort.m <-  distVincentyEllipsoid(cbind(data.effort.start$Lon, 
                                                data.effort.start$Lat), 
                                          cbind(data.effort.end$Lon, 
                                                data.effort.end$Lat))
  dist.effort <- dist.effort.m / 1000
  if(input$das_out_effort_units == 2) dist.effort <- dist.effort / 1.852
  
  # Create initial df
  data.effort.df <- data.frame(bft = data.effort.start$Bft, 
                               snf = data.effort.start$EffType2, 
                               dist = dist.effort, stringsAsFactors = FALSE)
  # Add in 0's for bft levels from 0 - 6 that aren't yet included
  bft.toadd <- (0:6)[!(0:6 %in% unique(data.effort.df$bft))]
  if (length(bft.toadd) == 0) {
    bft.toadd.df <- NULL
  } else {
    bft.toadd.df <- data.frame(bft = bft.toadd, snf = "S", dist = 0)
  }
  data.effort.df <- rbind(data.effort.df, bft.toadd.df)
  
  
  # Create df grouped by bft
  eff.to.return <- data.effort.df %>% 
    group_by(bft) %>%
    summarise(Std = sum(dist[snf == "S"]), NonStd = sum(dist[snf == "N"]), 
              Fine = sum(dist[snf == "F"]), Total = sum(dist))
  eff.to.return <- as.data.frame(eff.to.return)
  eff.to.return <- rbind(eff.to.return, 
                         c(1, apply(eff.to.return[,2:5], 2, sum)))
  eff.to.return$bft[length(eff.to.return$bft)] <- "All"
  
  # Set rows to NA if that type is not selected to plot
  if(input$das_effort == 3) {
    if(!("S" %in% efftype.snf)) eff.to.return$Std <- NA
    if(!("N" %in% efftype.snf)) eff.to.return$NonStd <- NA
    if(!("F" %in% efftype.snf)) eff.to.return$Fine <- NA
  }
  
  names(eff.to.return) <- c("Beaufort", "Standard", "Non-standard", 
                            "Fine", "Total")
  
  eff.to.return
})


output$das_out_effort_save_name_uiOut_text <- renderUI({
  # cruzDasOutEffort_Table()
  
  csv.name <- paste0("Effort_", Sys.time(), ".csv")
  csv.name <- gsub(" ", "_", csv.name)
  csv.name <- gsub(":", "-", csv.name)
  csv.name <- gsub("-", "", csv.name)
  
  textInput("das_out_effort_save_name", h5("Effort file name"), 
            value = csv.name)
})

output$das_out_effort_save <- downloadHandler(
  filename = function() input$das_out_effort_save_name, 
  content = function(file) {
    write.csv(cruzDasOutEffort_Table(), file = file, row.names = FALSE)
  }
)

# cruzDasOutEffort_Save <- eventReactive(input$das_out_effort_save_execute, {
#   validate(
#     need(!is.null(cruz.list$das.data), "Please load DAS file")
#   )
#   
#   csv.towrite <- cruzDasOutEffort_Table()
#   csv.name <- paste0("Outputs/", input$das_out_effort_save_name)
#   write.csv(csv.towrite, file = csv.name, quote = TRUE, row.names = FALSE)
#   
#   return("Saved to 'Outputs' folder")
# })
