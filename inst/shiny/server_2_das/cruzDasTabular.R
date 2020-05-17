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

  # Get filtered data, which also handles error checks
  data.list <- cruzDasSightRange()
  das.sight <- data.list$das.sight

  ### Filter by selected effort type if sightings are on-effort
  if(input$das_sightings_effort != 3) {
    das.sight <- das.sight %>%
      filter(toupper(.data$Mode) %in% input$das_out_sight_cp,
             toupper(.data$EffType) %in% input$das_out_sight_snf)
  }

  validate(
    need(nrow(das.sight) > 0,
         "No specified sightings occurred during selected mode/effort types")
  )

  das.sight.summ <- das.sight %>%
    group_by(.data$Sp) %>%
    summarise(std = sum(.data$EffType == "S"),
              nstd = sum(.data$EffType == "N"),
              fine = sum(.data$EffType == "F"),
              off_eff = sum(!.data$OnEffort),
              total = n())

  names(das.sight.summ) <- c("Species code", "Standard", "Non-standard",
                             "Fine", "Off effort", "Total")
  das.sight.summ
})

output$das_out_sight_save <- downloadHandler(
  filename = function() input$das_out_sight_save_name,
  content = function(file) {
    write.csv(cruzDasOutSight_Table(), file = file, row.names = FALSE)
  }
)


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
