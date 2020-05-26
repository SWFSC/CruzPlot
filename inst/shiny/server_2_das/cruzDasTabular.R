### cruzDasOutTabular
## Code for tabular output of sighitngs and effort


###############################################################################
# Sightings table
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

  das.sight.summ <- das.sight %>%
    group_by(.data$Sp) %>%
    summarise(std = sum(.data$EffType == "S"),
              nstd = sum(.data$EffType == "N"),
              fine = sum(.data$EffType == "F"),
              off_eff = sum(!.data$OnEffort),
              total = n()) %>%
    rename("Sp" = .data$Sp, "Standard" = .data$std, "Non-standard" = .data$nstd,
           "Fine" = .data$fine, "Off effort" = .data$off_eff, "Total" = .data$total)


  das.sight.sp <- das.sight.summ %>%
    select(.data$Sp) %>%
    left_join(req(cruz.list$sp.codes), by = c("Sp" = "Code")) %>%
    rename("Species code" = .data$Sp, "Abbreviation" = .data$Abbr,
           "Scientific name" = .data$Name_Scientific,
           "Common name" = .data$Name_Common)

  das.sight.sp %>%
    select(c(1, as.numeric(input$das_out_sciname))) %>%
    left_join(das.sight.summ, by = c("Species code" = "Sp"))
})


# Download sightings table
output$das_out_sight_save <- downloadHandler(
  filename = function() {
    gsub("-", "", paste0("CruzPlot_sightings_", Sys.Date(), ".csv"))
  },

  content = function(file) {
    write.csv(cruzDasOutSight_Table(), file = file, row.names = FALSE)
  }
)


###############################################################################
# Effort
cruzDasOutEffort_Table <- reactive({
  das.eff.lines <- cruzDasEffort()

  # Calculate distance
  # TODO - what distance calculation method to use?
  dist.effort.m <-  geosphere::distVincentyEllipsoid(
    cbind(das.eff.lines$st_lon, das.eff.lines$st_lat),
    cbind(das.eff.lines$end_lon, das.eff.lines$end_lat)
  )

  dist.effort.km <- dist.effort.m / 1000
  das.eff.lines$dist <-  if (input$das_out_effort_units == 2) {
    dist.effort.km / 1.852
  } else {
    dist.effort.km
  }

  # Create summary tables
  if (input$das_effort == 2) {
    data.frame(
      Bft = "All",
      Standard = sum(das.eff.lines$dist[das.eff.lines$EffType == "S"]),
      `Non-standard` = sum(das.eff.lines$dist[das.eff.lines$EffType == "N"]),
      Fine = sum(das.eff.lines$dist[das.eff.lines$EffType == "F"]),
      Total = sum(das.eff.lines$dist),
      stringsAsFactors = FALSE
    )

  } else if (input$das_effort == 3) {
    eff.summ <- das.eff.lines %>%
      group_by(Bft) %>%
      summarise(Standard = sum(dist[EffType == "S"]),
                Non_standard = sum(dist[EffType == "N"]),
                Fine = sum(dist[EffType == "F"]),
                Total = sum(dist))

    eff.summ.all <- rbind(eff.summ, vapply(eff.summ, sum, 1)) %>%
      mutate(Bft = c(head(Bft, -1), "All")) %>%
      rename(`Non-standard` = Non_standard)

  } else {
    validate("Error: invalid 'Effort to plot' (das_effort) selection")
  }
})


# Save effort table
output$das_out_effort_save <- downloadHandler(
  filename = function() {
    u.txt <- switch(as.numeric(input$das_out_effort_units), "km", "nmi")
    gsub("-", "", paste0("CruzPlot_effort_", u.txt, "_", Sys.Date(), ".csv"))
  },

  content = function(file) {
    write.csv(cruzDasOutEffort_Table(), file = file, row.names = FALSE)
  }
)

###############################################################################
