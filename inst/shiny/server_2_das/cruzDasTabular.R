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
              total = n())

  # 'Filter' summary for columns specified by sighting filters, and rename
  #   Data has already been filtered, just need to make the table pretty
  if (input$das_sight_effort == 3) {
    das.sight.summ$std <- NA
    das.sight.summ$nstd <- NA
    das.sight.summ$fine <- NA

  } else {
    if (input$das_sight_effort == 2) das.sight.summ$off_eff <- NA

    if (!("S" %in% input$das_sight_snf)) das.sight.summ$std <- NA
    if (!("N" %in% input$das_sight_snf)) das.sight.summ$nstd <- NA
    if (!("F" %in% input$das_sight_snf)) das.sight.summ$fine <- NA
  }

  das.sight.summ.all <- if (input$das_out_allcheck) {
    c(list(`Species code` = "All"), lapply(select(das.sight.summ, -Sp), sum))
  } else {
    NULL
  }

  # Add in selected species identifiers, join, and do name wrangling
  das.sight.sp <- das.sight.summ %>%
    select(.data$Sp) %>%
    left_join(req(cruz.list$sp.codes), by = c("Sp" = "Code"))

  das.sight.sp %>%
    rename("Species code" = .data$Sp, "Abbreviation" = .data$Abbr,
           "Scientific name" = .data$Name_Scientific,
           "Common name" = .data$Name_Common) %>%
    select(c(1, as.numeric(input$das_out_sciname))) %>%
    left_join(das.sight.summ, by = c("Species code" = "Sp")) %>%
    bind_rows(das.sight.summ.all) %>%
    rename("Standard" = .data$std, "Non-standard" = .data$nstd,
           "Fine" = .data$fine, "Off effort" = .data$off_eff, "Total" = .data$total)
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
  das.eff.lines <- cruzDasEffortRange()

  # Calculate distance
  # TODO - provide flexible distance calculation method?
  dist.effort.m <- geosphere::distVincentyEllipsoid(
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
    eff.out <- data.frame(
      Bft = "All",
      std = sum(das.eff.lines$dist[das.eff.lines$EffType == "S"]),
      nstd = sum(das.eff.lines$dist[das.eff.lines$EffType == "N"]),
      fine = sum(das.eff.lines$dist[das.eff.lines$EffType == "F"]),
      total = sum(das.eff.lines$dist),
      stringsAsFactors = FALSE
    )

  } else if (input$das_effort == 3) {
    eff.summ <- das.eff.lines %>%
      group_by(Bft) %>%
      summarise(std = sum(dist[EffType == "S"]),
                nstd = sum(dist[EffType == "N"]),
                fine = sum(dist[EffType == "F"]),
                total = sum(dist))

    eff.out <- rbind(eff.summ, vapply(eff.summ, sum, 1)) %>%
      mutate(Bft = c(head(Bft, -1), "All"))

  } else {
    validate("Error: invalid 'Effort to plot' (das_effort) selection")
  }

  # 'Filter' summary for columns specified by effort type filters, and rename
  if (!("S" %in% input$das_effort_snf)) eff.out$std <- NA
  if (!("N" %in% input$das_effort_snf)) eff.out$nstd <- NA
  if (!("F" %in% input$das_effort_snf)) eff.out$fine <- NA

  eff.out %>%
    rename(Beaufort = Bft, Standard = std, `Non-standard` = nstd,
           Fine = fine, Total = total)


  # name.total <- paste("Total", ifelse(input$das_out_effort_units == 1, "(km)", "(nmi)"))
  # names(eff.out) <- c(head(names(eff.out), -1), name.total)
  # eff.out
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
