# cruzDasEffort for CruzPlot - file 1 of effort processing
#   cruzDasEffortEvent() returns selected and filtered effort data
#   Do filter stuff in cruzDasEffortFilter()

###############################################################################
cruzDasEffortEvent <- reactive({
  #----------------------------------------------------------------------------
  req(input$das_effort != 0)
  das.proc <- req(cruz.list$das.data)

  eff.events <- if (input$das_effort == 2) c("R", "E") else c("R", "V", "E")

  das.eff <- das.proc %>%
    filter(.data$OnEffort | .data$Event == "E",
           .data$Event %in% eff.events)

  validate(
    need(sum(das.eff$Event == "R") == sum(das.eff$Event == "E"),
         "There are not an equal number of R and E events in the data")
  )
  validate(
    need(all((which(das.eff$Event == "E") - which(das.eff$Event == "R")) > 0),
         "R and E events do not properly alternate"),
    need(identical(tail(das.eff$Event, 1), "E"),
         "The DAS data effort must end with an E event")
  )


  # For simplified effort, we don't need Beaufort values
  # For detailed effort, we remove R to V events; distance should be 0
  # Each continuous effort section will end with an E event
  das.eff.lines <- das.eff %>%
    mutate(st_lat = .data$Lat,
           st_lon = .data$Lon,
           end_lat = c(.data$Lat[-1], NA),
           end_lon = c(.data$Lon[-1], NA)) %>%
    filter(.data$Event != "E")

  if (input$das_effort == 3)
    das.eff.lines <- das.eff.lines %>% filter(Event != "R")


  #----------------------------------------------------------------------------
  # Verbosely remove any effort lines with NA lat/lon and return
  ll.na <- which(
    is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$end_lat) |
      is.na(das.eff.lines$st_lon) | is.na(das.eff.lines$end_lon)
  )
  if (length(ll.na) > 0) {
    table.out <- das.eff.lines %>%
      slice(ll.na) %>%
      select(Event, DateTime, Lat, Lon, OnEffort,
             Cruise, file_das, line_num) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise))
    txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following effort", txt.out, "had an NA latitude or longitude value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      tags$br(),
      tags$h5("This notice will not be shown again unless a new DAS file is loaded"),
      easyClose = FALSE,
      size = "l"
    ))
  }


  # Filter for non-NA lines
  das.eff.lines %>%
    filter(!is.na(.data$st_lat), !is.na(.data$end_lat),
           !is.na(.data$st_lon), !is.na(.data$end_lon))
})

###############################################################################





# Turn off effort legend when switching to Simplified Effort
observeEvent(input$das_effort, {
  if (input$das_effort == 2) {
    updateCheckboxInput(session, "eff_legend", value = FALSE)
    cruz.eff.leg(FALSE)

    if (input$eff_legend_title == "Effort by Beaufort") {
      updateTextInput(session, "eff_legend_title", value = "")
      cruz.eff.leg.title("")
    }
  }

  if (input$das_effort == 3) {
    updateCheckboxInput(session, "eff_legend", value = TRUE)
    cruz.eff.leg(TRUE)

    if (input$eff_legend_title == "") {
      updateTextInput(session, "eff_legend_title", value = "Effort by Beaufort")
      cruz.eff.leg.title("Effort by Beaufort")
    }
  }
})

# Use reactiveVals so that map doesn't have to plot twice due to update
observeEvent(input$eff_legend, {
  cruz.eff.leg(input$eff_legend)
})

observeEvent(input$eff_legend_title, {
  cruz.eff.leg.title(input$eff_legend_title)
})


# Get and return parameters for effort legend
cruzDasEffortLegend <- reactive({
  req(input$das_effort != 1)

  ### General parameters, set in Legends section
  eff.leg.pos <- input$eff_legend_pos
  if (eff.leg.pos == 1) {
    validate(
      need(!is.na(input$eff_legend_lat), "Please enter a valid effort legend latitude value"),
      need(!is.na(input$eff_legend_lon), "Please enter a valid effort legend longitude value")
    )
    eff.leg.x = input$eff_legend_lon
    eff.leg.y = input$eff_legend_lat

  } else {
    eff.leg.x <- eff.leg.pos
    eff.leg.y <- NULL
  }

  font.fam <- font.family.vals[as.numeric(input$eff_legend_font)]

  # eff.leg.title <- if (input$eff_legend_title == "") NULL else input$eff_legend_title
  eff.leg.title <- if (cruz.eff.leg.title() == "") NULL else cruz.eff.leg.title()

  eff.leg.bty <- ifelse(input$eff_legend_boxCol == 1, "n", "o")
  eff.leg.box.col <- ifelse(input$eff_legend_boxCol == 2, NA, "black")
  eff.leg.box.lwd <- ifelse(input$eff_legend_boxCol == 2, 0, 1)
  eff.leg.box.cex <- input$eff_legend_textSize


  ### Parameters that are effort-type specific
  if (input$das_effort == 2) {
    # Simplified effort
    eff.leg.lab <- "Simplified effort"
    eff.leg.col <- input$das_effort_simp_col
    eff.leg.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    # Detailed effort
    if (input$das_effort_det_byBft) {
      # Detailed effort - plot by Bft
      bft.range <- cruzDasEffortFilterBeaufortVal()
      bft.which <- (bft.range[1]:bft.range[2]) + 1
      validate(
        need(length(bft.which) <= 10, "Beaufort legend error 1"),
        need(length(bft.which) <= length(input$das_effort_det_bft_col),
             "Beaufort legend error 2")
      )

      eff.leg.lab <- (0:9)[bft.which]
      eff.leg.col <- input$das_effort_det_bft_col[bft.which]
      eff.leg.lwd <- input$das_effort_det_bft_lwd

    } else {
      # Detailed effort - plot by S/N/F
      snf.idx <- which(c("S", "N", "F") %in% req(input$das_effort_snf))
      eff.leg.lab <- c("Standard", "Non-standard", "Fine")[snf.idx]
      eff.leg.col <- c(input$das_effort_det_col_s, input$das_effort_det_col_n, input$das_effort_det_col_f)[snf.idx]
      eff.leg.lwd <- c(input$das_effort_det_lwd_s, input$das_effort_det_lwd_n, input$das_effort_det_lwd_f)[snf.idx]
    }
  }


  ### Return list
  list(
    eff.leg.x = eff.leg.x, eff.leg.y = eff.leg.y,
    eff.leg.title = eff.leg.title, eff.leg.lab = eff.leg.lab,
    eff.leg.col = eff.leg.col, eff.leg.lwd = eff.leg.lwd,
    eff.leg.bty = eff.leg.bty, eff.leg.box.col = eff.leg.box.col,
    eff.leg.box.lwd = eff.leg.box.lwd,
    eff.leg.box.cex = eff.leg.box.cex, font.fam = font.fam
  )
})





# File 2 of effort processing
#   cruzDasEffortRange() subsets effort data to map range
#   cruzDasEffortFilter() returns filtered effort data - with helper functions
#   cruzDasEffortParams() takes filtered data and returns plotting params


###############################################################################
###############################################################################
### Final effort function - gets filtered data from cruzDasEffortFilter()
cruzDasEffortRange <- reactive({
  das.eff.lines <- cruzDasEffortEvent()

  # Check for any effort lines with NA coordinates
  ll.na <- sum(is.na(das.eff.lines$st_lat) | is.na(das.eff.lines$st_lon) |
                 is.na(das.eff.lines$end_lat) | is.na(das.eff.lines$end_lon))
  validate(
    need(ll.na == 0,
         "Error processing effort line positions - please report this as an issue")
  )

  # Adjust longitudes if world2 map is being used
  if (cruz.map.range$world2) {
    das.eff.lines <- das.eff.lines %>%
      mutate(st_lon = ifelse(.data$st_lon < 0, .data$st_lon + 360, .data$st_lon),
             end_lon = ifelse(.data$end_lon < 0, .data$end_lon + 360, .data$end_lon))
  }

  # Remove any effort lines with both st and end points outside map range
  lon.range <- cruz.map.range$lon.range
  lat.range <- cruz.map.range$lat.range

  das.eff.lines.range <- das.eff.lines %>%
    filter(between(.data$st_lat, lat.range[1], lat.range[2]),
           between(.data$st_lon, lon.range[1], lon.range[2]),
           between(.data$end_lat, lat.range[1], lat.range[2]),
           between(.data$end_lon, lon.range[1], lon.range[2]))

  validate(
    need(nrow(das.eff.lines.range) > 0,
         "No effort lines are completely within the map boundaries")
  )

  # Return
  das.eff.lines.range
})


###############################################################################
###############################################################################
observeEvent(input$das_sightings, {
  if (!input$das_sightings) {
    updateCheckboxInput(session, "das_effort_filter_same", value = FALSE)
  }
})


###############################################################################
# Top-level function for filtering effort line data
cruzDasEffortFilter <- reactive({
  #Called in draw_setVals

  das.eff.lines <- cruzDasEffortRange()

  if (input$das_effort_filter_same) {
    validate(
      need(input$das_sightings,
           "Sightings must be plotted to use the sighting filters for effort lines")
    )
  }

  ### Collect logical vectors
  keep1 <- cruzDasEffortFilterMode()
  keep2 <- cruzDasEffortFilterEfftype()
  keep3 <- if (input$das_effort == 3) cruzDasEffortFilterBeaufort() else TRUE
  keep4 <- cruzDasEffortFilterDate()
  keep5 <- cruzDasEffortFilterCruise()

  keep.all <- keep1 & keep2 & keep3 & keep4 & keep5
  keep.all.na <- which(is.na(keep.all))

  ### Show modal if any filter values are NA
  # Can be this simple b/c (NA & F) output is (F)
  if (length(keep.all.na) > 0) {
    table.out <- das.eff.lines %>%
      slice(keep.all.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise)) %>%
      select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
             # File = file_das, #Can take up too much space
             `Line number` = line_num)
    if (input$das_effort != 3) table.out <- table.out %>% select(Bft)

    txt.out <- ifelse(nrow(table.out) == 1, "line had", "lines have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This line", "These lines")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following effort", txt.out, "at least one NA filter value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      easyClose = TRUE,
      footer = "Click anywhere or press any button to close this notice",
      size = "l"
    ))
  }

  keep.all[is.na(keep.all)] <- FALSE
  das.eff.lines.filt <- das.eff.lines[keep.all, ]

  ### Final checks and return
  validate(
    need(sum(is.na(das.eff.lines.filt$Event)) == 0,
         "Error in CruzPlot effort filtering - please report this as an issue")
  )
  validate(need(nrow(das.eff.lines.filt) > 0, "No effort lines match the provided filters"))

  das.eff.lines.filt
})


###############################################################################
# Individual filters filter for indicies of data.effort to keep

#------------------------------------------------------------------------------
.func_eff_filt_validate <- function(x, x.txt) {
  if (anyNA(x)) warning(paste("some", x.txt, "filter values were NA"))

  validate(
    need(any(x), paste("No effort lines within the map range",
                       "match the given", x.txt, "filter"))
  )
  x
}


#------------------------------------------------------------------------------
### Closing/passing mode filter
cruzDasEffortFilterMode <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  keep <- das.eff.lines$Mode %in% input$das_effort_cp
  .func_eff_filt_validate(keep, "mode (closing/passing)")
})


#------------------------------------------------------------------------------
### S/N/F effort type filter
cruzDasEffortFilterEfftype <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  keep <- das.eff.lines$EffType %in% input$das_effort_snf
  .func_eff_filt_validate(keep, "effort type (standard/non-standard/fine)")
})


#------------------------------------------------------------------------------
### Beaufort filter
cruzDasEffortFilterBeaufortVal <- reactive({
  # Separate function to be used in Legend
  if (input$das_effort_filter_same) {
    eff.bft.min <- as.numeric(input$das_sight_minBft)
    eff.bft.max <- as.numeric(input$das_sight_maxBft)
  } else {
    eff.bft.min <- as.numeric(input$das_effort_minBft)
    eff.bft.max <- as.numeric(input$das_effort_maxBft)
  }

  validate(
    need(eff.bft.min <= eff.bft.max,
         "Effort filter: minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  c(eff.bft.min, eff.bft.max)
})

cruzDasEffortFilterBeaufort <- reactive ({
  das.eff.lines <- cruzDasEffortRange()
  bft.vals <- cruzDasEffortFilterBeaufortVal()

  keep <- if (identical(bft.vals, c(0, 9))) {
    TRUE
  } else {
    between(das.eff.lines$Bft, bft.vals[1], bft.vals[2])
  }
  .func_eff_filt_validate(keep, "Beaufort")
})


#------------------------------------------------------------------------------
### Date Filter
cruzDasEffortFilterDate <- reactive({
  das.eff.lines <- cruzDasEffortRange()

  eff.date.vals <- if (input$das_effort_filter_same) {
    input$das_sight_dateRange
  } else {
    input$das_effort_dateRange
  }

  validate(
    need(eff.date.vals[1] <= eff.date.vals[2],
         "Effort filter: minimum date must be less than or equal to maximum date")
  )

  keep <- between(
    as.Date(das.eff.lines$DateTime), eff.date.vals[1], eff.date.vals[2]
  )
  .func_eff_filt_validate(keep, "date")
})


#------------------------------------------------------------------------------
### Cruise number filter
cruzDasEffortFilterCruise <- reactive ({
  das.eff.lines <- cruzDasEffortRange()

  if (input$das_effort_filter_same) {
    if (is.null(input$das_sight_cruiseNum)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_sight_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_eff_filt_validate(keep, "cruise number") }

  } else {
    if (is.null(input$das_effort_cruise)) {
      TRUE
    } else {
      eff.cruise.vals <- as.numeric(input$das_effort_cruise)
      keep <- das.eff.lines$Cruise %in% eff.cruise.vals
      .func_eff_filt_validate(keep, "cruise number")}
  }
})


###############################################################################
###############################################################################
### Get effort plotting colors and line widths
cruzDasEffortParams <- reactive({
  if (input$das_effort == 2) {
    ## If simplified effort, simple results
    eff.col <- input$das_effort_simp_col
    eff.lwd <- input$das_effort_simp_lwd

  } else if (input$das_effort == 3) {
    ## If detailed effort, not as simple
    das.eff.lines <- cruzDasEffortFilter()

    # Use Beaufort or effort type values to generate colors
    if (input$das_effort_det_byBft) {
      bft.cols <- input$das_effort_det_bft_col
      bft.col.num <- cruzDasEffortFilterBeaufortVal()[2] + 1
      validate(
        need(length(bft.cols) >= bft.col.num,
             paste("Please choose at least", bft.col.num,
                   "colors, one for each possible Beaufort value between 0",
                   "and the specified maximum Beaufort"))
      )

      eff.col <- bft.cols[das.eff.lines$Bft + 1]
      eff.lwd <- input$das_effort_det_bft_lwd

    } else {
      eff.col <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_col_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_col_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_col_f
      )
      eff.lwd <- case_when(
        das.eff.lines$EffType == "S" ~ input$das_effort_det_lwd_s,
        das.eff.lines$EffType == "N" ~ input$das_effort_det_lwd_n,
        das.eff.lines$EffType == "F" ~ input$das_effort_det_lwd_f
      )
    }
  }

  list(eff.col = eff.col, eff.lwd = eff.lwd)
})

###############################################################################






# cruzDasGeneral for CruzPlot
#   read and process DAS file
#   update: symbol type and color for when 'Input symbol properties as text' is clicked


###############################################################################
# Output flag indicating if DAS data has been loaded
output$das_loaded_flag <- reactive(isTruthy(cruz.list$das.data))
outputOptions(output, "das_loaded_flag", suspendWhenHidden = FALSE)

# Output flag indicating if SpCodes has been loaded
output$das_spcodes_loaded_flag <- reactive(isTruthy(cruz.list$sp.codes))
outputOptions(output, "das_spcodes_loaded_flag", suspendWhenHidden = FALSE)


###############################################################################
### Read and process DAS file(s)
das_file_load <- eventReactive(input$das_file, {
  # Clear reactive vals, and reset plot sightings and effort selections
  cruz.list$das.data <- NULL
  cruz.list$das.data.name <- NULL
  updateCheckboxInput(session, "das_sightings", value = FALSE)
  updateRadioButtons(session, "das_effort", selected = 1)

  # Get and check additional parameters
  skip <- input$das_file_skip
  days.gap <- input$das_file_days_gap
  reset.event  <- input$das_file_reset_event == 1
  reset.effort <- input$das_file_reset_effort == 1
  # reset.day    <- input$das_file_reset_day == 1

  v.tmp <- "skip must be a valid, whole number greater than or equal to 0"
  validate(need(!is.na(skip), v.tmp))
  validate(need(isTRUE(all.equal(skip %% 1, 0)), v.tmp))
  validate(need(skip >= 0, v.tmp))
  rm(v.tmp)

  v.tmp <- "days.gap must be a valid, whole number greater than or equal to 0"
  validate(need(!is.na(days.gap), v.tmp))
  validate(need(isTRUE(all.equal(days.gap %% 1, 0)), v.tmp))
  validate(need(days.gap >= 0, v.tmp))
  rm(v.tmp)

  # Process DAS file
  withProgress(message = "Processing DAS file", value = 0.6, {
    das.proc <- try(suppressWarnings(
      swfscDAS::das_process(
        input$das_file$datapath, skip = skip, days.gap = days.gap,
        reset.event = reset.event, reset.effort = reset.effort,
        reset.days = TRUE
      )
    ), silent = TRUE)
  })

  validate(
    need(isTruthy(das.proc),
         paste0("Error: unable to read and process the provided DAS file(s).",
                "\n\nThe call to das_read and das_process from the ",
                "swfscDAS package returned the following error. ",
                "Please use `swfscDAS::das_check` ",
                "for a more informative error message.\n\n",
                attr(das.proc, "condition")))
  )

  # Correct filename
  filename.key <- data.frame(
    tmp = basename(input$das_file$datapath),
    actual = input$das_file$name,
    stringsAsFactors = FALSE
  )

  das.proc <- das.proc %>%
    left_join(filename.key, by = c("file_das" = "tmp")) %>%
    mutate(file_das = .data$actual) %>%
    select(-.data$actual)

  # Save reactive values
  cruz.list$das.data <- das.proc
  # cruz.list$das.data.name <- input$das_file$name

  ""
}, ignoreInit = TRUE)


### Conditional flag for UI code for truthy cruz.list$das.data
output$cruzDasFile_Conditional <- reactive({
  isTruthy(cruz.list$das.data)
})
outputOptions(output, "cruzDasFile_Conditional", suspendWhenHidden = FALSE)


###############################################################################
# Help pages

### Help page for das file load
observeEvent(input$das_file_help, {
  showModal(modalDialog(
    tags$h5(tags$strong("Lines to skip:"),
            "The number of lines that are ignored before starting to read data in the provided file."),
    tags$h5(tags$strong("days.gap argument:"),
            "This argument helps the user keep information from spilling from",
            "one cruise into the next in concatenated DAS files.",
            "For instance, if days.gap is 20, then all of the DAS state/condition information",
            "(Cruise, Mode, Beaufort, Visibility, etc.)",
            "are reset when there are 20 or more days between records in the file."),
    tags$h5(tags$strong("reset.event argument:"),
            "This argument specifies what happens if a state/condition is not entered in the DAS data.",
            "For example, say the Beaufort value is left blank in a V event.",
            "If reset.event is TRUE, then the Beaufort value is set to NA",
            "in the processed data until the next V event.",
            "If reset.event is FALSE, then the previous Beaufort value will be carried through,",
            "meaning the NA value will be ignored.",
            "This argument should be FALSE only if state/condition information (e.g. Beaufort value)",
            "was purposefully not entered if it did not change."),
    tags$h5(tags$strong("reset.effort argument:"),
            "When using WinCruz, an R event or BR event series signifying the start of a new continuous effort section",
            "is generally immediately followed by a PVNW event sequence in which all state/condition values are updated.",
            "If reset.effort is TRUE, then the state/condition arguments are all reset to NA at the R event,",
            "or at the B event is it immediately precedes said R event.",
            "This argument should be FALSE only if 1) R events are not always followed by a PVNW event sequence or",
            "2) state/condition information was purposefully not entered if it did not change (similar to reset.event)."),
    tags$h5(tags$strong("reset.day argument:"),
            "This argument is always set to TRUE - the user cannot specify it when using CruzPlot.",
            "When TRUE, all of the state and condition information is reset at the beginning of each new day,",
            "rather than being carried over from the previous day."),
    easyClose = FALSE
  ))
})


### Help page for sighting events
observeEvent(input$das_sighting_events_help, {
  showModal(modalDialog(
    tags$h5(tags$strong("S event:"), "Cetacean sighting - standard"),
    tags$h5(tags$strong("G event:"), "Cetacean subgroup sighting"),
    tags$h5(tags$strong("K event:"), "Cetacean sighting - tracker. Historically only used during cruise number 1611 in 1998"),
    tags$h5(tags$strong("M event:"), "Cetacean sighting - matched. Historically only used during cruise number 1608 in 1997"),
    tags$h5(tags$strong("p event:"), "Pinniped sighting. Used beginning in 2018"),
    tags$h5(tags$strong("s event:"), "Standard cetacean resight. Corresponds to a previous S event"),
    tags$h5(tags$strong("g event:"), "Cetacean subgroup resight. Corresponds to a previous G event"),
    tags$h5(tags$strong("k event:"), "Tracker cetacean resight. Corresponds to a previous K event"),
    tags$br(), tags$br(),
    tags$h5("When plotting resights (s, k, or g events): 1) you can only plot a single sighting type (e.g. S and s event)",
            "and a single species at one time,",
            "and 2) The 'Symbol color...' entry corresponds the the order of the selected events.",
            "In addition, for resight plotting please ensure that the loaded DAS file is not a concatenated file,
            i.e. each sighting number in the file corresponds to a single sighting/resighting group."),
    easyClose = FALSE
  ))
})




###############################################################################
# Code for keeping current inputs the same when switching
#    from or to text symbol properties input
observeEvent(input$das_symbol_mult, {
  if (input$das_symbol_mult) {
    ### Convert codes to text
    # Covert numerics to symbols
    curr.pch <- as.numeric(input$das_symbol_type)
    if (length(curr.pch) == 0) curr.pch <- "1"

    # Covert color codes to color names
    curr.col <- input$das_symbol_color
    if (is.null(curr.col)) {
      curr.col <- "Black"

    } else {
      # This keeps numbers in order
      if (input$color_style == 1) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code %in% i), 1)
        curr.col <- symbol.col[curr.col.idx]

      } else if (input$color_style == 2) {
        curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.code.gray %in% i), 1)
        curr.col <- symbol.col.gray[curr.col.idx]
      }
    }

    updateTextInput(session, "das_symbol_type_mult", value = paste(curr.pch, collapse = ", "))
    updateTextInput(session, "das_symbol_color_mult", value = paste(curr.col, collapse = ", "))


  } else {
    ### Convert from text to codes
    # Convert symbol codes to symbols
    curr.pch <- suppressWarnings(
      as.numeric(unlist(strsplit(input$das_symbol_type_mult, ", ")))
    )
    if (length(curr.pch) == 0) {
      curr.pch <- 1
    } else {
      if (!all(curr.pch %in% unname(cruz.symbol.type))) curr.pch <- 1
    }
    updateSelectInput(session, "das_symbol_type", selected = curr.pch)


    # Covert color names to color codes
    curr.col <- unlist(strsplit(input$das_symbol_color_mult, ", "))

    if (is.null(curr.col)) {
      curr.col <- "black"

    } else {
      if (!all(curr.col %in% symbol.col)) {
        curr.col <- "black"
      } else {
        # This keeps numbers in order
        if (input$color_style == 1) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col %in% i), 1)
          curr.col <- symbol.col.code[curr.col.idx]

        } else if (input$color_style == 2) {
          curr.col.idx <- vapply(curr.col, function(i) which(symbol.col.gray %in% i), 1)
          curr.col <- symbol.col.code.gray[curr.col.idx]
        }
      }
    }
    updateSelectInput(session, "das_symbol_color", selected = curr.col)
  }
}, ignoreInit = TRUE)


###############################################################################
# Flags for inputs for plotting detailed effort not by Beaufort

output$das_effort_det_s_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("S" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_s_flag", suspendWhenHidden = FALSE)

output$das_effort_det_n_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("N" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_n_flag", suspendWhenHidden = FALSE)

output$das_effort_det_f_flag <- reactive({
  flag <- FALSE
  if (input$das_effort == 3) {
    if (!input$das_effort_det_byBft) {
      if ("F" %in% input$das_effort_snf) flag <- TRUE
    }
  }

  flag
})
outputOptions(output, "das_effort_det_f_flag", suspendWhenHidden = FALSE)

###############################################################################





# Initialize interactive reactive values, and reset them when appropriate

###############################################################################
sight <- reactiveValues(
  click = NULL,
  hover = NULL,
  hover.lab = NULL,
  lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)

effort <- reactiveValues(
  click = NULL,
  hover = NULL,
  lab = NULL,
  hover.lab = NULL,
  miss = FALSE,
  hover.miss = FALSE
)


###############################################################################
### When changed to a new page or tab: 1) Turn plot to not-interactive,
###   2) reset hover info, and 3) remove miss labels
observe({
  input$tabs
  input$tabset2

  updateRadioButtons(session, "das_sight_interactive", selected = 1)
  updateRadioButtons(session, "das_effort_interactive", selected = 1)

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})


###############################################################################
# Reset as needed

### If DAS file or map range changes, reset interactive everything
observe({
  cruz.list$das.data
  cruz.map.range$lon.range
  cruz.map.range$lat.range
  cruz.map.range$world2

  # Reset all things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE

  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If interactive selection changes, remove hover and miss
observeEvent(input$das_sight_interactive, {
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})
observeEvent(input$das_effort_interactive, {
  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})

### If sightings selections change, reset interactive sighting things
observe({
  # Sightings to plot and filters
  input$das_sighting_type
  input$das_sighting_code_1_all
  input$das_sighting_code_2_all
  input$das_sighting_events
  input$das_sighting_code_1
  input$das_sighting_code_2

  input$das_sight_effort
  input$das_sight_cp
  input$das_sight_snf
  input$das_sight_minBft
  input$das_sight_maxBft
  input$das_sight_dateRange
  input$das_sight_cruise
  input$das_sight_trunc

  # Reset sighting things
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})


### If effort selections change, reset interactive effort things
observe({
  # Effort to plot and filters
  input$das_effort
  input$das_effort_cp
  input$das_effort_snf

  input$das_effort_filter_same
  input$das_effort_minBft
  input$das_effort_maxBft
  input$das_effort_dateRange
  input$das_effort_cruise

  # Reset effort things
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$hover.lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})



###############################################################################
###############################################################################
# cruzDasInteractiveSight for CruzPlot

### Interactive sighting click
observeEvent(input$sight_click, {
  click.curr <- c(input$sight_click$x, input$sight_click$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(click.curr, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      sight$click <- c(sight$click, list(click.curr))
      sight$lab <- c(sight$lab, close.info[3])
    })
    sight$miss <- FALSE
  } else{
    sight$miss <- TRUE
  }
})


### Hover to display sighitng information
observeEvent(input$sight_hover, {
  sight$hover <- c(input$sight_hover$x, input$sight_hover$y)
  das.sight <- cruzDasSightFilter()$das.sight

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and return information to print
  sight.type <- cruzDasSightFilter()$sight.type
  close.info <- if (sight.type == 1) {
    # type = 1 means mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 1, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime, das.sight$SightNo)
  } else {
    # type = 2 means non-mammal sighting for function cruzClosestPt
    cruzClosestPt(sight$hover, type = 2, das.sight$Lat, das.sight$Lon,
                  das.sight$DateTime)
  }

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(sight$hover.lab <- close.info[3])
    sight$hover.miss <- FALSE
  } else {
    sight$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_sight_interactive_reset_last, {
  sight$click <- if (length(sight$click) == 1) NULL else head(sight$click, -1)
  sight$lab <- if (length(sight$lab) == 1) NULL else head(sight$lab, -1)
  sight$miss <- FALSE

  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_sight_interactive_reset_all, {
  sight$click <- NULL
  sight$hover <- NULL
  sight$hover.lab <- NULL
  sight$lab <- NULL
  sight$miss <- FALSE
  sight$hover.miss <- FALSE
})



###############################################################################
###############################################################################
# cruzDasInteractiveEffort for CruzPlot

### Click to add labels to map
observeEvent(input$effort_click, {
  click.curr <- c(input$effort_click$x, input$effort_click$y)
  das.effort <- cruzDasEffortFilter()
  # browser()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    click.curr, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.2) {
    isolate({
      effort$click <- c(effort$click, list(click.curr))
      effort$lab <- c(effort$lab, close.info[3])
    })
    effort$miss <- FALSE
  } else {
    effort$miss <- TRUE
  }
})


### Hover to see R and E lat/lon coordinates
observeEvent(input$effort_hover, {
  effort$hover <- c(input$effort_hover$x, input$effort_hover$y)
  das.effort <- cruzDasEffortFilter()

  param.unit <- cruzMapParam()$param.unit
  param.unit.diff <- c(param.unit[2]-param.unit[1], param.unit[4]-param.unit[3])
  # ^ Works because for world2 map, x range is 0, 360
  param.inch <- cruzMapParam()$param.inch
  x.ratio <- param.inch[1]/param.unit.diff[1]
  y.ratio <- param.inch[2]/param.unit.diff[2]

  # Determine closest point and if applicable get label information
  close.info <- cruzClosestPt(
    effort$hover, 3, das.effort$st_lat, das.effort$st_lon, das.effort$DateTime,
    das.lat2 = das.effort$end_lat, das.lon2 = das.effort$end_lon
  )

  dist.inch <- sqrt(
    (as.numeric(close.info[1])*x.ratio)^2 + (as.numeric(close.info[2])*y.ratio)^2
  )
  if (dist.inch <= 0.3) {
    isolate(effort$hover.lab <- close.info[3])
    effort$hover.miss <- FALSE
  } else {
    effort$hover.miss <- TRUE
  }
})


### Remove last point
observeEvent(input$das_effort_interactive_reset_last, {
  effort$click <- if (length(effort$click) == 1) NULL else head(effort$click, -1)
  effort$lab <- if (length(effort$lab) == 1) NULL else head(effort$lab, -1)
  effort$miss <- FALSE

  effort$hover <- NULL
  effort$hover.lab <- NULL
  effort$hover.miss <- FALSE
})


### Remove all points
observeEvent(input$das_effort_interactive_reset_all, {
  effort$click <- NULL
  effort$hover <- NULL
  effort$lab <- NULL
  effort$miss <- FALSE
  effort$hover.miss <- FALSE
})





## renderUI()'s for Plot DAS Data tab


###############################################################################
### Codes for mammals and turtles
# renderUIs for mammal and turtle species
output$das_sighting_code_1_uiOut_select <- renderUI({
  sp.mammals <- cruzSpeciesMammals()
  sp.codes.list <- as.list(sp.mammals$Code)
  names(sp.codes.list) <- paste(sp.mammals$Code, sp.mammals$Abbr, sep = " - ")

  selectInput("das_sighting_code_1", tags$h5("Select mammal species"),
              choices = sp.codes.list, multiple = TRUE,
              selected = NULL)
})
outputOptions(output, "das_sighting_code_1_uiOut_select", suspendWhenHidden = FALSE)

output$das_sighting_code_2_uiOut_select <- renderUI({
  sp.turtles <- cruzSpeciesTurtles()
  sp.codes.list <- as.list(sp.turtles$Code)
  names(sp.codes.list) <- paste(sp.turtles$Code, sp.turtles$Name_Scientific, sep = " - ")

  selectInput("das_sighting_code_2", tags$h5("Select turtle species"),
              choices = sp.codes.list, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_sighting_code_2_uiOut_select", suspendWhenHidden = FALSE)


###############################################################################
# Filters

# Date widgets
### Get min and max datws in DAS file
dateRange_min_max <- reactive({
  x <- req(cruz.list$das.data)
  input$das.file

  min.date <- as.character(as.Date(min(x$Date, na.rm = T)) - 1)
  max.date <- as.character(as.Date(max(x$Date, na.rm = T)) + 1)

  c(min.date, max.date)
})

### renderUI for date range filter for plotting sightings and effort
output$das_sight_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)

  dates <- dateRange_min_max()

  dateRangeInput("das_sight_dateRange", label = tags$h5("Date range"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_sight_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)

output$das_effort_dateRange_uiOut_date <- renderUI({
  req(cruz.list$das.data)
  dates <- dateRange_min_max()

  dateRangeInput("das_effort_dateRange", label = tags$h5("Date range"),
                 start = dates[1], end = dates[2])
})
outputOptions(output, "das_effort_dateRange_uiOut_date", suspendWhenHidden = FALSE, priority = 3)


###############################################################################
# Cruise number

### Reactive returning cruise numbers in the file
das_cruise_nums <- reactive({
  x <- req(cruz.list$das.data)
  input$das.file

  unique(na.omit(x$Cruise))
})

### Cruise number - sight
output$das_sight_cruise_uiOut_select <- renderUI({
  req(cruz.list$das.data)
  cruises <- das_cruise_nums()

  selectInput("das_sight_cruise", tags$h5("Cruise number(s)"),
              choices = cruises, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_sight_cruise_uiOut_select", suspendWhenHidden = FALSE, priority = 3)

### Cruise number - effort
output$das_effort_cruise_uiOut_select <- renderUI({
  req(cruz.list$das.data)
  cruises <- das_cruise_nums()

  selectInput("das_effort_cruise", tags$h5("Cruise number(s)"),
              choices = cruises, multiple = TRUE, selected = NULL)
})
outputOptions(output, "das_effort_cruise_uiOut_select", suspendWhenHidden = FALSE, priority = 3)

###############################################################################
### Truncation input
output$das_sight_trunc_uiOut_numeric <- renderUI({
  isolate(curr.value <- input$das_sight_trunc)

  trunc.units <- input$das_sight_trunc_units
  if(trunc.units == 1) widget.name <- "Truncation (km)"
  if(trunc.units == 2) widget.name <- "Truncation (nmi)"

  numericInput("das_sight_trunc", label = tags$h5(widget.name), value = curr.value)
})
outputOptions(output, "das_sight_trunc_uiOut_numeric", suspendWhenHidden = FALSE, priority = 3)

##############################################################################





# cruzDasSightFilter for CruzPlot - step 3 of processing species data
#   cruiseDasSightFilter() pulls individual filters together

#   cruzDasSightFilterEffort() returns a logical indicating which rows satisfy the effort filter
#   cruzDasSightFilterBeaufort() returns a logical indicating which rows satisfy the beaufort filter
#   cruzDasSightFilterDate() returns a logical indicating which rows satisfy the date filter
#   cruzDasSightFilterCruise() returns a logical indicating which rows are from the given cruise number(s)
#   cruzDasSightFilterTrunc() returns a logical indicating which rows are within the given truncation distance


###############################################################################
### Top-level function for filtering
cruzDasSightFilter <- reactive({
  data.list <- cruzDasSightRange()

  das.sight    <- data.list$das.sight
  sight.type   <- data.list$sight.type
  sp.codes     <- data.list$sp.codes
  sp.selection <- data.list$sp.selection

  ### Collect logical vectors
  keep1 <- cruzDasSightFilterEffort()
  keep2 <- if (input$das_sight_effort == 2) cruzDasSightFilterMode() else TRUE
  keep3 <- if (input$das_sight_effort == 2) cruzDasSightFilterEfftype() else TRUE
  keep4 <- cruzDasSightFilterBeaufort()
  keep5 <- cruzDasSightFilterDate()
  keep6 <- cruzDasSightFilterCruise()
  keep7 <- cruzDasSightFilterTrunc()

  keep.all <- keep1 & keep2 & keep3 & keep4 & keep5 & keep6 & keep7
  keep.all.na <- which(is.na(keep.all)) #works b/c (NA & F) output is (F)

  ### Verbosely change NA filter values to FALSE
  if (length(keep.all.na) > 0) {
    table.out <- das.sight %>%
      slice(keep.all.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise)) %>%
      select(Event, DateTime, OnEffort, Cruise, Mode, EffType, Bft,
             SightNo, SpCode, PerpDistKm,
             # File = file_das, #Can take up too much space
             `Line number` = line_num) %>%
      distinct()
    if (input$das_sight_effort == 2)
      table.out <- table.out %>% select(-Mode, -EffType)

    txt.out <- ifelse(nrow(table.out) == 1, "sighting", "sightings")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following", txt.out, "had at least one NA filter value.",
              txt.out2, "will be removed (filtered) and thus not plotted",
              "or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      easyClose = TRUE,
      footer = "Click anywhere or press any button to close this notice",
      size = "l"
    ))
  }

  keep.all[is.na(keep.all)] <- FALSE
  das.sight.filt <- das.sight[keep.all, ]

  ### Final checks, calculate sp.count, and return
  validate(
    need(sum(is.na(das.sight.filt$Event)) == 0,
         "Error in CruzPlot sighting filtering - please report this as an issue")
  )
  validate(need(nrow(das.sight) > 0, "No sightings match the given filters"))

  # Check that at least one sighting is still within map range, and get sp.count
  if (sight.type %in% c(1, 2)) {
    if (!sp.selection) sp.codes <- base::intersect(sp.codes, das.sight.filt$SpCode)

    # Calculate count for each species
    sp.count <- vapply(sp.codes, function(i, j) {
      sum(j$SpCode == i)
    }, 1, j = das.sight.filt, USE.NAMES = FALSE)

  } else {
    sp.count <- nrow(das.sight.filt)
  }

  list(
    das.sight = das.sight.filt, sight.type = sight.type,
    sp.codes = sp.codes, sp.selection = sp.selection, sp.count = sp.count
  )
})


###############################################################################
### Helper functions that filter sightings data by single property

.func_sight_filt_validate <- function(x, x.txt) {
  validate(
    need(any(x), paste("None of the specified sightings within the map range",
                       "match the given", x.txt, "filter"))
  )
  x
}

#------------------------------------------------------------------------------
# On/off effort
cruzDasSightFilterEffort <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  effort.val <- switch(as.numeric(input$das_sight_effort), c(0, 1), 1, 0)
  keep <- as.numeric(das.sight$OnEffort) %in% effort.val
  .func_sight_filt_validate(keep, "on/off effort")
})

# Mode: C/P
cruzDasSightFilterMode <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  keep <- das.sight$Mode %in% input$das_sight_cp
  .func_sight_filt_validate(keep, "mode (closing/passing)")
})

# Effort type: S/N/F
cruzDasSightFilterEfftype <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  keep <- das.sight$EffType %in% input$das_sight_snf
  .func_sight_filt_validate(keep, "effort type (standard/non-standard/fine)")
})

#------------------------------------------------------------------------------
# Beaufort
cruzDasSightFilterBeaufort <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  bft.min <- as.numeric(input$das_sight_minBft)
  bft.max <- as.numeric(input$das_sight_maxBft)

  validate(
    need(input$das_sight_minBft <= input$das_sight_maxBft,
         "Sightings filter: minimum Beaufort must be less than or equal to maximum Beaufort")
  )

  keep <- if (identical(c(bft.min, bft.max), c(0, 9))) {
    TRUE
  } else {
    between(das.sight$Bft, bft.min, bft.max)
  }
  .func_sight_filt_validate(keep, "Beaufort")
})

#------------------------------------------------------------------------------
# Dates
cruzDasSightFilterDate <- reactive({
  das.sight <- cruzDasSightRange()$das.sight
  date.vals <- input$das_sight_dateRange

  validate(
    need(input$das_sight_dateRange[1] <= input$das_sight_dateRange[2],
         "Sightings filter: minimum date must be less than or equal to maximum date")
  )

  keep <- between(
    as.Date(das.sight$DateTime), date.vals[1], date.vals[2]
  )
  .func_sight_filt_validate(keep, "date")
})

#------------------------------------------------------------------------------
# Cruise numbers
cruzDasSightFilterCruise <- reactive({
  das.sight <- cruzDasSightRange()$das.sight

  if (is.null(input$das_sight_cruise)) {
    # Return here to keep records that have 'NA' value
    TRUE

  } else {
    cruise.vals <- as.numeric(input$das_sight_cruise)
    keep <- das.sight$Cruise %in% cruise.vals
    .func_sight_filt_validate(keep, "cruise number")
  }
})

#------------------------------------------------------------------------------
# Perpendicular distance truncation
cruzDasSightFilterTrunc <- reactive({
  das.sight <- cruzDasSightRange()$das.sight

  pdist.val <- ifelse(
    input$das_sight_trunc_units == 1,
    input$das_sight_trunc, input$das_sight_trunc * 1.852
  )

  if (is.na(pdist.val)) {
    TRUE

  } else {
    keep <- das.sight$PerpDistKm <= pdist.val
    validate(
      need(any(keep),
           "There are no selected sightings within the given truncation distance")
    )
    .func_sight_filt_validate(keep, "truncation (perpendicular distance)")
  }
})

###############################################################################





# Returns parameters for sighting legend

cruzDasSightLegend <- reactive({
  symbol.list <- cruzDasSightSymbol()

  leg.df       <- symbol.list$leg.df
  sight.type   <- symbol.list$sight.type
  sp.codes     <- symbol.list$sp.codes
  sp.codes.len <- length(sp.codes)
  sp.count     <- symbol.list$sp.count
  das.sight    <- symbol.list$das.sight

  font.fam <- font.family.vals[as.numeric(input$das_legend_font)]

  names.lab <- input$das_legend_names

  if (sight.type %in% c(3, 4)) {
    leg.lab <- leg.df$SpCode
    if ("5" %in% names.lab) leg.lab <- paste0(leg.lab, ", n = ", sp.count)

  } else {
    sp.codes.all <- cruz.list$sp.codes
    temp.use <- vapply(
      sp.codes, function(i) which(sp.codes.all$Code == i), 1,
      USE.NAMES = FALSE
    )

    sp.codes.all.use <- sp.codes.all[temp.use, ]

    # # This piece cuts the common name at the first comma, which results in incorrect names for some codes
    # sp.codes.all.use$Name_Common <- vapply(sp.codes.all.use$Name_Common, function(i) {
    #   unlist(strsplit(i, ","))[1]
    # }, as.character(1))

    leg.lab <- NULL
    if ("1" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Code)
    if ("2" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Abbr)
    if ("3" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name_Scientific)
    if ("4" %in% names.lab) leg.lab <- paste(leg.lab, sp.codes.all.use$Name_Common)
    validate(
      need(leg.lab, "Please select species information to display in the sighting legend")
    )

    if (cruzDasSightEventResight()) leg.lab <- paste0(leg.lab, ", Event: ", input$das_sighting_events)

    if ("5" %in% names.lab) {
      if (cruzDasSightEventResight()) {
        validate(need(length(unique(das.sight$Event)) <= 2, "Error processing sighting legend"))
        # Primary sighting will always come before resight
        sp.count.res <- c(table(das.sight$Event)[input$das_sighting_events[1]],
                          table(das.sight$Event)[input$das_sighting_events[2]])
        sp.count.res[is.na(sp.count.res)] <- 0
        leg.lab <- paste0(leg.lab, ", n = ", sp.count.res)
        rm(sp.count.res)

      } else {
        leg.lab <- paste0(leg.lab, ", n = ", sp.count)
        # leg.lab <- if (any(names.lab %in% 1:4)) {
        #   paste0(leg.lab, ", n = ", sp.count)
        # } else {
        #   paste0("n = ", sp.count)
        # }
      }
    }
  }

  leg.title <- if (input$das_legend_title == "") NULL else input$das_legend_title
  leg.bty <-     ifelse(input$das_legend_boxCol == 1, "n", "o")
  leg.box.col <- ifelse(input$das_legend_boxCol == 2, NA, "black")
  leg.box.lwd <- ifelse(input$das_legend_boxCol == 2, 0, 1)
  leg.box.cex <- input$das_legend_textSize

  leg.pos <- input$das_legend_pos
  if (leg.pos == 1) {
    validate(
      need(!is.na(input$das_legend_lat), "Please enter a valid legend latitude value"),
      need(!is.na(input$das_legend_lon), "Please enter a valid legend longitude value")
    )
    leg.x <- input$das_legend_lon
    leg.y <- input$das_legend_lat

  } else {
    leg.x <- leg.pos
    leg.y <- NULL
  }

  list(
    leg.x = leg.x, leg.y = leg.y, leg.lab = leg.lab, leg.title = leg.title,
    leg.pch = leg.df$pch, leg.col = leg.df$col,
    leg.cex = leg.df$cex, leg.lwd = leg.df$lwd,
    leg.bty = leg.bty, leg.box.col = leg.box.col,
    leg.box.lwd = leg.box.lwd, leg.box.cex = leg.box.cex,
    font.fam = font.fam
  )
})





# Step 1 of processing species data
#   cruzDasSightSpeciesMammals() returns mammal species codes selected by user
#   cruzDasSightSpeciesTurtles() returns turtle species codes selected by user
#   cruzDasSightProcess() - run das_sight only once
#   cruzDasSightPosition - handle ship vs sighting position, verbosely remove NA positions
#   cruzDasSightSpecies() returns list of data frames containing data for selected species sightings,
#	    sighting type, and species codes; also computes sighting location based on angle and distance;
#     adds sight.lat, sight.lon, angle, distance (nmi) to data.sight dataframe


###############################################################################
# Extract and process species codes
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
# So that das_sight is run once
cruzDasSightSpeciesProcess <- reactive({
  swfscDAS::das_sight(req(cruz.list$das.data), return.format = "default")
})



###############################################################################
# Series of sighting-processing reactive functions
#   1) NA position filter, 2) Species/event filter

### Checks on NA positions to print modal, and 'select' selected position
cruzDasSightPosition <- reactive({
  das.sight <- cruzDasSightSpeciesProcess()

  #----------------------------------------------------------------------------
  # Verbosely remove sightings with NA positions
  ll.na <- if (input$das_sightings_position == 1) {
    which(is.na(das.sight$Lat) | is.na(das.sight$Lon))
  } else (
    which(
      is.na(das.sight$Lat) | is.na(das.sight$Lon) | is.na(das.sight$Course) |
        is.na(das.sight$Bearing) | is.na(das.sight$DistNm)
    )
  )

  if (length(ll.na) > 0) {
    table.out <- das.sight %>%
      slice(ll.na) %>%
      mutate(DateTime = as.character(DateTime),
             Cruise = as.character(Cruise),
             Resight = Event %in% c("s", "k", "g")) %>%
      select(Event, DateTime, Lat, Lon, OnEffort, Cruise,
             SightNo, SpCode, Resight,
             # File = file_das,
             `Line number` = line_num) %>%
      distinct()
    txt.out1 <- ifelse(nrow(table.out) == 1, "sighting has", "sightings have")
    txt.out2 <- ifelse(nrow(table.out) == 1, "This sighting", "These sightings")

    showModal(modalDialog(
      title = "CruzPlot notice",
      tags$h5("The following", txt.out1, "an NA value that causes the",
              "specified plotted position to be NA.",
              txt.out2, "will be automatically removed (filtered) and thus",
              "not plotted or included in tabular output:"),
      tags$br(), tags$br(),
      renderTable(table.out),
      tags$br(),
      tags$h5("This notice will not be shown again unless a new DAS file is loaded or",
              "'Position to plot' is changed.",
              "See the manual for more details"),
      easyClose = FALSE,
      size = "l"
    ))
  }

  das.sight <- if (input$das_sightings_position == 1) {
    das.sight %>% filter(!is.na(.data$Lat), !is.na(.data$Lon))
  } else {
    das.sight %>%
      filter(!is.na(.data$Lat), !is.na(.data$Lon),
             !is.na(.data$Course), !is.na(.data$Bearing), !is.na(.data$DistNm))
  }

  #----------------------------------------------------------------------------
  # Calculate sighting location,, select selected position, and return
  bearing2 <- (das.sight$Course + das.sight$Bearing) %% 360
  ll.sight <- geosphere::destPoint(
    matrix(c(das.sight$Lon, das.sight$Lat), ncol = 2),
    bearing2, das.sight$DistNm * 1852
  )

  # # Calculate sighting location using swfscMisc
  # ll.sight.dest <- apply(das.sight, 1, function(i) {
  # i <- as.numeric(i[c("Lat", "Lon", "bearing2", "DistNm")])
  #   swfscMisc::destination(i["Lat"], i["Lon"], i["bearing2"], i["DistNm"],
  #                          units = "nm", type = "ellipsoid")
  # })

  das.sight <- das.sight %>%
    mutate(Lat_ship = .data$Lat, Lon_ship = .data$Lon,
           Lat_sight = ll.sight[, "lat"],
           Lon_sight = ll.sight[, "lon"])


  # 'Select' ship or sighting position.
  #   Might as well do this here since position input is already being used
  if (input$das_sightings_position == 1) {
    das.sight$Lat <- das.sight$Lat_ship
    das.sight$Lon <- das.sight$Lon_ship

  } else if (input$das_sightings_position == 2) {
    das.sight$Lat <- das.sight$Lat_sight
    das.sight$Lon <- das.sight$Lon_sight
  }

  # Adjust longitudes if world2 map is being used
  # #this is done in cruzDasSightRange()
  # if (cruz.map.range$world2)
  #   das.sight$Lon <- ifelse(das.sight$Lon < 0, das.sight$Lon + 360, das.sight$Lon)

  das.sight
})


###############################################################################
# Filter for events specified either directly or by sighting type
#   And associated things that depend on the selected event
cruzDasSightEventResight <- reactive({
  (input$das_sighting_type == 1) &
    any(c("s", "k", "g") %in% input$das_sighting_events)
})

output$cruzDasSightEventResight_uiOut_message <- renderUI({
  req(cruzDasSightEventResight())
  tags$h5("When plotting resights, symbol color entries correspond to selected events")
})


cruzDasSightEvent <- reactive({
  das.sight <- cruzDasSightPosition()
  sight.type <- input$das_sighting_type

  if (sight.type == 2) {
    ### Turtle sightings
    das.sight <- das.sight %>% filter(.data$Event == "t")
    validate(
      need(nrow(das.sight) > 0,
           "There are no turtle sightings (t events) in the loaded DAS file(s)")
    )

  } else if (sight.type == 3) {
    ### Boat sightings
    das.sight <- das.sight %>% filter(.data$Event == "F")
    validate(
      need(nrow(das.sight) > 0,
           "There are no boat sightings (F events) in the loaded DAS file(s)")
    )

  } else if (sight.type == 1) {
    ### Marine mammal sightings
    sp.events <- input$das_sighting_events
    validate(need(sp.events, "Please select at least one event code to plot"))

    das.sight <- das.sight %>%
      filter(.data$Event %in% sp.events) %>%
      mutate(idx = seq_along(.data$Event))

    # Resights
    if (cruzDasSightEventResight()) {
      # Checks - other
      validate(
        need(length(unique(na.omit(cruz.list$das.data$file_das))) == 1,
             "You can only process resights when plotting data from a single DAS file")
        # ^b/c different files could have same SightNo, etc.
        # Doesn't solve concatenated files..
      )
      validate(
        need(sum(c("s", "k", "g") %in% sp.events) == 1,
             "You can only plot one type of resight at a time")
      )
      validate(
        if ("s" %in% sp.events) need("S" %in% sp.events, "To plot s events, S events must also be plotted"),
        if ("k" %in% sp.events) need("K" %in% sp.events, "To plot k events, K events must also be plotted"),
        if ("g" %in% sp.events) need("G" %in% sp.events, "To plot g events, G events must also be plotted"),
        need(length(sp.events) == 2,
             "When plotting resights, you can only plot the resight and the corresponding primary sighting event")
      )

      # Get species, etc., for s and k events
      if (any(c("s", "k") %in% sp.events)) {
        das.sight.main <- das.sight %>% filter(!(.data$Event %in% c("s", "k")))
        das.sight.res <- das.sight %>% filter(.data$Event %in% c("s", "k"))
        validate(
          need(all(das.sight.res$SightNo %in% das.sight.main$SightNo),
               paste("Not all of the selected s/k resight event(s) have primary sightings with",
                     "the same sighting numbers -",
                     "this is a DAS error that needs to be fixed to plot s/k events"))
        )

        col.names <- c("Prob", "SpCode", "SpCodeProb")
        d.toadd <- das.sight.main %>%
          select(SightNo, !!col.names) %>%
          filter(SightNo %in% das.sight.res$SightNo) %>%
          full_join(select(das.sight.res, -!!col.names), by = "SightNo") %>%
          select(!!names(das.sight.main))

        das.sight <- bind_rows(das.sight.main, d.toadd) %>% arrange(idx)
        rm(das.sight.main, das.sight.res, col.names, d.toadd)
      }

      # Get species, etc., for g events
      if (any("g" %in% sp.events)) {
        das.sight.main <- das.sight %>%
          filter(!(.data$Event %in% c("g"))) %>%
          mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
        das.sight.res <- das.sight %>%
          filter(.data$Event %in% c("g")) %>%
          mutate(ss_id = paste(SightNo, Subgroup, sep = "_"))
        validate(
          need(all(das.sight.res$ss_id %in% das.sight.main$ss_id),
               paste("Not all of the selected g resight event(s) have primary sightings with",
                     "the same sighting numbers/subgroup identified",
                     "- this is a DAS error that needs to be fixed to plot g events"))
        )

        col.names <- c("Prob", "SpCode", "SpCodeProb")
        d.toadd <- das.sight.main %>%
          select(.data$ss_id, !!col.names) %>%
          filter(.data$ss_id %in% das.sight.res$ss_id) %>%
          full_join(select(das.sight.res, -!!col.names), by = c("ss_id")) %>%
          select(!!names(das.sight.main))

        das.sight <- bind_rows(das.sight.main, d.toadd) %>%
          select(-ss_id) %>%
          arrange(idx)
        rm(das.sight.main, das.sight.res, col.names, d.toadd)
      }
    }

    validate(
      need(nrow(das.sight) > 0,
           paste("There are no mammal sightings for the selected event(s)",
                 "in the loaded DAS file(s)"))
    )

    das.sight <- das.sight %>% select(-idx)

  } else  {
    ### Error
    validate("Invalid input$das_sighting_type value")
  }

  das.sight
})


###############################################################################
# Filter for specfied species and (if applicable) events
cruzDasSightSpecies <- reactive({
  das.proc <- req(cruz.list$das.data)

  ### Sightings to plot
  sight.type <- input$das_sighting_type
  stopifnot(sight.type %in% 1:4)
  sp.selection <- isTRUE(
    (sight.type == 1 && input$das_sighting_code_1_all == 2) ||
      (sight.type == 2 && input$das_sighting_code_2_all == 2)
  )

  das.sight <- cruzDasSightEvent()

  #----------------------------------------------------------------------------
  if (sight.type == 1) {
    # 1: Mammals
    # Get species codes - also does validate() check for valid species
    sp.codes <- cruzDasSightSpeciesMammals()
    if (cruzDasSightEventResight()) {
      validate(
        need(length(sp.codes) == 1,
             "You currently can only plot resights for one species at a time")
      )
    }

    # Update probable sightings species if necessary
    if (input$das_sighting_probable) {
      das.sight$Prob[das.sight$Event %in% c("p", "s", "k", "g")] <- FALSE
      if (any(is.na(das.sight$Prob)))
        warning("A marine mammal sighting has an unexpected NA 'Prob' value")

      validate(
        need(sum(das.sight$Prob) > 0,
             "There are no probable sightings in the loaded DAS file(s)")
      )

      # 977 used as probable vaquita sighting on some cruises
      das.sight <- das.sight %>%
        mutate(SpCode = ifelse(.data$SpCode == "977", "041", .data$SpCode),
               SpCode = ifelse(.data$Prob, .data$SpCodeProb, .data$SpCode))
    }

    # Filter for selected species, and check that all selected species are in data
    das.sight <- das.sight %>% filter(.data$SpCode %in% sp.codes)

    if (input$das_sighting_code_1_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
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
    sp.codes <- cruzDasSightSpeciesTurtles()

    das.sight <- das.sight %>%
      filter(.data$SpCode %in% sp.codes)

    if (input$das_sighting_code_2_all == 2) {
      sp.codes.none <- base::setdiff(sp.codes, das.sight$SpCode)
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
    das.sight <- das.sight %>% mutate(SpCode = "Boat")
    sp.codes <- NULL


    #--------------------------------------------------------------------------
  } else {
    validate("Invalid sighting type (input$das_sighting_type) selection")
  }


  #   #--------------------------------------------------------------------------
  # # SMW: Added for vaquita-specifc cruise. Does not seem applicable anymore
  # } else if (sight.type == 4) {
  #   # 4: C-PODs
  #   # C-POD sightings are entered as objects with sighting angle and distance
  #   # the string "cpod" in the comment on the next line indicates object is a CPOD
  #   sp.codes <- NULL
  #
  #   validate(
  #     need(sum(das.proc$Event == "X") > 0,
  #          "There are no C-POD sightings in the loaded DAS file(s)")
  #   )
  #
  #
  #   ndx.x <- which(das.proc$Event == "X")
  #   comm.x1 <- apply(das.proc[ndx.X + 1, paste0("Data", 1:7)], 1, function(i) {
  #     paste(na.omit(i), collapse = "")
  #   })
  #   comm.x1.cpod <- grepl("cpod", comm.x1, ignore.case = TRUE)
  #   stopifnot(length(ndx.x) == length(comm.x1))
  #
  #   ndx.x <- ndx.x[comm.x1.cpod]
  #
  #   das.sight <- das.proc %>%
  #     slice(ndx.x) %>%
  #     mutate(Sp = "CPOD",
  #            Bearing = as.numeric(.data$Data2),
  #            DistNm = as.numeric(.data$Data4),
  #            PerpDistKm = abs(sin(.data$Bearing*pi/180) * .data$DistNm) * 1.852)
  #
  #   validate(
  #     need(nrow(das.sight) > 0,
  #          "There are no C-POD sightings in the loaded DAS file(s)")
  #   )
  #   # comment.str.df <- data.all[,6:13]
  #   # comment.str <- apply(comment.str.df,1,paste,collapse="")
  #   # ndx.cpod <- grep("cpod",comment.str)
  #   # ndx <- ndx.X[(ndx.X+1) %in% ndx.cpod]
  #   # data.sight <- data.all[ndx,]
  #   # angle <- as.numeric(data.sight$Data2)
  #   # dist.nmi <- as.numeric(data.sight$Data4)
  # }


  #----------------------------------------------------------------------------
  # Final check to ensure some sightings match provided selection
  validate(
    need(nrow(das.sight) > 0,
         "No sightings exist for the selected type and code(s)")
  )



  # If "Plot all..." was selected, only return codes in sighting data
  if ((sight.type == 1 && input$das_sighting_code_1_all == 1) |
      (sight.type == 2 && input$das_sighting_code_2_all == 1)) {
    sp.codes <- base::intersect(sp.codes, das.sight$SpCode)
  }

  # Return list
  list(das.sight = das.sight, sight.type = sight.type,
       sp.codes = sp.codes, sp.selection = sp.selection)
})

###############################################################################





# cruzDasSightRange for CruzPlot - step 2 of processing species data
#   cruzDasSightPosition returns das_sight with Lat/Lon columns adjusted for
#     world2 and ship/sighting position.
#     NOTE: Now done when removing records with NA positions in cruzDasSightProcess.R
#   cruzDasSightRange() returns list, which includes sightings within map range,
#     selected sighting type, species codes, and counts for each species


###############################################################################
cruzDasSightRange <- reactive({
  #----------------------------------------------------------------------------
  req(cruz.list$das.data)

  data.list <- cruzDasSightSpecies()

  das.sight    <- data.list$das.sight
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
         "No sightings are within the map boundaries for the selected sighting type/species")
  )


  #----------------------------------------------------------------------------
  list(
    das.sight = das.sight.filt, sight.type = sight.type,
    sp.codes = sp.codes, sp.selection = sp.selection
  )
})





# cruzDasSightSymbol for CruzPlot - step 4 of processing species data
#   cruzDasSightSymbol() returns list of sighting type, species count, and
#     point type, color, size, and linewidth for legend and points, respectively
#   cruzDasSightSymbolAnimalSelected() returns list of point type, color, size, and linewidth for selected mammal or turtle sightings
#   cruzDasSightSymbolAnimalAll() returns list of point type, color, size, and linewidth for all mammal or turtle sightings
#   cruzDasSightSymbolBoat() returns list of point type, color, size, and linewidth for boat sightings
#   cruzDasSightSymbolCPOD() returns list of point type, color, size, and linewidth for CPOD sightings

# Two things need to be done - get/format pch, col, cex, and lwd for 1) legend and 2) sighting points.
#   Boats and C-PODs al have singl value for each paramter, so (1) and (2) will be the same (and of length one).
#   For mammals and turtles, 1) legend stuff requires one value per species code,
#   while 2) sighting points require one value per sighting
#   For (2) for mammals/turtles, we can use the legend values to build a species - plot params key,
#   which is then joined with das.sight


###############################################################################
cruzDasSightSymbol <- reactive({
  data.list <- cruzDasSightFilter()

  sight.type   <- data.list$sight.type
  das.sight    <- data.list$das.sight
  sp.codes     <- data.list$sp.codes
  sp.count     <- data.list$sp.count
  sp.selection <- data.list$sp.selection

  # Species code sanity check
  if (sight.type %in% c(1, 2)) {
    stopifnot(isTRUE(all.equal(length(sp.codes), length(sp.count))))
  } else {
    stopifnot(is.null(sp.codes))
  }

  if (sight.type %in% c(3, 4)) {
    # Boats and C-Pods
    symbol.prop <- cruzDasSightSymbolBoat()
    leg.df <- data.frame(
      SpCode = ifelse(sight.type == 3, "Boat", "CPOD"),
      pch = symbol.prop$pt.pch,
      col = symbol.prop$pt.col,
      cex = symbol.prop$pt.cex,
      lwd = symbol.prop$pt.lwd,
      stringsAsFactors = FALSE
    )

  } else {
    # Mammals and turtles
    symbol.prop <- if (sp.selection) {
      cruzDasSightSymbolAnimalSelected()
    } else {
      cruzDasSightSymbolAnimalAll()
    }

    # For validate statements
    leg.pch <- symbol.prop$pt.pch
    leg.col <- symbol.prop$pt.col
    leg.cex <- symbol.prop$pt.cex
    leg.lwd <- symbol.prop$pt.lwd

    # Empty input check
    validate(
      need(leg.pch != "",
           "Please enter at least one number for symbol type"),
      need(leg.col != "",
           "Please enter at least one number for symbol color"),
      need(leg.cex != "",
           "Please enter at least one number for symbol size"),
      need(all(0 < leg.cex & leg.cex < 20),
           paste("Please ensure all symbol size entries are numbers",
                 "greater than zero and less than twenty")),
      need(leg.lwd != "",
           "Please enter at least one number for symbol line width"),
      need(all(0 < leg.lwd & leg.lwd < 20),
           paste("Please ensure all symbol line width entries are numbers",
                 "greater than zero and less than twenty"))
    )

    # For each parameter, rep() until it's length == sp.codes.len,
    #   and check that there aren't more selected than species
    if (cruzDasSightEventResight()) {
      # Requires that only one species is selected
      validate(
        need(length(leg.cex) == 1,
             "Please provide only one symbol size when plotting resights")
      )
      leg.df <- data.frame(
        SpCode = sp.codes,
        Event = sort(input$das_sighting_events, decreasing = TRUE),#Ensures S, s, or equivalent
        pch = .func_sight_symbol_pt(leg.pch, "type", 1),
        col = .func_sight_symbol_pt(leg.col, "color", 2),
        cex = .func_sight_symbol_pt(c(leg.cex, 0.75 * leg.cex), "size", 2),
        lwd = .func_sight_symbol_pt(leg.lwd, "line width", 1),
        stringsAsFactors = FALSE
      )

    } else {
      sp.codes.len <- length(sp.codes)
      leg.df <- data.frame(
        SpCode = sp.codes,
        pch = .func_sight_symbol_pt(leg.pch, "type", sp.codes.len),
        col = .func_sight_symbol_pt(leg.col, "color", sp.codes.len),
        cex = .func_sight_symbol_pt(leg.cex, "size", sp.codes.len),
        lwd = .func_sight_symbol_pt(leg.lwd, "line width", sp.codes.len),
        stringsAsFactors = FALSE
      )
    }
  }

  if (cruzDasSightEventResight()) {
    pt.df <- left_join(das.sight, leg.df, by = c("SpCode", "Event")) %>%
      select(.data$SpCode, .data$Lon, .data$Lat,
             .data$pch, .data$col, .data$cex, .data$lwd)
  } else {
    pt.df <- left_join(das.sight, leg.df, by = "SpCode") %>%
      select(.data$SpCode, .data$Lon, .data$Lat,
             .data$pch, .data$col, .data$cex, .data$lwd)
  }

  # # If specified, make the symbol color correspond to event code
  # if (sight.type %in% c(1, 2) & input$das_symbol_event) {
  #   pt.df
  # }

  list(
    sight.type = sight.type, sp.count = sp.count, sp.codes = sp.codes,
    leg.df = leg.df, pt.df = pt.df, das.sight = das.sight
  )
})



.func_sight_symbol_pt <- function(pt.x, pt.txt, sp.codes.len, valid.check = TRUE) {
  if (valid.check)
    validate(
      need(sp.codes.len >= length(pt.x),
           paste("There are more symbol", pt.txt, "entries than species"))
    )

  if (sp.codes.len > length(pt.x)) {
    pt.x <- rep(pt.x, ceiling(sp.codes.len / length(pt.x)))
    pt.x <- pt.x[1:sp.codes.len]
  }
  # else {
  #   pt.x <- pt.x[1:sp.codes.len]
  # }

  pt.x
}


###############################################################################
# Mammal or turtle symbol properties
# Plot parameters for plotting selected mammal/turtle species
cruzDasSightSymbolAnimalSelected <- reactive({
  if (!input$das_symbol_mult) {
    pt.pch <- as.numeric(input$das_symbol_type)
    pt.col <- input$das_symbol_color

  } else {
    pt.pch <- as.numeric(unlist(strsplit(input$das_symbol_type_mult, ",")))
    pt.col <- str_trim(unlist(strsplit(input$das_symbol_color_mult, ",")))
    # pt.pch <- as.numeric(unlist(strsplit(cruz.das.symbol.type(), ",")))
    # pt.col <- str_trim(unlist(strsplit(cruz.das.symbol.color(), ",")))
    validate(
      need(length(pt.pch) > 0, # duplicate of check in main function
           "Please enter at least one value for symbol type"),
      need(all(pt.pch %in% 0:20),
           paste("Not all symbol type entries are valid." ,
                 "Please be sure all entries are a whole number from 0 to 20")),
      need(length(pt.col) > 0,
           "Please enter at least one value for symbol color")
    )

    # Covert color names to color codes-codes established in server file,
    #   keeping them in order
    valid.message <- paste(
      "Not all symbol color entries are valid. Please be sure each entry",
      "matches a color in the Color and Formatting Options page"
    )
    if (input$color_style == 1) {
      validate(need(all(pt.col %in% symbol.col), valid.message))
      pt.col <- vapply(pt.col, function(i) which(symbol.col %in% i), 1)
      pt.col <- symbol.col.code[pt.col]

    } else if (input$color_style == 2) {
      validate(need(all(pt.col %in% symbol.col.gray), valid.message))
      pt.col <- vapply(pt.col, function(i) which(symbol.col.gray %in% i), 1)
      pt.col <- symbol.col.code.gray[pt.col]
    }
  }

  # Same whether input$das_symbol_mult is checked or not
  pt.cex <- as.numeric(unlist(strsplit(input$das_symbol_size, ",")))
  pt.lwd <- as.numeric(unlist(strsplit(input$das_symbol_linewidth, ",")))

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# Plot parameters for plotting all mammal/turtle species
cruzDasSightSymbolAnimalAll <- reactive({
  sp.codes.len <- length(cruzDasSightFilter()$sp.codes)
  pch.all <- unname(unlist(cruz.symbol.type))
  col.all <- c(
    "black", "red", "forestgreen", "orange",
    "blue", "tan4", "yellow", "aquamarine2", "bisque1", "hotpink",
    "green", "wheat3", "lightblue", "indianred2", "gray"
  )

  pt.pch <- rep(pch.all, ceiling(sp.codes.len / length(pch.all)))
  pt.pch <- pt.pch[1:sp.codes.len]

  pt.col <- rep(col.all, each = length(pch.all))
  if (length(pt.col) < sp.codes.len) stop("Sight symbol error - report as issue")
  pt.col <- pt.col[1:sp.codes.len]

  pt.cex <- 1
  pt.lwd <- 1

  stopifnot(
    length(pt.pch) == length(unique(cruzDasSightFilter()$das.sight$SpCode)),
    length(pt.col) == length(unique(cruzDasSightFilter()$das.sight$SpCode))
  )

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# Plot parameters for plotting boats or CPODs
cruzDasSightSymbolBoat <- reactive({
  pt.pch <- as.numeric(input$das_symbol_type_boat)
  pt.col <- input$das_symbol_color_boat
  pt.cex <- as.numeric(input$das_symbol_size_boat)
  pt.lwd <- as.numeric(input$das_symbol_linewidth_boat)

  list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
})

# CPOD symbol properties
# cruzDasSightSymbolCPOD <- reactive({
#   pt.pch <- as.numeric(input$das.symbol.type.cpod)
#   pt.col <- input$das.symbol.color.cpod
#   pt.cex <- as.numeric(input$das.symbol.size.cpod)
#   pt.lwd <- as.numeric(input$das.symbol.linewidth.cpod)
#
#   list(pt.pch = pt.pch, pt.col = pt.col, pt.cex = pt.cex, pt.lwd = pt.lwd)
# })

###############################################################################





### cruzDasOutTabular
## Code for tabular output of sighitngs and effort


###############################################################################
# Table of all sightings in file
cruzDasOutSight_TotTable <- reactive({
  req(cruzDasSightFilter())
  das.sight <- cruzDasSightSpeciesProcess()
  sight.type <- input$das_sighting_type

  if (sight.type == 2) {
    ### Turtles
    das.sight <- das.sight %>% filter(.data$Event == "t")
    df.join <- NULL
    df.out <- if (input$das_sighting_code_2_all == 1) {
      das.sight %>%
        group_by(Event) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      das.sight %>%
        filter(SpCode %in% input$das_sighting_code_2) %>%
        group_by(Event, SpCode) %>%
        summarise(Count = n(), .groups = "drop")
    }

  } else if (sight.type == 3) {
    ### Boats
    das.sight <- das.sight %>% filter(.data$Event == "F")
    df.join <- NULL
    df.out <- das.sight %>%
      group_by(Event) %>%
      summarise(Count = n(), .groups = "drop")

  } else if (sight.type == 1) {
    ### Marine mammals
    das.sight <- das.sight <- das.sight %>%
      filter(.data$Event %in% input$das_sighting_events)
    df.join <- data.frame(Event = input$das_sighting_events, stringsAsFactors = FALSE)

    df.out <- if (input$das_sighting_code_1_all == 1) {
      das.sight %>%
        group_by(Event) %>%
        summarise(Count = n(), .groups = "drop")
    } else {
      das.sight %>%
        filter(SpCode %in% input$das_sighting_code_1) %>%
        group_by(Event, SpCode) %>%
        summarise(Count = n(), .groups = "drop")
    }
  }

  if (isTruthy(df.join)) {
    df.out <- df.out %>%
      full_join(df.join, by = "Event") %>%
      mutate(Count = ifelse(is.na(.data$Count), 0, .data$Count)) %>%
      arrange(.data$Event)
  }

  t(df.out)
})

# Sightings table
cruzDasOutSight_Table <- reactive({
  req(cruz.list$das.data)
  validate(
    need(input$das_sightings,
         "'Plot sightings' must be selected to generate tabular sightings output")
  )


  # Get filtered data, which also handles error checks
  data.list <- cruzDasSightFilter()
  das.sight <- data.list$das.sight

  das.sight.summ <- das.sight %>%
    group_by(.data$SpCode) %>%
    summarise(std = sum(.data$OnEffort & .data$EffType == "S", na.rm = TRUE),
              #na.rm=TRUE b/c off effort sightings might not have effort type
              nstd = sum(.data$OnEffort & .data$EffType == "N", na.rm = TRUE),
              fine = sum(.data$OnEffort & .data$EffType == "F", na.rm = TRUE),
              off_eff = sum(!.data$OnEffort),
              total = n(),
              .groups = "drop")

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
    c(list(`Species code` = "All"), lapply(select(das.sight.summ, -SpCode), sum))
  } else {
    NULL
  }

  # Add in selected species identifiers, join, and do name wrangling
  das.sight.sp <- das.sight.summ %>%
    select(.data$SpCode) %>%
    left_join(req(cruz.list$sp.codes), by = c("SpCode" = "Code"))

  das.sight.sp %>%
    rename("Species code" = .data$SpCode, "Abbreviation" = .data$Abbr,
           "Scientific name" = .data$Name_Scientific,
           "Common name" = .data$Name_Common) %>%
    select(c(1, as.numeric(input$das_out_sciname))) %>%
    left_join(das.sight.summ, by = c("Species code" = "SpCode")) %>%
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
  das.eff.lines <- cruzDasEffortFilter() %>%
    mutate(st_lon = ifelse(.data$st_lon > 180, .data$st_lon - 360, .data$st_lon),
           end_lon = ifelse(.data$end_lon > 180, .data$end_lon - 360, .data$end_lon))

  # Calculate distance
  dist.effort.km <- geosphere::distVincentyEllipsoid(
    cbind(das.eff.lines$st_lon, das.eff.lines$st_lat),
    cbind(das.eff.lines$end_lon, das.eff.lines$end_lat)
  ) / 1000

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
                total = sum(dist),
                .groups = "drop")

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
