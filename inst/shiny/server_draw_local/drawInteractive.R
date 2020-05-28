# drawInteractive for CruzPlot: Plot interactive points


### Prep
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range

lon.diff <- abs(lon.range[2]-lon.range[1])
lat.diff <- abs(lat.range[2]-lat.range[1])

lon.mult <- 0.02
lat.mult1 <- 0.05
lat.mult2 <- 0.1


validate(
  need((input$das_effort_interactive == 1) || (input$das_sight_interactive == 1),
       message = "Cannot have both sighting and effort interactive plots selected")
)
if ((input$das_effort_interactive != 1) || (input$das_sight_interactive != 1)) {
  validate(
    need(cruz.list$das.data,
         "Please load a DAS data file before selecting an interactive plot")
  )
}


### Sighting labels
if (isTruthy(sight$click)) {
  for(i in seq_along(sight$click)) {
    text(x = sight$click[[i]][1], y = sight$click[[i]][2], labels = sight$lab[i], pos = 1)
  }
}

if (sight$miss)
  text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult1) + lat.range[1],
       labels = "Click was too far from a sighting", pos = 4)

if (isTruthy(sight$hover)) {
  if (sight$hover.miss) {
    text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult2) + lat.range[1],
         labels = "Cursor is too far from a sighting", pos = 4)
  } else {
    text(x = sight$hover[1], y = sight$hover[2], labels = sight$hover.lab, pos = 1)
  }
}


### Effort labels
if (isTruthy(effort$click)) {
  for(i in seq_along(effort$click)) {
    text(x = effort$click[[i]][1], y = effort$click[[i]][2], labels = effort$lab[i], pos = 1)
  }
}

if (effort$miss)
  text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult1) + lat.range[1],
       labels = "Click was too far from an effort start or end point", pos = 4)

if (isTruthy(effort$hover)) {
  if (effort$hover.miss) {
    text(x = (lon.diff*lon.mult) + lon.range[1], y = (lat.diff*lat.mult2) + lat.range[1],
         labels = "Cursor is too far from an effort start or end point", pos = 4)
  } else {
    text(x = effort$hover[1], y = effort$hover[2], labels = effort$hover.lab, pos = 1)
  }
}
