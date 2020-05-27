# drawInteractive for CruzPlot: Plot interactive points


### Prep
lon.range <- cruz.map.range$lon.range
lat.range <- cruz.map.range$lat.range

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
  for(i in 1:(length(sight$click)/2)) {
    text(x = sight$click[(2*i)-1], sight$click[(2*i)], sight$lab[i], pos = 1)
  }

  if (sight$miss)
    text(x = (lon.range[1] + 1), y = (lat.range[1] + 3),
         labels = "Click was not close enough to a sighting", pos = 4)
}

if (isTruthy(sight$hover)) {
  if (input$das_sight_interactive != 3) effort$hover <- NULL
  if (sight$hover.miss) {
    text(x = (abs(lon.range[2]-lon.range[1])*.1) + lon.range[1],
         y = (abs(lat.range[2]-lat.range[1])*.1) + lat.range[1],
         labels = "Cursor is not close enough to a sighting", pos = 4)
  } else {
    text(x = sight$hover[1], sight$hover[2], labels = sight$hover.lab, pos = 1)
  }
}


### Effort labels
if (isTruthy(effort$click)) {
  for(i in 1:(length(effort$click)/2)) {
    text(x = effort$click[(2*i)-1], effort$click[(2*i)], effort$lab[i], pos = 1)
  }

  if (effort$miss)
    text(x=(abs(lon.range[2]-lon.range[1])*.1)+lon.range[1],
         y = (abs(lat.range[2]-lat.range[1])*.1)+lat.range[1],
         labels = "Click was not close enough to an\neffort start or end point", pos = 4)
}

if (isTruthy(effort$hover)) {
  if (effort$hover.miss) {
    text(x = (abs(lon.range[2]-lon.range[1])*.1) + lon.range[1],
         y = (abs(lat.range[2]-lat.range[1])*.1) + lat.range[1],
         labels = "Cursor is not close enough to an\neffort start or end point", pos = 4)
  } else {
    text(x = effort$hover[1], effort$hover[2], labels = effort$hover.lab, pos = 1)
  }
}
