# errorTick for CruzPlot by Sam Woodman
#   Outputs error messages for invalid tick-related entries
#   Run within drawMap_setVals.R

### Check that tick intervals are possibly valid
validate(
  need(!is.na(cruz.tick$tick.interval.major),
       "Please enter a valid major tick interval value") %then%
    need(!is.na(input$tick.interval.minor),
         "Please enter a valid minor tick interval value") %then%
    need(cruz.tick$tick.interval.major > 0,
         "Please enter a major tick interval value greater than zero") %then%
    need(input$tick.interval.minor >= 0,
         paste("Please enter a minor tick interval value",
               "greater than or equal to zero"))
)

### Check that tick label size is a valid entry
validate(
  need(!is.na(input$label.tick.size),
       "Please enter a valid tick label size value") %then%
    need(input$label.tick.size >= 0,
         paste("Please enter a tick label size value",
               "greater than or equal to zero"))
)



### Check that actual values are valid given map rnages
if(!cruz.map.range$world2) {
  validate(
    need(lon.range[1] <= as.numeric(cruz.tick$label.lon.start), 
         message = "Start of longitude tick labels must be after left longitude value"),
    need(lon.range[2] >= as.numeric(cruz.tick$label.lon.start), 
         message = "Start of longitude tick labels must be before right longitude value")
  )
}
if(cruz.map.range$world2) {
  validate(
    need(as.numeric(cruz.tick$label.lon.start) != 0, 
         message = "Please use '180' rather than '0' for the start of longitude tick labels")
  )
  if(as.numeric(cruz.tick$label.lon.start) < 0)
  {
    validate(
      need((as.numeric(cruz.tick$label.lon.start) + 180) <= lon.range[2], 
           message = "Start of longitude tick labels must be before right longitude value")
    )
  }
  if(as.numeric(cruz.tick$label.lon.start) > 0)
  {
    validate(
      need(lon.range[1] <= (as.numeric(cruz.tick$label.lon.start)),
           message = "Start of longitude tick labels must be after left longitude value")
    )
  }
}
validate(
  need(lat.range[1] <= cruz.tick$label.lat.start,
       message = "Start of latitude tick labels must be greater than bottom latitude value"),
  need(lat.range[2] >= cruz.tick$label.lat.start, 
       message = "Start of latitude tick labels must be less than top latitude value"),
  need(!is.na(input$tick.length),
       message = "Please enter a valid tick length value")
)
