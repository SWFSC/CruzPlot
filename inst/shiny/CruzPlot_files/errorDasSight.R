# errorDasSight for CruzPlot by Sam Woodman
#   Outputs error messages for invalid sighting filters


validate(
  need(input$das.sight.minBeau <= input$das.sight.maxBeau,
       message = "Minimum beaufort must be less than or equal to maximum beaufort"),
  need(as.numeric(difftime(input$das.sight.dateRange[2], input$das.sight.dateRange[1])) >= 0, 
       message = paste("Minimum date, ", format(input$das.sight.dateRange[1], format = "%d%b%Y"), 
                       ", is after maximimum date, ", format(input$das.sight.dateRange[2], format = "%d%b%Y"), sep = ""))
)