# errorScaleBar for CruzPlot by Sam Woodman
#   Outputs error messages for invalid scale bar-related entries
#   Run within drawMap_setVals.R


validate(
  need(!is.na(input$label.title.size),
       "Please enter a valid title size value") %then%
    need(!is.na(input$label.axis.size),
         "Please enter a valid axis label size value") %then%
    need(input$label.title.size > 0, 
         "Please enter a title size greater than zero") %then%
    need(input$label.axis.size > 0, 
         "Please enter an axis label size greater than zero")
)

validate(
  need(lon.range[1] <= scale.bar$x1, 
       message = "Start of scale bar must be after left longitude value"),
  need(lon.range[2] >= (scale.bar$x2), 
       message = "End of scale bar must be before right longitude value"),
  need(lat.range[1] <= scale.bar$y,
       message = "Scale bar latitude must be greater than bottom latitude value"),
  need(lat.range[2] >= scale.bar$y, 
       message = "Scale bar latitude must be less than top latitude value")
)