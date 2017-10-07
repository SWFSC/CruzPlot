# errorLabel for CruzPlot by Sam Woodman
#   Outputs error messages for invalid title/axis label entries
#   Run within drawMap_setVals.R


### Check that tick intervals are possibly valid
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