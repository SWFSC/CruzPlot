# funcTickStart for CruzPlot by Sam Woodman
#   Input: vector of either longitude or latitude range and major tick interval
#   Returns: start location based on lat/lon input and length of tick interval

cruzTickStart <- function(l.range, b) 
{
  l.start <- ifelse(l.range[1]%%b>0, l.range[1]+b-l.range[1]%%b, l.range[1])
  if(!(l.range[1] < l.start && l.start < l.range[2])) l.start <- l.range[1] 
  
  return(l.start)
}