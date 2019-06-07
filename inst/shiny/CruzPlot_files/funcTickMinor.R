# funcTickMinor for CruzPlot by Sam Woodman
#   Inputs: range of figure, location of major tick intervals, width of major tick intervals, number of minor tick marks
#   Returns: vector with locations of the minor tick marks

cruzTickMinor <- function (deg.range, maj.ticks, tick.maj.interval, n=2) 
{
  sep <- tick.maj.interval/(n+1)
  min.ticks1 <- seq(maj.ticks[1], deg.range[2], by = sep)
  min.ticks2 <- rev(seq(maj.ticks[1], deg.range[1], by = -sep))
  min.ticks <- c(min.ticks2[1:length(min.ticks2)-1], min.ticks1)
  
  return(min.ticks)
}