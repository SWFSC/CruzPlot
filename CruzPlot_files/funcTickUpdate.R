# cruzTickUpdate for CruzPlot by Sam Woodman
#   Returns default starter value for major tick interval based on the longitude and latitude range

cruzTickUpdate <- function(lon.range, lat.range)
{
  lon.diff <- abs(lon.range[2]-lon.range[1])
  lat.diff <- abs(lat.range[2]-lat.range[1])
  tick.breaks <- c(0,2,5,10,40,75,120,361)
  tick.interval <- c(0.5, 1, 2, 5, 10, 15, 30)
  
  lon.tick.interval <- tick.interval[cut(lon.diff, breaks = tick.breaks, labels = tick.interval)]
  lat.tick.interval <- tick.interval[cut(lat.diff, breaks = tick.breaks, labels = tick.interval)]
  tick.maj.interval <- max(lon.tick.interval, lat.tick.interval)
  
  return(tick.maj.interval)
}