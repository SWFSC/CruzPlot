#' Tick calculations
#'
#' Tick calculations
#'
#' @name cruzTick
#'
#' @param deg.range numeric vector of the range of figure
#' @param lon.range numeric vector with two elements: the left and right longitude values
#' @param lat.range numeric vector with two elements: the left and right latitude values
#' @param l.range numeric vector with two elements: the left and right values
#'   fopr either the latitude or longitude
#' @param maj.ticks numeric vector of the location of major ticks
#' @param tick.maj.interval width of major tick intervals
#' @param n integer; default=2. Number of minor tick marks
#'
#' @details
#' Additional details...
#'
#' @returns
#' * cruzTickMinor: minor tick locations
#' * cruzTickStart: start location based on lat/lon input and length of tick interval
#' * cruzTickUpdate: Returns default starter value for major tick interval, based on the longitude and latitude range
#'
#' @export
cruzTickMinor <- function (deg.range, maj.ticks, tick.maj.interval, n=2) {
  sep <- tick.maj.interval / (n+1)
  min.ticks1 <- seq(maj.ticks[1], deg.range[2], by = sep)
  min.ticks2 <- rev(seq(maj.ticks[1], deg.range[1], by = -sep))
  min.ticks <- c(min.ticks2[1:length(min.ticks2)-1], min.ticks1)

  min.ticks
}


#' @name cruzTick
#' @export
cruzTickStart <- function(l.range, tick.maj.interval) {
  l.start <- ifelse(
    l.range[1]%%tick.maj.interval > 0,
    l.range[1] + tick.maj.interval - l.range[1]%%tick.maj.interval,
    l.range[1])

  if (!(l.range[1] < l.start && l.start < l.range[2])) l.start <- l.range[1]

  l.start
}


#' @name cruzTick
#' @export
cruzTickUpdate <- function(lon.range, lat.range) {
  lon.diff <- abs(lon.range[2] - lon.range[1])
  lat.diff <- abs(lat.range[2] - lat.range[1])
  tick.breaks <- c(0,2,5,10,40,75,120,361)
  tick.interval <- c(0.5, 1, 2, 5, 10, 15, 30)

  lon.tick.interval <- tick.interval[cut(lon.diff, breaks = tick.breaks, labels = tick.interval)]
  lat.tick.interval <- tick.interval[cut(lat.diff, breaks = tick.breaks, labels = tick.interval)]
  tick.maj.interval <- max(lon.tick.interval, lat.tick.interval)

  tick.maj.interval
}
