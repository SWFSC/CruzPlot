### Non-reactive functions used in CruzPlot R Shiny app


#------------------------------------------------------------------------------
# Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) substr(x, nchar(x) - n + 1, nchar(x))


#------------------------------------------------------------------------------
# cruzClosestPt for CruzPlot
#   Inputs: location of point on map, das data frame, type of sighting or effort
#   Returns label postion and text
#     type: 1 = effort, 2 = mammal sightings (with sighting number),
#     3= effort R and E locations, 4  = non-mammal sightings labeled with time

cruzClosestPt <- function(curr.pt, data.das, type) {
  if(type == 2 | type == 4) {
    x1 <- abs(data.das$sight.lon - curr.pt[1])
    y1 <- abs(data.das$sight.lat - curr.pt[2])
  }
  if (type == 1 | type == 3) {
    x1 <- abs(data.das$Lon - curr.pt[1])
    y1 <- abs(data.das$Lat - curr.pt[2])
  }
  min.index <- which.min(sqrt(x1^2 + y1^2))
  # "Cr:", data.das$Cruise[min.index], "\n",   # no cruise number for vaquita cruise
  lab <- paste(
    format(data.das$Date[min.index], format = "%d%b%y"))
  if(type  > 3) {
    lab <- paste0(
      substr(data.das$Date[min.index],12,13),
      substr(data.das$Date[min.index],15,16), "\n", lab
    )
  }
  if(type == 2) {
    lab <- paste0(
      "#", as.numeric(data.das$Data1[min.index]), "\n", lab, " ",
      substr(data.das$Date[min.index],12,13),
      substr(data.das$Date[min.index],15,16),
      substr(data.das$Date[min.index],18,19), "\n",
      round(data.das$Lat[min.index],4), ", ",
      round(data.das$Lon[min.index],4)
    )
  }
  #  if(type == 2) lab <- paste("#", as.numeric(data.das$Data1[min.index]), "\n", lab)
  if(type == 3) {
    # Identified E event
    if(min.index %% 2 == 0) {
      min.index.2 <- min.index-1
      r <- round(c(data.das$Lon[min.index.2], data.das$Lat[min.index.2]), 4)
      e <- round(c(data.das$Lon[min.index], data.das$Lat[min.index]), 4)
    }
    # Identified R event
    if(min.index %% 2 == 1) {
      min.index.2 <- min.index+1
      r <- round(c(data.das$Lon[min.index], data.das$Lat[min.index]), 4)
      e <- round(c(data.das$Lon[min.index.2], data.das$Lat[min.index.2]), 4)
    }
    lab <- paste(lab, "\n", "R:", r[1], ",", r[2], "\n", "E:", e[1], ",", e[2])
  }

  c(x1[min.index], y1[min.index], lab)
}


#------------------------------------------------------------------------------
# cruzSpeciesRead for CruzPlot
#   Input: .dat file
#   Returns: data frame containing the species code, abbreviation,
#     scientific name, and common name

cruzSpeciesRead <- function(file) {
  sp.codes <- scan(file, what=character(), sep="\n", quiet = T)
  Code<- str_trim(substring(sp.codes, 2, 4), side = "both")
  Abbr <- str_trim(substring(sp.codes, 6, 17), side = "both")
  Name.Scientific <- str_trim(substring(sp.codes, 18, 57), side = "both")
  Name.Common <- str_trim(substring(sp.codes, 58), side = "both")

  data.frame(Code, Abbr, Name.Scientific, Name.Common, stringsAsFactors = FALSE)
}


#------------------------------------------------------------------------------
# funcTickMinor for CruzPlot
#   Inputs: range of figure, location of major tick intervals,
#     width of major tick intervals, number of minor tick marks
#   Returns: vector with locations of the minor tick marks

cruzTickMinor <- function (deg.range, maj.ticks, tick.maj.interval, n=2) {
  sep <- tick.maj.interval / (n+1)
  min.ticks1 <- seq(maj.ticks[1], deg.range[2], by = sep)
  min.ticks2 <- rev(seq(maj.ticks[1], deg.range[1], by = -sep))
  min.ticks <- c(min.ticks2[1:length(min.ticks2)-1], min.ticks1)

  min.ticks
}


#------------------------------------------------------------------------------
# funcTickStart for CruzPlot
#   Input: vector of either longitude or latitude range and major tick interval
#   Returns: start location based on lat/lon input and length of tick interval

cruzTickStart <- function(l.range, b) {
  l.start <- ifelse(l.range[1]%%b > 0, l.range[1] + b - l.range[1]%%b, l.range[1])
  if(!(l.range[1] < l.start && l.start < l.range[2])) l.start <- l.range[1]

  l.start
}


#------------------------------------------------------------------------------
# cruzTickUpdate for CruzPlot
#   Returns default starter value for major tick interval based on the longitude and latitude range

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

#------------------------------------------------------------------------------
