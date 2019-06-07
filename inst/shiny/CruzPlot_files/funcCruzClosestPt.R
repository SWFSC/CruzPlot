# cruzClosestPt for CruzPlot by Sam Woodman
#   Inputs: location of point on map, das data frame, type of sighting or effort 
#   Returns label postion and text
#     type: 1 = effort, 2 = mammal sightings (with sighting number), 3= effort R and E locations, 4  = non-mammal sightings labeled with time

cruzClosestPt <- function(curr.pt, data.das, type)
{
  if(type == 2 | type == 4) {
    x1 <- abs(data.das$sight.lon - curr.pt[1])
    y1 <- abs(data.das$sight.lat - curr.pt[2])
  }
  if (type == 1 | type == 3) {
    x1 <- abs(data.das$Lon - curr.pt[1])
    y1 <- abs(data.das$Lat - curr.pt[2])
  }
  min.index <- which.min(sqrt(x1^2 + y1^2))
  lab <- paste(      # "Cr:", data.das$Cruise[min.index], "\n",   # no cruise number for vaquita cruise
               format(data.das$Date[min.index], format = "%d%b%y"))
  if(type  > 3) lab <- paste(substr(data.das$Date[min.index],12,13),substr(data.das$Date[min.index],15,16), "\n",lab,sep="") 
  if(type == 2) lab <- paste("#", as.numeric(data.das$Data1[min.index]), "\n", lab,
                             " ",substr(data.das$Date[min.index],12,13),substr(data.das$Date[min.index],15,16),substr(data.das$Date[min.index],18,19),
                             "\n",round(data.das$Lat[min.index],4),", ",round(data.das$Lon[min.index],4),sep="" )
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