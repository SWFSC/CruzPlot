#' Get closest point
#'
#' Get the closes point to a map click
#'
#' @param curr.pt description
#' @param type description
#' @param das.lat description
#' @param das.lon description
#' @param das.date description
#' @param das.sightno description
#' @param das.cruise description
#' @param das.lat2 description
#' @param das.lon2 description
#'
#' @details
#' Inputs: location of point on map, type of sighting or effort, and data from
#' DAS data type: 1 = mammal sightings (with sighting number), 2 = non-mammal
#' sightings labeled with time, 3 = effort R and E locations, 4 = effort
#' @returns Returns label postion and text
#'
#' @export
cruzClosestPt <- function(
    curr.pt,
    type,
    das.lat,
    das.lon,
    das.date,
    das.sightno = NULL,
    das.cruise = NULL,
    das.lat2 = NULL,
    das.lon2 = NULL
) {
  stopifnot(type %in% (1:3))

  # Determine the DAS point closest to the map point
  x1 <- abs(das.lon - curr.pt[1])
  y1 <- abs(das.lat - curr.pt[2])

  if (type %in% c(1, 2)) { #sightings
    min.index <- which.min(sqrt(x1^2 + y1^2))

  } else { #effort
    x2 <- abs(das.lon2 - curr.pt[1])
    y2 <- abs(das.lat2 - curr.pt[2])

    d.df <- data.frame(d1 = sqrt(x1^2 + y1^2), d2 = sqrt(x2^2 + y2^2))
    d.min <- apply(d.df, 1, min)
    min.index <- which.min(d.min)
  }

  # After determining closest DAS point, convert longitudes to [-180, 180] for display
  das.lon <- ifelse(das.lon > 180, das.lon - 360, das.lon)
  if (!is.null(das.lon2)) ifelse(das.lon2 > 180, das.lon2 - 360, das.lon2)

  # Extract DAS information
  das.date.val <- das.date[min.index]
  # "Cr:", data.das$Cruise[min.index], "\n",   # no cruise number for vaquita cruise
  lab.date <- paste(format(das.date.val, format = "%d%b%Y"))
  lab.dt <- paste(format(das.date.val, format = "%d%b%Y %H:%M"))

  # Make label to print on map
  if (type == 1) {
    # Marine mammal sighting
    lab <- paste0(
      "Sight# ", as.numeric(das.sightno[min.index]), "\n",
      lab.dt, "\n",
      round(das.lat[min.index], 2), ", ",
      round(das.lon[min.index], 2)
    )

  } else if (type == 2) {
    # Other sighting
    lab <- paste0(
      lab.dt, "\n",
      round(das.lat[min.index], 2), ", ", round(das.lon[min.index], 2)
    )

  } else if (type == 3) {
    # Simplified effort
    pt.st <- round(c(das.lon[min.index], das.lat[min.index]), 2)
    pt.end <- round(c(das.lon2[min.index], das.lat2[min.index]), 2)

    lab <- paste0(
      lab.dt, "\n",
      "R: ", paste(pt.st, collapse = ", "), "\n",
      "E: ", paste(pt.end, collapse = ", ")
    )

  }

  c(x1[min.index], y1[min.index], lab)
}
