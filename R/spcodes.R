#' CruzPlot spcodes filtering
#'
#' Filter spcodes file for turtle/mammal species
#'
#' @name spcodes
#'
#' @param spcodes spcodes data frame. 
#'   Expects the output of [swfscDAS::das_spcodes_read()]
#'
#' @details
#' These functions are filter a SpCodes data frame, 
#' from [swfscDAS::das_spcodes_read()], for either 
#' mammal (`spcodes_mammals`) or turtle (`spcodes_turtles`) species.
#' Specifically, this data frame must have four columns, with names
#' 'SpCode', 'Abbr', 'SciName', and 'CommonName'. 
#' 
#' To identify mammal/turtle species names, `CruzPlot` uses the 
#' following logic:
#' 
#' - a species code is a turtle species if it contains "turtle" 
#'   (case-insensitive) in the CommonName column
#' - a species code is a mammal species if the species code 
#'   is not a turtle species. 
#'
#' @returns 
#' The input data frame `spcodes`, filtered for either mammal
#' or turtle species rows.   
#' 
#' @examples
#' sp.codes.file <- system.file("extdata", "SpCodes.dat", package = "swfscDAS")
#' 
#' spcodes_turtles(swfscDAS::das_spcodes_read(sp.codes.file))
#' spcodes_mammals(swfscDAS::das_spcodes_read(sp.codes.file))
#' 
#' @export
spcodes_turtles <- function(spcodes) {
  # TODO: check that sp.codes is a valid sp.codes data frame

  # ind.turtles <- which(sp.codes$Code %in% turtle.codes)
  turtle.nona <- !is.na(spcodes$CommonName)
  turtle.flag <- str_detect(str_to_lower(spcodes$CommonName), "turtle")
  ind.turtles <- turtle.flag & turtle.nona

  spcodes[ind.turtles, ]
}


#' @name spcodes
#' @export
spcodes_mammals <- function(spcodes) {
  turtle.codes <- spcodes_turtles(spcodes)$SpCode
  ind.mammals <- which(!(spcodes$SpCode %in% turtle.codes))

  spcodes[ind.mammals, ]
}
