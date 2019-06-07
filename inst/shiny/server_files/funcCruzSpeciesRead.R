# cruzSpeciesRead for CruzPlot by Sam Woodman
#   Input: .dat file
#   Returns: data frame containing the species code, abbreviation, scientific name, and common name

cruzSpeciesRead <- function(file)
{
  sp.codes <- scan(file, what=character(), sep="\n", quiet = T)
  Code<- str_trim(substring(sp.codes, 2, 4), side = "both")
  Abbr <- str_trim(substring(sp.codes, 6, 17), side = "both")
  Name.Scientific <- str_trim(substring(sp.codes, 18, 57), side = "both")
  Name.Common <- str_trim(substring(sp.codes, 58), side = "both")
  
  data.frame(Code, Abbr, Name.Scientific, Name.Common, stringsAsFactors = FALSE)
}