# cruzSpecies for CruzPlot by Sam Woodman
#   cruzSpecies() returns data frame of all species information (code, abbreviation, scientific name, and common name) from .dat file
#   cruzSpeciesMammals() returns data frame of all species information from mammals in .dat file
#   cruzSpeciesTurtles() returns data frame of all species information from turtles in .dat file
#   render: mammal and turtle species info (code and scientific name) for user to choose from


# Species information
cruzSpecies <- reactive({
  sp.codes <- cruzSpeciesRead("SpCodes.dat") 
  return(sp.codes)
})

cruzSpeciesMammals <- reactive({
  sp.codes <- cruzSpecies()
  ind.mammals <- which(!sp.codes$Code %in% turtle.codes)
  sp.mammals <- sp.codes[ind.mammals,]
  return(sp.mammals)
})

cruzSpeciesTurtles <- reactive({
  sp.codes <- cruzSpecies()
  ind.turtles <- which(sp.codes$Code %in% turtle.codes)
  sp.turtles <- sp.codes[ind.turtles,]
  return(sp.turtles)
})

