# cruzDasRead for CruzPlot by Sam Woodman
#   Input: DAS data file 
#   Returns: data frame with information from each entry


cruzDasRead <- function (file) {
  #########################################################
  ### Parse and format DAS file
  # Read file and get initial details
  DAS <- readLines(file)
  #  DAS <- DAS[substr(DAS, 4,4)!="C"]
  DAS <- DAS[substr(DAS, 4,4) != "#"]
  Event <- substr(DAS, 4, 4)
  nDAS <- length(DAS)
  OnEffort <- (substr(DAS, 5, 5) == ".")
  
  # Format times and dates
  tm <- substr(DAS, 6, 11)
  tm <- gsub(" ", "", tm)
  dt <- substr(DAS, 13, 18)
  dt <- gsub(" ", "", dt)
  
  Date <- strptime(paste(dt, tm), "%m%d%y %H%M%S")
  Date.na <- is.na(Date)
  Date[Date.na] <- strptime(paste(dt, tm), "%m%d%y %H%M")[Date.na]
  # validate() for checking that everything is properly formatted?
  
  # Get lat/long info
  LatD <- as.numeric(substr(DAS, 21, 22))
  LatM <- as.numeric(substr(DAS, 24, 28))
  LonD <- as.numeric(substr(DAS, 31, 33))
  LonM <- as.numeric(substr(DAS, 35, 39))
  Lat <- (LatD + LatM/60) * ifelse(substr(DAS, 20, 20) == "S", -1, 1)
  Lon <- (LonD + LonM/60) * ifelse(substr(DAS, 30, 30) == "W", -1, 1)
  
  # Get data info
  Data1 <- gsub(" ", "", substr(DAS, 40, 44))
  Data1[Data1 == ""] <- NA
  Data2 <- gsub(" ", "", substr(DAS, 45, 49))
  Data2[Data2 == ""] <- NA
  Data3 <- gsub(" ", "", substr(DAS, 50, 54))
  Data3[Data3 == ""] <- NA
  Data4 <- gsub(" ", "", substr(DAS, 55, 59))
  Data4[Data4 == ""] <- NA
  Data5 <- gsub(" ", "", substr(DAS, 60, 64))
  Data5[Data5 == ""] <- NA
  Data6 <- gsub(" ", "", substr(DAS, 65, 69))
  Data6[Data6 == ""] <- NA
  Data7 <- gsub(" ", "", substr(DAS, 70, 74))
  Data7[Data7 == ""] <- NA
  Data8 <- gsub(" ", "", substr(DAS, 75, 79))
  Data8[Data8 == ""] <- NA
  
  
  #########################################################
  ### Add columns for helpful info 
  # Beaufort sea state
  Bft <- rep(NA, nDAS)
  event.V <- Event == "V"
  Bft[event.V] <- as.numeric(Data1[event.V])
  LastBft <- NA
  for (i in 1:nDAS) {
    if (is.na(Bft[i])) 
      Bft[i] <- LastBft
    else LastBft <- Bft[i]
  }
  
  # Cruise
  Cruise <- rep(NA, nDAS)
  event.B <- Event == "B"
  Cruise[event.B] <- as.numeric(Data1[event.B])
  LastCruise <- NA
  for (i in 1:nDAS) {
    if (is.na(Cruise[i])) 
      Cruise[i] <- LastCruise
    else LastCruise <- Cruise[i]
  }
  
  # Course
  Course <- rep(NA, nDAS)
  event.N <- Event == "N"
  Course[event.N] <- as.numeric(Data1[event.N])
  LastCourse <- NA
  for (i in 1:nDAS) {
    if (is.na(Course[i])) 
      Course[i] <- LastCourse
    else LastCourse <- Course[i]
  }
  
  # Bearing
  Bearing <- rep(NA, nDAS)
  event.N <- Event == "N"
  Bearing[event.N] <- ifelse(is.na(Data4[event.N]), 
                             -100, as.numeric(Data4[event.N]))
  #  Bearing[event.N] <- as.numeric(Data4[event.N])
  LastBearing <- NA
  for (i in 1:nDAS) {
    if (is.na(Bearing[i])) 
      Bearing[i] <- LastBearing
    else LastBearing <- Bearing[i]
  }
  Bearing <- ifelse(Bearing < 0,NA,Bearing)
  
  # Effort type 1 (C/P)
  EffType1 <- rep(NA, nDAS)
  event.B <- Event == "B"
  EffType1[event.B] <- toupper(as.character(Data2[event.B]))
  LastType1 <- NA
  for(j in 1:nDAS) {
    if(is.na(EffType1[j])) EffType1[j] <- LastType1
    else LastType1 <- EffType1[j]
  }
  
  # Effort type 2 (S/N/F)
  EffType2 <- rep(NA, nDAS)
  event.R <- Event == "R"
  EffType2[event.R] <- ifelse(is.na(Data1[event.R]), 
                              "S", as.character(Data1[event.R]))   
  # ^ effort type was not recorded in early data
  LastType2 <- NA
  for(i in 1:nDAS) {
    if(is.na(EffType2[i])) EffType2[i] <- LastType2
    else LastType2 <- EffType2[i]
  }
  
  return(data.frame(Event, OnEffort, Date, Lat, Lon, 
                    Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, 
                    Bft, Cruise, Course, Bearing, EffType1, EffType2, 
                    stringsAsFactors = FALSE))
}