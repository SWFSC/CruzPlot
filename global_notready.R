### global.R file for CruzPlot


###############################################################################
###############################################################################
###############################################################################
### Server code
# Countries to be removed for world2 map
# Reference: http://www.codedisqus.com/0yzeqXgekP/plot-map-of-pacific-with-filled-countries.html
remove <- c("UK:Great Britain", "France", "Spain", "Algeria", "Mali",
            "Burkina Faso", "Ghana", "Togo")
mapnames <- map("world2", fill=TRUE, plot=FALSE)$names
mapnames.hires <- map("world2Hires", fill=TRUE, plot=FALSE)$names
regions.rm <- mapnames[!(mapnames %in% remove)]
regions.rm.hires <- mapnames.hires[!(mapnames.hires %in% remove)]

bathy.col <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")

font.family = c("sans", "serif", "mono")

######################## MUST BE UPDATED IF TURTLE CODES IN SpCodes.dat ARE CHANGED ########################
turtle.codes <- c("CC", "CM", "DC", "EI", "HT", "LK", "LV", "ND", "UH", "UT") 
# NOTE: Assumed less likely to have new turtle codes added than mammal codes, so turtle codes are hardcoded
#    so that app can split up codes into mammal and turtle categories
############################################################################################################

# DAS data-symbol property text inputs
symbol.col <- c("Black", "Dark Blue", "Dark Red", "Green", "Orange", 
                "Blue", "Brown", "Red", "Yellow", "Aqua", "Tan", "Pink", 
                "Light Green", "Light Brown", "Light Blue", "Light Red", "Gray", "White")
symbol.col.code <- c("black", "darkblue", "red4", "forestgreen", "orange", 
                     "blue", "tan4", "red", "yellow", "aquamarine2", "bisque1", "hotpink", 
                     "green", "wheat3", "lightblue", "indianred2", "gray", "white")
symbol.col.gray <- list("Black", "Dark Gray", "Charcoal", "Gray", "Light Gray", "White")
symbol.col.code.gray <- c(1, 2, 3, 4, 5, 0)

cruz.palette.color <- list("Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
                           "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange", 
                           "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2", 
                           "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green", 
                           "Light Brown" = "wheat3", "Light Blue" = "lightblue", 
                           "Light Red" = "indianred2", "Gray" = "gray", "White" = "white")
cruz.palette.gray <- list("Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
                          "Gray" = 4, "Light Gray" = 5, "White" = 0)
cruz.symbol.type <- list("0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3, 
                         "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7, 
                         "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10, 
                         "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13, 
                         "14: Square with Up Triangle" = 14, "15: Filled Square" = 15, 
                         "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18, 
                         "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20)
cruz.line.type <- list("Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, 
                       "Long dash" = 5, "Dot-long dash" = 6)

# colors for displaying effort by Beaufort
# effort lines will be shown from 0 to => max.bft
# # number of colors = max.bft + 1
max.bft <- 3
bft.color <- c("darkblue", "blue", "green3", "red") 


###############################################################################
###############################################################################
###############################################################################
### Ui code
cruz.palette.color <- list("Black" = "black", "Dark Blue" = "darkblue", "Dark Red" = "red4",
                           "Brown" = "tan4", "Green" = "forestgreen", "Orange" = "orange", 
                           "Blue" = "blue", "Red" = "red", "Yellow" = "yellow","Aqua" = "aquamarine2", 
                           "Tan" = "bisque1", "Pink" = "hotpink", "Light Green" = "green", 
                           "Light Brown" = "wheat3", "Light Blue" = "lightblue", 
                           "Light Red" = "indianred2", "Gray" = "gray", "White" = "white")
cruz.palette.gray  <- list("Black" = 1, "Dark Gray" = 2, "Charcoal" = 3,
                           "Gray" = 4, "Light Gray" = 5, "White" = 0)
font.family        <- list("Sans" = 1, "Serif" = 2, "Mono" = 3)
cruz.symbol.type   <- list("0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3, 
                           "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7, 
                           "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10, 
                           "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13, 
                           "14: Square with Up Triangle" = 14, "15: Filled Square" = 15, 
                           "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18, 
                           "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20)
cruz.line.type     <- list("Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, 
                           "Long dash" = 5, "Dot-long dash" = 6)
cruz.beaufort      <- list("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, 
                           "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

ui.new.line <- function() {helpText(HTML("<br/>"))}
ui.selectize.instructions <- function() {
  helpText("To remove selected input(s), click the input(s) to remove and then click backspace or delete")
}