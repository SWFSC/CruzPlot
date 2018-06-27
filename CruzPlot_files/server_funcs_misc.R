# Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}