#' Create maps of shipboard DAS data
#'
#' A utility program oriented to create maps, plot data, and do basic data
#' summaries from data files in the "DAS" format, typically produced by WinCruz.
#'
#' @name CruzPlot-package
#' @aliases cruzplot CruzPlot
#' @title CruzPlot
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://swfsc.github.io/CruzPlot/}
#'
#' @import shiny
#' @importFrom dplyr .data %>% between case_when if_else left_join mutate select
#' @importFrom geosphere destPoint
#' @importFrom graphics abline axis par points polygon lines rect text title
#' @importFrom grDevices gray palette dev.off jpeg pdf png
#' @importFrom maps map
#' @importFrom shinydashboard box dashboardBody tabItems tabItem dashboardHeader
#'   dashboardPage dashboardSidebar sidebarMenu menuItem tabBox
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @importFrom stringr str_detect str_glue str_sub str_to_lower str_to_upper
#' @importFrom swfscDAS das_spcodes_read
#' @importFrom utils globalVariables packageVersion read.csv write.csv
#'
"_PACKAGE"

# Note: all DT functions are explicitly called via `DT::` in the code

# https://r-pkgs.org/package-within.html#echo-a-working-package
utils::globalVariables("app_state_save")
