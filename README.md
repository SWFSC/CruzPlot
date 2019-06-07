<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/smwoodman/CruzPlot.svg?branch=master)](https://travis-ci.org/smwoodman/CruzPlot)
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/smwoodman/CruzPlot?branch=master&svg=true)](https://ci.appveyor.com/project/smwoodman/CruzPlot)
<!-- badges: end -->
  
# CruzPlot
CruzPlot is an R package for creating maps, plotting data, and performing basic data summaries from data files in the "DAS" format produced by WinCruz.

## Installation
You must have both R and RStudio installed to use CruzPlot. To install CruzPlot, run the following code in RStudio:
```{r}
install.packages(devtools)
devtools::install_github("smwoodman/CruzPlot")
```

## Run CruzPlot
To open and run CruzPlot, run the following code in RStudio:
```{r}
install.packages("shiny")
shiny::runApp()
```

## Additional details
The CruzPlot manual can be accessed within CruzPlot (i.e., when running CruzPlot) by clicking the "CruzPlot Manual" tab. It is recommended for new users to read the first two pages of the manual for orientation purposes; the rest of the manual contains section-specific information (formatting requirements, etc.) and can be read on an as-needed basis.

If text or images overlap within the CruzPlot window, please make the browser window full screen or adjust the text size in your browser (e.g., Ctrl - minus ('-') on Windows systems)

CruzPlot was developed using R version 3.4.4 and RStudio version 1.1.447. Although unlikely, using older versions of these programs or outdated R packages could cause errors within CruzPlot.
