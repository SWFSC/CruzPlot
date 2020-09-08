
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CruzPlot

<!-- badges: start -->

[![CRAN
version](http://www.r-pkg.org/badges/version/CruzPlot)](https://cran.r-project.org/package=CruzPlot)
[![Travis build
status](https://travis-ci.com/smwoodman/CruzPlot.svg?branch=master)](https://travis-ci.com/smwoodman/CruzPlot)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/smwoodman/CruzPlot?branch=master&svg=true)](https://ci.appveyor.com/project/smwoodman/CruzPlot)
<!-- badges: end -->

CruzPlot is an R package that contains a GUI for creating maps, plotting
data, and performing basic data summaries from data files in the “DAS”
format produced by WinCruz.

## Installation

You can install the released version of CruzPlot from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("CruzPlot")
```

You can install the developmental version of CruzPlot from
[GitHub](https://github.com/). To use the development version of
CruzPlot, it is recommended to install the development version of
[swfscDAS](https://smwoodman.github.io/swfscDAS/index.html) as well:

``` r
# install.packages("remotes") # Install remotes package if needed
remotes::install_github("smwoodman/swfscDAS", build_vignettes = TRUE)
remotes::install_github("smwoodman/CruzPlot")
```

After updating CruzPlot, it is recommended to create and save new
workspaces, rather than using workspaces saved with a previous version
of CruzPlot. Please contact the developer if you have any issues.

## Running CruzPlot

To open and run CruzPlot, run the following code in RStudio:

``` r
CruzPlot::cruzplot_gui()
```

If text or images overlap within the CruzPlot window, you can make the
browser window full screen or adjust the text size in your browser
(e.g., Ctrl - minus (‘-’) on Windows systems)

## CruzPlot manual

The CruzPlot manual can be downloaded [at this
link](https://github.com/smwoodman/CruzPlot/blob/master/inst/shiny/www/CruzPlot_Manual_app.pdf),
or accessed when running CruzPlot by opening the “CruzPlot Manual” page.
It is recommended for new users to read the first two pages of the
manual for orientation purposes; the rest of the manual contains
section-specific information (formatting requirements, etc.) and can be
read on an as-needed basis.
