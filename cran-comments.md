## Release summary
This is a minor release to remove the use of `shiny:::%OR%`, which is not supported anymore, in the package

Re the notes at https://cran.rstudio.com//web/checks/check_results_CruzPlot.html ("All declared Imports should be used"): imports that aren't explicitly used are required by the Shiny app, which is the primary component of the CruzPlot package.

## Test environments (R-release = v4.0.4)
* Windows 10, R-release (local)
* win-builder (devel)
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R-release
* mac OS 10.15.7 (on github actions) R-release
* ubuntu 20.04 (on github actions) R-devel and release

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Downstream dependencies
No downstream dependencies
