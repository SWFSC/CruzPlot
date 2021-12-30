## Release summary
This is a minor release to fix several small bugs

Re the notes at https://cran.rstudio.com//web/checks/check_results_CruzPlot.html ("All declared Imports should be used"): imports that aren't explicitly used are required by the Shiny app, which is the primary component of the CruzPlot package.

## Test environments (R-release = v4.1.1)
* Windows 10, R-release (local)
* win-builder (devel)
* windows-latest (Microsoft Windows Server 2019, on github actions) R-release
* macOS-latest (on github actions) R-release
* ubuntu 20.04 (on github actions) R-devel and release

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Downstream dependencies
No downstream dependencies
