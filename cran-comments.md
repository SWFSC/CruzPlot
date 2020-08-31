## Resubmission
This is a third resubmission of CruzPlot (v1.4.2). In this version I have:
* Received permission from CRAN maintainers to omit the copyright holder since I developed this package on behalf of NOAA, a US government agency, and thus my understanding is that all of this work is in the public domain
* Rephrased '# code by Beaufort or effort type' to make it more clear that this comment is referring to color-coding objects, not stating that someone named Beaufort provided code

In the second resubmission of CruzPlot (now v1.4.2) I:
* Used on.exit to ensure I do not change the user's par and options
* Clarified the code documentation "# Color code by Bft or SNF "
* Ensured that all person() information correct
* Added standalone unit tests to test the functionality from other packages used in the Shiny app

In the first resubmission I: 
* In the Description field, single quoted 'WinCruz' and added references for both 'DAS' and 'WinCruz'

## Release summary
Initial release (v1.4.1)

## Test environments
* Windows 10, R 4.0.2 (local)
* win-builder (devel and release)
* OS X, R 4.0.2 (local)
* ubuntu 14.04.5 (on travis-ci.com, R devel, oldrel, and release)

## R CMD check results
There were no ERRORs or WARNINGs

There was one NOTE on win-builder: 

* "Possibly mis-spelled words in DESCRIPTION: DAS"

DAS is the name of a specific data format, and has an associated webpage reference in the Description

## Downstream dependencies
No downstream dependencies
