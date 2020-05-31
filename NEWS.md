# CruzPlot 1.1 (in development)

* Fixed two bugs that caused errors when changing the color scheme

* Mode and effort type filters are only available as sighting filters when strictly on effort sightings are plotted

* Both sighting and effort Beaufort filters are not applied when the minimum and maximum values are 0 and 9, respectively. Functionally, this means that records with NA Beaufort values can be plotted 

* Fixed a bug that caused extraneous NA values when filter information for a sighting or effort line was NA. 

* CruzPlot now verbosely removes, i.e. displays a pop-up window, when a sighting or effort line is removed because of one or more 1) NA position coordinates or 2) NA filter values

* By default, an effort legend is not included when plotting simplified effort

* The scale bar length now does not automatically update when the map range is changed

* The scale bar position only automatically updates when the map range is changed if the position is outside of the new map range


# CruzPlot 1.0

### General

* Reorganize CruzPlot tabs and sections for a cleaner display

* Make saved workspace more robust - workspaces saved with past versions of CruzPlot are no longer compatible

### Map

* Make map size dynamic and responsive to window size, and give the user dynamic control of the map window height

* Allow user to specify the resolution of saved map

* Fixed bug in tick labels where style '120' did not display negative values

* Add buttons for five default map ranges

* Remove StarterVals.csv - this functionality has been replaced by the default range buttons and being able to save multiple workspaces

* Allow user to control the download and import of bathymetric files for depth shading

### DAS data

* Use [swfscDAS](https://smwoodman.github.io/swfscDAS/) for processing DAS data

* Users can load their own species codes file or use default SpCodes.dat

* Removed CPOD plotting functionality

* Sightings can be plotted by event code (S/K/M/G/p)

* Sightings can be filtered by mode and effort type

* Simplified effort can be filtered by effort type (standard/non-standard/fine)

* Effort lines are explicitly filtered by those within the map range

* Users have more control over effort line properties and legend parameters

* Cruise number filters now are a dynamic dropdown with the cruise numbers from the DAS data

* Interactive sighting and effort maps both have combined view and label (hover and click) functionality


# CruzPlot 0.1
* Initial version - CruzPlot converted from a collection of scripts to an R package
