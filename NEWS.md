# CruzPlot 1.3 (in development)

* Catch up with changes made in swfscDAS. CruzPlot now depends on swfscDAS package version >= 1.2

* Fixed bug in plotting effort lines - the longitude coordinates did not get transformed correctly when the map spanned the 180th meridian

* Fixed bugs in interactive labeling


# CruzPlot 1.2

* Fixed bug in specifying scale bar position

* Fixed bug in tabular sighting output, and added table summarizing the total number of (unfiltered) sightings.


# CruzPlot 1.1

* User can now change the map range (zoom in) by drawing a box with their mouse cursor

* When the map range is changed, a) the scale bar length now does not automatically update and b) the scale bar position only automatically updates if the position is outside of the new map range

* Fixed two bugs that caused errors message to appear in the plot window when changing the color scheme

* Added local documentation for the various arguments used when reading and processing DAS data

* CruzPlot now verbosely removes, i.e. displays a pop-up window, when a sighting or effort line is removed because of one or more 1) NA position coordinates or 2) NA filter values

* Mode and effort type filters are only available as sighting filters when strictly 'on effort' sightings are plotted

* Both sighting and effort Beaufort filters are not applied when the minimum and maximum values are 0 and 9, respectively. Practically speaking, this means that records with NA Beaufort values can be plotted if desired, which is most relevant for off effort sightings

* Fixed a bug that caused extraneous NA values when filter information for a sighting or effort line was NA in the DAS data. 

* By default, an effort legend is not included when plotting simplified effort

* When plotting detailed effort by Beaufort, users only have to enter as many Beaufort colors as the maximum Beaufort filter value plus one

* Added the ability to plot resight events when one species code is selected


# CruzPlot 1.0

### General

* Reorganized CruzPlot tabs and sections for a cleaner display

* Made saved workspace more robust - note that this means that workspaces saved with past versions of CruzPlot are no longer compatible

### Map

* Made map size dynamic and responsive to window size, and give the user dynamic control of the map window height

* Users can specify the resolution of saved map

* Fixed bug in tick labels where style '120' did not display negative values

* Added buttons for five default map ranges

* Removed StarterVals.csv - this functionality has been replaced by the default range buttons and being able to save multiple workspaces

* Users have more direct control the download and import of bathymetric files for depth shading

### DAS data

* CruzPlot now uses [swfscDAS](https://smwoodman.github.io/swfscDAS/) for processing DAS data

* Users can load their own species codes file if they do not wish to use the default SpCodes.dat

* Removed CPOD plotting functionality

* Sightings can be plotted by event code (S/K/M/G/p) and filtered by mode (C/P) and effort type (S/N/F)

* Simplified effort can be filtered by effort type

* Effort lines are explicitly filtered by those within the map range

* Users have more control over effort line properties and legend parameters

* Cruise number filters are a dynamic dropdown with only the cruise numbers from the DAS data

* Interactive sighting and effort maps both have combined view and label (hover and click) functionality


# CruzPlot 0.1

* Initial version - CruzPlot converted from a collection of scripts to an R package
