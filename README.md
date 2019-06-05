# CruzPlot
CruzPlot is is a utility program oriented to create maps, plot data, and do basic data summaries from data files in the "DAS" format produced by WinCruz.

You must have both R and RStudio installed to run CruzPlot. Download CruzPlot by clicking the green ‘Clone or download’ button on the top-right of this webpage, selecting “Download ZIP”, and then unzipping the downloaded file. To run CruzPlot for the first time, run the following code in RStudio (CruzPlot will automatically install other necessary packages):

```{r}
install.packages("shiny")
shiny::runApp()
```

You can also start CruzPlot by opening either the ‘server.R’ file or 'ui'R' file in RStudio and clicking the "Run App" button with the green arrow next to it. This button is near the top of the 'Source' pane, which likely will be near the top of the RStudio window. 

CruzPlot was developed using R version 3.4.4 and RStudio version 1.1.447. Although unlikely, using other versions of these programs or outdated R packages could cause errors within CruzPlot.

The CruzPlot manual can be accessed within the GUI (i.e., when running CruzPlot) by clicking the "CruzPlot Manual". The manual is also located in the folder 'www' within the CruzPlot folder. Do not remove the 'CruzPlot_Manual_app.pdf' file from the 'www' folder. It is recommended for new users to read the first two pages of the manual for orientation purposes; the rest of the manual contains section-specific information (formatting requirements, etc.) and can be read on an as-needed basis.
