
## User Instructions

This shiny application is built to visualize spectral reflectance data collected from the Arctic LTER terrestrial experimental plots at Toolik Lake from 2007-onward.

Navigate using the tabs in the top bar to see the same data plotted differently:

  1. To compare control plots across vegetation types
  2. Site-level comparison (data averaged by plot and then by block)
  3. Block-level comparison per site (data averaged by plot)
  4. Plot-level (each data point is a measurement)

Within each tab, use the options on the left to choose which Vegation Index (see definitions below) to plot what subset of data to visualize (i.e. years, dates, sites, treatments to view).

### Spectral Band Definitions:

By default, the MODIS band definitions are used to calculate indices.

  - Lorna reflectance:	570-680
  - Lorna reflectance:	725-1000
  - Red defined by ITEX:	560-680
  - NIR defined by ITEX: 	725-1000
  - Blue defined by MODIS:	459-479
  - Red defined by MODIS:	620-670
  - NIR defined by MODIS:	841-876
  - Blue defined by SKYE:	455-480
  - Red defined by SKYE:	620-680
  - NIR defined by SKYE:	830-880
  - Red defined by Toolik GIS-drone (2018): 640-680
  - NIR defined by Toolik GIS-drone (2018): 820-890

### Vegetation Index Equations:

Equations formatted in LaTex should display correctly when App is opened in web browser.

  $$ NDIV = \frac{NIR-Red}{NIR+Red}$$

  $$ EVI = 2.5*\frac{NIR-Red}{NIR+6*Red-7.5*Blue+1}$$

  $$ EVI2 = 2.5*\frac{NIR-Red}{NIR+2.4*Red+1}$$

  $$ PRI (550_{reference}) = \frac{550nm-531nm}{550nm+531nm}$$

  $$ PRI (570_{reference}) = \frac{570nm-531nm}{570nm+531nm}$$

  $$ WBI = \frac{900nm}{970nm}$$

  $$ Chl_{Index} = \frac{750nm-705nm}{750nm+705nm}$$
