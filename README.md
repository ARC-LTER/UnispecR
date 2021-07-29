
# Unispec R 

R code for processing spectral data collected using a Unispec-DC instrument (PP systems) from the Arctic LTER long-term experimental plots. This code is written to streamline data processing, QAQC, preliminary analyses, and visualization and as such, organized into the following directories:

1. UnispecProtocol : process & quality check newly collected data, add to long term data set

2. UnispecRecord : standardize, process, and visualize long term data set

3. Visualizations : shiny application built to explore the ArcticLTER unispec record (2009-present)

4. Investigations : mainly code from 2019, with the following 
- spectral_instrument_comparison: campaign to assess whether NDVI was the same across instruments for the Unispec-SC, Unispec-DC, ASD, RapidSCAN, and Greenseeker
- plot heterogeneity: comparing 5 or 10 measurements per plot
- seasonal trends: 
- legacy "UnispecProtocol" scripts used for processing and quality checking the 2019 unispec data

User friendly scripts and summary documents currently under construction. 

## Prerequisites

The end goal is for code to be operable by somehow with a little but not much experience of R.  All code is written in R and utilizes the "tidyverse" packages. A good place to start for some background in R and good coding practices:  [R for Data Science](http://r4ds.had.co.nz/data-import.html). 

## Getting Started

Clone the project onto your local machine. 

```
git clone https://github.com/jlaundre/UnispecR.git
```

## Contributing

Any feedback on usability, improvements, features that would be useful, etc. is much appreciated! 


## Authors

* **Jim Laundre** - *Data Management* - jlaundre@mbl.edu
* **Ruby An** - *Code Development* - rubya@princeton.edu 

## Acknowledgments

* The many Terrestrial RA's that collected Unispec data over the years!  
* **Laura Gough** - *Arctic LTER Terrestrial PI* - lgough@towson.edu
