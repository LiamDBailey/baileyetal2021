# bailey2021

This is the GitHub repository that hosts the R package ```bailey2021```.


## What is this R package about?

This package aims at documenting analysis for the paper "Bird populations most exposed to climate change are less sensitive to climatic variation" currently under review in Nature Communications.
DOI of early pre-print: https://doi.org/10.1101/2020.08.16.252379

## How to use the package

The package includes a number of internal functions in the \R folder. These functions are combined in our analysis vignettes in the \vignettes folder. Our vignettes include:

- `Step1_Prepare_data.Rmd`: In this step we extract mean daily temperature data and laying date data. Run temperature window analysis for each population. Run structurial equation models for all populations with 1,000 bootstrap iterations to determine coefficients to determine phenological sensitivity.

- `Step2_Window_SpatialVariation.Rmd`: In Step 2, we model changes in temperature window characteristics
midpoint, duration and delay.

- `Step3_Beta_HabitatType.Rmd`: In Step 3, we model variables that affect spatial variation
in phenological sensitivity across our populations, including Habitat Type.

- `Step4_PhenologicalAdvancement.Rmd`: In Step 4, we estimate the expected phenological
advancement of every population as a product of phenological sensivity and climate change exposure
since 1950.

- `Step5_ReviewerComments.Rmd`: Step 5 includes any supplementary analyses that were carried out
to respond to reviewer comments.

- \code{\link{estimate_sensitivity}} will run slidingwin, randwin, and SEM
to generate all the code needed for analysis. Internally, this function
is running:

1. \code{\link{format_data}} is used to format the raw data (saved as
Brood_info.csv in extdata). Only GT/BT are included, NAs in lay date are removed.
Two methods of firstclutch are checked (calculated 30 days and observed).

2. For each population, \code{\link{run_climwin}} is called. This firstly
extract population relevant temperature data useing \code{\link{extract_temp_data}}.
This function uses special data for Vlieland and Sicily, where not temperature
grid cells were available. It then runs slidingwin and (if specified) randwin.

3. If specified, \code{\link{run_SEM}} is called. This runs an SEM of the
best window to remove effects of shared trends over time.

- \code{\link{estimate_exposure}} will estimate the rate of climate change for
each population 1950 - 2017. Note, this will exclude Vlieland and Sicily
which do not use data from the ECA&D gridded dataset.
