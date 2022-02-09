[![DOI](https://zenodo.org/badge/433887429.svg)](https://zenodo.org/badge/latestdoi/433887429)

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
