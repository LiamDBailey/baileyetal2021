% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_exposure.R
\name{estimate_exposure}
\alias{estimate_exposure}
\title{Estimate the rate of climate change for each population.}
\usage{
estimate_exposure(SEM_output, return = "SEM_output")
}
\arguments{
\item{SEM_output}{Data frame. Output generated by \code{\link{estimate_sensitivity}}.}

\item{return}{Should the function return the exposure joined to the SEM output ('SEM_output')
or return the full exposure model ('model')}
}
\value{
A data frame. SEM_output with new columns for climate change exposure.
}
\description{
For all populations where ECA&D gridded data is available, determine the
rate of climate change (mean temperature/year) from 1950 - 2017 in the period identified as the
best climate window. We are interested here to show how different populations may be exposed to
climate change differently, not necessarily explain exisiting trends;
therefore, we are most interested in giving the
best estimate of temperature change possible for that location.
}
\details{
We avoided using time periods of different length for each population
as this may lead to a bias in the esimates of climate change exposure.
e.g. shorter populations may show more variance in trends due to stochasticity
in temperature measurements from year to year. With this in mind, we exclude
Vlieland and Sicily from these estimations because we don't have temperature
data for the whole 1950 - 2017 period.

This process involves:
\itemize{

\item For every population/species, determine dates that correspond to the best window
identified using climwin.

\item For every year, extract the mean of daily mean temperature during the best
climate window.

\item Fit a simple linear model (meanT ~ year) and determine the coefficient and
standard error of the change in temperature over time within the best climate
window for every population.

}
}
\examples{
\dontrun{
#Run for one population

#Extract SEM output (contains information from climwin and SEM coefficients)
HOG_GT_SEM <- readRDS(here::here("data/unshared_files/HOG_GT_SEM.RDS"))

#Determine climate change exposure
estimate_exposure(HOG_GT_SEM)
}
}
