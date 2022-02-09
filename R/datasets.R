#' Simulated scenario where sensitivity and exposure are independent
#' and inter-annual temperature variation is low
#'
#' Used to estimate co-variance between sensitivity and exposure
#' under different conditions of inter-annual temperature variation
#'
#'
#'@name Appendix1_var01
NULL

#' Simulated scenario where sensitivity and exposure are independent
#' and inter-annual temperature variation is medium
#'
#' Used to estimate co-variance between sensitivity and exposure
#' under different conditions of inter-annual temperature variation
#'
#'
#'@name Appendix1_var02
NULL

#' Simulated scenario where sensitivity and exposure are independent
#' and inter-annual temperature variation is high
#'
#' Used to estimate co-variance between sensitivity and exposure
#' under different conditions of inter-annual temperature variation
#'
#'
#'@name Appendix1_var03
NULL

#' Lat/Long coordinates of study sites used to extract from gridded data
#'
#' @format A data frame with 40 rows and 4 columns
#' \describe{
#'   \item{Pop_ID}{Location of study. Site may have data on both GT and BT.}
#'   \item{Latitude}{Latitude of population in decimal degrees.}
#'   \item{Longitude}{Longitude of population in decimal degrees.}
#'   \item{Adjusted}{When TRUE, lat/long are adjusted slightly to overlap with
#'   gridded data.}
#'   }
#'
#'@name ECAD_locations
NULL

#' Primary data used as the basis for all analysis
#'
#' Note, in Vlieland and Sicily there are years where phenological data is
#' available but full temperature data is not. In these cases, Temp is NA.
#'
#' @format A data frame with 2,010 rows and 10 columns
#' \describe{
#'   \item{Pop_ID}{Unique population ID}
#'   \item{Species}{Species ID. Can be either GT (great tit/Parus major) or BT (blue tit/Cyanistes caeruleus)}
#'   \item{Sample_year}{Calendar year in which laying date was recorded.}
#'   \item{Mean}{Mean laying date (April days).}
#'   \item{SE}{Standard error of mean laying date (April days).}
#'   \item{Min}{Minimum observed laying date (April days).}
#'   \item{Count}{Total number of first nests laying date records for this population/species/year combination.}
#'   \item{Date}{Starting date used sliding window analysis (dd/mm/yyyy).}
#'   \item{SE_calc}{Inverse of standard error (1/SE), used to weight statistical models.}
#'   \item{Temp}{Mean temperature (oC) within population specific climate window}
#'   }
#'
#'@name Lay_date_summary
NULL

#' MODIS Terra+Aqua Combined Land Cover product (MCD12Q1) for each location in each year.
#'
#' Used to test the viability of remote sensing as an alternative method
#' to define habitat type. See `Step5_ReviewerComments.Rmd`.
#'
#'@name MODIS_data
NULL

#' Data frame to convert MODIS number values into text categories.
#'
#' This is required to convert MODIS data into our DEC/MIX/EVE categories.
#'
#'@name MODIS_translation
NULL

#' Data collected on site in the Sicily population from 01/01/2006 - 31/12/2017.
#'
#' @format A data frame with 4,383 rows and 4 columns
#' \describe{
#'  {Year}{Year of temperature data collection}
#'  {Month}{Month of temperature data collection}
#'  {Day}{Day of temperature data collection}
#'  {Temperature}{Mean daily temperature (oC)}
#'   }
#'
#'@name Sicily_temp_data
NULL

#' Data collected on Terschelling by KNMI from 01/01/1954 - 31/12/2016.
#'
#' @format A data frame with 23,011 rows and 5 columns
#' \describe{
#'   {STN}{KNMI station number}
#'   {Date}{Date of weather data (dd/mm/yyyy)}
#'   {Mean}{Daily mean temperature (oC)}
#'   {Min}{Daily minimum temperature (oC)}
#'   {Max}{Daily maximum temperature (oC)}
#'   }
#'
#'@name Vlieland_temp_data_old
NULL

#' Data collected on Vlieland by KNMI from 01/01/1996 - 26/07/2017.
#'
#' @format A data frame with 7,828 rows and 5 columns
#' \describe{
#'   {Station_no}{KNMI station number}
#'   {Date}{Date of weather data (yyyymmdd)}
#'   {Mean}{Daily mean temperature (oC)}
#'   {Min}{Daily minimum temperature (oC)}
#'   {Max}{Daily maximum temperature (oC)}
#'   }
#'
#'@name Vlieland_temp_data_new
NULL

#' Results of principal component analysis by Metzger et al. 2005 for each site.
#'
#' @format A data frame with 40 rows and 2 columns
#' \describe{
#'   {Pop_ID}{Unique population ID}
#'   {PC3_Precip}{Third principal component (precipitation)}
#'   }
#'
#'@name Metzger_PCA
NULL

#' Raster stack of European temperatures
#'
#' ECA&D maximum daily temperature raster stack (v16.0)
#' Data ranges from Jan 1st 1950 - Aug 31st 2017
#' @format A raster stack containing 24,715 individual raster levels
#'@name Euro_temp
NULL

#' Output containing climwin slopes, pvalues and population characteristics.
#'
#' For information on how the data are generated see Prepare_data.Rmd
#'@name climate_sensitivity_and_exposure
NULL

#' A SpatialPointsDataFrame with location information of all populations.
#'
#' Site location for all populations in decimal degrees including site information.
#' @format A SpatialPointsDataFrame with 43 rows and 14 variables
#' \describe{
#'   \item{Study_ID}{Three letter code for study site. In some cases, multiple populations are administered in the same study.}
#'   \item{Pop_ID}{Three letter code specifying the population ID.}
#'   \item{Site_Name}{Full name of population.}
#'   \item{Latitude}{Latitude of population in decimal degrees.}
#'   \item{Latitude_min}{Latitude of population minutes.}
#'   \item{Longitude}{Longitude of population in decimal degrees.}
#'   \item{Longitude_min}{Longitude of population minutes.}
#'   \item{Sunrise}{Time of sunrise (hh:mm:ss) on April 1st from https://www.esrl.noaa.gov/gmd/grad/solcalc/sunrise.html.}
#'   \item{Sunset}{Time of sunset (hh:mm:ss) on April 1st from https://www.esrl.noaa.gov/gmd/grad/solcalc/sunrise.html.}
#'   \item{Number_boxes}{Number of nest boxes used in the population (if provided).}
#'   \item{Area}{Area of study site in m2.}
#'   \item{Habitat_Type}{Deciduous (DEC), Mixed (MIX), or Evergreen (EVE).}
#'   \item{Reference}{Citation with information on the study site, if applicable.}
#'   \item{Habitat_Notes}{Notes on habitat used to classify Habitat_Type measure.}
#'   }
#'@name Pop_info
NULL

#' A mean laying date for every species/population combo
#'
#' A summary table showing mean laying date in April days and Julian days for
#' every species/population combination.
#' @format A tibble with 67 rows and 4 variables
#' \describe{
#'   \item{Pop_ID}{Three letter code specifying the population ID.}
#'   \item{Species}{Great tit (GT) or blue tit (BT).}
#'   \item{mean_LD}{Mean laying date (April days).}
#'   \item{mean_LD_Julian}{Mean laying date (Julian days)}
#'   }
#'@name Population_summary
NULL
