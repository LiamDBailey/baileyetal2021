#' Estimate the rate of climate change for each population.
#'
#' For all populations where ECA&D gridded data is available, determine the
#' rate of climate change (mean temperature/year) from 1950 - 2017 in the period identified as the
#' best climate window. We are interested here to show how different populations may be exposed to
#' climate change differently, not necessarily explain exisiting trends;
#' therefore, we are most interested in giving the
#' best estimate of temperature change possible for that location.
#'
#' We avoided using time periods of different length for each population
#' as this may lead to a bias in the esimates of climate change exposure.
#' e.g. shorter populations may show more variance in trends due to stochasticity
#' in temperature measurements from year to year. With this in mind, we exclude
#' Vlieland and Sicily from these estimations because we don't have temperature
#' data for the whole 1950 - 2017 period.
#'
#' This process involves:
#' \itemize{
#'
#' \item For every population/species, determine dates that correspond to the best window
#' identified using climwin.
#'
#' \item For every year, extract the mean of daily mean temperature during the best
#' climate window.
#'
#' \item Fit a simple linear model (meanT ~ year) and determine the coefficient and
#' standard error of the change in temperature over time within the best climate
#' window for every population.
#'
#' }
#'
#' @param SEM_output Data frame. Output generated by \code{\link{estimate_sensitivity}}.
#' @param return Should the function return the exposure joined to the SEM output ('SEM_output')
#' or return the full exposure model ('model')
#'
#' @return A data frame. SEM_output with new columns for climate change exposure.
#' @export
#' @import here
#' @import raster
#'
#' @examples
#' \dontrun{
#' #Run for one population
#'
#' #Extract SEM output (contains information from climwin and SEM coefficients)
#' HOG_GT_SEM <- readRDS(here::here("data/unshared_files/HOG_GT_SEM.RDS"))
#'
#' #Determine climate change exposure
#' estimate_exposure(HOG_GT_SEM)
#' }

estimate_exposure <- function(SEM_output, return = "SEM_output"){

  #Use internal data:
  #ECAD_locations: lat/long of each population
  #Some of these are slightly shifted to match grid
  #(e.g. Cardiff is in a different location to match the ECAD grid)
  ECAD_locations

  ## Temperature data file can be requested from https://www.ecad.eu/ archive
  temp_data <- raster::stack(here::here("data/unshared_files/tg_0.25deg_reg_v17.0.nc"))

  #Load output of all climwin files that contains information on the best window.
  SEM_output <- SEM_output %>%
    dplyr::mutate(Pop_sp = paste(Pop_ID, Species, sep = "_")) %>%
    dplyr::mutate(Open_date = as.Date("01/06/2018", format = "%d/%m/%Y") - WindowOpen,
                  Close_date = as.Date("01/06/2018", format = "%d/%m/%Y") - WindowClose)

  #Make location of current population into spatial points dataframe
  location_spatial <- sp::SpatialPointsDataFrame(coords = ECAD_locations[, c('Longitude', 'Latitude')],
                                             data = ECAD_locations,
                                             proj4string = sp::CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

  ###Extract temperature data###
  #Determine date data for each layer of the raster (allows us to sort by each year).
  temp_dates <- data.frame(Date = as.Date(names(temp_data), format = "X%Y.%m.%d"))

  #Make a subset that excludes Sicily and Vlieland as we don't have the same number of years
  #to estimate climate change.
  SEM_subset <- dplyr::filter(SEM_output, !Pop_ID %in% c("VLI", "SIC"))

  #Run through each row of the SEM_output dataset and determine rate of climate change since 1950 in the designated window
  exposure_output <- purrr::pmap_df(.l = list(Pop_sp = SEM_subset$Pop_sp, current_Pop_ID = SEM_subset$Pop_ID,
                                              Open_date = SEM_subset$Open_date, Close_date = SEM_subset$Close_date),
                                    .f = function(Pop_sp, current_Pop_ID, Open_date, Close_date){

                                      Open_date <- as.Date(Open_date, origin = "1970-01-01")
                                      Close_date <- as.Date(Close_date, origin = "1970-01-01")

                                      year      <- seq(min(lubridate::year(temp_dates$Date)), max(lubridate::year(temp_dates$Date)))
                                      mean_temp <- purrr::map_dbl(.x = year, .f = function(x){

                                        min_date <- as.Date(paste(lubridate::day(Open_date), lubridate::month(Open_date), x, sep = "/"),
                                                            format = "%d/%m/%Y")
                                        max_date <- as.Date(paste(lubridate::day(Close_date), lubridate::month(Close_date), x, sep = "/"),
                                                            format = "%d/%m/%Y")
                                        current_location = subset(location_spatial, Pop_ID == current_Pop_ID)

                                        mean(raster::extract(temp_data[[which(temp_dates$Date >= min_date & temp_dates$Date <= max_date)]],
                                                     current_location))

                                      })

                                      print(Pop_sp)

                                      mod <- stats::lm(mean_temp ~ year)

                                      if (return == "model") {

                                        return(dplyr::tibble(model = list(mod)))

                                      } else {

                                        return(data.frame(Pop_sp = Pop_sp,
                                                          CC_allyrs_beta = summary(mod)$coefficient[2],
                                                          CC_allyrs_SE = summary(mod)$coefficient[4], stringsAsFactors = FALSE))

                                      }

                                    })

  if (return == "model") {

    return(exposure_output)

  } else {

    SEM_output <- SEM_output %>%
      left_join(exposure_output, by = "Pop_sp")

    return(SEM_output)

  }

}
