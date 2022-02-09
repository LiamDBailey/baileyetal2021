#' Run climwin analysis on data for a single population/species combo.
#'
#' Run slidingwin and (optionally) randwin on data from a given bird population.
#' This is generally used inside function \code{\link{estimate_sensitivity}}.
#'
#' As part of this approach we:
#' \itemize{
#'
#' \item Calculate the annual mean laying date for the focal population.
#'
#' \item Calculate standard error of the laying date mean that will
#' be used for weighting in models.
#'
#' \item Exclude years with only 1 nest (where no SE could be calculated).
#'
#' \item Extract mean daily temperature data using \code{\link{extract_temp_data}}.
#'
#' \item For Vlieland and Sicily, remove records where climatic data is unavailable.
#'
#' \item Run climwin with a baseline model: laying date ~ year, weight = 1/SE.
#'
#' \item Our climate windows test for a linear relationship between mean temperature
#' and annual mean laying date up to 365 days before June 1st. Any missing climatic data
#' is interpolated by taking the mean value of temperature on that date in all other years.
#'
#' \item Run randwin using the same setup as climwin.
#'
#' }
#'
#' @param input_data A data frame with laying date data for a given population and species.
#' @param temp_data Raster stack of temperature data across Europe.
#' @param randwin Logical (TRUE/FALSE). Should randwin be run as well as slidingwin?
#' @param repeats If randwin is TRUE, the number of times that data will be randomised.
#' @param dummy_test To test code. If TRUE, will only analyse the first two years of data. Used for testing code.
#'
#' @importFrom dplyr group_by summarise mutate filter select
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom lubridate year month day ymd dmy
#' @import climwin
#'
#' @return A list containing the output of climwin and (if conducted) randwin.

run_climwin <- function(input_data, temp_data, randwin = FALSE,
                        repeats = NA, dummy_test = FALSE){

  #We've now done this summary elsewhere
  Lay_date_sum <- input_data

  if(dummy_test){

    Lay_date_sum <- Lay_date_sum[1:2, ]

  }

  Clim <- extract_temp_data(Lay_date_sum = Lay_date_sum)

  if (input_data$Pop_ID[1] == "VLI") {

    #There are still large chunks of NAs in the climate data in the 80s. We can't really deal with
    #these (i.e. we have no good data nearby to use for interpolation and there
    #are big chunks of data missing so our regular methods are no good).
    #Instead, we will remove data from the biological data where there is
    #any missing climate data within the previous year.
    #This is fairly conservative (i.e. there are some years where only 5-6 days are missing)
    #but we don't want to subjectively decide what is 'acceptable' missing climate data.
    #Vlieland has many years of data so this shouldn't cause any problems with sliding window estimation.
    Lay_date_sum <- Lay_date_sum %>%
      dplyr::group_by(Sample_year) %>%
      dplyr::mutate(NA_data = all(!is.na(dplyr::filter(Clim, Date <= lubridate::dmy(paste0("0106", Sample_year))
                                                       & Date >= lubridate::dmy(paste0("0106", Sample_year - 1)))$Temp))) %>%
      dplyr::filter(NA_data) %>%
      dplyr::select(-NA_data)

  } else if(input_data$Pop_ID[1] == "SIC"){

    #If we are using Sicily data, remove 2006. We only have data for 2007+
    Lay_date_sum <- Lay_date_sum %>%
      dplyr::filter(Sample_year > 2006)

  }

  #NOW THAT WE HAVE CLIMATE AND BIOLOGICAL DATA WE CAN RUN CLIMWIN!!
  set.seed(666)

  climwin_output <- climwin::slidingwin(xvar = list(Temp = Clim$Temp), cdate = Clim$Date,
                               bdate = Lay_date_sum$Date, baseline = stats::lm(Mean ~ Sample_year, data = Lay_date_sum, weights = SE_calc),
                               range = c(365, 0),
                               stat = "mean", func = "lin",
                               type = "relative",
                               cmissing = "method2", cinterval = "day")

  if(randwin){

    randwin_output <- climwin::randwin(repeats = repeats, xvar = list(Temp = Clim$Temp), cdate = Clim$Date,
                              bdate = Lay_date_sum$Date, baseline = stats::lm(Mean ~ Sample_year, data = Lay_date_sum, weights = SE_calc),
                              range = c(365, 0),
                              stat = "mean", func = "lin",
                              type = "relative",
                              cmissing = "method2", cinterval = "day")

    if(!dummy_test){

      saveRDS(object = climwin_output, file = paste0(input_data$Pop_ID[1], "_", input_data$Species[1], ".RDS"))
      saveRDS(object = randwin_output, file = paste0(input_data$Pop_ID[1], "_", input_data$Species[1], "_rand.RDS"))

    }

    return(list(LD_data = Lay_date_sum, clim_data = Clim, climwin = climwin_output, randwin = randwin_output))

  } else {

    if(!dummy_test){

      saveRDS(object = climwin_output, file = paste0(input_data$Pop_ID[1], "_", input_data$Species[1], ".RDS"))

    }

    return(list(LD_data = Lay_date_sum, clim_data = Clim, climwin = climwin_output))

  }

}
