#' Estimate climatic sensitivity for all data.
#'
#' For every species/population combination this function will:
#' - Format the laying date data using \code{\link{format_data}}.
#' - Use this format data to run climwin and detect the 'best window' where
#' temperature affects laying date. Using the function \code{\link{run_climwin}}
#' - Run randwin to determine the distribution of deltaAICc values that would
#' be expected from the same population with no climate signal.
#' Using the function \code{\link{run_climwin}}
#' - Run an SEM to detrend data.
#' Using the function \code{\link{run_SEM}}
#'
#' Important decisions that were made as part of the process:
#' - We only include years where there were at least 2 values of laying date recorded.
#' - We only include populations where there were at least 9 years of data recorded
#' that met this criteria.
#'
#' @param input_data Dataframe. Full dataset containing brood information of all birds from all populations.
#' @param first_clutch_method Logical. Whether first clutch is determined by data owner or >30 day rule
#' @param temp_data A stacked raster. Mean daily temperature data. Stored as 'tg_0.25deg_reg_v17.0' in extdata.
#' @param randwin Logical. Should randwin be run with climwin?
#' @param repeats Numeric. If randwin is TRUE, how many repeats should be run.
#' @param dummy_test Logical. If TRUE, will only use the first two rows of data. Used to test code.
#' @param SEM Logical. Should SEM analysis also be run.
#' @param i Numeric. If SEM is TRUE, the number of bootstrap iterations.
#'
#' @return A list with climwin and (if requested) SEM results.
#' @export
#' @import raster
#'
#' @examples
#' \dontrun{
#' ### EXAMPLE FOR HOGE VELUWE ###
#'
#' #Load mean temperature data
#' #Temperature data file can be requested from https://www.ecad.eu/ archive
#' temp_data  <- raster::stack(here::here("data/unshared_files/tg_0.25deg_reg_v17.0.nc"))
#'
#' #Load Brood_data.csv
#' input_data <- read.csv(here::here("data/unshared_files/Brood_info.csv"),
#' stringsAsFactors = FALSE, header = T, sep = ",")
#'
#' #Create a subset of data for just Hoge Veluwe
#' input_data_test <- dplyr::filter(input_data, Pop_ID %in% "HOG")
#'
#' #Run climwin and randwin
#' HOG_output <- estimate_sensitivity(input_data = input_data_test,
#' temp_data = temp_data,
#' randwin = TRUE, repeats = 100)
#' }

estimate_sensitivity <- function(input_data, first_clutch_method = "calc",
                            temp_data, randwin = FALSE, repeats,
                            SEM = FALSE, i = 1000, dummy_test = FALSE){

  #Identify each unique pop/species combo
  pop_summaries <- input_data %>%
    dplyr::group_by(Pop_ID, Species) %>%
    dplyr::summarise(Pop_ID = dplyr::first(Pop_ID),
                     Species = dplyr::first(Species), .groups = "drop")

  #Now, for each of these populations, we can run the climwin code
  #The climwin code extracts the temp data and also runs the analysis.
  #Use purrr to run through each Pop_ID species combo
  total_output <- purrr::pmap(.l = list(Pop = pop_summaries$Pop_ID,
                         Species = pop_summaries$Species),
                .f = function(Pop, Species_code, data, SEM){

                  #Filter data for the given species and population
                  pop_subset <- data %>%
                    dplyr::filter(Pop_ID == Pop & Species == Species_code)

                  climwin_output <- run_climwin(input_data = pop_subset,
                              temp_data = temp_data,
                              randwin = randwin,
                              repeats = repeats,
                              dummy_test = dummy_test)

                  if(SEM){

                    SEM_output <- run_SEM(Clim = climwin_output[[2]],
                                          Lay_date_sum = climwin_output[[1]],
                                          dataset = climwin_output[[3]][[1]]$Dataset[1, ] %>%
                                            dplyr::mutate(Pop_ID = Pop, Species = Species_code),
                                          i = i, dummy_test = dummy_test)

                    return(list(climwin = climwin_output, SEM = SEM_output))

                  } else {

                    return(list(climwin = climwin_output))

                  }

                }, data = input_data, SEM = SEM)

  names(total_output) <- paste(pop_summaries$Pop_ID, pop_summaries$Species, sep = "_")

  return(total_output)

}
