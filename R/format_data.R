#' Format raw nest data into the correct format for climwin analysis.
#'
#' Identify first clutches for our focal species.
#'
#' For all population/species data:
#' \itemize{
#' \item Filter only our two focal species (GT and BT).
#'
#' \item Filter only records where laying date was known.
#'
#' \item Determine the cutoff (30 days after the first laying date of the season)
#' at which nests would no longer be considered 'first' nests. This cutoff is unique
#' to each species in each population in each year.
#'
#' \item Classify nests as 'first' using this cutoff.
#'
#' }
#'
#' @param input_data Dataframe. Raw Brood_data from formatting populations
#'
#' @return A tibble with 7 columns
#' @export
#'
#' @examples
#' \dontrun{
#' #Load Brood_info.csv
#' input_data <- read.csv(here::here("data/unshared_files/Brood_info.csv"),
#' header = T, sep = ",", stringsAsFactors = FALSE)
#'
#' #Format data into correct format
#' format_data(input_data = input_data)
#' }

format_data <- function(input_data){

  #First, determine which nests are first nests
  #Use both the example given by the data owner
  #And our 30day cut-off
  first_nests <- input_data %>%
    #Only include records with GT or BT w/ laying date info
    dplyr::filter(Species %in% c("GT", "BT") & !is.na(LayingDate_AprilDays)) %>%
    #Determine the cutoff (first LD + 30) for each year and each species
    #Then determine if they're past it
    dplyr::group_by(Pop_ID, Sample_year, Species) %>%
    dplyr::mutate(isfirst_calc = LayingDate_AprilDays <= (min(LayingDate_AprilDays, na.rm = TRUE) + 30),
                  isfirst_obsv = Clutch_Type == "First") %>%
    dplyr::select(Sample_year, Pop_ID, Species, LayingDate_Date, LayingDate_AprilDays,
                  isfirst_calc, isfirst_obsv)

  message(paste(signif(sum(is.na(first_nests$isfirst_calc != first_nests$isfirst_obsv) |
                         first_nests$isfirst_calc != first_nests$isfirst_obsv)/nrow(first_nests) * 100,
                         digits = 3),
          "% of records show a discrepency between recorded clutch type and clutch type with 30 day rule"))

  return(first_nests)

}
