#' Prepare numbers for display in tables/plots
#'
#'To prepare for submission. If numbers <0.001 round to 3 significant figures and use scientific notation.
#'If numbers are larger, round numbers to 3 significant figures and pad to right if needed (i.e. if rounding leads to 0s)
#'
#' @param x A number (double)
#' @param digits Number of significant figures to include
#'
#' @return Character string.
#' @export
#' @import stringr
#'
#' @examples
#' #Small numbers (negative or positive) use scientific notation.
#' pad_number(-0.00000047641)
#' pad_number(0.00000047641)
#'
#' #Larger numbers do not. Use 3 significant figures
#' pad_number(0.0128)
#'
#' #Pad out to same length for all numbers
#' pad_number(0.1)
pad_number <- function(x, digits = 3){

  ifelse(abs(x) < 0.001,
         format(x, digits = digits, scientific = TRUE),
         stringr::str_pad(string = round(x, digits), width = nchar(x %/% 1) + digits + 1, side = "right", pad = "0"))

}

#' Generate data used for Supplementary Data 1
#'
#' @return A kable object
#' @export
#' @import kableExtra
#' @import dplyr
#' @import glue

gen_tableS1 <- function(){

  #Use internal datasets
  # Pop_info: Summary of populations e.g. lat/long, habitat type
  # climate_sensitivity_and_exposure: Estimated sensitivity and exposure of all populations
  # Population_summary: Mean laying date of each population/species over full period of study

  climate_sensitivity_and_exposure <- dplyr::left_join(climate_sensitivity_and_exposure, Population_summary, by = c("Pop_ID", "Species")) %>%
    mutate(Pop_sp = paste(Pop_ID, Species, sep = "_"))

  #Determine R2 for every population
  R2_files <- list.files(here::here("./data/unshared_files"), pattern = ".RDS", full.names = TRUE)
  R2_files <- R2_files[!grepl(pattern = "SEM|rand", R2_files)]

  pb <- utils::txtProgressBar(min = 0, max = length(R2_files), style = 3)
  R2_data <- purrr::map_dfr(.x = 1:length(R2_files),
                             .f = function(i){

                               utils::setTxtProgressBar(pb, i)

                               climwin_output <- readRDS(R2_files[i])

                               tibble(Pop_sp = gsub(pattern = ".RDS",
                                                    replacement = "",
                                                    x = basename(R2_files[i])),
                                      R2 = summary(climwin_output[[1]]$BestModel)$r.squared)


                             })

  SEM_files <- list.files(here::here("./data/unshared_files"), pattern = "SEM.RDS", full.names = TRUE)

  #Go through the internal data information and find a file for each of the populations
  base_table <- purrr::map_dfr(.x = SEM_files,
                         .f = function(file_name){

                           return(readRDS(file_name))

                         }) %>%
    dplyr::mutate(Pop_sp = paste(Pop_ID, Species, sep = "_"),
                  SEM_beta_abs = SEM_beta * -1) %>%
    dplyr::left_join(climate_sensitivity_and_exposure %>% dplyr::select(Pop_sp, PAIC, CC_allyrs_beta, CC_allyrs_SE), by = "Pop_sp") %>%
    dplyr::mutate(SEM_int = SEM_int * -1, Laydate_yr_beta = Laydate_yr_beta * -1) %>%
    dplyr::left_join(Pop_info %>% dplyr::select(Pop_ID, Nr_Nestbox, Site_Name, Latitude, Longitude), by = "Pop_ID") %>%
    dplyr::left_join(Population_summary %>%
                       dplyr::mutate(Pop_sp = paste(Pop_ID, Species, sep = "_")) %>%
                       dplyr::select(Pop_sp, mean_LD_Julian), by = "Pop_sp") %>%
    dplyr::left_join(R2_data, by = "Pop_sp") %>%
    dplyr::mutate(Midpoint = 153 - ((WindowOpen + WindowClose)/2),
                  WindowStart = format(lubridate::ymd("2016-06-01") - WindowOpen, "%b %d"),
                  WindowEnd = format(lubridate::ymd("2016-06-01") - WindowClose, "%b %d"),
                  Delay = mean_LD_Julian - Midpoint,
                  Species = dplyr::recode(Species, GT = "P.major", BT = "C.caeruleus")) %>%
    dplyr::mutate(Duration = WindowOpen - WindowClose,
                  Latitude = pad_number(Latitude, digits = 2),
                  Longitude = pad_number(Longitude, digits = 2),
                  Delay = pad_number(Delay),
                  SEM_int = pad_number(SEM_int),
                  `Nest boxes` = Nr_Nestbox,
                  `Number years` = sample.size,
                  `Phenological sensitivity (days advancement/C)` = as.character(glue::glue("{beta} [{lower}/{upper}]",
                                                                       beta = pad_number(SEM_beta_abs),
                                                                       lower = pad_number(SEM_beta_abs - (1.96*SEM_SE)),
                                                                       upper = pad_number(SEM_beta_abs + (1.96*SEM_SE)))),
                  `Year effect (advancement/year)` = as.character(glue::glue("{beta} [{lower}/{upper}]",
                                                          beta = pad_number(Laydate_yr_beta),
                                                          lower = pad_number(Laydate_yr_beta - (1.96*Laydate_yr_SE)),
                                                          upper = pad_number(Laydate_yr_beta + (1.96*Laydate_yr_SE)))),
                  `Climate change exposure (C/year)` = as.character(glue::glue("{beta} [{lower}/{upper}]",
                                                                      beta = pad_number(CC_allyrs_beta),
                                                                      lower = pad_number(CC_allyrs_beta - (1.96*CC_allyrs_SE)),
                                                                      upper = pad_number(CC_allyrs_beta + (1.96*CC_allyrs_SE)))),
                  `Phenological advancement (advancement/yr)` = pad_number(abs(SEM_beta_abs * CC_allyrs_beta)),
                  R2 = pad_number(R2))

  base_table %>%
    dplyr::select(Site = Site_Name, Species, Latitude, Longitude, `Nest boxes`, `Number years`, PAIC, `Window Start` = WindowStart, `Window End` = WindowEnd,
                  `Midpoint (Julian days)` = Midpoint, `Duration (Days)` = Duration, `Delay (Days)` = Delay, `Phenological Intercept (-April days)` = SEM_int,
                  `Phenological sensitivity (days advancement/C)`, `Year effect (advancement/year)`,
                  `Climate change exposure (C/year)`, `Phenological advancement (advancement/yr)`, R2) %>%
    dplyr::arrange(Site, Species) %>%
    utils::write.csv(file = here::here("./plots/Supplementary_data_S1.csv"), row.names = FALSE)

  (table_dat <- base_table %>%
      dplyr::select(Site = Site_Name, Species, Latitude, Longitude, `Nest boxes`, `Number years`, PAIC, `Window Start` = WindowStart, `Window End` = WindowEnd,
                    `Midpoint (Julian days)` = Midpoint, `Duration (Days)` = Duration, `Delay (Days)` = Delay, `Phenological Intercept (-April days)` = SEM_int,
                    `Phenological sensitivity (days advancement/C)`, `Year effect (advancement/year)`,
                    `Climate change exposure (C/year)`, `Phenological advancement (advancement/yr)`, R2) %>%
      dplyr::arrange(Site, Species) %>%
      dplyr::mutate_if(is.character, .funs = ~{

        dplyr::case_when(stringr::str_detect(as.character(.), "NA ") ~ "NA",
                         TRUE ~ .)

      }) %>%
      kableExtra::kable() %>%
      kableExtra::kable_styling(bootstrap_options = "bordered"))

  return(table_dat)

}
