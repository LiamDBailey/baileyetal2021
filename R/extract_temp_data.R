#' Extract temperature data for given population.
#'
#' Extract temperature data for every year where laying date data are available.
#'
#' For all populations except Vlieland and Sicily, we extract daily mean temperature
#' data from the ECA&D Gridded Temperature layer of Europe (v17.0).
#'
#' In four populations (Sagunto, Spain; Cardiff, UK; Barcelona, Spain; Askainen, Sweden)
#' data were not directly available at the study site from ECA&D. Instead, we used the
#' nearest grid square, which was never more than 8km from the actual site. See \code{\link{calculate_grid_distance}}
#' for calculation.
#'
#' For Vlieland, we used mean daily temperature from ROYAL NETHERLANDS METEOROLOGICAL INSTITUTE (KNMI).
#' From 1954, temperature data was collected on Terschelling (the neighbouring island).
#' From 1995, temperature data was collected directly on Vlieland.
#'
#' For Sicily, we used mean temperature data collected by the data owners at the site.
#'
#' @param Lay_date_sum Data frame. Summarised lay date data. This is needed to know what
#' date range to extract temps over. This function is generally used inside
#' \code{\link{run_climwin}}.
#'
#' @return R data frame object with daily mean temperature.
#' @export
#' @import janitor

extract_temp_data <- function(Lay_date_sum){

  Pop <- unique(Lay_date_sum$Pop_ID)

  load(here::here("data/Pop_info.rda"))

  if (!Pop %in% c("VLI", "SIC")) {

    #Make location of current population into spatial points dataframe
    location_spatial <- SpatialPoints(coords = Pop_info[Pop_info$Pop_ID == Pop, c('Longitude_new_temp', 'Latitude_new_temp')],
                                      proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

    ###Extract temperature data###
    #Determine date data for each layer of the raster (allows us to sort by each year).
    temp_dates <- data.frame(Date = as.Date(names(temp_data), format = "X%Y.%m.%d"))
    temp_dates$Year <- lubridate::year(temp_dates$Date)

    #Extract data from Euro temp for all the necessary dates for the site (i.e. everything from the year before the first recorded nest until the year of the most recent brood).
    message(paste("Currently extracting temperature data for", Lay_date_sum$Species[1], "in", Pop, sep = " "))
    Clim <- data.frame(Date = seq(as.Date(paste("01", "01", min(Lay_date_sum$Sample_year) - 1, sep = "/"), format = "%d/%m/%Y"), as.Date(paste("01", "06", max(Lay_date_sum$Sample_year), sep = "/"), format = "%d/%m/%Y"), "day"), Temp = NA)
    Clim$Temp <- ifelse(is.na(Clim$Temp),
                        as.numeric(raster::extract(temp_data[[which(temp_dates$Date %in% Clim$Date)]], location_spatial)),
                        NA)

    #Check that data extracted properly!!
    if (all(is.na(Clim$Temp))) {

      stop(paste0("Climate data failed to extract for ", Lay_date_sum$Pop_ID[1]))

    }

  } else if (Pop == "VLI") {

    message(paste("Currently extracting temperature data for", Lay_date_sum$Species[1], "in VLI", sep = " "))
    #Load relevant data from weather stations
    #Vlieland_temp_data_old: Data from Teschelling (available into the 1950s)
    #Vlieland_temp_data_new: Data from Vlieland (available from the 1990s)
    Clim_new <- Vlieland_temp_data_new %>%
      mutate(Date = lubridate::ymd(Date))
    Clim_old <- Vlieland_temp_data_old %>%
      mutate(Date = lubridate::dmy(Date))

    #Fill in data from Vlieland first, then using the older data.
    Clim <- data.frame(Date = seq(lubridate::dmy(paste("31", "05", min(Lay_date_sum$Sample_year) - 1, sep = "/")),
                                  lubridate::dmy(paste("01", "06", max(Lay_date_sum$Sample_year), sep = "/")), "days"), Temp = NA, Station = NA)

    for (i in 1:nrow(Clim)){

      if (Clim$Date[i] %in% Clim_new$Date) {

        Clim$Temp[i] <- Clim_new$Mean[which(Clim_new$Date %in% Clim$Date[i])]

        Clim$Station[i] <- Clim_new$Station_no[which(Clim_new$Date %in% Clim$Date[i])]

      } else {

        Clim$Temp[i] <- Clim_old$Mean[which(Clim_old$Date %in% Clim$Date[i])]

        Clim$Station[i] <- Clim_old$STN[which(Clim_old$Date %in% Clim$Date[i])]

      }

    }

  } else if (Pop == "SIC") {

    #Load the Sicily climate data
    message(paste("Currently extracting temperature data for", Lay_date_sum$Species[1], "in SIC", sep = " "))

    #Sicily_temp_data: Temperature data collected on site since 2006
    #NOTE: We don't have temp data in 2005, so data from the year 2006 cannot be analysed

    Clim <- Sicily_temp_data %>%
      janitor::clean_names() %>%
      dplyr::mutate(Date = as.Date(paste(i_year, month, day, sep = "/"), format = "%Y/%B/%d"),
                    Temp = as.numeric(temperature)) %>%
      dplyr::select(Date, Temp)

  }

  return(Clim)

}
