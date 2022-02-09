#' Calculate 95 and 99.5 CIs for a given model.
#'
#' Determine 95 and 99.5 CIS for both spaMM and lmer models.
#'
#' @param mod A model object. Fitted with either spaMM or lmer.
#' @param method Character string. For lmer, should CIs be
#' calculated with Wald or bootstrap?
#' @param model_type Character string. Is the model "lmer" or "spaMM".
#' @param ... Additional arguments passed to `confint` function
#' (e.g. iterations for bootstrap)
#'
#' @return A data frame with CIs for coefficients.
#' @export
#' @import purrr
#' @import lme4
#' @import dplyr
#'
#' @examples
#' #Fit test models
#' mod_lmer  <- lme4::lmer(disp ~ mpg + (1|cyl), data = mtcars)
#'
#' #Calculate CIs
#' confint_2levels(mod_lmer, method = "boot", model_type = "lmer", n = 100)
confint_2levels <- function(mod, method, model_type = "lmer", ...){

  if (model_type == "spaMM") {

    CIs <- purrr::pmap_df(.l = list(parm_name = names(fixef(mod)),
                             est = fixef(mod)),
                   .f = function(parm_name, est, ...){

                     intervals <- stats::confint(mod, parm = parm_name, verbose = FALSE, ...)

                     tibble(Variable = parm_name, est = est,
                                lower = intervals$normal[2],
                                upper = intervals$normal[3])

                   }, ...)

    return(CIs)

  } else {

    Wald_0.005 <- stats::confint(mod, level = 0.995, method = method, ...)
    Vars       <- row.names(Wald_0.005)
    Wald_0.005 <- data.frame(Wald_0.005)
    colnames(Wald_0.005) <- c("low_995", "hi_995")
    Wald_0.005$Variable  <- Vars
    Wald_0.005$Mean      <- c(NA, NA, fixef(mod))

    Wald_0.05 <- stats::confint(mod, level = 0.95, method = method, ...)
    Vars       <- row.names(Wald_0.05)
    Wald_0.05 <- data.frame(Wald_0.05)
    colnames(Wald_0.05) <- c("low_95", "hi_95")
    Wald_0.05$Variable  <- Vars

    return(left_join(Wald_0.005, Wald_0.05, by = c("Variable")))

  }

}

#' Calculate distance between study sites and temperature grid cells.
#'
#' For four populations (Sagunto, Spain; Cardiff, UK; Barcelona, Spain; Askainen, Sweden),
#' ECA&D grid data is not available at the exact study site. Instead, we used the nearest grid cell
#' available. This function returns information on how far these new points were from the original site.
#'
#' @return A dataframe with distances between actual point and point used for temperature measurement.
#' @export
#' @import rgeos
#' @import dplyr
#' @import sp
#'
#' @examples
#' calculate_grid_distance()
calculate_grid_distance <- function(){

  #Use internal data:
  #Pop_info: Info about populations e.g. habitat type, lat/long

  #For each of the four focal populations, determine the distance between actual location
  #and location used for temperature extraction.
  distances <- purrr::pmap_df(.l = list(c("SAG", "CAR", "BAR", "ASK")),
                              .f = ~{

                                #Make shapefile of actual population location
                                actual_location <- sp::SpatialPoints(coords = Pop_info[Pop_info$Pop_ID == ..1, c('Longitude', 'Latitude')],
                                                                     proj4string = sp::CRS(as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))

                                #Make shapefile of grid cell location
                                grid_location <- sp::SpatialPoints(coords = Pop_info[Pop_info$Pop_ID == ..1, c('Longitude_new_temp', 'Latitude_new_temp')],
                                                                   proj4string = sp::CRS(as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))

                                #Transform to a distance projection
                                actual_location <- sp::spTransform(actual_location, "+proj=robin +ellps=WGS84 +datum=WGS84 +no_defs")
                                grid_location <- sp::spTransform(grid_location, "+proj=robin +ellps=WGS84 +datum=WGS84 +no_defs")

                                #Find distance between points (divide by 1000 to get in km)
                                return(dplyr::tibble(Pop_ID = ..1, Distance = abs(rgeos::gDistance(actual_location, grid_location))/1000))

                              })

  return(distances)

}
