#' Simulation to address Ben Sheldon's comments
#'
#' This function will create one possible simulated relationship between sensitivity and exposure
#'
#' @param interpop_var The inter-population variation in the error of temperature
#' (basically causes populations to vary in the interannual variability in temp, as Ben suggested)
#' @param avg_sensitivity The beta of sensitivity (laying date ~ temperature)
#' @param sd_sensitivity The sd of the random slope term for laying date ~ temperature across pops
#' @param resid_sense The residual error in sensitivity (laying date ~ temperature)
#' @param avg_exposure The beta of exposure (temperature ~ year)
#' @param sd_exposure The sd of the random slope term for exposure (temperature ~ year) across pops
#' @param resid_exp The residual error in exposure (temperature ~ year)
#' @param return Should the function return a plot ("plot") or the Pearson's correlation
#' of sensitivity and exposure ("cor").
#'
#' @return Either a ggplot object ('plot') or numeric value representing the Pearson's correlation
#' between sensitivity and exposure.
#' @export
#' @import dplyr
#' @import tidyr

Ben_sim <- function(interpop_var, avg_sensitivity, sd_sensitivity, resid_sense,
                    avg_exposure, sd_exposure, resid_exp, return = "plot"){

  #let's say we run the following model for all pops combined: exp_model<-lmer(temp~year+(1+year|popID)
  #Let's say it gives the following output estimates
  #(to illustrate the point I actually do not think it matters whether you use the observed values or whether you just pick some values):
  mean_exposure     <- avg_exposure # the beta of year
  sd_exposure       <- sd_exposure   # the sd of the random slope year variation across pops
  error_exposure    <- resid_exp  # the residual error of temperature
  sd_error_exposure <- interpop_var # inter-population variation in the error of temperature (basically causes populations to vary in the interannual variability in temp, as Ben suggested)

  #let's say we also run the following model for all pops combined: sens_model<-lmer(laydate~temp+(1+temp|popID)
  mean_sensitivity  <- avg_sensitivity # the beta of temp
  sd_sensitivity    <- sd_sensitivity # the sd of the random slope temp variation across pops
  error_sensitivity <- resid_sense # the residual error of laydate

  #simulate some data according to above parameters assuming no dependency between sensitivity and exposure
  pops<-500
  years<-30 # Mean length of all study populations

  #Create raw table to fill temp and laydate
  raw_data <- tidyr::expand_grid(popnr = 1:pops,
                             year = 1:years) %>%
    group_by(.data$popnr) %>%
    mutate(exp_dev  = stats::rnorm(1,0,.data$sd_exposure),
           sens_dev = stats::rnorm(1,0,.data$sd_sensitivity),
           temp_dev = stats::rnorm(1,0,.data$sd_error_exposure)) %>%
    ungroup() %>%
    mutate(temp = purrr::pmap_dbl(.l = .,
                                  #Name function arguments to make code more readable
                                  .f = function(popnr, year, exp_dev, sens_dev, temp_dev, mean_exposure, error_exposure){

                                    (mean_exposure + exp_dev) * year + stats::rnorm(1, 0, ifelse((temp_dev + error_exposure) < 0, 0, temp_dev + error_exposure))

                                  }, mean_exposure = mean_exposure, error_exposure = error_exposure)) %>%
    mutate(laydate = purrr::pmap_dbl(.l = .,
                                     .f = function(popnr, year, exp_dev, sens_dev, temp_dev, temp, mean_sensitivity, error_sensitivity){

                                       (mean_sensitivity + sens_dev) * temp + stats::rnorm(1, 0, error_sensitivity)

                                     }, mean_sensitivity = mean_sensitivity, error_sensitivity = error_sensitivity))

  model_data <- raw_data %>%
    group_by(popnr) %>%
    tidyr::nest(data = c(temp, year, laydate)) %>%
    #Apply model to each one
    mutate(exp_mod = purrr::map(data, ~stats::lm(temp~year, data = .)),
           ld_mod = purrr::map(data, ~stats::lm(laydate~temp, data = .)),
           exp_coef = purrr::map_dbl(exp_mod, ~coef(.)[2]),
           ld_coef = purrr::map_dbl(ld_mod, ~coef(.)[2])) %>%
    ungroup() %>%
    dplyr::select(-data, -exp_mod, -ld_mod)

  return(model_data)

}


#' Run multiple iterations to simulate sensitivty/exposure relationship
#'
#' @param interpop_var The inter-population variation in the error of temperature
#' (basically causes populations to vary in the interannual variability in temp, as Ben suggested)
#' @param i Number of iterations to conduct
#' @param avg_sensitivity The beta of sensitivity (laying date ~ temperature)
#' @param sd_sensitivity The sd of the random slope term for laying date ~ temperature across pops
#' @param resid_sense The residual error in sensitivity (laying date ~ temperature)
#' @param avg_exposure The beta of exposure (temperature ~ year)
#' @param sd_exposure The sd of the random slope term for exposure (temperature ~ year) across pops
#' @param resid_exp The residual error in exposure (temperature ~ year)
#'
#' @export
#' @import progress
Ben_sim_i <- function(interpop_var = 2, i, avg_sensitivity, sd_sensitivity, resid_sense,
                      avg_exposure, sd_exposure, resid_exp){

  pb <- progress::progress_bar$new(total = i)

  purrr::map_df(.x = 1:i,
                .f = ~{

                  pb$tick()

                  Ben_sim(interpop_var = interpop_var,
                          avg_sensitivity = avg_sensitivity,
                          sd_sensitivity = sd_sensitivity,
                          resid_sense = resid_sense,
                          avg_exposure = avg_exposure,
                          sd_exposure = sd_exposure,
                          resid_exp = resid_exp) %>%
                    dplyr::mutate(i = ..1) %>%
                    dplyr::select(i, everything())

                })

}
