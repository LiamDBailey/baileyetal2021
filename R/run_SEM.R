#' Run SEM for a given dataset.
#'
#' Run a structural equation model for a given population/species combination
#' to disentangle the effect of temperature on laying date from
#' potential shared trends in the data.
#'
#' This process will:
#' \itemize{
#'
#' \item Use the function singlewin to return to data from the best climate window
#' for a given population. Use the same structure as the climwin analysis (see \code{\link{run_climwin}}).
#'
#' \item Create an SEM model with multiple pathways that accounts for a) Temp changing over time,
#' b) Laying date changing over time, c) Temp affecting laying date.
#'
#' \item Bootstrap with i iterations to estimate coefficients.
#'
#' }
#'
#' @param Clim Vector. Climate data for this population
#' @param Lay_date_sum Vector. Lay date data for this population (average first laydate for each year)
#' @param dummy_test Logical. If TRUE, will not save the RDS file.
#' @param i Numeric. Number of bootstrap iterations for the SEM model.
#' @param dataset Data frame. Dataframe with best temperature windows for each population/species
#' @param internal Logical. Is the function being run internally or called directly?
#'
#' @return Output a model summary and save an RDS file with SEM results
#' @export
#' @import lavaan

run_SEM <- function(Clim, Lay_date_sum, dataset, dummy_test = FALSE, i = 1000, internal = TRUE){

  if (internal) {

    Window_dat <- climwin::singlewin(xvar = list(Temp = Clim$Temp), cdate = Clim$Date,
                                     bdate = Lay_date_sum$Date, baseline = stats::lm(Mean ~ Sample_year, data = Lay_date_sum, weights = SE_calc),
                                     range = c(dataset$WindowOpen, dataset$WindowClose),
                                     stat = "mean", func = "lin",
                                     type = "relative",
                                     cmissing = "method2", cinterval = "day")

    #Combine this data with the laying date data
    SEM_data <- dplyr::bind_cols(Lay_date_sum,
                                 Window_dat$BestModelData %>%
                                   dplyr::select(-Sample_year))

  } else {

    SEM_data <- Clim[[1]]$BestModelData
    colnames(SEM_data)[1] <- "Mean"

    dataset <- Clim[[1]]$Dataset[1, ]

  }

  SEM_data <- SEM_data %>%
    dplyr::mutate(Sample_year = Sample_year - 2015)

  #Create the structure for an SEM where lay date can vary with both temperature and year and temperature changes over time
  SEM_mod <- ' Mean ~ Sample_year + climate
    climate ~ Sample_year
    Mean ~ 1'

  set.seed(666)

  #Fit a structural equation model with this structure
  #Include non-parametric boot strapping with 1000 iterations
  fit <- lavaan::cfa(SEM_mod, data = SEM_data, se = "bootstrap", bootstrap = i)

  #Extract the estimated relationship between temp and laying date
  #Extract the intercept for this relationship
  dataset$SEM_int   <- fit@ParTable$est[4]
  dataset$SEM_beta <- fit@ParTable$est[2]
  #Extract the SE for this relationship
  dataset$SEM_SE   <- fit@ParTable$se[2]

  #Add slope and SE of change over time
  dataset$Yr_beta <- fit@ParTable$est[1]
  dataset$Yr_SE   <- fit@ParTable$se[1]

  #Out of interest, we also want to extract the relationship between year and temp (i.e. how much has temperature changed over time in this window)
  dataset$CC_beta  <- fit@ParTable$est[3]
  dataset$CC_SE  <- fit@ParTable$se[3]

  if (internal) {

    #Finally, we will extract change in laying date over time.
    #We do this with our regular lay date information (i.e. we want to know how much laydate has changed over time in both pathways).
    dataset$Laydate_yr_beta <- summary(stats::lm(Mean ~ Sample_year, data = SEM_data, weight = SE_calc))$coef[2]
    dataset$Laydate_yr_SE   <- summary(stats::lm(Mean ~ Sample_year, data = SEM_data, weight = SE_calc))$coef[4]

  }

  if(!dummy_test){

    saveRDS(dataset, file = paste0(unique(dataset$Pop_ID), "_", unique(dataset$Species), "_SEM.RDS"))

  }

  return(dataset)

}
