#' Index of Disparity (unweighted) (IDISU)
#'
#'  The Index of Disparity (IDIS) is a relative measure of inequality that
#'  shows the average difference between each subgroup and the setting
#'  average, in relative terms. In the unweighted version (IDISU),
#'  all subgroups are weighted equally.
#'
#'  IDISU is calculated as the average of absolute differences between the
#'  subgroup estimates and the setting average, divided by the number of
#'  subgroups and the setting average, and multiplied by 100. For more
#'  information on this inequality measure see Schlotheuber, A., &
#'  Hosseinpoor, A. R. (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and IDISU is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  IDISU results.
#'
#'  **Interpretation:** IDISU has only positive values, with larger values
#'  indicating higher levels of inequality. IDISU is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; relative; non-weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate.
#'  Estimates must be available for at least 85% of subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of IDISU cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param setting_average The reported setting average. Setting average must
#' be unique for each setting, year, indicator combination.
#' If population is not specified for all subgroups, the setting average is
#' used.
#' @param sim The number of simulations to estimate 95% confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the 95% confidence
#' interval simulation. Default is 123456.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      idisu(pop = population,
#'            est = estimate,
#'            se = se,
#'            scaleval = indicator_scale
#'           )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated IDISU value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @importFrom dplyr %>% rowwise mutate ungroup
#' @export
#'
idisu <- function(pop=NULL,
                  est,
                  se = NULL,
                  scaleval,
                  setting_average = NULL,
                  sim = NULL,
                  seed = 123456,...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(est)>.15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    if(!is.null(pop)) pop <- pop[!is.na(est)]
    if(!is.null(setting_average)) setting_average <-
        setting_average[!is.na(est)]
    if(!is.null(se)) se <- se[!is.na(est)]
    if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if(is.null(pop) & is.null(setting_average)){
    stop('Population and setting average are both missing')
  }
  if(!is.null(pop)){
    if(anyNA(pop)){
      stop('Population is missing in some subgroups')
    }
    if(!is.numeric(pop)){
      stop('Population needs to be numeric')
    }
    if(all(pop == 0)){
      stop('The population is of size 0 in all cells')
    }
  } else {
    if(!is.null(setting_average) & !is.numeric(setting_average)){
      stop('Setting average not numeric')
    }
    if(!is.null(setting_average) & length(unique(setting_average))!=1){
      stop('Setting average not unique across subgroups')
    }
  }

  ## Warning
  if(any(is.na(se)) | is.null(se))
    warning("Standard errors are missing in all or some subgroups, confidence
  intervals will not be computed.")

  # Calculate summary measure

  if(!is.null(pop) & is.null(setting_average)){
    popsh <- pop/sum(pop)
    weighted_mean <- sum(popsh * est)
  } else{
    weighted_mean <-unique(setting_average)
  }
  n <- length(est)
  idisu <- (100/n) * sum(abs(est - weighted_mean)) / weighted_mean

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.ucl <- NA
  idisu_sim <- c()

  if(!any(is.na(se)) & !is.null(se)) {
    if(is.null(sim)){
      sim <- 100
    }

    input_data <- data.frame(est,
                             se,
                             scaleval)

    set.seed(seed)

    for (j in 1:sim) {

      # Simulate each estimate in the dataset
      simulated_data <- input_data %>%
        rowwise() %>%
        mutate(simulation =
                 {result <- if (scaleval != 100) {
                   repeat {
                     result <- rnorm(1, mean = est, sd = se)
                     if (result >= 0) break
                     }
                   result
                   } else {
                     repeat {
                       result <- rnorm(1, mean = est, sd = se)
                       if (result >= 0 & result <= 100) break
                       }
                     result
                     }
                 }) %>%
        ungroup()

      # Calculate weighted mean or use setting average

      if(!is.null(pop)){
        mean_sim <- sum(popsh * simulated_data$simulation)
      } else {
        mean_sim  <- unique(setting_average)
      }

       # Calculate summary measure using simulated estimates
        idisu_sim[j] <- with(
        simulated_data,((100/n) * sum(abs(simulation - mean_sim)) /
                          mean_sim))

    }

    boot.lcl  <- quantile(idisu_sim, probs = c(0.025), na.rm = TRUE)
    boot.ucl <- quantile(idisu_sim, probs = c(0.975), na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "idisu",
                    estimate = idisu,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}
