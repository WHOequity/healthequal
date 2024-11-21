#'  Mean difference from mean (unweighted) (MDMU)
#'
#'  The mean difference from mean (MDM) is an absolute measure of inequality
#'  that shows the mean difference between each subgroup and the mean (e.g.
#'  the national average).
#'
#'  The unweighted version (MDMU) is calculated as the sum of the absolute
#'  differences between the subgroup estimates  and the mean, divided
#'  by the number of subgroups. All subgroups are weighted equally. For more
#'  information on this inequality measure see Schlotheuber (2022) below.
#'
#'  95% confidence intervals are calculated using a Monte Carlo simulation-based
#'  method. The dataset is simulated a large number of times (e.g. 100), with
#'  the mean and standard error of each simulated dataset being the same as the
#'  original dataset. MDMU is calculated for each of the simulated sample
#'  datasets. The 95% confidence intervals are based on the 2.5th and 97.5th
#'  percentiles of the MDMU results. See Ahn (2019) below for further
#'  information.
#'
#'  **Interpretation:** MDMU only has positive values, with larger values
#'  indicating higher levels of inequality. MDMU is 0 if there is no
#'  inequality. MDMU has the same unit as the indicator.
#'
#'  **Type of summary measure:** Complex; absolute; non-weighted
#'
#'  **Applicability:** Non-ordered dimensions of inequality with more than two
#'  subgroups
#'
#' @param est The subgroup estimate. Estimates must be available for at least
#' 85% of subgroups.
#' @param se The standard error of the subgroup estimate. If this is missing,
#' 95% confidence intervals cannot be calculated.
#' @param pop The number of people within each subgroup.Population size must be
#' available for all subgroups.
#' @param scaleval The scale of the indicator. For example, the scale of an
#' indicator measured as a percentage is 100. The scale of an indicator measured
#' as a rate per 1000 population is 1000. If this is missing, 95% confidence
#' intervals cannot be calculated.
#' @param setting_average The overall indicator average for the setting of
#' interest. Setting average must be unique for each setting, year and indicator
#' combination. If population (pop) is not specified for all subgroups, the
#' setting average is used for the calculation.
#' @param sim The number of simulations to estimate 95% confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the 95% confidence
#' interval simulation. Default is 123456.
#' @param force TRUE/FALSE statement to force calculation when more than 85% of
#' subgroup estimates are missing.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mdmu(est = estimate,
#'           se = se,
#'           pop = population,
#'           scaleval = indicator_scale))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B. Improved Monte Carlo
#' methods for estimating confidence intervals for eleven commonly used health
#' disparity measures. PLoS One. 2019 Jul 1;14(7).
#' @return The estimated MDMU value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
mdmu <- function(est,
                 se = NULL,
                 pop = NULL,
                 scaleval = NULL,
                 setting_average = NULL,
                 sim = NULL,
                 seed = 123456,
                 force = FALSE,
                 ...) {

  # Variable checks
  ## Stop
  if (!force) {
    if (anyNA(est) & sum(is.na(est)) / length(est) > .15) {
      stop('Estimates are missing in more than 15% of subgroups.
           Specify force=TRUE to allow missing values.')
    }
  }
  if (!is.null(est)) {
    if (!is.numeric(est))
      stop('Estimates need to be numeric')
  }
  if (anyNA(est)) {
    pop <- pop[!is.na(est)]
    if (!is.null(se))
      se <- se[!is.na(est)]
    if (!is.null(scaleval))
      scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if (length(est) <= 2) {
    stop('Estimates must be available for more than two subgroups')
  }
  if (!is.null(se)) {
    if (!is.numeric(se))
      stop('Standard errors need to be numeric')
  }
  if(is.null(pop) & is.null(setting_average)){
    stop('Specify either the population or the setting average variable')
  }
  if(!is.null(pop)){
    if(anyNA(pop)){
      stop('Population is missing in some subgroups')
    }
    if(!is.numeric(pop)){
      stop('Population variable needs to be numeric')
    }
    if(all(pop == 0)){
      stop('Population variable is of size 0 in all subgroups')
    }
  } else {
    if(!is.null(setting_average) & !is.numeric(setting_average)){
      stop('Setting average needs to be numeric')
    }
    if(!is.null(setting_average) & length(unique(setting_average))!=1){
      stop('Setting average not unique across subgroups')
    }
  }
  if (!is.null(scaleval)) {
    if (length(unique(scaleval)) != 1) {
      stop('Indicator scale variable must be consistent across subgroups,
         for the same indicator')
    }
  }
  ## Warning
  if (any(is.na(se)) | is.null(se)) {
    warning('Standard errors are missing in all or some subgroups,
            confidence intervals will not be computed')
  }
  if (any(is.na(scaleval)) | is.null(scaleval)) {
    warning('Indicator scale variable is missing,
            confidence intervals will not be computed')
  }

  # Calculate summary measure

  popsh <- pop / sum(pop)
  if (!is.null(pop) & is.null(setting_average)) {
    weighted_mean <- sum(popsh * est)
  } else {
    weighted_mean <- unique(setting_average)
  }

  n <- length(est)
  mdmu <- sum(abs(est - weighted_mean)) / n

  # Calculate 95% confidence intervals

  boot.lcl <- NA
  boot.ucl <- NA
  mdmu_sim <- c()

  if (!any(is.na(se)) & !is.null(se) &
      !any(is.na(scaleval)) & !is.null(scaleval)) {
    if (is.null(sim)) {
      sim <- 100
    }

    input_data <- data.frame(est,
                             se,
                             scaleval)
    set.seed(seed)

    for (j in 1:sim) {
      ## Simulate each estimate in the dataset
      simulated_data <- input_data %>%
        rowwise() %>%
        mutate(simulation = {
                   result <- if (scaleval != 100) {
                     repeat {
                       result <- rnorm(1,
                                       mean = est,
                                       sd = se)
                       if (result > 0)
                         break
                     }
                     result
                   } else {
                     repeat {
                       result <- rnorm(1,
                                       mean = est,
                                       sd = se)
                       if (result >= 0 & result <= 100)
                         break
                     }
                     result
                   }
                 }) %>%
        ungroup()

      ## Calculate weighted mean or use setting average
      if (!is.null(pop)) {
        mean_sim <- sum(popsh * simulated_data$simulation)
      } else {
        mean_sim  <- unique(setting_average)
      }

     ## Calculate summary measure using simulated estimates
      mdmu_sim[j] <- with(simulated_data,
                          (sum(abs(simulation - mean_sim)) / n))

    }

    boot.lcl <- quantile(mdmu_sim,
                         probs = c(0.025),
                         na.rm = TRUE)
    boot.ucl <- quantile(mdmu_sim,
                         probs = c(0.975),
                         na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "mdmu",
                    estimate = mdmu,
                    se = NA,
                    lowerci = boot.lcl,
                    upperci = boot.ucl))
}
