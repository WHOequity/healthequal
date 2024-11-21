#'  Between-group standard deviation (BGSD)
#'
#'  Between-group standard deviation (BGSD) is an absolute measure of inequality
#'  that considers all population subgroups. Subgroups are weighted according
#'  to their population share.
#'
#'  BGSD is calculated as the square root of the weighted average of squared
#'  differences between the subgroup estimates and the setting average.
#'  Squared differences are weighted by each subgroup’s population share.
#'  For more information on this inequality measure see Schlotheuber (2022)
#'  below.
#'
#'  95% confidence intervals are calculated using a Monte Carlo simulation-based
#'  method. The dataset is simulated a large number of times (e.g. 100), with
#'  the mean and standard error of each simulated dataset being the same as the
#'  original dataset. BGSD is calculated for each of the simulated sample
#'  datasets. The 95% confidence intervals are based on the 2.5th and 97.5th
#'  percentiles of the BGSD results. See Ahn (2019) below for further
#'  information.
#'
#'  **Interpretation:** BGSD has only positive values, with larger values
#'  indicating higher levels of inequality. BGSD is 0 if there is no
#'  inequality. BGSD has the same unit as the health indicator.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
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
#' @param sim The number of simulations to estimate confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the confidence
#'  interval simulation. Default is 123456.
#' @param force TRUE/FALSE statement to force calculation when more than 85% of
#' subgroup estimates are missing.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      bgsd(est = estimate,
#'           se = se,
#'           pop = population,
#'           scaleval = indicator_scale))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B. Improved Monte Carlo
#' methods for estimating confidence intervals for eleven commonly used health
#' disparity measures. PLoS One. 2019 Jul 1;14(7).
#' @return The estimated BGSD value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
bgsd <- function(est,
                 se = NULL,
                 pop,
                 scaleval = NULL,
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
  if (anyNA(pop)) {
    stop('Population is missing in some subgroups')
  }
  if (!is.numeric(pop)) {
    stop('Population variable needs to be numeric')
  }
  if (all(pop == 0)) {
    stop('Population variable is of size 0 in all subgroups')
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
  weighted.mean <- sum(popsh * est)
  bgsd <- sqrt(sum(popsh * (est - weighted.mean) ^ 2))

  # Calculate 95% confidence intervals

  boot.lcl <- NA
  boot.ucl <- NA
  bgsd_sim <- c()

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

      ## Calculate weighted mean
      if (!is.null(pop)) {
        mean_sim <- sum(popsh * simulated_data$simulation)
      }

      ## Calculate summary measure using simulated estimates
      bgsd_sim[j] <- with(simulated_data,
                          sqrt(sum(popsh * (simulation - mean_sim) ^ 2)))

    }

    boot.lcl  <- quantile(bgsd_sim,
                          probs = c(0.025),
                          na.rm = TRUE)
    boot.ucl <- quantile(bgsd_sim,
                         probs = c(0.975),
                         a.rm = TRUE)

  }

  # Return data frame

  return(data.frame(measure = "bgsd",
                    estimate = bgsd,
                    se = NA,
                    lowerci = boot.lcl,
                    upperci = boot.ucl))
}
