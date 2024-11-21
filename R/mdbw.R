#'  Mean difference from best-performing subgroup (weighted) (MDBW)
#'
#'  The mean difference from the best-performing subgroup (MDB) is an absolute
#'  measure of inequality that shows the mean difference between each population
#'  subgroup and the subgroup with the best estimate. The best-performing subgroup is
#'  the subgroup with the highest value in the case of favourable indicators
#'  and the subgroup with the lowest value in the case of adverse indicators.
#'
#'  The weighted version (MDBW) is calculated as the weighted average of
#'  absolute differences between the subgroup estimates and the estimate for the
#'  best-performing subgroup, divided by the number of subgroups. Subgroups are
#'  weighted according to their population share. For more information on this
#'  inequality measure see Schlotheuber (2022) below.
#'
#'  95% confidence intervals are calculated using a Monte Carlo simulation-based
#'  method. The dataset is simulated a large number of times (e.g. 100), with
#'  the mean and standard error of each simulated dataset being the same as the
#'  original dataset. MDBW is calculated for each of the simulated sample
#'  datasets. The 95% confidence intervals are based on the 2.5th and 97.5th
#'  percentiles of the MDBW results. See Ahn (2019) below for further
#'  information.
#'
#'  **Interpretation:** MDBW only has positive values, with larger values
#'  indicating higher levels of inequality. MDBW is 0 if there is no
#'  inequality. MDBW has the same unit as the indicator.
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
#' @param favourable_indicator Records whether the indicator is favourable (1)
#' or adverse (0). Favourable indicators measure desirable health events where
#' the ultimate goal is to achieve a maximum level (such as skilled birth
#' attendance). Adverse indicators measure undesirable health events where the
#' ultimate goal is to achieve a minimum level (such as under-five mortality
#' rate).
#' @param sim The number of simulations to estimate 95% confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the 95% confidence
#'  interval simulation. Default is 123456.
#' @param force TRUE/FALSE statement to force calculation when more than 85% of
#' subgroup estimates are missing.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mdbw(est = estimate,
#'           se = se,
#'           pop = population,
#'           favourable_indicator,
#'           scaleval = indicator_scale))
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated MDBW value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @importFrom dplyr %>% rowwise mutate ungroup
#' @export
#'
mdbw <- function(est,
                 se = NULL,
                 pop,
                 favourable_indicator,
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
  if (!all(favourable_indicator %in% c(0,1))) {
    stop('Favourable indicator variable must contain 0 or 1')
  }
  if (length(unique(favourable_indicator)) != 1) {
    stop('Favourable indicator variable must be consistent across subgroups,
         for the same indicator')
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

  ref_est <- ifelse(favourable_indicator == 1,
                    max(est),
                    min(est))
  popsh <- pop/sum(pop)
  mdbw <- sum(popsh * abs(est - ref_est))

  # Calculate 95% confidence intervals

  boot.lcl2 <- NA
  boot.ucl2 <- NA
  mdbw_sim <- c()

  if (!any(is.na(se)) & !is.null(se) &
      !any(is.na(scaleval)) & !is.null(scaleval)) {
    if (is.null(sim)) {
      sim <- 100
    }

    input_data <- data.frame(est,
                             se,
                             popsh,
                             scaleval,
                             favourable_indicator)

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

      simulated_data <- simulated_data %>%
        mutate(ref_estimate_sim =
                 ifelse(favourable_indicator == 1,
                        max(simulated_data$simulation),
                        min(simulated_data$simulation)))

      ## Calculate summary measure using simulated estimates
      mdbw_sim[j] <- with(simulated_data,
                          (sum(popsh * abs(simulation - ref_estimate_sim))))

    }

    boot.lcl <- quantile(mdbw_sim,
                         probs = c(0.025),
                         na.rm = TRUE)
    boot.ucl <- quantile(mdbw_sim,
                         probs = c(0.975),
                         na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "mdbw",
                    estimate = mdbw,
                    se = NA,
                    lowerci = boot.lcl,
                    upperci = boot.ucl))
}
