#' Mean difference from mean (weighted) (MDMW)
#'
#'  The Mean Difference from Mean (MDM) is an absolute measure of inequality
#'  that shows the mean difference between each subgroup and the setting
#'  average. For the weighted version (MDMW), subgroups are weighted according
#'  to their population share.
#'
#'  The weighted version (MDMW) is calculated as the weighted average of
#'  absolute differences between the subgroup estimates and the setting average.
#'  Absolute differences are weighted by each subgroup's population share. For
#'  more information on this inequality measure see Schlotheuber, A., &
#'  Hosseinpoor, A. R. (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and MDMW is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  MDMW results.
#'
#'  **Interpretation:** MDMW only has positive values, with larger values
#'  indicating higher levels of inequality. MDMW is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate.
#'  Estimates must be available for all subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of MDMW cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param sim The number of simulations to estimate 95% confidence intervals
#' @param seed The random number generator (RNG) state for the 95% confidence
#'  interval simulation
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mdmw(pop = population,
#'           est = estimate,
#'           se = se,
#'           scaleval = indicator_scale
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated MDMW value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
mdmw <- function(pop,
                 est,
                 se = NULL,
                 scaleval,
                 sim = NULL,
                 seed = 123456, ...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(est) > .15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    pop <- pop[!is.na(est)]
    if(!is.null(se)) se <- se[!is.na(est)]
    if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if(anyNA(pop)){
    stop('Population is missing in some subgroups')
  }
  if(!is.numeric(pop)){
    stop('Population needs to be numeric')
  }
  if(all(pop == 0)){
    stop('The population is of size 0 in all cells')
  }

  ## Warning
  if(any(is.na(se)) | is.null(se))
    warning("Standard errors are missing in all or some subgroups, confidence
            intervals will not be computed.")

  # Calculate summary measure

  popsh <- pop/sum(pop)
  weighted_mean <- sum(popsh * est)
  mdmw <- sum(popsh * abs(est - weighted_mean))

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.ucl <- NA
  mdmw_sim <- c()

  if(!any(is.na(se)) & !is.null(se)) {
    if(is.null(sim)){
      sim <- 100
    }

    input_data <- data.frame(est,
                             se,
                             popsh,
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
                     if (result > 0) break
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

      # Calculate summary measure using simulated estimates
      weighted_mean_sim <- with(simulated_data, sum(popsh * simulation))
      mdmw_sim[j] <- with(
        simulated_data, sum(popsh * (abs(simulation - weighted_mean_sim))))

    }

    boot.lcl <- quantile(mdmw_sim, probs = c(0.025), na.rm = TRUE)
    boot.ucl <- quantile(mdmw_sim, probs = c(0.975), na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "mdmw",
                    estimate = mdmw,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}

