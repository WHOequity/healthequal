#' Index of Disparity (weighted) (IDISW)
#'
#'  The Index of Disparity (IDIS) is a relative measure of inequality that
#'  shows the average difference between each subgroup and the setting
#'  average, in relative terms. In the weighted version (IDISW), subgroups
#'  are weighted according to their population share.
#'
#'  IDISW is calculated as the weighted average of absolute differences
#'  between the subgroup estimates and the setting average, divided by
#'  the setting average, and multiplied by 100. Absolute differences are
#'  weighted by each subgroup’s population share. For more information
#'  on this inequality measure see Schlotheuber, A., & Hosseinpoor, A. R.
#'  (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and IDISW is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th centiles of the
#'  IDISW results.
#'
#'  **Interpretation:** IDISW has only positive values, with larger values
#'  indicating higher levels of inequality. IDISW is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate.
#'  Estimates must be available for all subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of IDISW cannot be calculated.
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
#'      idisw(pop = population,
#'            est = estimate,
#'            se,
#'            scaleval = indicator_scale
#'           )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated IDISW value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
idisw <- function(pop,
                  est,
                  se = NULL,
                  scaleval,
                  sim = NULL,
                  seed = 123456,...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(est)>.15){
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
  idisw <- 100 * sum(popsh * abs(est - weighted_mean)) / weighted_mean

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.ucl <- NA
  idisw_sim <- c()

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
      idisw_sim[j] <- with(
        simulated_data, 100 * (sum(popsh * abs(simulation - weighted_mean)) /
                              weighted_mean))

    }

    boot.lcl  <- quantile(idisw_sim, probs = c(0.025), na.rm = TRUE)
    boot.ucl <- quantile(idisw_sim, probs = c(0.975), na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "idisw",
                    estimate = idisw,
                    se = NA,
                    lowerci = boot.lcl[[1]],
                    upperci = boot.ucl[[1]])
         )
}

