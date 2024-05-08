#' Coefficient of variation (COV)
#'
#'  The Coefficient of Variation (COV) is a relative measure of inequality
#'  that considers all population subgroups. Subgroups are weighted according
#'  to their population share.
#'
#'  COV is calculated by dividing the between-group standard
#'  deviation (BGSD) by the setting average and multiplying the fraction
#'  by 100. BGSD is calculated as the square root of the weighted average
#'  of squared differences between the subgroup estimates and the
#'  setting average. Squared differences are weighted by each subgroup’s
#'  population share. For more information on this inequality measure see
#'  Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and COV is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  COV results.
#'
#'  **Interpretation:** COV only has positive values, with larger values
#'  indicating higher levels of inequality. COV is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate. Estimates must be
#'  available for at least 85% of subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of COV cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param sim The number of simulations to estimate confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the confidence
#'  interval simulation. Default is 123456.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      covar(pop = population,
#'             est = estimate,
#'             se = se,
#'             scaleval = indicator_scale
#'            )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated COV value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
covar <- function(pop,
                  est,
                  se = NULL,
                  scaleval,
                  sim = NULL,
                  seed = 123456,
                  ...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est)) / length(est) > .15){
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
  weighted.mean <- sum(popsh * est)
  bgsd <- sqrt(sum(popsh * (est - weighted.mean)^2))
  covar <- bgsd / weighted.mean * 100

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.ucl <- NA
  covar_sim <- c()

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

      # Calculate weighted mean

      if(!is.null(pop)){
        mean_sim <- sum(popsh * simulated_data$simulation)
      }

      # Calculate summary measure using simulated estimates
      covar_sim[j] <- with(
        simulated_data,
        (sqrt(sum(popsh * (simulation - mean_sim)^2))) / mean_sim * 100)

    }

    boot.lcl  <- quantile(covar_sim, probs = c(0.025), na.rm = TRUE,
                          names = FALSE)
    boot.ucl <- quantile(covar_sim, probs = c(0.975), na.rm = TRUE,
                          names = FALSE)
  }

  # Return data frame

  return(data.frame(measure = "cov",
                    estimate = covar,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}

