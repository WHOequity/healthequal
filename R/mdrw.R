#' Mean difference from a reference subgroup (weighted) (MDRW)
#'
#'  An absolute measure of inequality that shows the mean difference between
#'  each population subgroup and a reference subgroup. In the weighted
#'  version (MDRW), subgroups are weighted according to their population share.
#'
#'  The weighted version (MDRW) is calculated as the weighted average of
#'  absolute differences between the subgroup estimates and the estimate for the
#'  reference subgroup. Absolute differences are weighted by each subgroup’s
#'  population share. For more information on this inequality measure see
#'  Schlotheuber, A., & Hosseinpoor, A. R. (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and MDRW is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  MDRW results.
#'
#'  **Interpretation:** MDRW only has positive values, with larger values
#'  indicating higher levels of inequality. MDRW is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate.
#'  Estimates must be available for at least 85% of subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of MDRW cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param reference_subgroup Identifies a reference subgroup with the value of
#'  1.
#' @param sim The number of simulations to estimate 95% confidence intervals.
#' Default is 100.
#' @param seed The random number generator (RNG) state for the 95% confidence
#'  interval simulation. Default is 123456.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mdrw(pop = population,
#'           est = estimate,
#'           se = se,
#'           scaleval = indicator_scale,
#'           reference_subgroup
#'          )
#'      )
#'
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International journal of
#'  environmental research and public health, 19 (6), 3697.
#' @return The estimated MDRW value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @importFrom dplyr %>% rowwise mutate ungroup
#' @export
#'
mdrw <- function(pop,
                 est,
                 se = NULL,
                 scaleval,
                 reference_subgroup,
                 sim = NULL,
                 seed = 123456,...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(est) > .15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    pop <- pop[!is.na(est)]
    reference_subgroup <- reference_subgroup[!is.na(est)]
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
  if(sum(reference_subgroup)!=1){
    stop('The reference group is missing')
  }

  ## Warning
  if(any(is.na(se)) | is.null(se))
    warning("Standard errors are missing in all or some subgroups, confidence
  intervals will not be computed.")

  # Calculate summary measure

  ref_est <- est[reference_subgroup == 1]
  popsh <- pop / sum(pop)
  mdrw <- sum(popsh * abs(est - ref_est))

  # Calculate 95% confidence intervals

  boot.lcl <- NA
  boot.lcl <- NA
  mdrw_sim <- c()

  sim <- 100

  input_data <- data.frame(est,
                           se,
                           scaleval,
                           reference_subgroup)

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

    simulated_data <- simulated_data %>%
      mutate(ref_estimate_sim =
               simulated_data$simulation[reference_subgroup == 1])

    # Calculate summary measure using simulated estimates
    mdrw_sim[j] <- with(
      simulated_data, (sum(popsh * abs(simulation - ref_estimate_sim))))

  }

  boot.lcl <- quantile(mdrw_sim, probs = c(0.025), na.rm = TRUE,
                          names = FALSE)
  boot.ucl <- quantile(mdrw_sim, probs = c(0.975), na.rm = TRUE,
                          names = FALSE)

  # Return data frame

  return(data.frame(measure = "mdrw",
                    estimate = mdrw,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}

