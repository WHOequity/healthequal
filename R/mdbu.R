#' Mean difference from the best-performing subgroup (unweighted) (MDBU)
#'
#'  The Mean Difference from the Best-Performing Subgroup (MDB) is an absolute
#'  measure of inequality that shows the mean difference between each population
#'  subgroup and the subgroup with the best estimate. For the unweighted version
#'  (MDBU), all subgroups are weighted equally.
#'
#'  The unweighted version (MDBU) is calculated as the average of absolute
#'  differences between the subgroup estimates and the estimate for the
#'  best-performing subgroup, divided by the number of subgroups. For more
#'  information on this inequality measure see Schlotheuber, A., &
#'  Hosseinpoor, A. R. (2022) below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and MDBU is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  MDBU results.
#'
#'  **Interpretation:** MDBU only has positive values, with larger values
#'  indicating higher levels of inequality. MDBU is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; absolute; non-weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param est The subgroup estimate.
#'  Estimates must be available for at least 85% of subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of MDBU cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param favourable_indicator Records whether the indicator is
#'  favourable (1) or non-favourable (0). Favourable indicators measure
#'  desirable health events where the ultimate goal is
#'  to achieve a maximum level (such as skilled birth attendance).
#'  Non-favourable indicators measure undesirable health events where
#'  the ultimate goal is to achieve a minimum level (such as under-five
#'  mortality rate).
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
#'      mdbu(est = estimate,
#'           se,
#'           favourable_indicator,
#'           scaleval = indicator_scale
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated MDBU value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
mdbu <- function(est,
                 se = NULL,
                 favourable_indicator,
                 scaleval,
                 sim = NULL,
                 seed = 123456,...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(est)>.15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    if(!is.null(se)) se <- se[!is.na(est)]
    if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if(length(unique(favourable_indicator))!=1){
    stop('Favourable indicator not unique across subgroups')
  }

  ## Warning
  if(any(is.na(se)) | is.null(se))
    warning("Standard errors are missing in all or some subgroups, confidence
  intervals will not be computed.")

  # Calculate summary measure

  ref_est <- ifelse(favourable_indicator == 1, max(est), min(est))
  n <- length(est)
  mdbu <- sum(abs(est - ref_est)) / n

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.lcl <- NA
  mdbu_sim <- c()

  if(!any(is.na(se)) & !is.null(se)) {
    if(is.null(sim)){
      sim <- 100
    }

    input_data <- data.frame(est,
                             se,
                             scaleval,
                             favourable_indicator)

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
                    ifelse(favourable_indicator == 1,
                        max(simulated_data$simulation),
                        min(simulated_data$simulation)))

      # Calculate summary measure using simulated estimates
      mdbu_sim[j] <- with(
        simulated_data, (sum(abs(simulation - ref_estimate_sim)) / n))

    }

    boot.lcl <- quantile(mdbu_sim, probs = c(0.025), na.rm = TRUE)
    boot.ucl <- quantile(mdbu_sim, probs = c(0.975), na.rm = TRUE)
  }

  # Return data frame

  return(data.frame(measure = "mdbu",
                    estimate = mdbu,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}

