#' Mean difference from a reference subgroup (unweighted) (MDRU)
#'
#'  The Mean Difference from a Reference Subgroup (MDR) is an absolute measure
#'  of inequality that shows the mean difference between each population
#'  subgroup and a reference subgroup. For the unweighted version (MDRU), all
#'  subgroups are weighted equally.
#'
#'  The unweighted version (MDRU) is calculated as the average of absolute
#'  differences between the subgroup estimates and the estimate for the
#'  reference subgroup, divided by the number of subgroups. For more information
#'  on this inequality measure see Schlotheuber, A., & Hosseinpoor, A. R. (2022)
#'  below.
#'
#'  95% confidence intervals are calculated using a methodology of simulated
#'  estimates. The dataset is simulated a large number of times (e.g., 100)
#'  and MDRU is calculated for each of the simulated samples. The 95%
#'  confidence intervals are based on the 2.5th and 97.5th percentiles of the
#'  MDRU results.
#'
#'  **Interpretation:** MDRU only has positive values, with larger values
#'  indicating higher levels of inequality. MDRU is zero if there is no
#'  inequality.
#'
#'  **Type of summary measure:** Complex; absolute; non-weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#' @param est The subgroup estimate.
#'  Estimates must be available for all subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of MDRU cannot be calculated.
#' @param scaleval The scale of the indicator. For example, the
#'  scale of an indicator measured as a percentage is 100. The
#'  scale of an indicator measured as a rate per 1000 population is 1000.
#' @param reference_subgroup Identifies a reference subgroup with the value of
#'  1.
#' @param sim The number of simulations to estimate 95% confidence intervals
#' @param seed The random number generator (RNG) state for the 95% confidence
#'  interval simulation
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mdru(est = estimate,
#'           se = se,
#'           scaleval = indicator_scale,
#'           reference_subgroup
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#' @return The estimated MDRU value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
mdru <- function(est,
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
    reference_subgroup<- reference_subgroup[!is.na(est)]
    if(!is.null(se)) se <- se[!is.na(est)]
    if(!is.null(scaleval)) scaleval <- scaleval[!is.na(est)]
    est <- est[!is.na(est)]
  }
  if(sum(reference_subgroup)!=1){
    stop('The reference group is missing')
  }

  ## Warning
  if(any(is.na(se)) | is.null(se))
    warning("Standard errors are missing in all or some subgroups, confidence
  intervals will not be computed.")

  # Calculate summary measure

  ref_est <- est[reference_subgroup==1]
  n <- length(est)
  mdru <- sum(abs(est - ref_est)) / n

  # Calculate 95% confidence intervals

  se.formula <- NA
  boot.lcl <- NA
  boot.lcl <- NA
  mdru_sim <- c()

  if(is.null(sim)){
    sim <- 100
  }

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

    simulated_data <- simulated_data %>%
      mutate(ref_estimate_sim =
               simulated_data$simulation[reference_subgroup == 1])

    # Calculate summary measure using simulated estimates
    mdru_sim[j] <- with(
      simulated_data, (sum(abs(simulation - ref_estimate_sim)) / n))

  }

  boot.lcl <- quantile(mdru_sim, probs = c(0.025), na.rm = TRUE)
  boot.ucl <- quantile(mdru_sim, probs = c(0.975), na.rm = TRUE)

  # Return data frame

  return(data.frame(measure = "mdru",
                    estimate = mdru,
                    lowerci = boot.lcl,
                    upperci = boot.ucl)
         )
}

