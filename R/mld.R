#'  Mean log deviation (MLD)
#'
#'  The Mean Log Deviation (MLD) is a relative measure of inequality
#'  that considers all population subgroups. Subgroups are weighted
#'  according to their population share.
#'
#'  MLD is calculated as the sum of products between the negative
#'  natural logarithm of the share of the indicator of each subgroup
#'  and the population share of each subgroup. MLD may be more easily
#'  readable when multiplied by 1000. For more information on this
#'  inequality measure see Schlotheuber, A., & Hosseinpoor, A. R.
#'  (2022) below.
#'
#'  **Interpretation:** MLD is zero if there is no inequality. Greater absolute
#'  values indicate higher levels of inequality. MLD is more sensitive to
#'  differences further from the setting average (by the use of the logarithm).
#'
#'  **Type of summary measure:** Complex; relative; weighted
#'
#'  **Applicability:** Non-ordered; more than two subgroups
#'
#'  **Warning:** The confidence intervals are approximate
#'  and might be biased. See Ahn J. et al. (1978) below for
#'  further information on the standard error formula.
#'
#' @param pop The number of people within each subgroup.
#'  Population size must be available for all subgroups.
#' @param est The subgroup estimate. Estimates must be
#'  available for all subgroups.
#' @param se The standard error of the subgroup estimate.
#'  If this is missing, 95% confidence intervals of MLD cannot be calculated.
#' @param conf.level confidence level of the interval.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      mld(pop = population,
#'          est = estimate,
#'          se = se
#'          )
#'      )
#' @references Schlotheuber, A., & Hosseinpoor, A. R. (2022).
#' Summary measures of health inequality: A review of existing
#'  measures and their application. International Journal of
#'  Environmental Research and Public Health, 19 (6), 3697.
#'
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B, Luta G. Variance
#'  Estimation and Confidence Intervals for 11 Commonly Used
#'  Health Disparity Measures. JCO Clin Cancer Inform. 2018
#'  Dec;2:1--19.
#'
#' @return The estimated MLD value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
mld <- function(pop,
                est,
                se = NULL,
                conf.level = 0.95,...){

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est))/length(pop) > .15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    pop <- pop[!is.na(est)]
    se <- se[!is.na(est)]
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
  warning("Standard errors contain missing values, confidence
                             intervals will not be computed.")

  # Calculate summary measure

  est_nonzero <- ifelse(est == 0, 0.000001, est)
  popsh <- pop / sum(pop)
  weighted_mean <- sum(popsh * est_nonzero)
  mld <- sum(popsh * (- log(est_nonzero / weighted_mean))) * 1000

  # Calculate 95% confidence intervals
  se.formula <- NA
  lowerci <- NA
  upperci <- NA

  if(sum(is.na(se) == 0) & !is.null(se)){

    rj <- est_nonzero / weighted_mean
    se.formula <-
      sqrt(sum((((se^2) * (popsh^2)) / (weighted_mean^2)) * ((1 - (1/rj))^2)))
    cilevel <- 1 - ((1-conf.level)/2)
    lowerci <- mld - se.formula * qnorm(cilevel)
    upperci <- mld + se.formula * qnorm(cilevel)

    }

  # Return data frame

  return(data.frame(measure = "mld",
                    estimate = mld,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci)
         )
}

