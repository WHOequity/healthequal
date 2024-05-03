#' Between-group variance (BGV)
#'
#'  Between-Group Variance (BGV) is an absolute measure of inequality that
#'  considers all population subgroups. Subgroups are weighted according to
#'  their population share.
#'
#'  BGV is calculated as the weighted average of squared differences between
#'  the subgroup estimates  and the setting average. Squared differences
#'  are weighted by each subgroup’s population share. For more information
#'  on this inequality measure see Schlotheuber, A., & Hosseinpoor, A. R.
#'  (2022) below.
#'
#'  **Interpretation:** BGV has only positive values, with larger values
#'  indicating higher levels of inequality. BGV is zero if there is no
#'  inequality.
#'  BGV is more sensitive to outlier estimates as it gives more weight to the
#'  estimates that are further from the setting average. It is reported as the
#'  squared unit of the health indicator.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
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
#'  If this is missing, 95% confidence intervals of BGV cannot be calculated.
#' @param conf.level confidence level of the interval.
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      bgv(pop = population,
#'           est = estimate,
#'           se = se
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
#' @return The estimated BGV value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
bgv <- function(pop,
                est,
                se = NULL,
                conf.level = 0.95,
                ...) {

  # Variable checks
  ## Stop
  if(anyNA(est) & sum(is.na(est)) / length(est) > .15){
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if(anyNA(est)){
    pop <- pop[!is.na(est)]
    if(!is.null(se)) se <- se[!is.na(est)]
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

  popsh <- pop / sum(pop)
  weighted.mean <- sum(popsh * est)
  bgv <- sum(popsh * (est - weighted.mean)^2)

  # Calculate 95% confidence intervals

  se.formula <- NA
  lowerci <- NA
  upperci <- NA

  if(!any(is.na(se)) & !is.null(se)) {

    se2 <- sum((popsh^2) * (se^2) * ((est - weighted.mean)^2))
    s2  <- sum((popsh^2) * (se^2))
    s4  <- sum((popsh^4) * (se^4))
    se4 <- sum((popsh^2) * ((1 - popsh)^2) * (se^4))

    se.formula <- sqrt(4 * se2 + 2 * (s2^2 - s4 + se4))
    cilevel <- 1 - ((1-conf.level)/2)
    lowerci <- bgv - se.formula * qnorm(cilevel)
    upperci <- bgv + se.formula * qnorm(cilevel)

  }

  # Return data frame

  return(data.frame(measure = "bgv",
                    estimate = bgv,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci)
         )
}

