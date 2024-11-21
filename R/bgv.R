#'  Between-group variance (BGV)
#'
#'  Between-group variance (BGV) is an absolute measure of inequality that
#'  considers all population subgroups. Subgroups are weighted according to
#'  their population share.
#'
#'  BGV is calculated as the weighted average of squared differences between
#'  the subgroup estimates and the setting average. Squared differences
#'  are weighted by each subgroup’s population share. For more information
#'  on this inequality measure see Schlotheuber (2022) below.
#'
#'  **Interpretation:** BGV has only positive values, with larger values
#'  indicating higher levels of inequality. BGV is 0 if there is no
#'  inequality. BGV is reported as the squared unit of the indicator.
#'  BGV is more sensitive to outlier estimates as it gives more weight to the
#'  estimates that are further from the setting average.
#'
#'  **Type of summary measure:** Complex; absolute; weighted
#'
#'  **Applicability:** Non-ordered dimensions of inequality with more than two
#'  subgroups
#'
#'  **Warning:** The confidence intervals are approximate and might be biased.
#'  See Ahn (2018) below for further information on the standard error formula.
#'
#' @param est The subgroup estimate. Estimates must be available for at least
#' 85% of subgroups.
#' @param se The standard error of the subgroup estimate. If this is missing,
#' 95% confidence intervals cannot be calculated.
#' @param pop The number of people within each subgroup.Population size must be
#' available for all subgroups.
#' @param conf.level Confidence level of the interval. Default is 0.95 (95%).
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      bgv(est = estimate,
#'          pop = population,
#'          se = se))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B, Luta G. Variance
#' estimation and confidence intervals for 11 commonly used health disparity
#' measures. JCO Clin Cancer Inform. 2018;2:1-19. doi:10.1200/CCI.18.00031.
#' @return The estimated BGV value, corresponding estimated standard error,
#' and confidence interval as a `data.frame`.
#' @export
#'
bgv <- function(est,
                se = NULL,
                pop,
                conf.level = 0.95,
                ...) {

  # Variable checks
  ## Stop
  if (anyNA(est) & sum(is.na(est)) / length(est) > .15) {
    stop('Estimates are missing in more than 15% of subgroups')
  }
  if (!is.null(est)) {
    if (!is.numeric(est))
      stop('Estimates need to be numeric')
  }
  if (anyNA(est)) {
    pop <- pop[!is.na(est)]
    if (!is.null(se))
      se <- se[!is.na(est)]
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
  ## Warning
  if (any(is.na(se)) | is.null(se)) {
    warning('Standard errors are missing in all or some subgroups, confidence
            intervals will not be computed')
  }

  # Calculate summary measure

  popsh <- pop / sum(pop)
  weighted.mean <- sum(popsh * est)
  bgv <- sum(popsh * (est - weighted.mean) ^ 2)

  # Calculate 95% confidence intervals

  se.formula <- NA
  lowerci <- NA
  upperci <- NA

  if (!any(is.na(se)) & !is.null(se)) {
    se2 <- sum((popsh ^ 2) * (se ^ 2) * ((est - weighted.mean) ^ 2))
    s2  <- sum((popsh ^ 2) * (se ^ 2))
    s4  <- sum((popsh ^ 4) * (se ^ 4))
    se4 <- sum((popsh ^ 2) * ((1 - popsh) ^ 2) * (se ^ 4))

    se.formula <- sqrt(4 * se2 + 2 * (s2 ^ 2 - s4 + se4))
    cilevel <- 1 - ((1 - conf.level) / 2)
    lowerci <- bgv - se.formula * qnorm(cilevel)
    upperci <- bgv + se.formula * qnorm(cilevel)
  }

  # Return data frame

  return(data.frame(measure = "bgv",
                    estimate = bgv,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci))
}
