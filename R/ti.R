#'  Theil index (TI)
#'
#'  The Theil index (TI) is a relative measure of inequality that considers
#'  all population subgroups. Subgroups are weighted according to their
#'  population share.
#'
#'  TI measures the extent to which the shares of the population and shares of
#'  the health indicator differ across subgroups, weighted by shares of the
#'  health indicator. TI is calculated as the sum of products of the natural
#'  logarithm of the share of the indicator of each subgroup, the share of the
#'  indicator of each subgroup and the population share of each subgroup.
#'  TI may be easily interpreted when multiplied by 1000. For more
#'  information on this inequality measure see Schlotheuber (2022) below.
#'
#'  **Interpretation:** TI is 0 if there is no inequality. Greater absolute
#'  values indicate higher levels of inequality. TI is more sensitive to
#'  differences further from the setting average (by the use of the logarithm).
#'  TI has no unit.
#'
#'  **Type of summary measure:** Complex; relative; weighted
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
#' @param force TRUE/FALSE statement to force calculation when more than 85% of
#' subgroup estimates are missing.
#' @param ...  Further arguments passed to or from other methods.
#' @examples
#' # example code
#' data(NonorderedSample)
#' head(NonorderedSample)
#' with(NonorderedSample,
#'      ti(est = estimate,
#'         se = se,
#'         pop = population))
#' @references Schlotheuber, A, Hosseinpoor, AR. Summary measures of health
#' inequality: A review of existing measures and their application. Int J
#' Environ Res Public Health. 2022;19(6):3697. doi:10.3390/ijerph19063697.
#' @references Ahn J, Harper S, Yu M, Feuer EJ, Liu B, Luta G. Variance
#' estimation and confidence intervals for 11 commonly used health disparity
#' measures. JCO Clin Cancer Inform. 2018;2:1-19. doi:10.1200/CCI.18.00031.
#' @return The estimated TI value, corresponding estimated standard error,
#'  and confidence interval as a `data.frame`.
#' @export
#'
ti <- function(est,
               se = NULL,
               pop,
               conf.level = 0.95,
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
  ## Warning
  if (any(is.na(se)) | is.null(se)) {
    warning('Standard errors are missing in all or some subgroups, confidence
            intervals will not be computed')
  }

  # Calculate summary measure

  est_nonzero <- ifelse(est == 0, 0.000001, est)
  popsh <- pop / sum(pop)
  weighted_mean <- sum(popsh * est_nonzero)
  rj <- est_nonzero / weighted_mean
  ti <- sum(popsh * rj * log(rj)) * 1000

  # Calculate 95% confidence intervals

  se.formula <- NA
  lowerci <- NA
  upperci <- NA

  if (sum(is.na(se) == 0) & !is.null(se)) {
    ti_var_prep <- sum(rj * (1 + log(rj)) * popsh)
    se.formula <- sqrt(sum(((1 + log(rj) - ti_var_prep) ^ 2) *
                             ((popsh ^ 2) * (se ^ 2) / (weighted_mean ^2))))
    cilevel <- 1 - ((1 - conf.level) / 2)
    lowerci <- ti - se.formula * qnorm(cilevel)
    upperci <- ti + se.formula * qnorm(cilevel)
  }

  # Return data frame

  return(data.frame(measure = "ti",
                    estimate = ti,
                    se = se.formula,
                    lowerci = lowerci,
                    upperci = upperci))
}
